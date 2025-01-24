// Copyright (C) 2022-2025, VigilantDoomer
//
// This file is part of VigilantBSP program.
//
// VigilantBSP is free software: you can redistribute it
// and/or modify it under the terms of GNU General Public License
// as published by the Free Software Foundation, either version 2 of
// the License, or (at your option) any later version.
//
// VigilantBSP is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with VigilantBSP.  If not, see <https://www.gnu.org/licenses/>.
package main

//go:generate go run gen/codegen.go -- --target=znodegen.go --include="nodegen.go;node_intro.go;node_vmap.go;node_outro.go;picknode.go;diffgeometry.go;convexity.go;multitree_plain.go;multiformat_tree.go;zdefs.go;superblocks.go;zenscore.go;mylogger.go;intgeometry.go;zensideness.go;node_rearrange.go;stknode.go"
//go:generate go run gen/codegen.go -- --target=rejectFAST.go --include="reject.go;rejectRMB.go;rejectDFS.go;rejectLOS.go;rejectdefs.go"
//go:generate go run gen/codegen.go -- --target=rejectSYMM.go --include="reject.go;rejectRMB.go;rejectDFS.go;rejectLOS.go;rejectSymmDefs.go"

import (
	"bytes"
	"encoding/binary"
	"fmt"
	"math"
	"os"
	"reflect"
)

const BRANCH_NODES = 1
const BRANCH_REJECT = 2
const BRANCH_BLOCKMAP = 3

// ScrollerGroup is a combination of a subset of linedef parameters, which
// together identify a single group
type ScrollerGroup struct {
	StartVertex uint16
	EndVertex   uint16
	FrontSdef   uint16
}

type Level struct {
	BlockmapLumpIdx     int
	RejectLumpIdx       int
	SSectorsLumpIdx     int
	SegsLumpIdx         int
	NodesLumpIdx        int
	VerticesLumpIdx     int
	LinedefsLumpIdx     int
	wriBus              *WriteBusControl
	newLines            WriteableLines
	RejectChan          chan RejectResponse
	NodesChan           chan NodesResult
	BlockmapLumpChannel chan []byte
	WroteRejectAlready  bool
	le                  []LumpEntry
	f                   *os.File // input file descriptor, is not always loaded into this structure
	fc                  *FileControl
	mapName             string
}

// DoLevel is executed once per level, but Level struct is allocated once and is
// reused for all levels, hence it must do reinitialization of all fields
func (l *Level) DoLevel(le []LumpEntry, idx int, rejectsize map[int]uint32,
	troll *Troll, action *ScheduledLump, rejectStart uint32, f *os.File,
	wriBus *WriteBusControl, fileControl *FileControl) {

	var linedefs []Linedef
	var vertices []Vertex
	var hexenThings []HexenThing
	var hexenLinedefs []HexenLinedef
	var absLines AbstractLines
	var solidLines SolidLines
	l.newLines = nil
	var linesToIgnore []bool
	var sidedefs []Sidedef
	var sectors []Sector
	var bounds LevelBounds
	var bcontrol chan BconRequest   // for internally built solid-only blockmap
	var bgenerator chan BgenRequest // for internally built solid-only blockmap
	l.BlockmapLumpChannel = nil     // for generating regular blockmap lump
	l.RejectChan = nil              // for generating reject
	l.NodesChan = nil               // for nodes, segs, ssectors
	l.BlockmapLumpIdx = 0
	l.RejectLumpIdx = 0
	l.SSectorsLumpIdx = 0
	l.SegsLumpIdx = 0
	l.NodesLumpIdx = 0
	l.VerticesLumpIdx = 0
	l.LinedefsLumpIdx = 0
	l.wriBus = wriBus
	l.le = nil
	l.f = nil
	l.WroteRejectAlready = false
	l.mapName = GetLumpName(le, action)
	loadedThings := false
	loadedLinedefsAndVertices := false
	blockmapNeedsSectors := config.RemoveNonCollideable
	rejectSkip := config.Reject == REJECT_ZEROFILLED || config.Reject == REJECT_DONTTOUCH
	rejectDone := rejectSkip
	blockmapDone := !config.RebuildBlockmap
	Log.Printf("Processing level %s:\n", l.mapName)
	if action.LevelFormat == FORMAT_HEXEN {
		Log.Printf("Level is in Hexen format.\n")
	}
	for _, subaction := range action.Level {
		idx = subaction.DirIndex
		bname := ByteSliceBeforeTerm(le[idx].Name[:])
		if bytes.Equal(bname, []byte("REJECT")) && config.Reject != REJECT_DONTTOUCH {
			l.RejectLumpIdx = idx
			if config.Reject == REJECT_ZEROFILLED {
				calcrejectsize, leidx_ok := rejectsize[action.DirIndex]
				if !leidx_ok {
					panic("Rejectsize was not stored for key = <level marker lump DirIndex>")
				}
				le[idx].Size = calcrejectsize
				private_offset := troll.PopOffset(calcrejectsize)
				le[idx].FilePos = rejectStart + private_offset
				Log.Printf("Lump number %d (%s) has its size set to %d bytes.\n", idx, "REJECT", le[idx].Size)
			} else {
				// for handleRejectNagging possibility (otherwise prefer to keep
				// less initialized pointers)
				l.le = le
				l.f = f
				l.fc = fileControl
			}
		} else if bytes.Equal(bname, []byte("BLOCKMAP")) && config.RebuildBlockmap {
			l.BlockmapLumpIdx = idx
		} else if bytes.Equal(bname, []byte("SSECTORS")) && config.RebuildNodes {
			l.SSectorsLumpIdx = idx
		} else if bytes.Equal(bname, []byte("SEGS")) && config.RebuildNodes {
			l.SegsLumpIdx = idx
		} else if bytes.Equal(bname, []byte("NODES")) && config.RebuildNodes {
			l.NodesLumpIdx = idx
		} else {
			copyLump := true
			if bytes.Equal(bname, []byte("LINEDEFS")) {
				l.LinedefsLumpIdx = idx
				if action.LevelFormat == FORMAT_DOOM {
					// TODO for format doom, support zokumbsp's scrolling lines
					// for format Hexen need not supported as of yet (different
					// action sets and action field size). However, disable it
					// if nodes building is disabled, as we only write
					// lines, etc. when building nodes.
					cntLinedefs := le[idx].Size / DOOM_LINEDEF_SIZE
					linedefs = make([]Linedef, cntLinedefs, cntLinedefs)
					f.Seek(int64(le[idx].FilePos), 0)
					binary.Read(f, binary.LittleEndian, linedefs)
				} else { // FORMAT_HEXEN
					cntLinedefs := le[idx].Size / HEXEN_LINEDEF_SIZE
					hexenLinedefs = make([]HexenLinedef, cntLinedefs, cntLinedefs)
					f.Seek(int64(le[idx].FilePos), 0)
					binary.Read(f, binary.LittleEndian, hexenLinedefs)
				}
				copyLump = !config.RebuildNodes // !!! linedefs' vertices might be renumbered by nodes builder
			} else if bytes.Equal(bname, []byte("VERTEXES")) {
				l.VerticesLumpIdx = idx
				cntVertices := le[idx].Size >> 2
				vertices = make([]Vertex, cntVertices, cntVertices)
				f.Seek(int64(le[idx].FilePos), 0)
				binary.Read(f, binary.LittleEndian, vertices)
				// bounds
				bounds = GetBounds(vertices)
				copyLump = !config.RebuildNodes // vertices are created and renumbered by nodes builder
			} else if bytes.Equal(bname, []byte("SIDEDEFS")) {
				cntSidedefs := le[idx].Size / DOOM_SIDEDEF_SIZE
				sidedefs = make([]Sidedef, cntSidedefs, cntSidedefs)
				f.Seek(int64(le[idx].FilePos), 0)
				binary.Read(f, binary.LittleEndian, sidedefs)
				// this is kept intact, for now at least
			} else if bytes.Equal(bname, []byte("SECTORS")) {
				cntSectors := le[idx].Size / DOOM_SECTOR_SIZE
				sectors = make([]Sector, cntSectors, cntSectors)
				f.Seek(int64(le[idx].FilePos), 0)
				binary.Read(f, binary.LittleEndian, sectors)
				// this is kept intact
			} else if action.LevelFormat == FORMAT_HEXEN && bytes.Equal(bname, []byte("THINGS")) {
				vrThing := HexenThing{}
				cntThings := le[idx].Size / uint32(binary.Size(vrThing))
				hexenThings = make([]HexenThing, cntThings, cntThings)
				f.Seek(int64(le[idx].FilePos), 0)
				binary.Read(f, binary.LittleEndian, hexenThings)
			} // this is kept intact
			// copy along every entry that is not going to be modified
			if copyLump {
				tmpBuf := make([]byte, le[idx].Size, le[idx].Size)
				f.ReadAt(tmpBuf, int64(le[idx].FilePos))
				wriBus.SendRawLump(tmpBuf, idx, "", "")
			}
		}
		// Using linedefs and vertices when avaiable, generate both BLOCKMAP
		// lump and some other blockmaps (solid lines only, for example), and
		// set universal interfaces for the rest of action
		if !loadedLinedefsAndVertices && (linedefs != nil ||
			hexenLinedefs != nil) && vertices != nil {
			loadedLinedefsAndVertices = true
			if action.LevelFormat == FORMAT_DOOM {
				// Scroll effects are for doom format only, not hexen
				if config.RebuildBlockmap && config.RebuildNodes {
					// To provide for zokumbsp "scrolling at x speed"
					// functionality, linedefs need to be created/moved or
					// deleted, which means invalidating their indices. Thus
					// it is only possible to do this when both blockmap and
					// nodes are being rebuilt, as they reference linedefs.
					// Also, without rebuilding nodes, the LINEDEF lump is not
					// modified
					linedefs, linesToIgnore = RefreshScrollerSpeed(linedefs,
						vertices, false)
				} else {
					// Otherwise if either only blockmap or only nodes are
					// being rebuilt, we still need to locate scrollers to mark
					// them as omitted from either
					if config.RebuildBlockmap || config.RebuildNodes {
						_, linesToIgnore = RefreshScrollerSpeed(linedefs,
							vertices, true)
					}
				}

				absLines = &DoomLinedefs{
					linedefs: linedefs,
					vertices: vertices,
				}
				solidLines = &DoomSolidLinedefs{
					linedefs: linedefs,
					vertices: vertices,
				}
				l.newLines = &DoomLinedefs{
					linedefs: linedefs,
					vertices: vertices,
				}
			} else { // FORMAT_HEXEN
				absLines = &HexenLinedefs{
					linedefs: hexenLinedefs,
					vertices: vertices,
					things:   nil,
					sidedefs: nil,
				}
				solidLines = &HexenSolidLinedefs{
					linedefs: hexenLinedefs,
					vertices: vertices,
				}
				l.newLines = &HexenLinedefs{
					linedefs: hexenLinedefs,
					vertices: vertices,
					things:   nil, // to be assigned!
					sidedefs: nil, // to be assigned!
				}
			}

			// We can generate blockmap! Or several...

			// These are for "solid lines"-only blockmap which is used internally
			// by reject or nodes builder. This blockmap is not the one written
			// to lump
			bcontrol = make(chan BconRequest, 4)
			bgenerator = make(chan BgenRequest, 4)
			go SolidBlocks_Control(SolidBlocks_Input{
				lines:         solidLines,
				bounds:        bounds,
				control:       bcontrol,
				genworker:     bgenerator,
				linesToIgnore: linesToIgnore,
			})
		}

		if !blockmapDone && loadedLinedefsAndVertices && (!blockmapNeedsSectors ||
			(sectors != nil && sidedefs != nil)) {
			blockmapDone = true // don't trigger twice
			// And this actually generates BLOCKMAP lump
			Log.Printf("Generating BLOCKMAP...\n")
			l.BlockmapLumpChannel = make(chan []byte)
			go BlockmapGenerator(BlockmapInput{
				lines:           l.newLines,
				bounds:          bounds,
				XOffset:         config.BlockmapXOffset,
				YOffset:         config.BlockmapYOffset,
				useZeroHeader:   config.UseZeroHeader,
				internalPurpose: false,
				gcShield:        nil,
				linesToIgnore:   linesToIgnore,
			}, l.BlockmapLumpChannel, action.LevelFormat, sectors, sidedefs)
		}

		// Hexen needs more to build nodes - it needs things to know where
		// polyobjects are
		if action.LevelFormat == FORMAT_HEXEN && loadedLinedefsAndVertices &&
			!loadedThings && hexenThings != nil && sidedefs != nil {
			l.newLines.(*HexenLinedefs).things = hexenThings
			l.newLines.(*HexenLinedefs).sidedefs = sidedefs
			loadedThings = true
		}

		// Reject requires linedefs, vertices, sidedefs and sectors. It also
		// uses solid-only blockmap
		if !rejectDone && loadedLinedefsAndVertices && len(sidedefs) > 0 && len(sectors) > 0 {
			rejectDone = true // don't trigger twice
			l.RejectChan = make(chan RejectResponse)
			go RejectGenerator(RejectInput{
				lines:         absLines,
				bounds:        bounds,
				sectors:       sectors,
				sidedefs:      sidedefs,
				rejectChan:    l.RejectChan,
				bcontrol:      bcontrol,
				bgenerator:    bgenerator,
				linesToIgnore: linesToIgnore,
				rmbFrame:      action.RMBOptions,
				fileControl:   fileControl,
				mapName:       l.mapName,
			})
		}

		// Nodes require a lot of things, and in case of Hexen, even THINGS lump
		// is needed
		if config.RebuildNodes && l.NodesChan == nil &&
			loadedLinedefsAndVertices && len(sidedefs) > 0 &&
			len(sectors) > 0 && (action.LevelFormat != FORMAT_HEXEN ||
			loadedThings) {
			// buffered channel -- allow the sender to move on, because we may be
			// delayed by reject business
			l.NodesChan = make(chan NodesResult, 1)
			// diagonality - global config
			diagonalPenalty := config.DiagonalPenalty
			applyDiagonalPenalty := config.PenalizeDiagonality == PENALIZE_DIAGONALITY_ALWAYS
			if action.LevelFormat == FORMAT_HEXEN {
				applyDiagonalPenalty = applyDiagonalPenalty ||
					(config.PenalizeDiagonality == PENALIZE_DIAGONALITY_HEXEN)
			}
			if !applyDiagonalPenalty {
				diagonalPenalty = 0
			}
			// sideness cache (see zensideness.go) is only used for Zennode-like
			// partitioner and can never be used with any other partitioner;
			// for Zennode-like partitioner, defaults to enabled only for
			// multi-tree mode
			cacheSideness := config.PickNodeUser == PICKNODE_ZENLIKE &&
				config.CacheSideness != CACHE_SIDENESS_NEVER &&
				(config.CacheSideness == CACHE_SIDENESS_ALWAYS ||
					config.MultiTreeMode != MULTITREE_NOTUSED)
			nodeType := config.NodeType   // global config
			treeWidth := config.TreeWidth // global config
			if treeWidth == 0 {           // was auto mode
				if config.MultiTreeMode == MULTITREE_HARD {
					treeWidth = 2 // minimum possible width cap
				} else if config.MultiTreeMode == MULTITREE_NOTUSED && config.StkNode {
					treeWidth = -1 // here partitions are just collected for debug output, so uncapped is ok
				}
			}
			nodesInput := &NodesInput{
				lines:      l.newLines,
				sectors:    sectors,
				sidedefs:   sidedefs,
				bcontrol:   bcontrol,
				bgenerator: bgenerator,
				nodesChan:  l.NodesChan, // through this we'll get results

				pickNodeUser:      config.PickNodeUser,   // global config
				pickNodeFactor:    config.PickNodeFactor, // global config
				diagonalPenalty:   diagonalPenalty,
				minorIsBetterUser: config.MinorCmpUser, // global config
				linesToIgnore:     linesToIgnore,
				nodeType:          nodeType,

				depthArtifacts: config.DepthArtifacts, // global config

				multiTreeMode:   config.MultiTreeMode,   // global config
				specialRootMode: config.SpecialRootMode, // global config

				detailFriendliness: config.NodeDetailFriendliness, // global config
				cacheSideness:      cacheSideness,
				stkNode:            config.MultiTreeMode == MULTITREE_HARD || config.StkNode, // global config
				width:              treeWidth,
			}
			if nodeType == NODETYPE_DEEP || nodeType == NODETYPE_VANILLA ||
				nodeType == NODETYPE_VANILLA_RELAXED ||
				nodeType == NODETYPE_VANILLA_OR_DEEP ||
				nodeType == NODETYPE_VANILLA_OR_ZCOMPRESSED ||
				nodeType == NODETYPE_VANILLA_OR_ZEXTENDED {

				// note on vanilla_or_zdoom modes:
				// NodesGenerator will call ZNodesGenerator from within
				go NodesGenerator(nodesInput)

			} else if nodeType == NODETYPE_ZDOOM_COMPRESSED ||
				nodeType == NODETYPE_ZDOOM_EXTENDED {

				go ZNodesGenerator(nodesInput)

			} else {
				Log.Panic("Node format not implemented (internal number: %d)\n",
					nodeType)
			}
			// NOTE don't access nodesInput after launching NodesGenerator.
			// NodesGenerator can edit nodesInput fields without synchronization
		}
	}

	// All lumps belonging to this level were read from source
	// Code from here is executed ONCE for entire level

	// If either reject or nodes not used, we need to shutdown solid-only
	// blockmap builder control on their behalf here
	if bcontrol != nil && rejectSkip {
		bcontrol <- BconRequest{
			Sender:  SOLIDBLOCKS_REJECT,
			Message: BCON_NONEED_SOLID_BLOCKMAP,
		}
	}
	if bcontrol != nil && !config.RebuildNodes {
		bcontrol <- BconRequest{
			Sender:  SOLIDBLOCKS_NODES,
			Message: BCON_NONEED_SOLID_BLOCKMAP,
		}
	}

	// Wait for channels to deliver results of node/reject/blockmap building,
	// write them
	l.WaitForAndWriteData()
}

// This will receive results of work from other threads via channels, then write
// them. If deterministic, lump data is written in strict order. Otherwise it is
// written in the order it was received
// TODO In non-deterministic mode, it would be good to write non-level lumps in
// parallel to level processing, but must have read bus then as well. Not yet done.
func (l *Level) WaitForAndWriteData() {
	// Parallel wait, even in deterministic mode -- I have refactored code to
	// deduplicate it. So determinism now is only about writing order
	deterministic := config.Deterministic
	NA := reflect.Value{}
	branches := make([]reflect.SelectCase, 0)
	meaning := make([]int, 0)
	if l.NodesChan != nil {
		branches = append(branches, reflect.SelectCase{
			Dir:  reflect.SelectRecv,
			Chan: reflect.ValueOf(l.NodesChan),
			Send: NA,
		})
		meaning = append(meaning, BRANCH_NODES)
	}

	if l.RejectChan != nil {
		branches = append(branches, reflect.SelectCase{
			Dir:  reflect.SelectRecv,
			Chan: reflect.ValueOf(l.RejectChan),
			Send: NA,
		})
		meaning = append(meaning, BRANCH_REJECT)
	}

	if l.BlockmapLumpChannel != nil {
		branches = append(branches, reflect.SelectCase{
			Dir:  reflect.SelectRecv,
			Chan: reflect.ValueOf(l.BlockmapLumpChannel),
			Send: NA,
		})
		meaning = append(meaning, BRANCH_BLOCKMAP)
	}

	var nodesResult NodesResult
	var rejectResp *RejectResponse
	var bmdata []byte

	for len(branches) > 0 {
		chi, recv, recvOk := reflect.Select(branches)
		canDelete := true
		if recvOk {
			cur := meaning[chi]
			switch cur {
			case BRANCH_NODES:
				{
					nodesResult = (recv.Interface()).(NodesResult)
					if !deterministic {
						l.WriteNodes(nodesResult)
					}
				}
			case BRANCH_REJECT:
				{
					tmpResp := (recv.Interface()).(RejectResponse)
					if tmpResp.nagger != nil {
						// damn, it brought not results but a request for a lump
						// because NOPROCESS
						canDelete = false // keep, will have to wait for completion
						data := l.handleRejectNagging(tmpResp.nagger)
						if data != nil {
							rejectResp = &RejectResponse{
								data: data, // we copy directly
							}
						}
					} else {
						if !l.WroteRejectAlready {
							rejectResp = &RejectResponse{}
							*rejectResp = tmpResp
						}
						// canDelete == true
					}
					if !l.WroteRejectAlready && rejectResp != nil {
						// don't override rejectResp with future tmpResp
						l.WroteRejectAlready = true
						if !deterministic {
							l.wriBus.SendRawLump(rejectResp.data, l.RejectLumpIdx, "REJECT", "")
						}
					}
				}
			case BRANCH_BLOCKMAP:
				{
					bmdata = (recv.Interface()).([]byte)
					if !deterministic {
						l.wriBus.SendRawLump(bmdata, l.BlockmapLumpIdx, "BLOCKMAP", "")
					}
				}
			default:
				{
					Log.Error("What? Unknown branch %d.\n", cur)
				}
			}
		} // else "channel closed" event happened. Code shouldn't allow it anyway

		// Now delete this branch
		if canDelete {
			if chi < len(branches)-1 {
				copy(branches[chi:], branches[chi+1:])
				copy(meaning[chi:], meaning[chi+1:])
			}
			branches = branches[:len(branches)-1]
			meaning = meaning[:len(meaning)-1]
		}
	}

	if deterministic {
		if l.NodesChan != nil {
			l.WriteNodes(nodesResult)
		}

		if l.RejectChan != nil {
			l.wriBus.SendRawLump(rejectResp.data, l.RejectLumpIdx, "REJECT", "")
		}

		if l.BlockmapLumpChannel != nil {
			l.wriBus.SendRawLump(bmdata, l.BlockmapLumpIdx, "BLOCKMAP", "")
		}
	}
}

// handleRejectNagging returns non-nil value only if copies directly, otherwise must
// return nil
func (l *Level) handleRejectNagging(nagger *RejectNagger) []byte {
	// shall set l.WroteRejectAlready if copied reject itself
	if nagger.replyTo == nil {
		Log.Panic("Level machinery was requested to load an existing reject, but reply address was not set. (programmer error)\n")
	}
	// Whether to check the number of sectors in origin because off by 1 means grossly
	// wrong reject data, even if size of reject is the same.
	// Doesn't seem to be needed for any non-toy maps (>4 sectors on a map):
	// 9x9=81/8=11, and for any N>8 reject for N^2 grows more than 1 byte a time
	// 8x8=64/8=8
	// 7x7=49/8=7
	// 6x6=36/8=5
	// 5x5=25/8=4
	// 4x4=16/8=2
	// 3x3=9/8=2
	// 2x2=4/8=1
	// 1x1=1/8=1
	// So will pretend those four last cases don't exist, and not check anything at
	// source wad but lump size of requested REJECT.

	// not previous build of current map, but something else
	// it may be the input wad also, but PinExternalWad will take care of it and
	// return structure for input wad, too. It can't be, however, output wad.
	var dir []LumpEntry
	var rej *LumpEntry
	var fromWhere string
	var exWad *PinnedWad

	if nagger.currentWadAndMap {
		dir = l.le
		if l.RejectLumpIdx == 0 || l.le[l.RejectLumpIdx].Size == 0 {
			Log.Error("The original reject lump is empty or non-existent\n")
			return declineRejectNagging(nagger)
		}
		rej = &(dir[l.RejectLumpIdx])
		fromWhere = "previous build of current map"
	} else {
		wadToLoad := l.fc.ResolveFilepath(nagger.wadToLoad)
		exWad = l.fc.PinExternalWad(wadToLoad)
		if exWad == nil {
			Log.Error("Couldn't load wad \"%s\" for NOPROCESS\n", wadToLoad)
			return declineRejectNagging(nagger)
		}
		mapToLoad := nagger.mapToLoad
		if mapToLoad == "" {
			// in a different wad, lookup level with the same name as the name of
			// current map being built
			mapToLoad = l.mapName
		}
		exLvl := exWad.LookupLevel(mapToLoad)
		if exLvl == nil {
			Log.Error("Couldn't find level \"%s\" in wad \"%s\" for NOPROCESS\n",
				mapToLoad, wadToLoad)
			return declineRejectNagging(nagger)
		}
		rejSch := exWad.LookupLumpInLevel(exLvl, "REJECT")
		if rejSch == nil {
			Log.Error("REJECT lump does not exist in level \"%s\" in wad \"%s\" (requested by NOPROCESS)\n",
				mapToLoad, wadToLoad)
			return declineRejectNagging(nagger)
		}
		dir = exWad.le
		rej = &(dir[rejSch.DirIndex])
		fromWhere = fmt.Sprintf("wad \"%s\" map \"%s\"", wadToLoad, mapToLoad)
	}

	// First, where are we loading from
	if int(rej.Size) != rejectLumpSize_nonUDMF(nagger.numSectors) {
		Log.Error("Original reject lump is incorrect size, must have had different number of sectors\n")
		return declineRejectNagging(nagger)
	}
	rejBytes := make([]byte, rej.Size)
	atPos := int64(rej.FilePos)
	if nagger.currentWadAndMap {
		n, err := l.f.ReadAt(rejBytes, atPos)
		if err != nil || n < int(rej.Size) {
			Log.Error("Failed to read original reject lump\n")
			return declineRejectNagging(nagger)
		}
	} else {
		n, err := exWad.readerAt.ReadAt(rejBytes, atPos)
		if err != nil || n < int(rej.Size) {
			Log.Error("Failed to read original reject lump\n")
			return declineRejectNagging(nagger)
		}
	}

	Log.Printf("Sourcing reject from %s\n", fromWhere)
	if nagger.mustDirectlyCopy {
		nagger.replyTo <- LevelSaysToNagging{
			copied: true,
		}
		return rejBytes // wriBus will write it
	} else {
		// there will be (probably) more processing by RMB
		nagger.replyTo <- LevelSaysToNagging{
			rejectData: rejBytes,
		}
		return nil
	}

	// return declineRejectNagging(nagger) // go vet says this is unreachable
}

func declineRejectNagging(nagger *RejectNagger) []byte {
	nagger.replyTo <- LevelSaysToNagging{
		goToHell: true,
	}
	return nil
}

func (l *Level) WriteNodes(nodesResult NodesResult) {
	l.wriBus.SendGenericLump(l.newLines.GetLinedefs(),
		l.LinedefsLumpIdx, "LINEDEFS", "") // only vertices renumbering might have happened, nothing else... yet
	l.wriBus.SendGenericLump(l.newLines.GetVertices(),
		l.VerticesLumpIdx, "VERTEXES", "")
	if nodesResult.deepNodes != nil {
		// write deep nodes
		s := " [deep]"
		l.wriBus.SendGenericLump(nodesResult.deepSegs,
			l.SegsLumpIdx, "SEGS", s)
		l.wriBus.SendGenericLump(nodesResult.deepSubsectors,
			l.SSectorsLumpIdx, "SSECTORS", s)
		l.wriBus.SendDeepNodesLump(nodesResult.deepNodes,
			l.NodesLumpIdx, "NODES", s)
	} else if nodesResult.rawNodes != nil {
		// write Zdoom extended or compressed nodes
		s := " [zdoom ext]"
		if nodesResult.compressed {
			s = " [zdoom comp]"
		}
		l.wriBus.SendRawLump(nil, l.SegsLumpIdx, "SEGS", " [empty]")
		l.wriBus.SendRawLump(nil, l.SSectorsLumpIdx, "SSECTORS", " [empty]")
		l.wriBus.SendRawLump(nodesResult.rawNodes, l.NodesLumpIdx, "NODES", s)
	} else {
		// write standard nodes
		// also handles failure condition for all node formats
		s := " [van/lr]"
		if nodesResult.nodes == nil {
			s = " [empty]"
		}
		l.wriBus.SendGenericLump(nodesResult.segs,
			l.SegsLumpIdx, "SEGS", s)
		l.wriBus.SendGenericLump(nodesResult.subsectors,
			l.SSectorsLumpIdx, "SSECTORS", s)
		l.wriBus.SendGenericLump(nodesResult.nodes,
			l.NodesLumpIdx, "NODES", s)
	}
}

// Implements scroll speed multipliers by ensuring appropriate number of
// dummy linedefs with Action = 48 exist. Because they could have been created
// before, this is how they are checked for whether additional need to be
// created, or some need to be deleted. If parameter dryRun == true, no linedefs
// are created / deleted. In either case, all dummy linedefs are marked in a
// second return parameter to be skipped from included in both BSP tree and
// blockmap
// NOTE for action = 1048, only 2 last digits decide scroll speed, not the
// entire tag number
// TODO the dummy linedefs should not now be included in seg list or blockmap.
// They might need to also be moved to the end of the list so that they may be
// beyond >=65535 index as they're not going to be referenced by 2-byte integer
// from other wad file structures (check if vanilla could correctly operate on
// them still, else it might be in vain)
func RefreshScrollerSpeed(linedefs []Linedef, vertices []Vertex, dryRun bool) ([]Linedef,
	[]bool) {
	scrollerGroupList := make([]ScrollerGroup, 0)
	backSdefForGroup := make([]uint16, 0) // not part of identity, but needs to be copied
	// map from ScrollerGroup to array of producers (linedef indices)
	scrollerToProducerMap := make(map[ScrollerGroup][]int)
	for i, linedef := range linedefs {
		if linedef.Action == 48 {
			// Normal scroll action
			addProducer(&scrollerGroupList, &backSdefForGroup,
				&scrollerToProducerMap,
				ScrollerGroup{
					StartVertex: linedef.StartVertex,
					EndVertex:   linedef.EndVertex,
					FrontSdef:   linedef.FrontSdef,
				},
				i, linedef.BackSdef)

		} else if linedef.Action == 1048 && linedef.Tag > 0 {
			// Remote scroll action
			// Find the closest wall with the same tag. Criteria for closest
			// wall: shortest distance between start vertices of the two
			// linedefs
			distance := -1.0 // Impossible value
			nearest := -1
			for j, linedef2 := range linedefs {
				if linedef2.Tag == linedef.Tag && i != j {
					sv1 := vertices[linedef.StartVertex]
					sv2 := vertices[linedef2.StartVertex]
					distanceNew := DistanceTwoPoints(sv1.XPos,
						sv1.YPos, sv2.XPos, sv2.YPos)
					if distance < 0.0 || distanceNew < distance {
						nearest = j
						distance = distanceNew
					}
				}
			}
			if nearest != -1 {
				// Group attributes are taken from the linedef whose sidedef
				// needs to be scrolled, but the producer is the linedef with
				// the remote scroll action
				linedef2 := linedefs[nearest]
				addProducer(&scrollerGroupList, &backSdefForGroup,
					&scrollerToProducerMap,
					ScrollerGroup{
						StartVertex: linedef2.StartVertex,
						EndVertex:   linedef2.EndVertex,
						FrontSdef:   linedef2.FrontSdef,
					},
					i, linedef2.BackSdef)
			} else {
				Log.Verbose(1, "Linedef %d has action 1048 (remote scroll) and non-zero tag value of %d, but another linedef with same tag value doesn't exist.\n",
					i, linedef.Tag)
			}
		}
	}

	if len(scrollerGroupList) == 0 {
		// No scrollers - no modifications needed
		return linedefs, nil
	}

	// Otherwise we need to match scrolling effect multipliers to the tags, and
	// also mark all dummy linedefs as excluded from blockmap and BSP tree
	dummies := make([]bool, len(linedefs))

	addLineCount := make([]uint16, len(scrollerGroupList))
	linedefsToCull := make([]int, 0)
	for i, scrollerGroup := range scrollerGroupList {
		producers := scrollerToProducerMap[scrollerGroup]
		scrollPower := linedefs[producers[0]].Tag // the earliest takes precedence
		if linedefs[producers[0]].Action == 1048 {
			// 2 last digits decide scroll speed for Action = 1048, not
			// entire tag number
			scrollPower = scrollPower % 100
		}
		// Also add all remote scroll (1048) in every position. Don't add earliest
		// twice of course
		for idx, j := range producers {
			if idx != 0 && linedefs[j].Action == 1048 {
				// 2 last digits decide scroll speed for Action = 1048, not
				// entire tag number
				scrollPower += linedefs[j].Tag % 100
			}
		}
		if scrollPower == 0 {
			scrollPower = 1
		}
		currPower := uint16(0)
		for idx, j := range producers {
			if linedefs[j].Action == 48 {
				currPower++
				// Also let's reset tag value (except for the first, of course)
				if idx != 0 {
					linedefs[j].Tag = 0
					// Only if dryRun == true are dummies recorded here rather
					// than later, because if dryRun == false some may be
					// removed
					if dryRun {
						dummies[j] = true
					}
				}
			}
		}

		if dryRun {
			continue
		}

		if currPower == scrollPower {
			// This group has nothing to add or remove
			addLineCount[i] = 0
		} else if currPower > scrollPower {
			// Nothing to add, but some lines need to be removed
			addLineCount[i] = 0
			remove := currPower - scrollPower
			removed := uint16(0)
			for idx, j := range producers {
				if removed == remove {
					break
				}
				if idx > 0 && linedefs[j].Action == 48 {
					linedefsToCull = append(linedefsToCull, j)
					removed++
				}
			}
			if removed != remove {
				Log.Error("Needed to remove %d scroller lines to match the tag, but removed %d scroller lines.\n",
					remove, removed)
			}
		} else { // currPower < scrollPower
			// New lines to be added to the end of the list
			addLineCount[i] = scrollPower - currPower
		}
	}

	if dryRun {
		// We are not changing linedefs, merely locating dummy ones to omit them
		// fromm blockmap/nodes (of which lumps only one is being built)
		return linedefs, dummies
	}
	// now dryRun definitely false

	// Create new array of linedefs
	ret := make([]Linedef, len(linedefs))

	// Remove linedefs first, via copying only those that are not removed
	copyStart := 0
	removed := 0
	for i := 0; i < len(linedefsToCull); i++ {
		copyEnd := linedefsToCull[i]
		if copyStart != copyEnd {
			copy(ret[copyStart-removed:], linedefs[copyStart:copyEnd])
		}
		copyStart = copyEnd + 1
		removed++
	}
	if copyStart < len(linedefs) {
		copy(ret[copyStart-removed:], linedefs[copyStart:len(linedefs)])
	}
	ret = ret[:len(linedefs)-removed]

	// Locate all remaining linedefs, add them to dummies. Remember that
	// some may have changed index, so compute new index
	for _, scrollerGroup := range scrollerGroupList {
		producers := scrollerToProducerMap[scrollerGroup]
		for idx, j := range producers {
			if linedefs[j].Action == 48 {
				// If it's the first line, is not dummy
				if idx != 0 {
					// Actualize the number or skip altogether, if it is
					// deleted
					deleted := false
					shift := 0
					for _, chk := range linedefsToCull {
						if chk == j {
							deleted = true
							break
						} else {
							if chk < j {
								shift++
							}
						}
					}
					if deleted {
						continue
					}
					dummies[j-shift] = true
				}
			}
		}
	}

	// Add new linedefs to the end
	for i := 0; i < len(addLineCount); i++ {
		cnt := addLineCount[i]
		group := scrollerGroupList[i]
		for j := uint16(0); j < cnt; j++ {
			ret = append(ret, Linedef{
				StartVertex: group.StartVertex,
				EndVertex:   group.EndVertex,
				FrontSdef:   group.FrontSdef,
				BackSdef:    backSdefForGroup[i],
				Flags:       0,
				Tag:         0,
				Action:      48,
			})
			dummies = append(dummies, true)
		}
	}

	// The new array of linedefs is returned
	return ret, dummies
}

func addProducer(scrollerGroupList *[]ScrollerGroup, backSdefForGroup *[]uint16,
	stpMap *map[ScrollerGroup][]int, scrollerGroup ScrollerGroup,
	linedefIdx int, backsdef uint16) {
	producers, exists := (*stpMap)[scrollerGroup]
	if !exists {
		producers = make([]int, 0)
		*backSdefForGroup = append(*backSdefForGroup, backsdef)
		*scrollerGroupList = append(*scrollerGroupList, scrollerGroup)
	}
	producers = append(producers, linedefIdx)
	(*stpMap)[scrollerGroup] = producers
}

func DistanceTwoPoints(sx, sy, ex, ey int16) float64 {
	dx := float64(sx) - float64(ex)
	dy := float64(sy) - float64(ey)
	return math.Sqrt(dx*dx + dy*dy)
}
