// Copyright (C) 2022-2023, VigilantDoomer
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

import (
	"bytes"
	"encoding/binary"
	"fmt"
	"log"
	"math"
	"sort"
	"strconv"
	"time"
)

// Nodes Generator, ooh yes

// TODO investigate Zennode's partitioning of _convex_ multi-sector nodes
// (something current IsItConvex does not even identify) into subsectors. Could
// it be worth it?
// TODO work more on superblocks: consider sector lists; utilizing existing
// superblocks instead of creating new from scratch in actual partitioning
// process (would require merging next and nextInSuperBlock pointers); research
// aj-bsp superblock-related code more. I theoritize that reusing existing
// superblocks when whole superblocks go to left/right side, would make sector
// lists especially viable - I expect major speed improvements in
// visplaneKillough and visplaneVigilant modes
// TODO "wide trees" algorithm, possible using multiple threads + partitioning
// single sector doesn't affect visplanes, so this part can be done with
// convexity.go and without wide treatment
// TODO potentially mergeable sectors in "advanced visplane reduction" - since
// visplanes are built separately for ceilings and floors, maybe sector
// equivalencies also need to be split into ceiling and floor equivalencies.
// This proposition will increase algorithm's running time and complexity,
// however, I also wonder how often it will actually make a difference and how
// often a positive one. It actually may be very difficult to estimate,
// something like this should be more of a secondary score. Current experiments,
// however, didn't lead to improvements on maps I tested - the code for them
// was moved into separate branch
// TODO ZokumBSP (maybe Zennode too) sorts segs in case splits upset "seg
// ordering", as some effects (which ones?) allegedly depend on the order of
// segs in subsectors matching the numerical order of linedefs. Prior to
// VigilantBSP v0.75, there was a definitive source of disruption - when "cull
// invisible segs" is used: when segs that could have been invisible determined
// to be NOT so, they are inserted at the END of the seg array, right before
// a single node is created. In VigilantBSP 0.75, this disruption no longer
// occurs in this case (again, this change leads to a different BSP generated
// for "cull invisible segs" modes compared to previous versions, because when
// multiple segs score the same in partition selection - which occurs frequently
// - the earliest seg wins). It remains to be seen if sorting segs is still
// necessary when forming a subsector

const SSECTOR_NORMAL_MASK = 0x8000
const SSECTOR_DEEP_MASK = 0x80000000

const DETAILED_LEVEL_THRESHOLD = 500

// Passed to the nodes builder goroutine
type NodesInput struct {
	lines              WriteableLines
	solidLines         AbstractLines // when a solid-only blockmap is needed to detect void space for more accurate node pick quality weighting
	sectors            []Sector
	sidedefs           []Sidedef
	bcontrol           chan BconRequest
	bgenerator         chan BgenRequest
	nodesChan          chan<- NodesResult
	pickNodeUser       int
	diagonalPenalty    int
	pickNodeFactor     int
	minorIsBetterUser  int
	linesToIgnore      []bool // dummy linedefs such as for scrolling
	depthArtifacts     bool
	nodeType           int
	multiTreeMode      int
	specialRootMode    int
	detailFriendliness int
	cacheSideness      bool
	stkNode            bool
	width              int
}

type Number int

type WideNumber int64

// PickNodeFunc is a signature of all PickNode* function variants (see
// picknode.go)
// Yes, I consciously use a function reference stored in structs in lieu of
// interface - interfaces are "fat", function references should work faster
type PickNodeFunc func(*NodesWork, *NodeSeg, *NodeBounds, *Superblock) *NodeSeg

// Some node partitioning algos implement separate CreateNode function for the
// the case of partitioning non-convex node with only one sector. Thus this
// CreateNodeSSFunc exists.
// SS stands for "single sector". In case you worried.
type CreateNodeSSFunc func(*NodesWork, *NodeSeg, *NodeBounds, *Superblock) *NodeInProcess
type StkCreateNodeSSFunc func(*NodesWork, *NodeSeg, *NodeBounds, *Superblock, *StkQueue) *NodeInProcess

// For stknode.go, so that StkCreateNode BFS part can also execute special
// non-convex single-sector partitioner
type SingleSectorDivisorFunc func(w *NodesWork, ts *NodeSeg, rs **NodeSeg,
	ls **NodeSeg, bbox *NodeBounds, super *Superblock, rightsSuper,
	leftsSuper **Superblock, partsegs *[]PartSeg)

// Which secondary metric to use
type MinorIsBetterFunc func(current, prev MinorCosts) bool

type DoLinesIntersectFunc func(c *IntersectionContext) uint8

// Returned from nodes builder goroutine through a channel
type NodesResult struct {
	subsectors     []SubSector
	segs           []Seg
	nodes          []Node
	deepSubsectors []DeepSubSector
	deepSegs       []DeepSeg
	deepNodes      []DeepNode
	rawNodes       []byte // for Zdoom extended/compressed nodes - all data sans the signature
}

type NodesTotals struct {
	numNodes               int
	numSSectors            int
	numSegs                uint32
	maxSegCountInSubsector uint32
	segSplits              int
	preciousSplit          int
}

// NodesWork encapsulates data used for nodebuilding thread. When multi-tree is
// built, each tree uses a copy of original NodesWork, with some fields copied
// deep, but some are copied SHALLOW (and must thus never be written to once
// multi-tree starts)! Be cautious to support this functionality when adding
// new fields by modifying NodesWork method GetInitialStateClone()
type NodesWork struct {
	// mlog - if any multi-trees are used, the verbose output will be retained
	// only for the chosen tree, and displayed only after all trees are built
	// This allows to use verbosity properly with multi-tree modes
	// In single-tree mode, mlog should be nil, and calls executed on that
	// (Go supports execution on nil objects) will be forwarded to global Log
	// variable (central VigilantBSP logger)
	mlog            *MiniLogger
	lines           WriteableLines
	sides           []Sidedef
	sectors         []Sector
	allSegs         []*NodeSeg // internal format for calculations
	subsectors      []SubSector
	segs            []Seg // game format for actual SEGS lump
	nodeX           int
	nodeY           int
	nodeDx          int
	nodeDy          int
	totals          *NodesTotals
	vertices        []NodeVertex
	nodes           []Node
	nreverse        uint32
	segAliasObj     *SegAliasHolder
	pickNode        PickNodeFunc
	createNodeSS    CreateNodeSSFunc    // SS stands for "single sector". In case you worried.
	stkCreateNodeSS StkCreateNodeSSFunc // to be replaced by use of stkDivisorSS

	stkDivisorSS SingleSectorDivisorFunc // hard multi-tree uses this rather than createNodeSS

	sectorHits       []uint8          // used by PickNode_Visplane* variants, keeps track of sectors on both sides
	incidental       []IntVertexPairC // used by PickNode_visplaneVigilant, stores list segs collinear with partition line being evaluated to compute the length without overlap
	solidMap         *Blockmap
	nonVoidCache     map[int]NonVoidPerAlias
	dgVertexMap      *VertexMap // vertex map for computing void parts
	blockity         *BlockityLines
	deepNodes        []DeepNode // also used for Zdoom (extended/compressed) nodes while in production
	deepSubsectors   []DeepSubSector
	deepSegs         []DeepSeg
	SsectorMask      uint32
	blocksHit        []BlocksHit
	diagonalPenalty  int
	pickNodeFactor   int
	pickNodeUser     int
	minorIsBetter    MinorIsBetterFunc
	doLinesIntersect DoLinesIntersectFunc
	multipart        bool       // whether multiple partition with same PRIMARY costs are considered (for depth evaluation, not to be confused for multi-tree)
	parts            []*NodeSeg // this one is for HARD multi-tree - the list of partition candidates with same secondary costs as well
	width            int        // and this is the width cap for HARD multi-tree, can be -1 (unlimited)
	depthArtifacts   bool
	nodeType         int
	vertexCache      map[SimpleVertex]int // for nodes without extra precision
	vertexExists     int
	sidenessCache    *SidenessCache
	zenScores        []DepthScoreBundle
	qallocSupers     *Superblock // quick alloc supers - also AJ-BSP ideato decrease allocations

	stkExtra map[*NodeInProcess]StkNodeExtraData // TODO make a better implementation: map with a pointer key is not really a good thing
	// Stuff related to zdoom nodes, with exception to deepNodes, which is above
	zdoomVertexHeader *ZdoomNode_VertexHeader
	zdoomVertices     []ZdoomNode_Vertex
	zdoomSubsectors   []uint32
	zdoomSegs         []ZdoomNode_Seg
	vertexMap         *VertexMap
}

type NodeVertex struct {
	X   Number
	Y   Number
	idx uint32
}

type SimpleVertex struct {
	X int
	Y int
}

type NodeBounds struct {
	Xmin int
	Ymin int
	Xmax int
	Ymax int
}

type NodeSeg struct {
	StartVertex        *NodeVertex
	EndVertex          *NodeVertex
	Angle              int16
	Linedef            uint16
	flags              uint8  // flip and flags for internal use go here
	Offset             uint16 // distance along linedef to start of seg
	next               *NodeSeg
	partner            *NodeSeg
	psx, psy, pex, pey Number // start, end coordinates
	pdx, pdy, perp     Number // used in intersection calculations
	len                Number
	sector             uint16
	secEquiv           uint16 // only initialized if sectorEquivalencies are computed
	alias              int
	block              *Superblock
	nextInSuper        *NodeSeg // not yet ready to port everything to superblock use, need conventional "next" to be separate field
	sidenessIdx        int      // uses to linearize access to sidenessCache (since the traversal happens through superblocks and not in seg creation order)
}

type NodeInProcess struct {
	X            int16
	Y            int16
	Dx           int16
	Dy           int16
	Rbox         [4]int16 // right bounding box
	Lbox         [4]int16 // left bounding box
	RChild       uint32   // This may contain either int16 Node.RChild or int32 DeepNode.RChild value
	LChild       uint32   // likewise
	nextL, nextR *NodeInProcess
}

// DoLinesIntersect and ComputeIntersection use this
type IntersectionContext struct {
	psx, psy, pex, pey Number // start, end of partition coordinates
	pdx, pdy           Number // used in intersection calculations
	lex, lsx, ley, lsy Number // - same for checking line
}

const ( // BAMAction "enum"
	BAM_NOSPECIAL = iota
	BAM_REPLACE
	BAM_ADDITIVE
)

type BAMEffect struct {
	Action int   // BAMAction "enum"
	Value  int16 // value related to action
}

const FRACBITS = 16

const VMAP_BLOCK_SHIFT = 8 + FRACBITS

const VMAP_BLOCK_SIZE = 1 << VMAP_BLOCK_SHIFT

const VMAP_SAFE_MARGIN = 2.0 // Floating point garbage

// VertexMap is a ZDBSP thingy. Allows to lookup close-enough vertices among
// existing ones to the one created as the result of intersection. I am not sure
// as to what the exact consequences of choosing a very slightly different
// vertex instead of exact match are. More investigation needed
type VertexMap struct {
	w          *NodesWork
	Grid       [][]*FloatVertex
	Snapshot   []int
	BlocksWide int
	BlocksTall int
	MinX, MinY float64
	MaxX, MaxY float64
}

// FloatVertex is a vertex with floating point coordinates and associated id
type FloatVertex struct {
	Id int
	X  float64
	Y  float64
}

const (
	// SEG_FLAG_FLIP - seg's direction is opposite to its linedef direction.
	// Used to be separate field in NodeSeg
	SEG_FLAG_FLIP = uint8(0x01)
	// SEG_FLAG_NOCACHE - doLinesIntersect result for check=<this seg> is not
	// stored in sideness cache
	// (used by Zenlike partitioner under certain conditions)
	// Usual reason seg is not cached is that it has been split, but programmer
	// might introduce other reasons. Since sideness cache takes a lot of memory,
	// it might be beneficial (hypothesis) to omit segs which are rarely evaluated
	// directly (thanks to quick sideness analysis of superblocks), IF this is
	// shown to benefit performance
	SEG_FLAG_NOCACHE = uint8(0x02)
	// SEG_FLAG_GLOBAL_FLIP - also used for cache purposes, since cache only
	// stores value per alias but not the direction of partition line, all
	// values in cache are assumed to be stored against a certain direction,
	// with partition segs following opposite direction marked with this flag.
	// This flag has no correlation with SEG_FLAG_FLIP - the two must never be
	// confused!
	SEG_FLAG_GLOBAL_FLIP = uint8(0x04)
)

const (
	CONVEX_SUBSECTOR = iota
	NONCONVEX_ONESECTOR
	NONCONVEX_MULTISECTOR
)

// goroutine, replies on input.nodesChan
func NodesGenerator(input *NodesInput) {
	start := time.Now()
	// let other goroutines working on something else work on the shared read-only
	// copy of vertices (and linedefs), we will be having our own copy to write to
	input.lines.UnshareData()
	// remove the remnants of any previous nodebuilder work
	input.lines.PruneUnusedVertices()
	// perform detection of polyobjects (sectors containing polyobjects must not
	// be split if possible)
	input.lines.DetectPolyobjects()

	allSegs, intVertices := createSegs(input.lines, input.sidedefs,
		input.linesToIgnore, input.nodeType == NODETYPE_ZDOOM_EXTENDED ||
			input.nodeType == NODETYPE_ZDOOM_COMPRESSED)
	if len(allSegs) == 0 {
		Log.Error("Failed to create any SEGs (BAD). Quitting (%s)\n.", time.Since(start))
		// First respond to solid blocks control goroutine that we won't serve
		// any requests to them
		input.bcontrol <- BconRequest{
			Sender:  SOLIDBLOCKS_NODES,
			Message: BCON_NONEED_SOLID_BLOCKMAP,
		}
		// And to this channel we reply that all is fucked
		input.nodesChan <- NodesResult{} // all fields are nil, no lump data generated
		return
	}

	ssectorMask := uint32(SSECTOR_NORMAL_MASK)
	// Deep, Zdoom extended and Zdoom compressed use the same convention
	if input.nodeType != NODETYPE_VANILLA {
		ssectorMask = SSECTOR_DEEP_MASK
	}

	// See if we need a blockmap of solid-only lines
	requestedSolid := input.pickNodeUser == PICKNODE_VISPLANE_ADV
	if requestedSolid {
		// Vigilant way of evaluating a node line length without void parts
		// depends on it available
		input.bcontrol <- BconRequest{
			Sender:  SOLIDBLOCKS_NODES,
			Message: BCON_NEED_SOLID_BLOCKMAP,
		}
	} else {
		// Other methods don't need this computation
		input.bcontrol <- BconRequest{
			Sender:  SOLIDBLOCKS_NODES,
			Message: BCON_NONEED_SOLID_BLOCKMAP,
		}
	}

	rootSeg := allSegs[0]
	// And now we'll have bounds based on the initial SEGs we built, yay
	rootBox := FindLimits(rootSeg)
	Log.Printf("Initial number of segs is %d.\n", len(allSegs))
	Log.Printf("Nodes: rendered part of map goes from (%d,%d) to (%d,%d)\n",
		rootBox.Ymax, rootBox.Xmin, rootBox.Ymin, rootBox.Xmax)

	doLinesIntersect := doLinesIntersectStandard
	if input.nodeType == NODETYPE_ZDOOM_COMPRESSED ||
		input.nodeType == NODETYPE_ZDOOM_EXTENDED {
		// Zdoom extended nodes keep "standard" because override (zdefs.go) is
		// placed on it not the other func
	} else {
		heur := false
		switch input.detailFriendliness {
		case NODE_DETAIL_ALWAYS:
			doLinesIntersect = doLinesIntersectDetail
		case NODE_DETAIL_SUPPRESS:
			doLinesIntersect = doLinesIntersectStandard
		case NODE_DETAIL_HEURISTIC:
			heur = true
		default: // should be NODE_DETAIL_AUTO
			heur = input.nodeType == NODETYPE_VANILLA
			if !heur { // deep nodes default to detailed version
				doLinesIntersect = doLinesIntersectDetail
			}
		}
		if heur {
			// use heuristic to see if level is detailed, if yes, use
			// detail-friendly version (but that generates more segs and nodes)
			detail := IsTooDetailed(input.lines, input.linesToIgnore, rootBox)
			if detail {
				doLinesIntersect = doLinesIntersectDetail
			}
		}

	}

	var solidMap *Blockmap
	if requestedSolid {
		blockmapGetter := make(chan *Blockmap)
		input.bgenerator <- BgenRequest{
			Action:  BGEN_RETRIEVE_BLOCKMAP,
			ReplyTo: blockmapGetter,
		}
		solidMap = <-blockmapGetter
	}

	sectorEquiv, whichLen := input.computeSectorEquivalence()
	if sectorEquiv == nil {
		whichLen = len(input.sectors)
	} else {
		input.applySectorEquivalence(allSegs, sectorEquiv)
	}

	workData := NodesWork{
		lines:   input.lines,
		sides:   input.sidedefs,
		sectors: input.sectors,
		allSegs: allSegs,
		totals: &NodesTotals{
			numNodes:               0,
			numSSectors:            0,
			numSegs:                0,
			maxSegCountInSubsector: 0,
			segSplits:              0,
			preciousSplit:          0,
		},
		vertices:         intVertices,
		segAliasObj:      new(SegAliasHolder),
		pickNode:         PickNodeFuncFromOption(input.pickNodeUser),
		createNodeSS:     CreateNodeSSFromOption(input.pickNodeUser),
		stkCreateNodeSS:  StkCreateNodeSSFromOption(input.pickNodeUser),
		stkDivisorSS:     StkSingleSectorDivisorFromOption(input.pickNodeUser),
		sectorHits:       make([]byte, whichLen),
		incidental:       make([]IntVertexPairC, 0),
		solidMap:         solidMap,
		nonVoidCache:     make(map[int]NonVoidPerAlias),
		SsectorMask:      ssectorMask,
		blocksHit:        make([]BlocksHit, 0),
		diagonalPenalty:  input.diagonalPenalty,
		pickNodeFactor:   input.pickNodeFactor,
		pickNodeUser:     input.pickNodeUser,
		minorIsBetter:    MinorCmpFuncFromOption(input.minorIsBetterUser),
		multipart:        input.minorIsBetterUser == MINOR_CMP_DEPTH,
		depthArtifacts:   input.depthArtifacts,
		nodeType:         input.nodeType,
		doLinesIntersect: doLinesIntersect,
		zenScores:        make([]DepthScoreBundle, 0, len(allSegs)),
		sidenessCache: &SidenessCache{
			maxKnownAlias: 0,
		},
		width: input.width,
	}
	if input.nodeType == NODETYPE_ZDOOM_EXTENDED ||
		input.nodeType == NODETYPE_ZDOOM_COMPRESSED {
		workData.zdoomVertexHeader = &ZdoomNode_VertexHeader{
			ReusedOriginalVertices: uint32(input.lines.GetVerticesCount()),
		}
		workData.vertexMap = CreateVertexMap(&workData, rootBox.Xmin, rootBox.Ymin,
			rootBox.Xmax, rootBox.Ymax)
		PopulateVertexMap(workData.vertexMap, allSegs)
	} else {
		workData.vertexCache = make(map[SimpleVertex]int)
		PopulateVertexCache(workData.vertexCache, allSegs)

	}
	if workData.solidMap != nil {
		// Need to initialize dgVertexMap - the new line tracing algo for
		// advanced visplane reduction partitioner uses it, and it is distinct
		// from the regular vertex map (which gets used for extended nodes) in
		// many aspects.
		lineBox := GetLineBounds(workData.lines)
		dgVertexMap := CreateVertexMap(&workData, lineBox.Xmin, lineBox.Ymin,
			lineBox.Xmax, lineBox.Ymax)
		PopulateVertexMapFromLines(dgVertexMap, workData.lines)
		workData.dgVertexMap = dgVertexMap
	}
	workData.segAliasObj.Init()
	initialSuper := workData.doInitialSuperblocks(rootBox)
	if input.cacheSideness {
		workData.buildSidenessCache(rootSeg, initialSuper)
	}

	// The main act
	var rootNode *NodeInProcess
	if input.multiTreeMode == MULTITREE_NOTUSED { // This is a most commonly used single-tree mode
		if input.stkNode {
			// breadth-first node creation (for debug purposes, debug parameter
			// --stknode, this variant is normally used for HARD multi-tree
			// only and is only available for single-tree to make it easier
			// compare results in deterministic mode and ensure they are
			// identical)
			rootNode = StkEntryPoint(&workData, rootSeg, rootBox, initialSuper)
		} else {
			// classic recursive node creation (the default)
			rootNode = CreateNode(&workData, rootSeg, rootBox, initialSuper)
		}
	} else if input.multiTreeMode == MULTITREE_ROOT_ONLY {
		// Create multiple trees - bruteforce ROOT node among all (one-sided,
		// two-sided, or every - depending on input.specialRootMode), the REST
		// nodes are picked as NORMAL. So multiple trees are generated (in parallel
		// with a limited number of worker threads) and the "best" is chosen
		// This is NOT the way Zokumbsp does multi-tree, but rather a more
		// simple feature requested by user nicknamed jerko

		if workData.sidenessCache.maxKnownAlias == 0 {
			// Damn cache, if it was not built, needs still be locked. If was build,
			// should be locked already
			// This is really just used as defense against programmers who might
			// not have yet learned the fact that cache is built before
			// multitree and is shared between multitree threads
			workData.sidenessCache.readOnly = true
		}

		rootNode = MTPSentinel_MakeBestBSPTree(&workData, rootBox, initialSuper,
			input.specialRootMode)
	} else {
		Log.Panic("Multi-tree variant not implemented.\n")
	}
	// NOTE rootBox, rootSeg, initialSuper must NOT be used past this point!

	// Ok, so now we are printing stats, checking limits, and reverting the
	// tree to produce the final data

	// Merge in (and print most stuff buffered) from the workData log (associated
	// with respective tree)
	Log.Merge(workData.mlog, "Merging buffered output (if any) from the chosen tree.\n")
	workData.mlog = nil

	// Debugging helper. Sits here just in case some programmer decides to use
	// Log.Push() to debug things related to nodes (this is where they actually
	// get printed)
	Log.Flush()

	Log.Printf("Created %d subsectors, %d nodes. Got %d segs. Split segs %d times.\n",
		workData.totals.numSSectors, workData.totals.numNodes,
		workData.totals.numSegs, workData.totals.segSplits)

	if requestedSolid {
		// If requested solid blockmap before, tell them we are done with it
		// - the CreateNode() ran its course
		input.bcontrol <- BconRequest{
			Sender:  SOLIDBLOCKS_NODES,
			Message: BCON_DONE_WITH_SOLID_BLOCKMAP,
		}
	}
	hLeft := 0
	hRight := 0
	if rootNode.nextL != nil {
		hLeft = HeightOfNodes(rootNode.nextL) + 1
	}
	if rootNode.nextR != nil {
		hRight = HeightOfNodes(rootNode.nextR) + 1
	}

	Log.Printf("Splits reused already created vertices to avoid duplicates: %d times\n", workData.vertexExists)

	if input.stkNode && config.Deterministic { // reference to global: config
		if input.nodeType == NODETYPE_VANILLA {
			workData.RearrangeBSPVanilla(rootNode)
		} else if input.nodeType == NODETYPE_DEEP {
			workData.RearrangeBSPDeep(rootNode)
		} else { // NODETYPE_EXTENDED / NODETYPE_COMPRESSED
			workData.RearrangeBSPExtended(rootNode)
		}
	}

	Log.Printf("Height of left and right subtrees = (%d,%d)\n", hLeft, hRight)
	if input.nodeType == NODETYPE_VANILLA {
		workData.reverseNodes(rootNode)
	} else {
		// Same node structure for Deep, Extended and Compressed non-GL nodes
		workData.reverseDeepNodes(rootNode)
	}
	Log.Printf("Max seg count in subsector: %d\n", workData.totals.maxSegCountInSubsector)
	if workData.totals.preciousSplit > 0 {
		// TODO Hexen contains some awkwardly designed polyobjects, placed in
		// non-convex sectors, sectors joined with others (map10) although
		// not remotely etc. Until polyobject containing part is determined more
		// precisely, this report (and the total itself is not much use).
		// NOTE I intend to use this field (totals.preciousSplit) for multi-tree
		// so that it would select trees where polyobjs were not damaged. But
		// currently the number paints worse picture than there actually is: in
		// a lot of cases with non-zero number polyobjs are functioning as
		// intended
		// It seems that rather than being computed in-progress, the splits need
		// to be counted after the tree is done, via determining the number of
		// subsectors within the sector, and plus determining whether the sector
		// was originally convex, and if not, the minimal number of convex
		// polygons it could have been split into (not entirely sure, mind you...
		// I doubt we can read the mind of the user to know which exactly
		// subsection of sector should have been polyobject performing in if
		// sector is non-convex. Who knows if the user may have made a blunder
		// and it was not supposed to be working anyway?)
		//Log.Printf("Bad splits (split precious lines or lines of polyobj containing sector): %d\n",
		//	workData.totals.preciousSplit)
	}

	// Now we can actually UNDO all our hard work if limits were exceeded
	if uint32(workData.totals.numSSectors)&workData.SsectorMask == workData.SsectorMask {
		Log.Error("Number of subsectors is too large (%d) for this format. Lumps NODES, SEGS, SSECTORS will be emptied.\n",
			workData.totals.numSSectors)
		workData.emptyNodesLumps()
	} else if workData.tooManySegsCantFix() {
		Log.Error("Number of segs is too large (%d) for this format. Lumps NODES, SEGS, SSECTORS will be emptied.\n",
			len(workData.segs))
		workData.emptyNodesLumps()
	}

	if input.cacheSideness {
		Log.Printf("[nodes] Sideness cache was generated for %d INITIAL aliases (out of %d total aliases) for %d (out of %d) linedefs \n",
			workData.sidenessCache.maxKnownAlias,
			workData.segAliasObj.maxAlias, workData.sidenessCache.colCount,
			input.lines.Len())
	}

	if input.nodeType == NODETYPE_DEEP {
		input.nodesChan <- NodesResult{
			deepNodes:      workData.deepNodes,
			deepSegs:       workData.deepSegs,
			deepSubsectors: workData.deepSubsectors,
		}
	} else if input.nodeType == NODETYPE_VANILLA {
		input.nodesChan <- NodesResult{
			nodes:      workData.nodes,
			segs:       workData.segs,
			subsectors: workData.subsectors,
		}
	} else { // NODETYPE_EXTENDED / NODETYPE_COMPRESSED
		if workData.deepNodes == nil {
			// Was emptied for whatever reason (number of subsectors too
			// large even for this format?! see check somewhere above)
			input.nodesChan <- NodesResult{}
		} else {
			// Ok, so we need to write whole bytestream. And we will allocate
			// it all in memory for now, for simplicity
			input.nodesChan <- NodesResult{
				rawNodes: workData.getZdoomNodesBytes(),
			}
		}
	}
	Log.Printf("Nodes took %s\n", time.Since(start))
}

func RoundToPrecision(n float64) Number {
	if n < 0 {
		return Number(-1 * int(.5-n))
	} else {
		return Number(int(.5 + n))
	}
}

func (n Number) Abs() Number {
	if n < Number(0) {
		return -n
	} else if n > Number(0) {
		return n
	} else {
		return 0
	}
}

func (n Number) Ceil() int {
	return int(n)
}

func (n Number) Floor() int {
	return int(n)
}

func (n Number) Floor16() int16 {
	return int16(n)
}

// On Number, this truncates x. The value of n is ignored (is used only
// to determine type of truncation). See also zdefs.go!ZNumber.Trunc
func (n Number) Trunc(x float64) float64 {
	return float64(int64(x))
}

func PickNodeFuncFromOption(userOption int) PickNodeFunc {
	switch userOption {
	case PICKNODE_TRADITIONAL:
		{
			return PickNode_traditional
		}
	case PICKNODE_VISPLANE:
		{
			return PickNode_visplaneKillough
		}
	case PICKNODE_VISPLANE_ADV:
		{
			return PickNode_visplaneVigilant
		}
	case PICKNODE_MAELSTROM:
		{
			return PickNode_maelstrom
		}
	case PICKNODE_ZENLIKE:
		{
			return PickNode_ZennodeDepth
		}
	default:
		{
			Log.Panic("Invalid argument\n")
			return nil
		}
	}
}

func CreateNodeSSFromOption(userOption int) CreateNodeSSFunc {
	if userOption == PICKNODE_VISPLANE_ADV {
		return CreateNodeForSingleSector
	}
	return CreateNode
}

func StkCreateNodeSSFromOption(userOption int) StkCreateNodeSSFunc {
	if userOption == PICKNODE_VISPLANE_ADV {
		return StkCreateNodeForSingleSector
	}
	return StkCreateNode
}

func StkSingleSectorDivisorFromOption(userOption int) SingleSectorDivisorFunc {
	if userOption == PICKNODE_VISPLANE_ADV {
		return VigilantSingleSectorDivisor
	}
	return DefaultSingleSectorDivisor
}

func MinorCmpFuncFromOption(userOption int) MinorIsBetterFunc {
	switch userOption {
	case MINOR_CMP_NOOP:
		{
			return minorIsBetter_Dummy
		}
	case MINOR_CMP_SEGS:
		{
			return minorIsBetter_Segs
		}
	case MINOR_CMP_SECTORS:
		{
			return minorIsBetter_Sectors
		}
	case MINOR_CMP_BALANCE:
		{
			return minorIsBetter_Balanced
		}
	case MINOR_CMP_DEPTH:
		{
			// the decisive minor comparison is done elsewhere
			return minorIsBetter_Always
		}
	default:
		{
			Log.Panic("Invalid argument\n")
			return nil
		}
	}
}

func (w *NodesWork) emptyNodesLumps() {
	w.deepNodes = nil
	w.deepSegs = nil
	w.deepSubsectors = nil
	w.nodes = nil
	w.segs = nil
	w.subsectors = nil
}

// tooManySegsCantFix checks if there is overflow in SSECTORS when referencing
// segs. In some cases, it can (and then will) be fixed, so this function can
// mutate data
func (w *NodesWork) tooManySegsCantFix() bool {
	// Currently only normal nodes are checked, not deep nodes or extended nodes
	// (w.segs expected to be nil for both of those)
	if w.segs == nil { // assume no overflow is possible for advanced node formats
		return false
	}
	l := len(w.subsectors)
	if l == 0 { // wtf number of subsectors should never be zero
		return true
	}

	UNSIGNED_MAXSEGINDEX := uint16(65535)
	VANILLA_MAXSEGINDEX := uint16(32767)

	if w.lastSubsectorOverflows(UNSIGNED_MAXSEGINDEX) {
		couldFix, pivotSsector := w.fitSegsToTarget(UNSIGNED_MAXSEGINDEX)
		if !couldFix {
			return true
		}
		Log.Printf("You almost exceeded UNSIGNED addressable seg limit in SSECTORS (that would have made it invalid for all ports) - managed to reorder segs to avoid that. Changed subsector: %d\n",
			pivotSsector)
		Log.Printf("Too many segs to run in vanilla, need ports that treat FirstSeg field in SUBSECTORS as unsigned.\n")
	} else if w.lastSubsectorOverflows(VANILLA_MAXSEGINDEX) {
		couldFix, pivotSsector := w.fitSegsToTarget(VANILLA_MAXSEGINDEX)
		if !couldFix {
			Log.Printf("Too many segs to run in vanilla, need ports that treat FirstSeg field in SUBSECTORS as unsigned.\n")
			// But it can run in some ports, so don't return true (failure)
		} else {
			Log.Printf("You almost exceeded vanilla addressable seg limit in SSECTORS - managed to reorder segs to avoid that. Changed subsector: %d\n",
				pivotSsector)
		}
	}

	return false
}

// lastSubsectorOverflows returns true, if at least one of the following
// condition regarding last subsector is true:
// 1. FirstSeg field is not correct because original value overflowed int16
// type (was >= 65536). This is detected by adding FirstSeg and SegCount and
// comparing to total seg count
// 2. FirstSeg field value exceeds maxSegIndex
// NOTE that while (the converted version of) this might end up in znodegen.go,
// it won't be actually called there
func (w *NodesWork) lastSubsectorOverflows(maxSegIndex uint16) bool {
	l := len(w.subsectors)
	firstSeg := int(w.subsectors[l-1].FirstSeg)
	segCnt := int(w.subsectors[l-1].SegCount)
	if len(w.segs) > (firstSeg + segCnt) {
		// seg count is not valid
		return true
	}
	if firstSeg > int(maxSegIndex) {
		return true
	}
	return false
}

// fitSegsToTarget reorders seg array (for SEG lump) so that all subsectors
// can have their FirstSeg value <= maxSegIndex, if possible, and updates
// subsectors array to point to correct segs. It is assumed this value overflows
// (must have been tested before call)
// If this couldn't be done, return false
// Assumes segs are not shared between different subsectors
// Assumes sequential order of segs matches sequential order of subsectors
// before call
// NOTE that while (the converted version of) this might end up in znodegen.go,
// it won't be actually called there
func (w *NodesWork) fitSegsToTarget(maxSegIndex uint16) (bool, int) {
	newMaxSegIndex := uint32(len(w.segs)) - w.totals.maxSegCountInSubsector
	if newMaxSegIndex > uint32(maxSegIndex) {
		// Nothing can be done
		return false, 0
	}

	// not necessary the only one, btw. We'll fetch the last one
	biggestSubsector := -1
	for i := len(w.subsectors) - 1; i >= 0; i-- {
		if uint32(w.subsectors[i].SegCount) == w.totals.maxSegCountInSubsector {
			biggestSubsector = i
		}
	}
	if biggestSubsector == -1 {
		Log.Panic("Couldn't find subsector whose seg count matches computed maximum. (programmer error)\n")
	}

	// if maxSegIndex was 65535, all FirstSeg values following biggestSubsector
	// were overflowed, so one has to recompute the value from scratch
	newAddr := uint16(0)
	if biggestSubsector > 0 {
		newAddr = w.subsectors[biggestSubsector-1].FirstSeg +
			w.subsectors[biggestSubsector-1].SegCount
	}

	pivot := w.subsectors[biggestSubsector].FirstSeg
	pivot2 := pivot + w.subsectors[biggestSubsector].SegCount
	// Move pivot:pivot2 subslice to the end of w.segs slice
	w.segs = append(w.segs[:pivot], append(w.segs[pivot2:], w.segs[pivot:pivot2]...)...)
	for i := biggestSubsector + 1; i < len(w.subsectors); i++ {
		w.subsectors[i].FirstSeg = newAddr
		newAddr += w.subsectors[i].SegCount
	}
	w.subsectors[biggestSubsector].FirstSeg = uint16(newMaxSegIndex)
	return true, biggestSubsector
}

// IsTooDetailed returns whether level is detailed enough that it needs more
// accurate intersection evaluation than that provided by original
// doLinesIntersect checker from BSP v5.2 to ensure nodes are built correctly
// The improvement on accuracy does result in more seg/sector splits, however,
// which means it is not advised to be used for vanilla maps unless absolutely
// necessary
func IsTooDetailed(lines AbstractLines, linesToIgnore []bool, bounds *NodeBounds) bool {
	bmi := BlockmapInput{
		lines:           lines,
		bounds:          *NodeBoundsToLevelBounds(bounds),
		XOffset:         0,
		YOffset:         0,
		useZeroHeader:   false,
		internalPurpose: true,
		gcShield:        nil,
		linesToIgnore:   linesToIgnore,
	}
	bm := CreateBlockmap(bmi)
	hitmap := make([]int, len(bm.blocklist))
	for i, _ := range bm.blocklist {
		score := 0
		for _, l := range bm.blocklist[i] {
			id := uint16(l)
			x1, y1, x2, y2 := lines.GetAllXY(id)
			dx := x2 - x1
			dy := y2 - y1
			if dx < 0 {
				dx = -dx
			}
			if dy < 0 {
				dy = -dy
			}
			score++
			twosided := uint16(lines.GetFlags(id))&LF_TWOSIDED == LF_TWOSIDED
			orthogonal := dx == 0 || dy == 0
			short := dx < 8 && dy < 8
			veryshort := dx < 4 && dy < 4
			if !orthogonal {
				score++
			}
			if short {
				score += 1
			}
			if veryshort {
				score += 3
			}
			if !orthogonal && short && twosided {
				score += 2
			}
		}
		hitmap[i] = score
	}
	scoremax := 0
	xl := int(bm.header.XBlocks)
	xh := xl - 1
	yl := int(bm.header.YBlocks)
	yh := yl - 1
	for i, _ := range hitmap {
		xj := i / xl
		xi := i - xj*xl
		// Consider entire 3x3 blocks of 128x128 pixels, centered on current
		// block
		score9 := hitmapAccAllIdx(xi, xj, xh, yh, hitmap)
		if scoremax < score9 {
			scoremax = score9
		}
	}
	ret := scoremax > DETAILED_LEVEL_THRESHOLD
	Log.Verbose(1, "<High amount of detail> = %t maxscore = %d threshold = %d\n", ret, scoremax, DETAILED_LEVEL_THRESHOLD)
	return ret
}

func hitmapAccIdx(i, j int, hiI, hiJ int, hitmap []int) int {
	if i < 0 || j < 0 || i > hiI || j > hiJ {
		return 0
	}
	x := (hiI+1)*j + i
	if x > len(hitmap)-1 {
		return 0 // safety measure
	}
	return hitmap[x]
}

func hitmapAccAllIdx(i, j int, hiI, hiJ int, hitmap []int) int {
	return hitmapAccIdx(i-1, j-1, hiI, hiJ, hitmap) +
		hitmapAccIdx(i, j-1, hiI, hiJ, hitmap) +
		hitmapAccIdx(i+1, j-1, hiI, hiJ, hitmap) +
		hitmapAccIdx(i-1, j, hiI, hiJ, hitmap) +
		hitmapAccIdx(i, j, hiI, hiJ, hitmap) +
		hitmapAccIdx(i+1, j, hiI, hiJ, hitmap) +
		hitmapAccIdx(i-1, j+1, hiI, hiJ, hitmap) +
		hitmapAccIdx(i, j+1, hiI, hiJ, hitmap) +
		hitmapAccIdx(i+1, j+1, hiI, hiJ, hitmap)
}

// createSegs iterates over linedefs to create initial segs
// In some circumstances, it won't create a seg:
// 1. Linedef is zero length
// 2. A sidedef has a lower texture with BSPNOSEG name, seg will not be created
// for this side (but may be created for other side)
// 3. Linedef was tagged with "no render" tag (998 for Doom)
// 4. Removing "invisible" lines was allowed in configuration, and the
// line has same sector on both sides and doesn't seem to be part of
// self-referencing sector effect nor was tagged as precious, has no textures
// either (handled by culler)
// 5. Fast/remote scroller effect dummy lines (linesToIgnore[i] == true)
func createSegs(lines WriteableLines, sidedefs []Sidedef,
	linesToIgnore []bool, extNode bool) ([]*NodeSeg, []NodeVertex) {
	res := make([]*NodeSeg, 0, 65536)
	var rootCs, lastCs *NodeSeg
	// This is responsible for handling for which lines we don't create segs,
	// if nil then no segs will be skipped besides the explicitly marked ones
	var cull *Culler
	if config.CullInvisibleSegs != CULL_SEGS_DONT { // reference to global: config
		// Ok, so we will skip some linedefs instead of creating segs for them
		cull = new(Culler)
		// What algo is used to determine what gets skipped (and what gets
		// processed later, when having evidence that it was skipped wrong)
		if config.CullInvisibleSegs == CULL_SEGS_SLOPPY { // reference to global: config
			cull.SetMode(CREATE_SEGS_SLOPPY, sidedefs)
		} else {
			cull.SetMode(CREATE_SEGS, sidedefs)
		}
		cull.SetWriteableLines(lines)
	}
	// This array MUST match the vertices array that lines used, that is
	// corresponding indexes MUST represent the same vertex, even if in
	// different ways
	myVertices := make([]NodeVertex, lines.GetVerticesCount())
	l := lines.Len()
	for i := uint16(0); i < l; i++ {
		if linesToIgnore != nil && linesToIgnore[i] {
			continue
		}
		x1, y1, x2, y2 := lines.GetAllXY(i)
		vidx1, vidx2 := lines.GetLinedefVertices(i)
		storeNodeVertex(myVertices, x1, y1, uint32(vidx1))
		storeNodeVertex(myVertices, x2, y2, uint32(vidx2))
		if x1 == x2 && y1 == y2 {
			continue
		}
		// For nil culler, this is always false. Note that we need to let
		// culler process explicit "do-not-render" lines for correct analysis
		// (and culler needs to duplicate this check to ensure he doesn't spew
		// them back)
		culled := cull.AddLine(i)
		if lines.IsDoNotRender(i) {
			Log.Verbose(1, "Linedef %d will not be rendered because it was either tagged so (tag 998) or assigned an action with numeric code 1086.\n",
				i)
			continue
		}

		if culled {
			continue // skip creating segs for this line - at least, for now
		} else {
			// Creating segs for this line is not a question - do it
			firstSdef := lines.GetSidedefIndex(i, true)
			secondSdef := lines.GetSidedefIndex(i, false)
			addSegsPerLine(myVertices, i, lines, vidx1, vidx2, firstSdef, secondSdef,
				&rootCs, &lastCs, sidedefs, &res, extNode)
		}

	}
	// If culler is not nil, will identify self-referencing sectors and their
	// lines that need segs creation but were skipped. For nil culler these
	// were not skipped anyway
	cull.Analyze()

	// Now that analysis determined some lines were skipped in error, we create
	// segs for those lines here
	unculled := false
	for cull.SpewBack() {
		line := cull.GetLine()
		vidx1, vidx2 := lines.GetLinedefVertices(line)
		firstSdef := lines.GetSidedefIndex(line, true)
		secondSdef := lines.GetSidedefIndex(line, false)
		if cull.GetMode() == CREATE_SEGS_SLOPPY {
			// Sloppy gives false positives often enough, and has a chance
			// of false negative (the latter would happen in a rather
			// non-conventional setup).
			Log.Verbose(1, "Linedef %d will be rendered after all, I think sector %d may be self-referencing.\n",
				line, sidedefs[firstSdef].Sector)
		} else {
			Log.Verbose(1, "Linedef %d will be rendered after all - used to create self-referencing effect for sector %d.\n",
				line, sidedefs[firstSdef].Sector)
		}
		// We need to add segs for this line after all.
		addSegsPerLine(myVertices, line, lines, vidx1, vidx2, firstSdef, secondSdef,
			&rootCs, &lastCs, sidedefs, &res, extNode)
		unculled = true
	}

	if unculled {
		// A note in Zennode claims that it is important for some effects (which
		// ones?) that order of segs match the order of linedefs.
		restoreSegOrder(res)
	}

	return res, myVertices
}

func addSegsPerLine(myVertices []NodeVertex, i uint16, lines WriteableLines, vidx1, vidx2 int,
	firstSdef, secondSdef uint16, rootCs **NodeSeg, lastCs **NodeSeg, sidedefs []Sidedef,
	res *[]*NodeSeg, extNode bool) {
	var lcs, rcs *NodeSeg
	horizon := lines.IsHorizonEffect(i)
	action := lines.GetAction(i)
	bamEffect := lines.GetBAMEffect(i) // support for linedef actions = 1080...1083
	// FIXME what if sidedef index is invalid? Well, so far it looks like we
	// don't perform validity check for ANY references (say, from lines to
	// vertices)
	if firstSdef != SIDEDEF_NONE {
		if !bytes.Equal(sidedefs[firstSdef].LoName[:], []byte("BSPNOSEG")) {
			// Action = 1085 is do not render front side (zokumbsp)
			if action == 1085 {
				Log.Verbose(1, "Will not create seg for front sidedef of linedef %d - instructed so by action's numeric code set to 1085 on linedef.\n",
					i)
			} else {
				lcs = addSeg(myVertices, i, vidx1, vidx2, rootCs,
					&(sidedefs[firstSdef]), lastCs, horizon, bamEffect)
				*res = append(*res, lcs)
			}
		} else {
			// Implementing BSPNOSEG (zokumbsp specification)
			Log.Verbose(1, "Will not create seg for FRONT sidedef of linedef %d (because BSPNOSEG was used as lower texture)\n", i)
		}
	} else {
		Log.Printf("Warning: linedef %d doesn't have front sidedef\n", i)
	}
	if secondSdef != SIDEDEF_NONE {
		if !bytes.Equal(sidedefs[firstSdef].LoName[:], []byte("BSPNOSEG")) {
			// Action = 1084 is do not render back side (zokumbsp)
			if action == 1084 {
				Log.Verbose(1, "Will not create seg for BACK sidedef of linedef %d - instructed so by action's numeric code set to 1084 on linedef.\n",
					i)
			} else {
				rcs = addSeg(myVertices, i, vidx2, vidx1, rootCs,
					&(sidedefs[secondSdef]), lastCs, horizon, bamEffect)
				rcs.flags |= SEG_FLAG_FLIP
				*res = append(*res, rcs)
			}
		} else {
			// Implementing BSPNOSEG (zokumbsp specification)
			Log.Verbose(1, "Will not create seg for back sidedef of linedef %d (because BSPNOSEG was used as lower texture)\n", i)
		}
	} else if uint16(lines.GetFlags(i))&LF_TWOSIDED != 0 {
		Log.Printf("Warning: linedef %d is marked as 2-sided but doesn't have back sidedef\n", i)
	}
	if extNode && (horizon || bamEffect.Action != BAM_NOSPECIAL) {
		// Internally, angle is still computed with all the effects. But with
		// or without the effect, angle is not stored in Zdoom nodes format
		// because that format does not provide any field for it
		Log.Verbose(1, "Linedef %d has BAM or horizon effect specified, but it cannot be supported in Zdoom nodes format and so will be ignored.\n",
			i)
	}
	if lcs != nil && rcs != nil {
		// Partner segs as used here are optimization technique to speed up
		// PickNode_XXX subroutines, and have nothing to do with GL meaning of
		// the term. We build regular software nodes here.
		// When a seg gets split, its partner seg also gets split, but partners
		// will "break up" if they go to different subtrees (left, right).
		// In so far as partnership is preserved (same subtree), partners are
		// always encountered next to each other
		lcs.partner = rcs
		rcs.partner = lcs
	}
}

func storeNodeVertex(vs []NodeVertex, x, y int, idx uint32) {
	vs[idx] = NodeVertex{
		X:   Number(x),
		Y:   Number(y),
		idx: idx,
	}
}

func addSeg(vs []NodeVertex, i uint16, vidx1, vidx2 int, rootCs **NodeSeg,
	sdef *Sidedef, lastCs **NodeSeg, horizon bool, bamEffect BAMEffect) *NodeSeg {
	s := new(NodeSeg)
	if *lastCs == nil {
		*rootCs = s
		*lastCs = s
	} else {
		(*lastCs).next = s
		*lastCs = s
	}
	s.next = nil
	s.StartVertex = &(vs[vidx1])
	s.EndVertex = &(vs[vidx2])
	s.sector = sdef.Sector
	s.partner = nil
	s.pex = s.EndVertex.X
	s.psx = s.StartVertex.X
	s.pdx = s.pex - s.psx
	s.pey = s.EndVertex.Y
	s.psy = s.StartVertex.Y
	s.pdy = s.pey - s.psy
	s.perp = s.pdx*s.psy - s.psx*s.pdy
	s.Linedef = i
	flen := math.Sqrt(float64(s.pdx)*float64(s.pdx) + float64(s.pdy)*float64(s.pdy))
	s.len = Number(flen)
	s.Offset = 0
	if bamEffect.Action == BAM_REPLACE { // zokumbsp actions 1081, 1083
		// Completely replaces value, computation not needed
		s.Angle = bamEffect.Value
	} else {
		// Default computation of angle
		s.Angle = int16(computeAngle(s.pdx, s.pdy))
		// Check if need to add to this value
		if bamEffect.Action == BAM_ADDITIVE { // zokumbsp actions 1080, 1082
			s.Angle += bamEffect.Value
		}
	}
	// BSP v5.2 horizon effect was specified for linedef
	if horizon {
		s.Angle += int16(float64(sdef.XOffset) * float64(65536.0/360.0))
	}
	return s
}

func computeAngle(dx, dy Number) int {
	w := math.Atan2(float64(dy), float64(dx)) * float64(65536.0/(math.Pi*2))

	if w < 0 {
		w = 65536.0 + w
	}

	return int(w)
}

// restoreSegOrder orders segs so that indices of linedef they come from are
// in ascending order. This occurs naturally when segs are created, unless an
// option to remove some segs was used, which causes some segs to be inserted
// out-of-order
// NOTE not suitable for sorting segs after splits already happened - built
// under assumption there is at most 2 segs per linedef encountered in the
// supplied array
func restoreSegOrder(segs []*NodeSeg) {
	sort.Sort(InitialSegOrderByLinedef(segs))
	for i, x := range segs {
		if i < len(segs)-1 {
			x.next = segs[i+1]
		} else {
			x.next = nil
		}
	}
	// For safety, this should be already guaranteed! If/when GL nodes
	// generation is implemented, such error would be grave
	for _, x := range segs {
		if x.partner != nil && x.next != x.partner && x.partner.next != x {
			x.partner = nil
			Log.Verbose(1, "restoreSegOrder: partner was not referencing a neighbor seg!\n")
		}
	}
}

type InitialSegOrderByLinedef []*NodeSeg

func (x InitialSegOrderByLinedef) Len() int { return len(x) }
func (x InitialSegOrderByLinedef) Less(i, j int) bool {
	// primitive ordering - doesn't include offset, because damn offset
	// should be always ZERO for the intended use case
	return (x[i].Linedef < x[j].Linedef) ||
		(x[i].Linedef == x[j].Linedef && x[i].getFlip() < x[j].getFlip())
}
func (x InitialSegOrderByLinedef) Swap(i, j int) { x[i], x[j] = x[j], x[i] }

// FindLimits scans all SEGs in a list to find a minimal bounding box within
// which they all fit. It does this by stepping through the segs and
// comparing the vertices at both ends (taken from BSP nodebuilder)
func FindLimits(ts *NodeSeg) *NodeBounds {
	var r NodeBounds
	// using 32-bit signed min/max values as initial, even though most engines
	// don't have coords this big
	r.Xmax = -2147483648
	r.Ymax = -2147483648
	r.Xmin = 2147483647
	r.Ymin = 2147483647
	for true {
		fv := ts.StartVertex
		tv := ts.EndVertex

		if fv.X < Number(r.Xmin) {
			r.Xmin = fv.X.Floor()
		}
		if fv.X > Number(r.Xmax) {
			r.Xmax = fv.X.Ceil()
		}
		if fv.Y < Number(r.Ymin) {
			r.Ymin = fv.Y.Floor()
		}
		if fv.Y > Number(r.Ymax) {
			r.Ymax = fv.Y.Ceil()
		}

		if tv.X < Number(r.Xmin) {
			r.Xmin = tv.X.Floor()
		}
		if tv.X > Number(r.Xmax) {
			r.Xmax = tv.X.Ceil()
		}
		if tv.Y < Number(r.Ymin) {
			r.Ymin = tv.Y.Floor()
		}
		if tv.Y > Number(r.Ymax) {
			r.Ymax = tv.Y.Ceil()
		}

		if ts.next == nil {
			break
		}
		ts = ts.next
	}
	return &r
}

func NodeBoundsToLevelBounds(n *NodeBounds) *LevelBounds {
	return &LevelBounds{
		Xmin: int16(n.Xmin),
		Ymin: int16(n.Ymin),
		Xmax: int16(n.Xmax),
		Ymax: int16(n.Ymax),
	}
}

func UnionNodeBounds(b1, b2 *NodeBounds) *NodeBounds {
	bu := &NodeBounds{
		Xmin: b1.Xmin,
		Xmax: b1.Xmax,
		Ymin: b1.Ymin,
		Ymax: b1.Ymax,
	}
	if b2.Xmin < bu.Xmin {
		bu.Xmin = b2.Xmin
	}
	if b2.Xmax > bu.Xmax {
		bu.Xmax = b2.Xmax
	}
	if b2.Ymin < bu.Ymin {
		bu.Ymin = b2.Ymin
	}
	if b2.Ymax > bu.Ymax {
		bu.Ymax = b2.Ymax
	}
	return bu
}

func GetLineBounds(lines AbstractLines) *NodeBounds {
	var r NodeBounds
	// using 32-bit signed min/max values as initial, even though most engines
	// don't have coords this big
	r.Xmax = -2147483648
	r.Ymax = -2147483648
	r.Xmin = 2147483647
	r.Ymin = 2147483647
	l := lines.Len()
	for i := uint16(0); i < l; i++ {
		x1, x2, y1, y2 := lines.GetAllXY(i)

		if x1 < r.Xmin {
			r.Xmin = x1
		}
		if x1 > r.Xmax {
			r.Xmax = x1
		}
		if y1 < r.Ymin {
			r.Ymin = y1
		}
		if y1 > r.Ymax {
			r.Ymax = y1
		}

		if x2 < r.Xmin {
			r.Xmin = x2
		}
		if x2 > r.Xmax {
			r.Xmax = x2
		}
		if y2 < r.Ymin {
			r.Ymin = y2
		}
		if y2 > r.Ymax {
			r.Ymax = y2
		}
	}
	return &r
}

// splitDist computes offset: distance along linedef to start of seg
// takes flip bit into account
func splitDist(lines AbstractLines, seg *NodeSeg) int {
	var dx, dy float64

	if seg.getFlip() == 0 {
		// from start vertex of linedef
		x1, y1, _, _ := lines.GetAllXY(seg.Linedef)
		fx1 := Number(x1)
		fy1 := Number(y1)
		dx = float64(fx1 - seg.StartVertex.X)
		dy = float64(fy1 - seg.StartVertex.Y)
	} else { // seg.Flip == 1
		// from end vertex of linedef
		_, _, x2, y2 := lines.GetAllXY(seg.Linedef)
		fx2 := Number(x2)
		fy2 := Number(y2)
		dx = float64(fx2 - seg.StartVertex.X)
		dy = float64(fy2 - seg.StartVertex.Y)
	}

	if dx == 0 && dy == 0 {
		Log.Error("Trouble in splitDist %d!%d %f,%f\n", seg.Linedef, seg.getFlip(), dx, dy)
	}
	t := math.Sqrt((dx * dx) + (dy * dy))
	return int(t)
}

// computeIntersection calculates the point of intersection of two lines.
// ps?->pe? & ls?->le?
// returns xcoord int, ycoord int
func (c *IntersectionContext) computeIntersection() (Number, Number) {
	dx := c.pex - c.psx
	dy := c.pey - c.psy
	dx2 := c.lex - c.lsx
	dy2 := c.ley - c.lsy

	if dx == 0 && dy == 0 {
		Log.Error("Trouble in computeIntersection dx,dy\n")
	}
	if dx2 == 0 && dy2 == 0 {
		Log.Error("Trouble in computeIntersection dx2,dy2\n")
	}
	// Truncation was introduced because this is how BSP v5.2 computed this
	// On regular/Deep nodes, I keep it, but it is not performed for extended
	// nodes. Testing should be performed to see if there is reason to keep it
	// for regular version
	// UPD: made conversion more resistant to numerical overflow. This might
	// have performance impact, especially on 32-bit computers.
	l2 := dx2.Trunc(math.Sqrt(float64(dx2)*float64(dx2) + float64(dy2)*float64(dy2)))

	a := float64(dx)        // no normalization of a,b necessary,
	b := float64(dy)        // since division by d in formula for w
	a2 := float64(dx2) / l2 // cancels it out.
	b2 := float64(dy2) / l2
	d := b*a2 - a*b2
	if d != 0 {
		w := ((a * float64(c.lsy-c.psy)) + (b * float64(c.psx-c.lsx))) / d

		a = float64(c.lsx) + (a2 * w)
		b = float64(c.lsy) + (b2 * w)
		return RoundToPrecision(a), RoundToPrecision(b)
	} else {
		// outx = c.lsx, outy = c.lsy
		return c.lsx, c.lsy
	}
}

// psx,psy,pex,pey = partition line coordinates
// lsx,lsy,lex,ley = checking line coordinates
// doLinesIntersect returns 'val' which has 3 bits assigned to the the start and 3
// to the end. These allow a decent evaluation of the lines state.
// bit 0,1,2 = checking lines starting point and bits 4,5,6 = end point
// these bits mean 	0,4 = point is on the same line
// 						1,5 = point is to the left of the line
//						2,6 = point is to the right of the line
// There are some failsafes in here, these mainly check for small errors in the
// side checker.
// VigilantDoomer: "small errors in side checker" - you see, PickNode_* doesn't
// evaluate where segs go precisely as it is done here (*might* have been done
// on purpose, for the sake of performance, by Lee Killough). Something to keep
// in mind going forward, as doing things the same way would enable me to borrow
// superblocks idea from AJ-BSP (the speed-up from that I expect would be
// greater than performance loss from doing things more accurately)
func doLinesIntersectStandard(c *IntersectionContext) uint8 {
	dx2 := c.psx - c.lsx // Checking line -> partition
	dy2 := c.psy - c.lsy
	dx3 := c.psx - c.lex
	dy3 := c.psy - c.ley

	a := c.pdy*dx2 - c.pdx*dy2
	b := c.pdy*dx3 - c.pdx*dy3
	if DiffSign(a, b) && (a != 0) && (b != 0) { // Line is split, just check that

		x, y := c.computeIntersection()
		dx2 = c.lsx - x // Find distance from line start
		dy2 = c.lsy - y // to split point
		if dx2 == 0 && dy2 == 0 {
			a = 0
		} else {
			// NOTE this margin may lead to errors on complex, very detailed
			// levels (or areas), so:
			// 1. For non-extended nodes, there is alternate function
			// doLinesIntersectDetail
			// 2. For extended nodes, I had to completely replace the check
			// with more expensive but accurate one from ZDBSP
			// -- VigilantDoomer
			l := WideNumber(dx2)*WideNumber(dx2) + WideNumber(dy2)*WideNumber(dy2)
			if l < WideNumber(4) {
				// If either ends of the split
				// are smaller than 2 pixs then
				// assume this starts on part line
				a = 0
			}
		}
		dx3 = c.lex - x // Find distance from line end
		dy3 = c.ley - y // to split point
		if dx3 == 0 && dy3 == 0 {
			b = 0
		} else {
			l := WideNumber(dx3)*WideNumber(dx3) + WideNumber(dy3)*WideNumber(dy3)
			if l < WideNumber(4) { // same as start of line
				b = 0
			}
		}
	}

	var val uint8

	if a == 0 {
		val = val | 16 // start is on middle
	} else if a < 0 {
		val = val | 32 // start is on left side
	} else {
		val = val | 64 // start is on right side
	}

	if b == 0 {
		val = val | 1 // end is on middle
	} else if b < 0 {
		val = val | 2 // end is on left side
	} else {
		val = val | 4 // end is on right side
	}

	return val
}

// doLinesIntersect for detailed maps
func doLinesIntersectDetail(c *IntersectionContext) uint8 {
	dx2 := c.psx - c.lsx // Checking line -> partition
	dy2 := c.psy - c.lsy
	dx3 := c.psx - c.lex
	dy3 := c.psy - c.ley

	a := c.pdy*dx2 - c.pdx*dy2
	b := c.pdy*dx3 - c.pdx*dy3
	if DiffSign(a, b) && (a != 0) && (b != 0) { // Line is split, just check that

		x, y := c.computeIntersection()
		dx2 = c.lsx - x // Find distance from line start
		dy2 = c.lsy - y // to split point
		if dx2 == 0 && dy2 == 0 {
			a = 0
		} else {
			// Margin decreased to account for maps such as in Water Spirit, etc.
			// -- VigilantDoomer
			l := WideNumber(dx2)*WideNumber(dx2) + WideNumber(dy2)*WideNumber(dy2)
			if l < WideNumber(2) { // Killough used 4 not 2
				// If either ends of the split
				// are smaller than 1 (was 2) pixs then
				// assume this starts on part line
				a = 0
			}
		}
		dx3 = c.lex - x // Find distance from line end
		dy3 = c.ley - y // to split point
		if dx3 == 0 && dy3 == 0 {
			b = 0
		} else {
			l := WideNumber(dx3)*WideNumber(dx3) + WideNumber(dy3)*WideNumber(dy3)
			if l < WideNumber(2) { // same as start of line
				b = 0
			}
		}
	}

	var val uint8

	if a == 0 {
		val = val | 16 // start is on middle
	} else if a < 0 {
		val = val | 32 // start is on left side
	} else {
		val = val | 64 // start is on right side
	}

	if b == 0 {
		val = val | 1 // end is on middle
	} else if b < 0 {
		val = val | 2 // end is on left side
	} else {
		val = val | 4 // end is on right side
	}

	return val
}

func DiffSign(a, b Number) bool {
	return (a ^ b) < 0
}

// IsItConvex returns whether the list of segs:
// 1. CONVEX_SUBSECTOR: is convex and fit to be put into a subsector, no
// futher partitioning is needed nor desired
// 2. NONCONVEX_ONESECTOR: is non-convex, but has only one sector so perhaps
// can use a special algorithm to do optimal partition of it
// 3. NONCONVEX_MULTISECTOR: generic non-convex soup which we have to recurse
// into with CreateNode calls
func (w *NodesWork) isItConvex(ts *NodeSeg) int {
	nonConvexityMode := NONCONVEX_ONESECTOR
	// All ssectors must come from same sector unless it's marked
	// "special" with sector tag >= 900. Original idea, Lee Killough

	var sector uint16
	if ts.getFlip() != 0 {
		sector = w.sides[w.lines.GetSidedefIndex(ts.Linedef, false)].Sector
	} else {
		sector = w.sides[w.lines.GetSidedefIndex(ts.Linedef, true)].Sector
	}
	if w.sectors[sector].Tag < 900 {
		line := ts.next
		for line != nil {
			var csector uint16
			if line.getFlip() != 0 {
				csector = w.sides[w.lines.GetSidedefIndex(line.Linedef, false)].Sector
			} else {
				csector = w.sides[w.lines.GetSidedefIndex(line.Linedef, true)].Sector
			}
			if csector != sector {
				if w.sectors[csector].Tag < 900 {
					return NONCONVEX_MULTISECTOR // MUST SPLIT
				} else {
					// Special partitioning mode for non-convex single sector
					// subtrees doesn't support hacks like this
					nonConvexityMode = NONCONVEX_MULTISECTOR
				}
			}
			line = line.next
		}
	} else {
		// Special partitioning mode for non-convex single sector
		// subtrees doesn't support hacks like this
		nonConvexityMode = NONCONVEX_MULTISECTOR
	}

	// all of the segs must be on the same side all the other segs
	var c IntersectionContext
	for line := ts; line != nil; line = line.next {
		c.psx = line.StartVertex.X
		c.psy = line.StartVertex.Y
		c.pex = line.EndVertex.X
		c.pey = line.EndVertex.Y
		c.pdx = c.psx - c.pex // Partition line DX,DY
		c.pdy = c.psy - c.pey
		for check := ts; check != nil; check = check.next {
			if line != check {
				c.lsx = check.StartVertex.X
				c.lsy = check.StartVertex.Y
				c.lex = check.EndVertex.X
				c.ley = check.EndVertex.Y
				val := w.doLinesIntersect(&c)
				if val&34 != 0 {
					if nonConvexityMode == NONCONVEX_ONESECTOR {
						w.mlog.DumpSegs(ts)
					}
					return nonConvexityMode // MUST SPLIT
				}
			}
		}
	}

	// no need to split the list: these Segs can be put in a SSector
	return CONVEX_SUBSECTOR
}

// Zennode (the nodebuilder, not the algo inspired by it) does special
// optimization for convex multi-sectors. But what are they in first place
func (w *NodesWork) IsConvexMultiSector(ts *NodeSeg) bool {
	multisector := false
	sector := ts.sector
	for check := ts; check != nil; check = check.next {
		if check.sector != sector {
			multisector = true
			break
		}
	}
	if !multisector { // had only single sector
		return false
	}

	// all of the segs must be on the same side all the other segs
	var c IntersectionContext
	for line := ts; line != nil; line = line.next {
		c.psx = line.StartVertex.X
		c.psy = line.StartVertex.Y
		c.pex = line.EndVertex.X
		c.pey = line.EndVertex.Y
		c.pdx = c.psx - c.pex // Partition line DX,DY
		c.pdy = c.psy - c.pey
		for check := ts; check != nil; check = check.next {
			if line != check {
				c.lsx = check.StartVertex.X
				c.lsy = check.StartVertex.Y
				c.lex = check.EndVertex.X
				c.ley = check.EndVertex.Y
				val := w.doLinesIntersect(&c)
				if val&34 != 0 { // non-convex
					return false
				}
			}
		}
	}
	return true // convex MULTI-sector
}

// Adds a subsector. This version handles only deep and vanilla nodes format.
// For prototype of Zdoom counterpart, see zdefs.go!ZCreateSSector_Proto, it is
// that function from which ZExt_CreateSSector is generated, not this one
func (w *NodesWork) CreateSSector(tmps *NodeSeg) uint32 {
	// TODO check that stuff is within limits. Currently node lumps are emptied
	// AFTER the whole process completes if limit for current format was
	// exceeded. Might try to detect condition earlier?
	var subsectorIdx uint32
	var oldNumSegs uint32
	if w.nodeType == NODETYPE_DEEP {
		subsectorIdx = uint32(len(w.deepSubsectors))
		w.deepSubsectors = append(w.deepSubsectors, DeepSubSector{})
		oldNumSegs = uint32(len(w.deepSegs))
	} else { // vanilla
		subsectorIdx = uint32(len(w.subsectors))
		w.subsectors = append(w.subsectors, SubSector{})
		oldNumSegs = uint32(len(w.segs))
	}

	w.totals.numSSectors++
	var currentCount uint32
	if w.nodeType == NODETYPE_DEEP {
		w.deepSubsectors[subsectorIdx].FirstSeg = oldNumSegs
		for ; tmps != nil; tmps = tmps.next {
			w.deepSegs = append(w.deepSegs, DeepSeg{
				StartVertex: uint32(tmps.StartVertex.idx),
				EndVertex:   uint32(tmps.EndVertex.idx),
				Angle:       tmps.Angle,
				Linedef:     tmps.Linedef,
				Flip:        tmps.getFlip(),
				Offset:      tmps.Offset,
			})
		}
		currentCount = uint32(len(w.deepSegs)) - oldNumSegs
	} else { // vanilla
		w.subsectors[subsectorIdx].FirstSeg = uint16(oldNumSegs) // MIGHT overflow - try changing layout if that happens
		for ; tmps != nil; tmps = tmps.next {
			w.segs = append(w.segs, Seg{
				StartVertex: uint16(tmps.StartVertex.idx),
				EndVertex:   uint16(tmps.EndVertex.idx),
				Angle:       tmps.Angle,
				Linedef:     tmps.Linedef,
				Flip:        tmps.getFlip(),
				Offset:      tmps.Offset,
			})
		}
		currentCount = uint32(len(w.segs)) - oldNumSegs
	}
	if currentCount > w.totals.maxSegCountInSubsector {
		w.totals.maxSegCountInSubsector = currentCount
	}
	w.totals.numSegs += currentCount
	if w.nodeType == NODETYPE_DEEP {
		w.deepSubsectors[subsectorIdx].SegCount = uint16(currentCount)
	} else {
		w.subsectors[subsectorIdx].SegCount = uint16(currentCount)
	}
	return uint32(w.totals.numSSectors - 1)
}

// Adds a new vertex at specified position (or might found an existing one and
// return it).
func (w *NodesWork) AddVertex(x, y Number) *NodeVertex {
	rec := SimpleVertex{int(x), int(y)}
	idx, exists := w.vertexCache[rec]
	if exists { // happens rather rarely, and not at all on some maps
		w.vertexExists++
		//w.mlog.Printf("Vertex that existed: %d\n", idx)
		return &(w.vertices[idx])
	}
	idx = len(w.vertices)
	if w.lines.AddVertex(int(x), int(y)) != idx {
		Log.Panic("Inconsistent state in NodesWork.AddVertex\n")
	}
	w.vertices = append(w.vertices, NodeVertex{
		X:   x,
		Y:   y,
		idx: uint32(idx),
	})
	w.vertexCache[rec] = idx
	return &(w.vertices[idx])
}

func (w *NodesWork) SetNodeCoords(part *NodeSeg, bbox *NodeBounds,
	c *IntersectionContext) {
	// In node format without extra precision of segs, part may no longer be
	// alongside the original linedef, so picking linedef coords instead of
	// seg coords (like some nodebuilders do who also do GL nodes output) is not
	// a good idea.
	w.nodeX = int(part.StartVertex.X)
	w.nodeY = int(part.StartVertex.Y)
	w.nodeDx = part.EndVertex.X.Ceil() - w.nodeX
	w.nodeDy = part.EndVertex.Y.Ceil() - w.nodeY
	// Check if there is overflow
	if w.nodeDx <= 32767 && w.nodeDy <= 32767 && w.nodeDx >= -32768 &&
		w.nodeDy >= -32768 {
		// No overflow - good
		return
	}

	// If we are still here, we have an atypically long line.

	// Scale down via integer division, if can do _with zero remainders_
	dd := GCD(Abs(w.nodeDx), Abs(w.nodeDy))
	if dd != 1 {
		w.nodeDx = w.nodeDx / dd
		w.nodeDy = w.nodeDy / dd
		if dd > 2 {
			// Distance from partition line to other segs - if it is large - can
			// cause numerical overflows and visual glitches in run-time even in
			// advanced ports (PrBoom-plus etc). In particular, without this
			// step there would be glitches in PrBoom-Plus when standing near
			// either end of original linedef. Thus I try to keep it small by
			// placing segment at the line's center
			// This mitigation can't do anything for vanilla, though - vanilla
			// has significant visual glitches with ANY long line in view (not
			// just partition lines), which can not be helped
			d2 := dd / 2
			w.nodeX += d2 * w.nodeDx
			w.nodeY += d2 * w.nodeDy
		}
	}

	if w.nodeDx <= 32767 && w.nodeDy <= 32767 && w.nodeDx >= -32768 &&
		w.nodeDy >= -32768 {
		w.mlog.Verbose(1, "Prevented partition line coords overflow (from segment of linedef %d).\n",
			part.Linedef)
		return
	}

	// Could do nothing
	// TODO could be acceptable enough if I scale down with rounding? Not sure -
	// the angle would be different and that should be probably considered when
	// sorting/splitting segs to each side?

	w.mlog.Verbose(1, "Overflow: partition line DX=%d, DY=%d (from segment of linedef %d) can not be represented correctly due to (-32768,32767) signed int16 range limit in format. Parts of map will not be rendered correctly in any port.\n",
		w.nodeDx, w.nodeDy, part.Linedef)
}

// GCD returns greatest common divisor for POSITIVE integers
func GCD(a, b int) int {
	for b != 0 {
		tmp := b
		b = a % b
		a = tmp
	}
	return a
}

// Hard multi-tree needs a stable way to identify segs to use as partitions
// Segs are normally divided many times, and are unique to their threads
// Where as PartSeg is an identifier that would work across threads
func (w *NodesWork) GetPartSegs(ts *NodeSeg, best *NodeSeg, receiver *[]PartSeg) {
	wasNil := false
	if len(w.parts) == 0 {
		if w.parts == nil { // just in case
			w.parts = make([]*NodeSeg, 0)
			wasNil = true
		}
		w.parts = append(w.parts, best)
	}
	if *receiver == nil {
		*receiver = make([]PartSeg, 0)
	}

	limit := w.width
	if limit < 0 { // unlimited
		limit = len(w.parts)
	} else if limit > len(w.parts) {
		// limit is greater, but the way I am going to use the value I need to
		// lower it to actual length to avoid go panicking
		limit = len(w.parts)
	}

	for _, v := range w.parts[:limit] {
		partseg := getPartSeg(ts, v)
		*receiver = append(*receiver, partseg)
	}
	fold := false
	for _, v := range *receiver {
		if v.Occurence == -1 {
			fold = true
			break
		}
	}
	if fold {
		// receiver contained unrecognised segs - replace with just one
		// unrecognised marker (signaling "wtf" condition)
		// TODO might actually be a normal condition sometime later when
		// not all partitions are from segs, but currently very suspicious as
		// all partitioners currently only build partitions from segs
		w.mlog.Printf("Best partition didn't come from seg\n")
		*receiver = (*receiver)[:0]
		*receiver = append(*receiver, PartSeg{
			Linedef:   0,
			Occurence: -1,
		})
	}
	if wasNil {
		w.parts = nil
	} else {
		w.parts = w.parts[:0]
	}
}

func getPartSeg(ts *NodeSeg, target *NodeSeg) PartSeg {
	ldef := target.Linedef
	occurence := 0
	for tmps := ts; tmps != nil; tmps = tmps.next {
		if tmps.Linedef == ldef {
			occurence++
		}
		if tmps == target {
			return PartSeg{
				Linedef:   ldef,
				Occurence: occurence,
			}
		}
	}
	return PartSeg{
		Linedef:   0,
		Occurence: -1, // marker that was not found
	}
}

// Split a list of segs (ts) into two using the method described at bottom of
// file, this was taken from OBJECTS.C in the DEU5beta source. Well, the actual
// splitting was refactored to DivideSegsActual (called at the end of this
// method), because a twin named "DivideSegsForSingleSector" exists in
// convexity.go
func (w *NodesWork) DivideSegs(ts *NodeSeg, rs **NodeSeg, ls **NodeSeg, bbox *NodeBounds,
	super *Superblock, rightsSuper, leftsSuper **Superblock, partsegs *[]PartSeg) {
	// Pick best node to use
	best := w.pickNode(w, ts, bbox, super)

	if best == nil {
		// To programmers: write PickNode so it never happens
		Log.Panic("Couldn't pick nodeline!\n")
	}

	if partsegs != nil {
		w.GetPartSegs(ts, best, partsegs)
	}

	c := &IntersectionContext{
		psx: best.StartVertex.X,
		psy: best.StartVertex.Y,
		pex: best.EndVertex.X,
		pey: best.EndVertex.Y,
	}
	c.pdx = c.psx - c.pex
	c.pdy = c.psy - c.pey

	// Node line coords
	w.SetNodeCoords(best, bbox, c)

	w.DivideSegsActual(ts, rs, ls, bbox, best, c, super, rightsSuper, leftsSuper)
}

// This does the actual splitting job (partition already picked) using the
// method described at bottom of file, this was taken from OBJECTS.C in the
// DEU5beta source.
func (w *NodesWork) DivideSegsActual(ts *NodeSeg, rs **NodeSeg, ls **NodeSeg,
	bbox *NodeBounds, best *NodeSeg, c *IntersectionContext, super *Superblock,
	rightsSuper, leftsSuper **Superblock) {
	// When we get to here, best is a pointer to the partition seg.
	// Using this partition line, we must split any lines that are intersected
	// into a left and right half, flagging them to be put their respective sides
	// Ok, now we have the best line to use as a partitioning line, we must
	// split all of the segs into two lists (rightside & leftside).
	var rights, lefts, strights, stlefts *NodeSeg // start empty

	*rightsSuper = w.getNewSuperblock(super)
	*leftsSuper = w.getNewSuperblock(super)
	(*rightsSuper).SetBounds(bbox)
	(*leftsSuper).SetBounds(bbox)
	w.returnSuperblockToPool(super)

	asector := -1 // track how many sectors there are

	// Split segs (if needed) and sort all segs we have into left and right subtree
	tmpsNext := ts.next
	for tmps := ts; tmps != nil; tmps = tmpsNext {
		if asector == -1 {
			asector = int(tmps.sector)
		} else if asector != int(tmps.sector) {
			asector = -2
		}
		// progress()
		// Yes up to four segs may happen at once, if splitting a seg that has
		// a partner
		addToRs := [2]*NodeSeg{nil, nil}
		addToLs := [2]*NodeSeg{nil, nil}
		if tmps != best && tmps != best.partner {
			// Process seg AND its partner (if one exists). Must keep partnership valid
			w.CategoriseAndMaybeDivideSeg(addToRs[:], addToLs[:], c, &tmps)
		} else { // tmps == best || tmps == best.partner
			// Kept this consistent with how BSP would do things, and the way
			// PickNode sorts colinear partner segs -- VigilantDoomer
			addToRs[0] = best
			addToLs[0] = best.partner
			if best.partner != nil {
				// sorry, and goodbye - going to different subtrees
				tmps.partner.partner = nil
				tmps.partner = nil
				// 20230307 Now that I have experimented with algorithms that
				// could indeed lead to satisfying tmps == best.partner before
				// tmps == best, I know that tmps = tmps.next assignment must
				// be done unconditionally
				tmps = tmps.next
				// let's see why
				// if tmps == best is what occurs, the layout is
				// [..., best == tmps, best.next == best.partner ...]
				// so we do both at once and then skip
				// if tmps == best.partner is what occurs, we have
				// [..., smth == tmps, smth.next == smth.partner == best, ...]
				// once again, standing on tmps we do both and must skip both
			}
		}
		tmpsNext = tmps.next

		addSegToSide(addToRs[:], &rights, &strights)
		addSegToSide(addToLs[:], &lefts, &stlefts)
	}

	if strights == nil {
		// VigilantDoomer - this branch must never happen (seg from which
		// partition is derived is hardcored to go right)
		Log.Panic("Should have had partition at the right side (hardcoded to go there).\n")
	}

	if stlefts == nil { // This one definitely happens often enough
		// And the reason was identified, indeed. PickNode* and
		// doLinesIntersect() identify left-sideness a bit differently,
		// apparently as result of aggressive optimization in PickNode* done by
		// Lee Killough (and inherited in vigilantBSP through BSP v5.2 code).
		// Currently, I settled for incorporating a small punishing charge  in
		// my PickNode_visplaneVigilant to be assigned when partition with no
		// left side according to doLinesIntersect would be picked. This is one
		// of the last cost charges to be calculated so it doesn't impact
		// performance, but still prevents some extreme cases with too many
		// sectors all clustered at one side.
		if asector > 0 {
			w.mlog.Verbose(1, "No left side, moving partition into left side (single sector #%d)\n",
				asector)
		} else {
			if config.VerbosityLevel >= 3 { // reference to global: config
				// Ok, let's actually see the entire sector list
				sectorsHit := make(map[uint16]bool)
				for tmps := ts; tmps != nil; tmps = tmps.next {
					sectorsHit[tmps.sector] = true
				}
				s := ""
				for sector := range sectorsHit {
					s += "," + strconv.Itoa(int(sector))
				}
				if s != "" { // should be always true
					s = s[1:] // remove "," at the beginning (",3,4,5" => "3,4,5")
				}
				w.mlog.Verbose(1, "No left side, moving partition into left side (sectors %s)\n", s)
			} else {
				w.mlog.Verbose(1, "No left side, moving partition into left side (multiple sectors in this node)\n")
			}
		}
		lefts = best
		stlefts = best
		var prev *NodeSeg
		prev = nil
		for tmps := strights; tmps != nil; tmps = tmps.next {
			if tmps == best {
				if prev != nil {
					prev.next = tmps.next
				} else {
					strights = tmps.next
				}
			}
			prev = tmps
		}
		// VigilantDoomer: the next line of code was present in BSP v5.2 already
		stlefts.next = nil
		prev.next = nil // Make sure end of list = NULL
		// VigilantDoomer since stlefts now points to one seg: partition line,
		// stlefts.partner == nil (newBest.partner == nil) always. I didn't
		// include the check here, but it should be this way - ensure split/sort
		// segs loop does it
	}

	if rights.next != nil {
		rights.next = nil
	}

	if lefts.next != nil {
		lefts.next = nil
	}

	for tmps := strights; tmps != nil; tmps = tmps.next {
		(*rightsSuper).AddSegToSuper(tmps)
	}

	for tmps := stlefts; tmps != nil; tmps = tmps.next {
		(*leftsSuper).AddSegToSuper(tmps)
	}

	*rs = strights
	*ls = stlefts
}

// This doesn't have to recreate segs anymore - progress!
func addSegToSide(segToAdd []*NodeSeg, thisside, stThisSide **NodeSeg) {
	if segToAdd[0] != nil {
		// add to specified side
		segToAdd[0].next = nil
		if *thisside == nil {
			*thisside = segToAdd[0]
			*stThisSide = segToAdd[0]

		} else {
			(*thisside).next = segToAdd[0]
			*thisside = segToAdd[0]
		}
		// add second seg if there was one (in which case it is a partner)
		if segToAdd[1] != nil {
			segToAdd[1].next = nil
			(*thisside).next = segToAdd[1]
			*thisside = segToAdd[1]
		}
	}
}

// CategoriseAndMaybeDivideSeg decides whether seg needs to be split (and
// splits it if so), and also which side this seg/the ones resulting from
// split goes/go. If seg has a partner, it categorises two segs at once (and
// if one of them gets split, the other gets split too)
// Returns true if seg was split
func (w *NodesWork) CategoriseAndMaybeDivideSeg(addToRs []*NodeSeg,
	addToLs []*NodeSeg, c *IntersectionContext, tmps **NodeSeg) bool {
	// Assumptions atmps.partner == nil || atmps.partner == atmps.next
	// This is also the function that does the bulk of maintaining this
	// assumption for future
	atmps := *tmps
	if atmps.partner != nil && atmps.partner != atmps.next {
		Log.Panic("Partnership was not kept up to date!\n")
	}
	c.lsx = atmps.StartVertex.X
	c.lsy = atmps.StartVertex.Y
	c.lex = atmps.EndVertex.X
	c.ley = atmps.EndVertex.Y
	val := w.doLinesIntersect(c)
	if ((val&2 != 0) && (val&64 != 0)) || ((val&4 != 0) && (val&32 != 0)) {
		// Partition intersects this seg in tmps parameter, so it needs to be split
		if w.lines.IsTaggedPrecious(atmps.Linedef) &&
			!w.lines.SectorIgnorePrecious(atmps.sector) {
			// undesirable split MIGHT have occured. Hexen.wad actually has lot
			// of setups that are non-convex sectors, etc. so this doesn't
			// necessarily mean things were broken
			w.totals.preciousSplit++
		}
		x, y := c.computeIntersection()
		newVertex := w.AddVertex(x, y)
		news := new(NodeSeg)
		atmps.alias = 0 // clear alias, as angle may change after split
		if w.sidenessCache.maxKnownAlias > 0 {
			if getGlobalFlip(atmps) {
				atmps.flags |= SEG_FLAG_GLOBAL_FLIP
			} else {
				atmps.flags &= ^uint8(SEG_FLAG_GLOBAL_FLIP)
			}
		}
		atmps.flags |= SEG_FLAG_NOCACHE // not in cache because split
		*news = *atmps
		atmps.next = news
		news.StartVertex = newVertex
		atmps.EndVertex = newVertex
		news.Offset = uint16(splitDist(w.lines, news))
		w.totals.segSplits++
		if atmps.partner != nil {
			if w.lines.IsTaggedPrecious(atmps.partner.Linedef) &&
				!w.lines.SectorIgnorePrecious(atmps.partner.sector) {
				w.totals.preciousSplit++
			}
			w.totals.segSplits++
			atmps.partner.alias = 0 // clear alias, as angle may change after split
			if w.sidenessCache.maxKnownAlias > 0 {
				if getGlobalFlip(atmps.partner) {
					atmps.partner.flags |= SEG_FLAG_GLOBAL_FLIP
				} else {
					atmps.partner.flags &= ^uint8(SEG_FLAG_GLOBAL_FLIP)
				}
			}
			atmps.partner.flags |= SEG_FLAG_NOCACHE // not in cache because split
			// Split partner too
			newVertex2 := newVertex
			news2 := new(NodeSeg)
			*news2 = *(atmps.partner)
			news2.StartVertex = newVertex2
			atmps.partner.EndVertex = newVertex2
			news2.Offset = uint16(splitDist(w.lines, news2))
			// Now that basic parameters are set, let's deal with partnership
			// If you find these hard to understand, draw these things on paper
			oldpartner := atmps.partner
			atmps.partner = news2
			news2.partner = atmps
			oldpartner.partner = news
			news.partner = oldpartner
			oldpartner.next = news2
			news.next = oldpartner
			// Move cursor to the last part of partner seg, so both segs are
			// not evaluated again, and the next one will be the original next
			// of partner seg before it was split
			*tmps = news2
		} else {
			// Move cursor to the part that comes last. Next will be the seg
			// that was not yet passed
			*tmps = news
		}

		// Update length and angle of segs (BSP v5.2 never did this)
		// Length is used in internal calculations, but angle is used by
		// game engine. Both must be updated
		w.recomputeSegs(atmps, news)
		if atmps.partner != nil {
			w.recomputeSegs(news.partner, atmps.partner)
		}

		if val&32 != 0 {
			addToLs[0] = atmps
			addToLs[1] = atmps.partner // news2 if non-nil
		}
		if val&64 != 0 {
			addToRs[0] = atmps
			addToRs[1] = atmps.partner // news2 if non-nil
		}
		if val&2 != 0 {
			addToLs[0] = news
			addToLs[1] = news.partner
		}
		if val&4 != 0 {
			addToRs[0] = news
			addToRs[1] = news.partner
		}
		return true
	} else {
		partnersBreakUp := false
		// Not split, which side ?
		if val&34 != 0 {
			// to the left
			addToLs[0] = atmps
			addToLs[1] = atmps.partner
		}
		if val&68 != 0 {
			// to the right
			addToRs[0] = atmps
			addToRs[1] = atmps.partner
		}
		if (val&1 != 0) && (val&16 != 0) {
			// On same line
			/* 06/01/97 Lee Killough: this fixes a bug ever since 1.2x,
			   probably 1.0, of BSP: when partitioning a parallel seg,
			   you must take its vertices' orientation into account, NOT the
			   flip bits, to determine which side of the partitioning line a
			   parallel seg should go on. If you simply flip the linedef in
			   question, you will be flipping both its vertices and sidedefs,
			   and the flip bits as well, even though the basic geometry has
			   not changed. Thus you need to use the vertices' orientation
			   (whether the seg is in the same direction or not, regardless
			   of its original linedef's being flipped or not), into account.

			   Originally, some segs were partitioned backwards, and if
			   it happened that there were different sectors on either
			   side of the seg being partitioned, it could leave holes
			   in space, causing either invisible barriers or disappearing
			   Things, because the ssector would be associated with the
			   wrong sector.

			   The old logic of tmps->flip != best->flip seems to rest on
			   the assumption that if two segs are parallel, they came
			   from the same linedef. This is clearly not always true.   */

			/*  if (tmps.flip != best.flip)   old logic -- wrong!!! */

			/* We know the segs are parallel or nearly so, so take their
			   dot product to determine their relative orientation. */

			if ((c.lsx-c.lex)*c.pdx + (c.lsy-c.ley)*c.pdy) < 0 {
				addToLs[0] = atmps
				addToRs[0] = atmps.partner
				partnersBreakUp = true
			} else {
				addToRs[0] = atmps
				addToLs[0] = atmps.partner
				partnersBreakUp = true
			}
		}
		if atmps.partner != nil {
			*tmps = atmps.partner // .partner the same as .next in this instance
			if partnersBreakUp {
				// sorry, and goodbye - going to different subtrees
				atmps.partner.partner = nil
				atmps.partner = nil
			}
		}
		return false
	}
}

// After you split seg into two, a lot of properties actually need to be
// recomputed. (It's amazing bsp v5.2 managed to go without this.) The man that
// brought this to public attention is zokum, but I saw recomputation logic in
// AJ-BSP as well.
func (w *NodesWork) recomputeSegs(originSeg, newSeg *NodeSeg) {
	// originSeg is the one that existed before split on this iteration,
	// newSeg is the one that was created because of the split on this iteration

	// NOTE length computation here not good enough for extended nodes, due to
	// Offset being integer. Instead for extended nodes, will recompute length
	// in recomputeOneSeg
	originNewLen := int(newSeg.Offset) - int(originSeg.Offset)
	originSeg.len = Number(originNewLen)
	newSeg.len = newSeg.len - Number(originNewLen)

	oldAngle := originSeg.Angle
	w.recomputeOneSeg(originSeg)
	w.recomputeOneSeg(newSeg)
	if oldAngle != originSeg.Angle || oldAngle != newSeg.Angle {
		// Happens often enough btw
		w.mlog.Verbose(3, "Angle changed after splitting seg:  %d -> (%d ; %d)\n",
			oldAngle, originSeg.Angle, newSeg.Angle)
	}
}

func (w *NodesWork) recomputeOneSeg(s *NodeSeg) {
	s.pex = s.EndVertex.X
	s.psx = s.StartVertex.X
	s.pdx = s.pex - s.psx
	s.pey = s.EndVertex.Y
	s.psy = s.StartVertex.Y
	s.pdy = s.pey - s.psy
	s.perp = s.pdx*s.psy - s.psx*s.pdy
	w.updateSegLenBetter(s) // anchor to inject precise len computation for extended nodes
	bamEffect := w.lines.GetBAMEffect(s.Linedef)
	if bamEffect.Action == BAM_REPLACE { // zokumbsp actions 1081, 1083
		s.Angle = bamEffect.Value
	} else {
		// Default computation of angle
		s.Angle = int16(computeAngle(s.pdx, s.pdy))
		if bamEffect.Action == BAM_ADDITIVE { // zokumbsp actions 1080, 1082
			s.Angle += bamEffect.Value
		}
	}
	horizon := w.lines.IsHorizonEffect(s.Linedef)
	if horizon {
		// I check flip bit just in case. Actually I made horizon effect apply
		// to only one-sided lines, but if someone changes that, the angle
		// shall be reflected correctly nonetheless
		isFront := s.getFlip() == 0
		sdef := w.lines.GetSidedefIndex(s.Linedef, isFront)
		s.Angle += int16(float64(w.sides[sdef].XOffset) * float64(65536.0/360.0))
	}
}

// Placeholder that will be overridden for extended nodes when code generator
// is used
func (w *NodesWork) updateSegLenBetter(s *NodeSeg) {
	// does nothing for normal nodes. For normal nodes, seg length is already
	// computed "good enough" using integer offset, but for extended nodes,
	// this won't fly and so you'll find an override defined in zdefs.go for
	// code generator to parse and generate znodegen.go
}

func CreateNode(w *NodesWork, ts *NodeSeg, bbox *NodeBounds, super *Superblock) *NodeInProcess {
	res := new(NodeInProcess)
	var rights *NodeSeg
	var lefts *NodeSeg
	var rightsSuper *Superblock
	var leftsSuper *Superblock
	// Divide node in two
	w.totals.numNodes++
	w.DivideSegs(ts, &rights, &lefts, bbox, super, &rightsSuper, &leftsSuper, nil)
	// NOTE after DivideSegs return, super may no longer be valid
	super = nil
	res.X = int16(w.nodeX)
	res.Y = int16(w.nodeY)
	res.Dx = int16(w.nodeDx)
	res.Dy = int16(w.nodeDy)

	// These will form the left box
	leftBox := FindLimits(lefts)
	res.Lbox[BB_TOP] = int16(leftBox.Ymax)
	res.Lbox[BB_BOTTOM] = int16(leftBox.Ymin)
	res.Lbox[BB_LEFT] = int16(leftBox.Xmin)
	res.Lbox[BB_RIGHT] = int16(leftBox.Xmax)
	state := w.isItConvex(lefts)
	if state == CONVEX_SUBSECTOR {
		res.nextL = nil
		res.LChild = w.CreateSSector(lefts) | w.SsectorMask
		w.returnSuperblockToPool(leftsSuper)
	} else if state == NONCONVEX_ONESECTOR {
		res.nextL = w.createNodeSS(w, lefts, leftBox, leftsSuper)
		res.LChild = 0
	} else {
		if config.VerbosityLevel >= 1 && w.IsConvexMultiSector(lefts) { // reference to global: config
			w.dumpMultiSectorSegs(lefts)
		}
		res.nextL = CreateNode(w, lefts, leftBox, leftsSuper)
		res.LChild = 0
	}
	leftsSuper = nil

	// These will form the right box
	rightBox := FindLimits(rights)
	res.Rbox[BB_TOP] = int16(rightBox.Ymax)
	res.Rbox[BB_BOTTOM] = int16(rightBox.Ymin)
	res.Rbox[BB_LEFT] = int16(rightBox.Xmin)
	res.Rbox[BB_RIGHT] = int16(rightBox.Xmax)
	state = w.isItConvex(rights)
	if state == CONVEX_SUBSECTOR {
		res.nextR = nil
		res.RChild = w.CreateSSector(rights) | w.SsectorMask
		w.returnSuperblockToPool(rightsSuper)
	} else if state == NONCONVEX_ONESECTOR {
		res.nextR = w.createNodeSS(w, rights, rightBox, rightsSuper)
		res.RChild = 0
	} else {
		if config.VerbosityLevel >= 1 && w.IsConvexMultiSector(rights) { // reference to global: config
			w.dumpMultiSectorSegs(rights)
		}
		res.nextR = CreateNode(w, rights, rightBox, rightsSuper)
		res.RChild = 0
	}
	rightsSuper = nil

	//CheckNodeBounds(bbox, leftBox, rightBox)

	return res
}

func (w *NodesWork) dumpMultiSectorSegs(ts *NodeSeg) {
	if config.VerbosityLevel < 1 { // reference to global: config
		return
	}
	var buf bytes.Buffer
	buf.WriteString("Multiple sectors detected that form something CONVEX!\n")
	for check := ts; check != nil; check = check.next {
		buf.WriteString(fmt.Sprintf(
			"  Sector: %v Linedef: %v Flip: %v (%v,%v) - (%v, %v)\n",
			check.sector, check.Linedef, check.getFlip(),
			check.psx, check.psy, check.pex, check.pey))
	}
	w.mlog.Verbose(1, buf.String())
}

func HeightOfNodes(node *NodeInProcess) int {
	lHeight := 1
	rHeight := 1
	if node.nextL != nil {
		lHeight = HeightOfNodes(node.nextL) + 1
	}
	if node.nextR != nil {
		rHeight = HeightOfNodes(node.nextR) + 1
	}
	if lHeight < rHeight {
		return rHeight
	}
	return lHeight
}

// Node reversal for standard nodes
func (w *NodesWork) reverseNodes(node *NodeInProcess) uint32 {
	if w.nodes == nil {
		w.nodes = make([]Node, w.totals.numNodes)
		w.nreverse = 0
	}
	if config.StraightNodes { // reference to global: config
		// this line shall be executed for root node only
		// root node still needs to be placed last, even as if the rest of tree
		// is written "unreversed"
		return w.convertNodesStraight(node, uint32(w.totals.numNodes-1))
	}
	if node.nextR != nil {
		node.RChild = w.reverseNodes(node.nextR)
	}
	if node.nextL != nil {
		node.LChild = w.reverseNodes(node.nextL)
	}

	w.nodes[w.nreverse] = Node{
		X:      node.X,
		Y:      node.Y,
		Dx:     node.Dx,
		Dy:     node.Dy,
		Rbox:   node.Rbox,
		Lbox:   node.Lbox,
		LChild: int16(node.LChild),
		RChild: int16(node.RChild),
	}

	w.nreverse++
	return w.nreverse - 1
}

// Node reversal for deep/extended nodes
func (w *NodesWork) reverseDeepNodes(node *NodeInProcess) uint32 {
	if w.deepNodes == nil {
		w.deepNodes = make([]DeepNode, w.totals.numNodes)
		w.nreverse = 0
	}
	if config.StraightNodes { // reference to global: config
		// this line shall be executed for root node only
		// root node still needs to be placed last, even as if the rest of tree
		// is written "unreversed"
		return w.convertDeepNodesStraight(node, uint32(w.totals.numNodes-1))
	}
	if node.nextR != nil {
		node.RChild = w.reverseDeepNodes(node.nextR)
	}
	if node.nextL != nil {
		node.LChild = w.reverseDeepNodes(node.nextL)
	}

	w.deepNodes[w.nreverse] = DeepNode{
		X:      node.X,
		Y:      node.Y,
		Dx:     node.Dx,
		Dy:     node.Dy,
		Rbox:   node.Rbox,
		Lbox:   node.Lbox,
		LChild: int32(node.LChild),
		RChild: int32(node.RChild),
	}

	w.nreverse++
	return w.nreverse - 1
}

// Writes nodes without reversal, except the root node is still last
// Standard nodes version
func (w *NodesWork) convertNodesStraight(node *NodeInProcess, idx uint32) uint32 {
	rnode := w.nreverse
	lnode := w.nreverse
	if node.nextR != nil {
		w.nreverse++
	}
	if node.nextL != nil {
		w.nreverse++
	}
	if node.nextR != nil {
		node.RChild = w.convertNodesStraight(node.nextR, rnode)
		lnode++
	}
	if node.nextL != nil {
		node.LChild = w.convertNodesStraight(node.nextL, lnode)
	}
	w.nodes[idx] = Node{
		X:      node.X,
		Y:      node.Y,
		Dx:     node.Dx,
		Dy:     node.Dy,
		Rbox:   node.Rbox,
		Lbox:   node.Lbox,
		LChild: int16(node.LChild),
		RChild: int16(node.RChild),
	}
	return idx
}

// Writes nodes without reversal, except the root node is still last
// Deep/extended nodes version
func (w *NodesWork) convertDeepNodesStraight(node *NodeInProcess, idx uint32) uint32 {
	rnode := w.nreverse
	lnode := w.nreverse
	if node.nextR != nil {
		w.nreverse++
	}
	if node.nextL != nil {
		w.nreverse++
	}
	if node.nextR != nil {
		node.RChild = w.convertDeepNodesStraight(node.nextR, rnode)
		lnode++
	}
	if node.nextL != nil {
		node.LChild = w.convertDeepNodesStraight(node.nextL, lnode)
	}
	w.deepNodes[idx] = DeepNode{
		X:      node.X,
		Y:      node.Y,
		Dx:     node.Dx,
		Dy:     node.Dy,
		Rbox:   node.Rbox,
		Lbox:   node.Lbox,
		LChild: int32(node.LChild),
		RChild: int32(node.RChild),
	}
	return idx
}

// computeSectorEquivalence()
// Returns an array that maps sector indices to the least index of respective
// equivalent sectors, such that:
// equivalence[sector_index] = equivalent_sector_index
// where sector_index and equivalent_sector_index sometimes match.
// Sector equivalence is when two sectors has compatible properties such that,
// depending on how nodes where generated and granted the possiblity to do so,
// can produce compatible visplanes to be merged by game engine at run-time,
// thus decreasing visplanes and possibly preventing overflow
// The existence of visplane merging was verified and documented by Lee Killough
// upon Doom source release, but he never adjusted logic in BSP nodebuilder to
// account for equivalence. --VigilantDoomer
func (ni *NodesInput) computeSectorEquivalence() ([]uint16, int) {
	if ni.pickNodeUser != PICKNODE_VISPLANE_ADV { // not used by other node picking algos
		return nil, 0
	}
	equiv := make([]uint16, len(ni.sectors))
	hmap := make(map[Sector]uint16)
	uniq := 0
	for i, sector := range ni.sectors {
		// Don't consider sectors that have specials or are assigned tags
		// as being equivalent to any other sectors, even to sectors with same
		// properties. These properties might change out of sync (blinking lights
		// that blink in order, floors lowering etc.). But we will assume that
		// zero tagged things normally don't demerge against each other (unless
		// they are doors opened with DR tags, but ok, I'll neglect that for now)
		if sector.Special == 0 && sector.Tag == 0 {
			ASector := sector
			// Names shorter than 8 symbols can have garbage after the zero
			// byte, and refuse to be recognised as identical if garbage bytes
			// differ in otherwise identical names
			sanitizeName(ASector.CeilName[:8])
			sanitizeName(ASector.FloorName[:8])
			// Go can handle hashing variable of fixed size structure type
			v, ok := hmap[ASector]
			if ok {
				equiv[i] = v
			} else { // first instance of this
				hmap[ASector] = uint16(uniq)
				equiv[i] = uint16(uniq)
				uniq++
			}
		} else {
			// This branch was absent up to and including to v0.69a,
			// which was an error (all non-mergeable sectors where "merged"
			// into sector 0)
			equiv[i] = uint16(uniq)
			uniq++
		}
	}
	return equiv, uniq
}

func sanitizeName(a []byte) []byte {
	var i int
	for i = 0; i < len(a); i++ {
		if a[i] == 0 {
			break
		}
	}
	if i < len(a) {
		for j := i; j < len(a); j++ {
			a[j] = 0
		}
	}
	return a
}

// Store the number of equivalent sector directly in NodeSeg for fast access
// in pickNode's hot loop
func (ni *NodesInput) applySectorEquivalence(allSegs []*NodeSeg, sectorEquiv []uint16) {
	for i, _ := range allSegs {
		allSegs[i].secEquiv = sectorEquiv[allSegs[i].sector]
	}
}

// Create superblocks from initial list of segs before the start of partitioning
func (w *NodesWork) doInitialSuperblocks(rootBox *NodeBounds) *Superblock {
	ret := w.newSuperblockNoProto()
	ret.nwlink = w
	ret.SetBounds(rootBox)
	for _, seg := range w.allSegs {
		ret.AddSegToSuper(seg)
	}
	return ret
}

func (w *NodesWork) newSuperblockNoProto() *Superblock {
	ret := &Superblock{}
	if w.pickNodeUser == PICKNODE_VISPLANE || w.pickNodeUser == PICKNODE_ZENLIKE {
		ret.sectors = make([]uint16, 0)
		ret.secMap = make(map[uint16]struct{})
	} else if w.pickNodeUser == PICKNODE_VISPLANE_ADV {
		ret.secEquivs = make([]uint16, 0)
		ret.secMap = make(map[uint16]struct{})
	}
	return ret
}

func (w *NodesWork) getZdoomNodesBytes() []byte {
	w.zdoomVertexHeader.NumExtendedVertices = uint32(len(w.vertices) -
		int(w.zdoomVertexHeader.ReusedOriginalVertices))
	w.zdoomVertices = make([]ZdoomNode_Vertex,
		w.zdoomVertexHeader.NumExtendedVertices)
	for i, srcv := range w.vertices[w.zdoomVertexHeader.ReusedOriginalVertices:] {
		w.zdoomVertices[i].X = srcv.X.ToFixed16Dot16()
		w.zdoomVertices[i].Y = srcv.Y.ToFixed16Dot16()
	}
	var writ *ZStream
	if w.nodeType == NODETYPE_ZDOOM_COMPRESSED {
		writ = CreateZStream(ZNODES_COMPRESSED_SIG[:], true)
	} else {
		writ = CreateZStream(ZNODES_PLAIN_SIG[:], false)
	}
	// NOTE always LittleEndian per Zdoom specs
	vertexHeader := *(w.zdoomVertexHeader)
	binary.Write(writ, binary.LittleEndian, vertexHeader)
	binary.Write(writ, binary.LittleEndian, w.zdoomVertices)
	binary.Write(writ, binary.LittleEndian, uint32(len(w.zdoomSubsectors)))
	binary.Write(writ, binary.LittleEndian, w.zdoomSubsectors)
	binary.Write(writ, binary.LittleEndian, uint32(len(w.zdoomSegs)))
	binary.Write(writ, binary.LittleEndian, w.zdoomSegs)
	binary.Write(writ, binary.LittleEndian, uint32(len(w.deepNodes)))
	binary.Write(writ, binary.LittleEndian, w.deepNodes)
	ret, err := writ.FinalizeAndGetBytes()
	if err != nil {
		Log.Panic("IO error at writing Zdoom nodes stream: %s\n", err.Error())
	}
	return ret
}

const FIXED16DOT16_MULTIPLIER = 65536.0

// Redundant declaration on Number, but will be relevant on ZNumber
func (n Number) ToFixed16Dot16() int32 {
	return int32(float64(n) * FIXED16DOT16_MULTIPLIER)
}

// GetInitialStateClone (kinda like Clone or DeepCopy, but not quite) for that
// full-of-state NodesWork structure. Multi-tree algorithms depend very much on
// it!
// GetInitialStateClone won't produce data relevant to superblocks. You will
// have to call doInitialSuperblocks on the result to obtain new root superblock
// and actualize nextInSuper field in NodeSeg's
func (w *NodesWork) GetInitialStateClone() *NodesWork {
	newW := new(NodesWork)
	*newW = *w
	// TODO write tests to ensure programmers keep this function up-to-date
	// (use reflection etc. to do deep tests about references). There shall be
	// NO shared data

	if w.mlog != nil {
		newW.mlog = CreateMiniLogger()
	}

	newW.sides = make([]Sidedef, len(w.sides))
	for i := 0; i < len(newW.sides); i++ {
		newW.sides[i] = w.sides[i]
	}

	newW.sectors = make([]Sector, len(w.sectors))
	for i := 0; i < len(newW.sectors); i++ {
		newW.sectors[i] = w.sectors[i]
	}

	newW.totals = &NodesTotals{
		numNodes:               0,
		numSSectors:            0,
		numSegs:                0,
		maxSegCountInSubsector: 0,
		segSplits:              0,
	}

	newW.vertices = make([]NodeVertex, len(w.vertices))
	for i := 0; i < len(newW.vertices); i++ {
		newW.vertices[i] = w.vertices[i]
	}

	if w.vertexCache != nil {
		newW.vertexCache = make(map[SimpleVertex]int)
		for k, v := range w.vertexCache {
			newW.vertexCache[k] = v
		}
	}

	if !w.sidenessCache.readOnly {
		Log.Panic("Sideness cache [nodes] must be finalized before running multi-tree\n")
	}

	if w.vertexMap != nil {
		newW.vertexMap = w.vertexMap.Clone()
		newW.vertexMap.w = newW
	}

	if w.dgVertexMap != nil {
		newW.dgVertexMap = w.dgVertexMap.Clone()
		newW.dgVertexMap.w = newW
	}

	newW.segAliasObj = w.segAliasObj.Clone()
	newW.sectorHits = make([]byte, len(w.sectorHits))
	newW.incidental = make([]IntVertexPairC, 0)

	// NOTE newW.solidMap should not be written, only read from. Else we are screwed

	newW.nonVoidCache = make(map[int]NonVoidPerAlias)
	newW.blocksHit = make([]BlocksHit, 0)
	newW.zenScores = make([]DepthScoreBundle, 0, cap(w.zenScores))

	newW.qallocSupers = nil // never share between threads!
	newW.stkExtra = nil     // never share between threads!

	if w.parts != nil {
		newW.parts = make([]*NodeSeg, 0)
	}

	if newW.zdoomVertexHeader != nil {
		newW.zdoomVertexHeader = new(ZdoomNode_VertexHeader)
		*newW.zdoomVertexHeader = *w.zdoomVertexHeader
	}

	// "deep copy" (or at least an approximationof such) for lines. That damn
	// interface! Especially in case of Hexen
	newW.lines = w.lines.Clone()

	newW.allSegs = make([]*NodeSeg, len(w.allSegs))
	for i := 0; i < len(newW.allSegs); i++ {
		newW.allSegs[i] = new(NodeSeg)
		*(newW.allSegs[i]) = *(w.allSegs[i])
		// zero out superblock data - no way to clone it from here
		newW.allSegs[i].nextInSuper = nil
		newW.allSegs[i].block = nil
		//
		// Fucking StartVertex and EndVertex
		newW.allSegs[i].StartVertex = &(newW.vertices[w.allSegs[i].StartVertex.idx])
		newW.allSegs[i].EndVertex = &(newW.vertices[w.allSegs[i].EndVertex.idx])
	}

	// Links between segs can only be updated after all segs are filled in
	for i := 0; i < len(newW.allSegs); i++ {
		if i < len(newW.allSegs)-1 {
			newW.allSegs[i].next = newW.allSegs[i+1]
		} else {
			newW.allSegs[i].next = nil
		}
		if w.allSegs[i].partner != nil {
			// which one, previous or next?
			if i > 0 && w.allSegs[i].partner == w.allSegs[i-1] {
				newW.allSegs[i].partner = newW.allSegs[i-1]
			} else {
				newW.allSegs[i].partner = newW.allSegs[i+1]
			}
		}
	}

	return newW
}

func CreateVertexMap(w *NodesWork, minx, miny, maxx, maxy int) *VertexMap {
	// Mitigation for a possible crash when producing line traces against solid
	// lines (void and non-void differentiation) in advanced visplane reduction
	// for, apparently, bent segs (previously split segs whose angle is
	// different from angle of the original linedef - in node format that lacks
	// precision) can produce intersection vertice slightly outside the bounds
	minx = minx - VMAP_SAFE_MARGIN
	miny = miny - VMAP_SAFE_MARGIN
	maxx = maxx + VMAP_SAFE_MARGIN
	maxy = maxy + VMAP_SAFE_MARGIN

	vm := &VertexMap{
		w:    w,
		MinX: float64(minx),
		MinY: float64(miny),
		BlocksWide: int((float64(maxx-minx+1)*
			FIXED16DOT16_MULTIPLIER + float64(VMAP_BLOCK_SIZE-1)) /
			float64(VMAP_BLOCK_SIZE)),
		BlocksTall: int((float64(maxy-miny+1)*
			FIXED16DOT16_MULTIPLIER + float64(VMAP_BLOCK_SIZE-1)) /
			float64(VMAP_BLOCK_SIZE)),
	}
	vm.MaxX = vm.MinX + float64(vm.BlocksWide*VMAP_BLOCK_SIZE-1)/FIXED16DOT16_MULTIPLIER
	vm.MaxY = vm.MinY + float64(vm.BlocksTall*VMAP_BLOCK_SIZE-1)/FIXED16DOT16_MULTIPLIER
	vm.Grid = make([][]*FloatVertex, vm.BlocksWide*vm.BlocksTall)
	return vm
}

func (vm *VertexMap) Clone() *VertexMap {
	if vm == nil {
		return nil
	}
	newVm := &VertexMap{}
	*newVm = *vm
	newVm.w = nil
	newVm.Grid = make([][]*FloatVertex, 0, len(vm.Grid))
	for _, it := range vm.Grid {
		cpit := make([]*FloatVertex, 0, len(it))
		for _, it2 := range it {
			nv := &FloatVertex{}
			*nv = *it2
			cpit = append(cpit, nv)
		}
		newVm.Grid = append(newVm.Grid, cpit)
	}
	if vm.Snapshot != nil {
		newVm.Snapshot = make([]int, len(vm.Grid))
		for i, it := range vm.Snapshot {
			newVm.Snapshot[i] = it
		}
	}
	return newVm
}

func (vm *VertexMap) GetBlock(x, y float64) int {
	// assert x >= MinX
	// assert y >= MinY
	// assert x <= MaxX
	// assert y <= MaxY
	// The above constraints are actually violated sometimes by some epsilon
	// because floating point is used and not fixed point like in ZDBSP. Such
	// cases don't produce out of bounds index, though, because of being really
	// close to the border values. For some runaway cases, see a different
	// mitigation below
	ret := int(uint((x-vm.MinX)*FIXED16DOT16_MULTIPLIER)>>VMAP_BLOCK_SHIFT +
		(uint((y-vm.MinY)*FIXED16DOT16_MULTIPLIER)>>VMAP_BLOCK_SHIFT)*
			uint(vm.BlocksWide))
	if ret < 0 || ret >= len(vm.Grid) {
		vm.w.mlog.Verbose(1, "Vertex map index out of range, source values: x=%f, y=%f xmin,ymin=(%f,%f) xmax,ymax=(%f,%f)\n",
			x, y, vm.MinX, vm.MinY, vm.MaxX, vm.MaxY)
		// Allow vertex map to function without panic in such cases, should they
		// happen
		// Accumulating such errors (if wrong borders are specified) can cause
		// slowdown, but not crash, and should not result in malfunction
		return 0
	}
	return ret
}

func (vm *VertexMap) SelectVertexExact(x, y float64, id int) *FloatVertex {
	block := &(vm.Grid[vm.GetBlock(x, y)])
	for _, it := range *block {
		if it.X == x && it.Y == y {
			return it
		}
	}
	return vm.insertVertex(x, y, id)
}

func (vm *VertexMap) SelectVertexClose(x, y float64) *FloatVertex {
	block := &(vm.Grid[vm.GetBlock(x, y)])
	for _, it := range *block {
		if math.Abs(it.X-x) < VERTEX_EPSILON &&
			math.Abs(it.Y-y) < VERTEX_EPSILON {
			return it
		}
	}
	return vm.insertVertex(x, y, -1)
}

func (vm *VertexMap) insertVertex(x, y float64, id int) *FloatVertex {
	// If a vertex is near a block boundary, then it will be inserted on
	// both sides of the boundary so that SelectVertexClose can find
	// it by checking in only one block.
	ret := &FloatVertex{
		X:  x,
		Y:  y,
		Id: id,
	}
	minx := vm.MinX
	if minx < (x - VERTEX_EPSILON) {
		minx = x - VERTEX_EPSILON
	}
	maxx := vm.MaxX
	if maxx > (x + VERTEX_EPSILON) {
		maxx = x + VERTEX_EPSILON
	}
	miny := vm.MinY
	if miny < (y - VERTEX_EPSILON) {
		miny = y - VERTEX_EPSILON
	}
	maxy := vm.MaxY
	if maxy > (y + VERTEX_EPSILON) {
		maxy = y + VERTEX_EPSILON
	}
	blk := [4]int{vm.GetBlock(minx, miny),
		vm.GetBlock(maxx, miny),
		vm.GetBlock(minx, maxy),
		vm.GetBlock(maxx, maxy)}
	blcount := [4]int{
		len(vm.Grid[blk[0]]),
		len(vm.Grid[blk[1]]),
		len(vm.Grid[blk[2]]),
		len(vm.Grid[blk[3]])}
	for i := 0; i < 4; i++ {
		if len(vm.Grid[blk[i]]) == blcount[i] {
			vm.Grid[blk[i]] = append(vm.Grid[blk[i]], ret)
		}
	}
	return ret
}

// RestoreOrBeginSnapshot() removes all vertices from map that were added
// since previous RestoreOrBeginSnapshot() call. Snapshots are useful to create
// distinct vertex spaces for line traces in diffgeometry operations (volatile
// vertices computed there might not end up being vertices actually placed on
// the map, and have additional restriction of being sortable alongside the
// line)
// As with the rest of VertexMap methods, snapshots do NOT provide for
// concurrent access
func (vm *VertexMap) RestoreOrBeginSnapshot() {
	if vm.Snapshot == nil {
		// begin
		vm.Snapshot = make([]int, len(vm.Grid))
		for i, it := range vm.Grid {
			vm.Snapshot[i] = len(it)
		}
	} else {
		// restore
		for i, it := range vm.Grid {
			vm.Grid[i] = it[:vm.Snapshot[i]]
		}
	}
}

func PopulateVertexMap(vm *VertexMap, allSegs []*NodeSeg) {
	for _, seg := range allSegs {
		vm.SelectVertexExact(float64(seg.psx), float64(seg.psy),
			int(seg.StartVertex.idx))
		vm.SelectVertexExact(float64(seg.pex), float64(seg.pey),
			int(seg.EndVertex.idx))
	}
}

func PopulateVertexMapFromLines(vm *VertexMap, lines AbstractLines) {
	l := int(lines.Len())
	for i := 0; i < l; i++ {
		x1, x2, y1, y2 := lines.GetAllXY(uint16(i))
		vm.SelectVertexExact(float64(x1), float64(y1), i)
		vm.SelectVertexExact(float64(x2), float64(y2), i)
	}
}

func PopulateVertexCache(cache map[SimpleVertex]int, allSegs []*NodeSeg) {
	for _, it := range allSegs {
		rec := SimpleVertex{int(it.StartVertex.X), int(it.StartVertex.Y)}
		cache[rec] = int(it.StartVertex.idx)
		rec = SimpleVertex{int(it.EndVertex.X), int(it.EndVertex.Y)}
		cache[rec] = int(it.EndVertex.idx)
	}
}

func (s *NodeSeg) getFlip() int16 {
	if s.flags&SEG_FLAG_FLIP != 0 {
		return 1
	}
	return 0
}

func (w *NodesWork) getNewSuperblock(template *Superblock) *Superblock {
	if w.qallocSupers == nil {
		ret := &Superblock{}
		ret.InitSectorsIfNeeded(template)
		ret.nwlink = w
		return ret
	}
	ret := w.qallocSupers
	w.qallocSupers = w.qallocSupers.subs[0] // see returnSuperblockToPool
	ret.subs[0] = nil
	ret.nwlink = w
	needMap := false
	if ret.sectors != nil { // shall give same result as template.sectors != nil
		ret.sectors = ret.sectors[:0]
		needMap = true
	}
	if ret.secEquivs != nil { // shall give same result as template.secEquivs != nil
		ret.secEquivs = ret.secEquivs[:0]
		needMap = true
	}
	if needMap {
		ret.secMap = make(map[uint16]struct{})
	}
	return ret
}

func (w *NodesWork) returnSuperblockToPool(block *Superblock) {
	if block.segs == nil {
		// pseudoSuperblock - don't touch! shared between threadds
		return
	}
	for num := 0; num < 2; num++ {
		if block.subs[num] == nil {
			continue
		}
		w.returnSuperblockToPool(block.subs[num])
		block.subs[num] = nil
	}
	block.segs = nil
	block.secMap = nil
	block.realNum = 0
	block.parent = nil
	block.nwlink = nil
	// qallocSupers is pool of reusable (but limited to current thread)
	// superblocks chained via subs[0]
	block.subs[0] = w.qallocSupers
	w.qallocSupers = block
}

// first seg is not stored for Zdoom extended/compressed nodes - compute
func getFirstSegExtended(ssubsectors []uint32, ssector uint32) uint32 {
	cum := uint32(0)
	for i := uint32(0); i < ssector; i++ {
		cum += ssubsectors[i]
	}
	return cum
}

// suppressErrorDueToExtraLogImport is never called
// znodegen.go gets "log" import added to it as the result of
// code generation, but nothing uses it, and unused imports cause build
// to fail, as go is very strict about it. So this stupid function will be
// copied to znodegen and use up the import
func suppressErrorDueToExtraLogImport(ts *NodeSeg) {
	// Notice it is lower case "log" that I never use in actually called code
	log.Panic("Function that was not supposed to be called was called.\n")
}

/*---------------------------------------------------------------------------*

	This message has been taken, complete, from OBJECTS.C in DEU5beta source.
	It outlines the method used here to pick the nodelines.

	IF YOU ARE WRITING A DOOM EDITOR, PLEASE READ THIS:

   I spent a lot of time writing the Nodes builder.  There are some bugs in
   it, but most of the code is OK.  If you steal any ideas from this program,
   put a prominent message in your own editor to make it CLEAR that some
   original ideas were taken from DEU.  Thanks.

   While everyone was talking about LineDefs, I had the idea of taking only
   the Segs into account, and creating the Segs directly from the SideDefs.
   Also, dividing the list of Segs in two after each call to CreateNodes makes
   the algorithm faster.  I use several other tricks, such as looking at the
   two ends of a Seg to see on which side of the nodeline it lies or if it
   should be split in two.  I took me a lot of time and efforts to do this.

   I give this algorithm to whoever wants to use it, but with this condition:
   if your program uses some of the ideas from DEU or the whole algorithm, you
   MUST tell it to the user.  And if you post a message with all or parts of
   this algorithm in it, please post this notice also.  I don't want to speak
   legalese; I hope that you understand me...  I kindly give the sources of my
   program to you: please be kind with me...

   If you need more information about this, here is my E-mail address:
   Raphael.Quinet@eed.ericsson.se (Rapha‰l Quinet).

   Short description of the algorithm:
     1 - Create one Seg for each SideDef: pick each LineDef in turn.  If it
	 has a "first" SideDef, then create a normal Seg.  If it has a
	 "second" SideDef, then create a flipped Seg.
     2 - Call CreateNodes with the current list of Segs.  The list of Segs is
	 the only argument to CreateNodes.
     3 - Save the Nodes, Segs and SSectors to disk.  Start with the leaves of
	 the Nodes tree and continue up to the root (last Node).

   CreateNodes does the following:
     1 - Pick a nodeline amongst the Segs (minimize the number of splits and
	 keep the tree as balanced as possible).
     2 - Move all Segs on the right of the nodeline in a list (segs1) and do
	 the same for all Segs on the left of the nodeline (in segs2).
     3 - If the first list (segs1) contains references to more than one
	 Sector or if the angle between two adjacent Segs is greater than
	 180ш, then call CreateNodes with this (smaller) list.  Else, create
	 a SubSector with all these Segs.
     4 - Do the same for the second list (segs2).
     5 - Return the new node (its two children are already OK).

   Each time CreateSSector is called, the Segs are put in a global list.
   When there is no more Seg in CreateNodes' list, then they are all in the
   global list and ready to be saved to disk.

*---------------------------------------------------------------------------*/
