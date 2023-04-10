// Code generated from other source files. DO NOT EDIT.
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
	"reflect"
	"runtime"
	"sort"
	"strconv"
	"sync"
	"time"
)

type ZExt_PickNodeFunc func(*ZExt_NodesWork, *ZExt_NodeSeg, *NodeBounds, *ZExt_Superblock) *ZExt_NodeSeg

type ZExt_CreateNodeSSFunc func(*ZExt_NodesWork, *ZExt_NodeSeg, *NodeBounds, *ZExt_Superblock) *NodeInProcess
type ZExt_StkCreateNodeSSFunc func(*ZExt_NodesWork, *ZExt_NodeSeg, *NodeBounds, *ZExt_Superblock, *ZExt_StkQueue) *NodeInProcess

type ZExt_SingleSectorDivisorFunc func(w *ZExt_NodesWork, ts *ZExt_NodeSeg, rs **ZExt_NodeSeg, ls **ZExt_NodeSeg, bbox *NodeBounds, super *ZExt_Superblock, rightsSuper,
	leftsSuper **ZExt_Superblock, partsegs *[]PartSeg)

type ZExt_DoLinesIntersectFunc func(c *ZExt_IntersectionContext) uint8

type ZExt_NodesWork struct {
	mlog            *MiniLogger
	lines           WriteableLines
	sides           []Sidedef
	sectors         []Sector
	allSegs         []*ZExt_NodeSeg
	subsectors      []SubSector
	segs            []Seg
	nodeX           int
	nodeY           int
	nodeDx          int
	nodeDy          int
	totals          *NodesTotals
	vertices        []ZExt_NodeVertex
	nodes           []Node
	nreverse        uint32
	segAliasObj     *SegAliasHolder
	pickNode        ZExt_PickNodeFunc
	createNodeSS    ZExt_CreateNodeSSFunc
	stkCreateNodeSS ZExt_StkCreateNodeSSFunc

	stkDivisorSS ZExt_SingleSectorDivisorFunc

	sectorHits       []uint8
	incidental       []ZExt_IntVertexPairC
	solidMap         *Blockmap
	nonVoidCache     map[int]ZExt_NonVoidPerAlias
	dgVertexMap      *ZExt_VertexMap
	blockity         *BlockityLines
	deepNodes        []DeepNode
	deepSubsectors   []DeepSubSector
	deepSegs         []DeepSeg
	SsectorMask      uint32
	blocksHit        []ZExt_BlocksHit
	diagonalPenalty  int
	pickNodeFactor   int
	pickNodeUser     int
	minorIsBetter    MinorIsBetterFunc
	doLinesIntersect ZExt_DoLinesIntersectFunc
	multipart        bool
	parts            []*ZExt_NodeSeg
	width            int
	depthArtifacts   bool
	nodeType         int
	vertexCache      map[SimpleVertex]int
	vertexExists     int
	sidenessCache    *SidenessCache
	zenScores        []ZExt_DepthScoreBundle
	qallocSupers     *ZExt_Superblock
	upgradableToDeep bool

	stkExtra map[*NodeInProcess]StkNodeExtraData

	zdoomVertexHeader *ZdoomNode_VertexHeader
	zdoomVertices     []ZdoomNode_Vertex
	zdoomSubsectors   []uint32
	zdoomSegs         []ZdoomNode_Seg
	vertexMap         *ZExt_VertexMap
}

type ZExt_NodeVertex struct {
	X   ZNumber
	Y   ZNumber
	idx uint32
}

type ZExt_NodeSeg struct {
	psx, psy, pex, pey ZNumber
	pdx, pdy, perp     ZNumber
	len                ZNumber
	nextInSuper        *ZExt_NodeSeg
	next               *ZExt_NodeSeg
	partner            *ZExt_NodeSeg
	alias              int
	sector             uint16
	secEquiv           uint16
	flags              uint8
	sidenessIdx        int
	block              *ZExt_Superblock
	StartVertex        *ZExt_NodeVertex
	EndVertex          *ZExt_NodeVertex
	Angle              int16
	Linedef            uint16
	Offset             uint16
}

type ZExt_IntersectionContext struct {
	psx, psy, pex, pey ZNumber
	pdx, pdy           ZNumber
	lex, lsx, ley, lsy ZNumber
}

func ZExt_NodesGenerator(input *NodesInput) {
	start := time.Now()

	input.lines.UnshareData()

	input.lines.PruneUnusedVertices()

	input.lines.DetectPolyobjects()

	upgradableToDeep := false
	if input.nodeType == NODETYPE_VANILLA_OR_DEEP {
		input.nodeType = NODETYPE_VANILLA
		upgradableToDeep = true
	}

	allSegs, intVertices := ZExt_createSegs(input.lines, input.sidedefs,
		input.linesToIgnore, input.nodeType == NODETYPE_ZDOOM_EXTENDED ||
			input.nodeType == NODETYPE_ZDOOM_COMPRESSED)
	if len(allSegs) == 0 {
		Log.Error("Failed to create any SEGs (BAD). Quitting (%s)\n.", time.Since(start))

		input.bcontrol <- BconRequest{
			Sender:  SOLIDBLOCKS_NODES,
			Message: BCON_NONEED_SOLID_BLOCKMAP,
		}

		input.nodesChan <- NodesResult{}
		return
	}

	ssectorMask := uint32(SSECTOR_NORMAL_MASK)

	if input.nodeType != NODETYPE_VANILLA {
		ssectorMask = SSECTOR_DEEP_MASK
	}

	requestedSolid := input.pickNodeUser == PICKNODE_VISPLANE_ADV
	if requestedSolid {

		input.bcontrol <- BconRequest{
			Sender:  SOLIDBLOCKS_NODES,
			Message: BCON_NEED_SOLID_BLOCKMAP,
		}
	} else {

		input.bcontrol <- BconRequest{
			Sender:  SOLIDBLOCKS_NODES,
			Message: BCON_NONEED_SOLID_BLOCKMAP,
		}
	}

	rootSeg := allSegs[0]

	rootBox := ZExt_FindLimits(rootSeg)
	Log.Printf("Initial number of segs is %d.\n", len(allSegs))
	Log.Printf("Nodes: rendered part of map goes from (%d,%d) to (%d,%d)\n",
		rootBox.Ymax, rootBox.Xmin, rootBox.Ymin, rootBox.Xmax)

	doLinesIntersect := ZExt_doLinesIntersectStandard
	if input.nodeType == NODETYPE_ZDOOM_COMPRESSED ||
		input.nodeType == NODETYPE_ZDOOM_EXTENDED {

	} else {
		heur := false
		switch input.detailFriendliness {
		case NODE_DETAIL_ALWAYS:
			doLinesIntersect = ZExt_doLinesIntersectDetail
		case NODE_DETAIL_SUPPRESS:
			doLinesIntersect = ZExt_doLinesIntersectStandard
		case NODE_DETAIL_HEURISTIC:
			heur = true
		default:
			heur = input.nodeType == NODETYPE_VANILLA
			if !heur {
				doLinesIntersect = ZExt_doLinesIntersectDetail
			}
		}
		if heur {

			detail := IsTooDetailed(input.lines, input.linesToIgnore, rootBox)
			if detail {
				doLinesIntersect = ZExt_doLinesIntersectDetail
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
		input.ZExt_applySectorEquivalence(allSegs, sectorEquiv)
		Log.Verbose(1, "Number of sector equivalencies vs number of sectors: %d/%d\n",
			whichLen, len(input.sectors))
	}

	workData := ZExt_NodesWork{
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
		pickNode:         ZExt_PickNodeFuncFromOption(input.pickNodeUser),
		createNodeSS:     ZExt_CreateNodeSSFromOption(input.pickNodeUser),
		stkCreateNodeSS:  ZExt_StkCreateNodeSSFromOption(input.pickNodeUser),
		stkDivisorSS:     ZExt_StkSingleSectorDivisorFromOption(input.pickNodeUser),
		sectorHits:       make([]byte, whichLen),
		incidental:       make([]ZExt_IntVertexPairC, 0),
		solidMap:         solidMap,
		nonVoidCache:     make(map[int]ZExt_NonVoidPerAlias),
		SsectorMask:      ssectorMask,
		blocksHit:        make([]ZExt_BlocksHit, 0),
		diagonalPenalty:  input.diagonalPenalty,
		pickNodeFactor:   input.pickNodeFactor,
		pickNodeUser:     input.pickNodeUser,
		minorIsBetter:    MinorCmpFuncFromOption(input.minorIsBetterUser),
		multipart:        input.minorIsBetterUser == MINOR_CMP_DEPTH,
		depthArtifacts:   input.depthArtifacts,
		nodeType:         input.nodeType,
		doLinesIntersect: doLinesIntersect,
		sidenessCache: &SidenessCache{
			maxKnownAlias: 0,
		},
		width:            input.width,
		upgradableToDeep: upgradableToDeep,
	}
	if input.nodeType == NODETYPE_ZDOOM_EXTENDED ||
		input.nodeType == NODETYPE_ZDOOM_COMPRESSED {
		workData.zdoomVertexHeader = &ZdoomNode_VertexHeader{
			ReusedOriginalVertices: uint32(input.lines.GetVerticesCount()),
		}
		workData.vertexMap = ZExt_CreateVertexMap(&workData, rootBox.Xmin, rootBox.Ymin,
			rootBox.Xmax, rootBox.Ymax)
		ZExt_PopulateVertexMap(workData.vertexMap, allSegs)
	} else {
		workData.vertexCache = make(map[SimpleVertex]int)
		ZExt_PopulateVertexCache(workData.vertexCache, allSegs)

	}
	if workData.solidMap != nil {

		lineBox := GetLineBounds(workData.lines)
		dgVertexMap := ZExt_CreateVertexMap(&workData, lineBox.Xmin, lineBox.Ymin,
			lineBox.Xmax, lineBox.Ymax)
		ZExt_PopulateVertexMapFromLines(dgVertexMap, workData.lines)
		workData.dgVertexMap = dgVertexMap
	}
	if input.pickNodeUser == PICKNODE_ZENLIKE {

		workData.zenScores = make([]ZExt_DepthScoreBundle, 0, len(allSegs))
	}
	workData.segAliasObj.Init()
	initialSuper := workData.doInitialSuperblocks(rootBox)
	if input.cacheSideness {
		workData.buildSidenessCache(rootSeg, initialSuper)
	}

	var rootNode *NodeInProcess
	treeCount := 0
	if input.multiTreeMode == MULTITREE_NOTUSED {
		if input.stkNode {

			rootNode = ZExt_StkEntryPoint(&workData, rootSeg, rootBox, initialSuper)
		} else {

			rootNode = ZExt_CreateNode(&workData, rootSeg, rootBox, initialSuper)
		}
	} else if input.multiTreeMode == MULTITREE_ROOT_ONLY {

		if workData.sidenessCache.maxKnownAlias == 0 {

			workData.sidenessCache.readOnly = true
		}

		rootNode, treeCount = ZExt_MTPSentinel_MakeBestBSPTree(&workData, rootBox,
			initialSuper, input.specialRootMode)
	} else {
		Log.Panic("Multi-tree variant not implemented.\n")
	}

	Log.Merge(workData.mlog, "Merging buffered output (if any) from the chosen tree.\n")
	workData.mlog = nil

	Log.Flush()

	Log.Printf("Created %d subsectors, %d nodes. Got %d segs. Split segs %d times.\n",
		workData.totals.numSSectors, workData.totals.numNodes,
		workData.totals.numSegs, workData.totals.segSplits)

	if requestedSolid {

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

	if workData.upgradableToDeep && workData.nodeType == NODETYPE_VANILLA &&
		workData.tooManySegsCantFix(true) {
		workData.upgradeToDeep()
	}

	if input.stkNode && config.Deterministic {
		if workData.nodeType == NODETYPE_VANILLA {
			workData.RearrangeBSPVanilla(rootNode)
		} else if workData.nodeType == NODETYPE_DEEP {
			workData.RearrangeBSPDeep(rootNode)
		} else {
			workData.RearrangeBSPExtended(rootNode)
		}
	}

	Log.Printf("Height of left and right subtrees = (%d,%d)\n", hLeft, hRight)
	if workData.nodeType == NODETYPE_VANILLA {
		workData.reverseNodes(rootNode)
	} else {

		workData.reverseDeepNodes(rootNode)
	}
	Log.Printf("Max seg count in subsector: %d\n", workData.totals.maxSegCountInSubsector)
	if workData.totals.preciousSplit > 0 {

	}

	if workData.upgradableToDeep {
		if workData.nodeType == NODETYPE_DEEP {
			Log.Printf("I have switched to DeeP nodes format to avoid overflow.\n")
		} else {
			Log.Printf("I have kept nodes in vanilla format.\n")
		}
	}

	if uint32(workData.totals.numSSectors)&workData.SsectorMask == workData.SsectorMask {
		Log.Error("Number of subsectors is too large (%d) for this format. Lumps NODES, SEGS, SSECTORS will be emptied.\n",
			workData.totals.numSSectors)
		workData.emptyNodesLumps()
	} else if workData.tooManySegsCantFix(false) {
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

	if workData.nodeType == NODETYPE_DEEP {
		input.nodesChan <- NodesResult{
			deepNodes:      workData.deepNodes,
			deepSegs:       workData.deepSegs,
			deepSubsectors: workData.deepSubsectors,
		}
	} else if workData.nodeType == NODETYPE_VANILLA {
		input.nodesChan <- NodesResult{
			nodes:      workData.nodes,
			segs:       workData.segs,
			subsectors: workData.subsectors,
		}
	} else {
		if workData.deepNodes == nil {

			input.nodesChan <- NodesResult{}
		} else {

			input.nodesChan <- NodesResult{
				rawNodes:   workData.getZdoomNodesBytes(),
				compressed: workData.nodeType == NODETYPE_ZDOOM_COMPRESSED,
			}
		}
	}

	if treeCount == 0 {
		Log.Printf("Nodes took %s\n", time.Since(start))
	} else {
		dur := time.Since(start)
		avg := time.Duration(int64(dur) / int64(treeCount))
		Log.Printf("Nodes took %s (avg %s per tree)\n", dur, avg)
	}
}

func (n ZNumber) Floor16() int16 {
	return int16(n)
}

func ZExt_computeAngle(dx, dy ZNumber) int {
	w := math.Atan2(float64(dy), float64(dx)) * float64(65536.0/(math.Pi*2))

	if w < 0 {
		w = 65536.0 + w
	}

	return int(w)
}

func ZExt_FindLimits(ts *ZExt_NodeSeg) *NodeBounds {
	var r NodeBounds

	r.Xmax = -2147483648
	r.Ymax = -2147483648
	r.Xmin = 2147483647
	r.Ymin = 2147483647
	for true {
		fv := ts.StartVertex
		tv := ts.EndVertex

		if fv.X < ZNumber(r.Xmin) {
			r.Xmin = fv.X.Floor()
		}
		if fv.X > ZNumber(r.Xmax) {
			r.Xmax = fv.X.Ceil()
		}
		if fv.Y < ZNumber(r.Ymin) {
			r.Ymin = fv.Y.Floor()
		}
		if fv.Y > ZNumber(r.Ymax) {
			r.Ymax = fv.Y.Ceil()
		}

		if tv.X < ZNumber(r.Xmin) {
			r.Xmin = tv.X.Floor()
		}
		if tv.X > ZNumber(r.Xmax) {
			r.Xmax = tv.X.Ceil()
		}
		if tv.Y < ZNumber(r.Ymin) {
			r.Ymin = tv.Y.Floor()
		}
		if tv.Y > ZNumber(r.Ymax) {
			r.Ymax = tv.Y.Ceil()
		}

		if ts.next == nil {
			break
		}
		ts = ts.next
	}
	return &r
}

func ZExt_splitDist(lines AbstractLines, seg *ZExt_NodeSeg) int {
	var dx, dy float64

	if seg.getFlip() == 0 {

		x1, y1, _, _ := lines.GetAllXY(seg.Linedef)
		fx1 := ZNumber(x1)
		fy1 := ZNumber(y1)
		dx = float64(fx1 - seg.StartVertex.X)
		dy = float64(fy1 - seg.StartVertex.Y)
	} else {

		_, _, x2, y2 := lines.GetAllXY(seg.Linedef)
		fx2 := ZNumber(x2)
		fy2 := ZNumber(y2)
		dx = float64(fx2 - seg.StartVertex.X)
		dy = float64(fy2 - seg.StartVertex.Y)
	}

	if dx == 0 && dy == 0 {
		Log.Error("Trouble in splitDist %d!%d %f,%f\n", seg.Linedef, seg.getFlip(), dx, dy)
	}
	t := math.Sqrt((dx * dx) + (dy * dy))
	return int(t)
}

func ZExt_doLinesIntersectDetail(c *ZExt_IntersectionContext) uint8 {
	dx2 := c.psx - c.lsx
	dy2 := c.psy - c.lsy
	dx3 := c.psx - c.lex
	dy3 := c.psy - c.ley

	a := c.pdy*dx2 - c.pdx*dy2
	b := c.pdy*dx3 - c.pdx*dy3
	if ZDiffSign(a, b) && (a != 0) && (b != 0) {

		x, y := c.computeIntersection()
		dx2 = c.lsx - x
		dy2 = c.lsy - y
		if dx2 == 0 && dy2 == 0 {
			a = 0
		} else {

			l := ZWideNumber(dx2)*ZWideNumber(dx2) + ZWideNumber(dy2)*ZWideNumber(dy2)
			if l < ZWideNumber(2) {

				a = 0
			}
		}
		dx3 = c.lex - x
		dy3 = c.ley - y
		if dx3 == 0 && dy3 == 0 {
			b = 0
		} else {
			l := ZWideNumber(dx3)*ZWideNumber(dx3) + ZWideNumber(dy3)*ZWideNumber(dy3)
			if l < ZWideNumber(2) {
				b = 0
			}
		}
	}

	var val uint8

	if a == 0 {
		val = val | 16
	} else if a < 0 {
		val = val | 32
	} else {
		val = val | 64
	}

	if b == 0 {
		val = val | 1
	} else if b < 0 {
		val = val | 2
	} else {
		val = val | 4
	}

	return val
}

func (w *ZExt_NodesWork) isItConvex(ts *ZExt_NodeSeg) int {
	nonConvexityMode := NONCONVEX_ONESECTOR

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
					return NONCONVEX_MULTISECTOR
				} else {

					nonConvexityMode = NONCONVEX_MULTISECTOR
				}
			}
			line = line.next
		}
	} else {

		nonConvexityMode = NONCONVEX_MULTISECTOR
	}

	var c ZExt_IntersectionContext
	for line := ts; line != nil; line = line.next {
		c.psx = line.StartVertex.X
		c.psy = line.StartVertex.Y
		c.pex = line.EndVertex.X
		c.pey = line.EndVertex.Y
		c.pdx = c.psx - c.pex
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
						w.mlog.ZExt_DumpSegs(ts)
					}
					return nonConvexityMode
				}
			}
		}
	}

	return CONVEX_SUBSECTOR
}

func (w *ZExt_NodesWork) IsConvexMultiSector(ts *ZExt_NodeSeg) bool {
	multisector := false
	sector := ts.sector
	for check := ts; check != nil; check = check.next {
		if check.sector != sector {
			multisector = true
			break
		}
	}
	if !multisector {
		return false
	}

	var c ZExt_IntersectionContext
	for line := ts; line != nil; line = line.next {
		c.psx = line.StartVertex.X
		c.psy = line.StartVertex.Y
		c.pex = line.EndVertex.X
		c.pey = line.EndVertex.Y
		c.pdx = c.psx - c.pex
		c.pdy = c.psy - c.pey
		for check := ts; check != nil; check = check.next {
			if line != check {
				c.lsx = check.StartVertex.X
				c.lsy = check.StartVertex.Y
				c.lex = check.EndVertex.X
				c.ley = check.EndVertex.Y
				val := w.doLinesIntersect(&c)
				if val&34 != 0 {
					return false
				}
			}
		}
	}
	return true
}

func (w *ZExt_NodesWork) GetPartSegs(ts *ZExt_NodeSeg, best *ZExt_NodeSeg, receiver *[]PartSeg) {
	wasNil := false
	if len(w.parts) == 0 {
		if w.parts == nil {
			w.parts = make([]*ZExt_NodeSeg, 0)
			wasNil = true
		}
		w.parts = append(w.parts, best)
	}
	if *receiver == nil {
		*receiver = make([]PartSeg, 0)
	}

	limit := w.width
	if limit < 0 {
		limit = len(w.parts)
	} else if limit > len(w.parts) {

		limit = len(w.parts)
	}

	for _, v := range w.parts[:limit] {
		partseg := ZExt_getPartSeg(ts, v)
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

func ZExt_getPartSeg(ts *ZExt_NodeSeg, target *ZExt_NodeSeg) PartSeg {
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
		Occurence: -1,
	}
}

func (w *ZExt_NodesWork) DivideSegs(ts *ZExt_NodeSeg, rs **ZExt_NodeSeg, ls **ZExt_NodeSeg, bbox *NodeBounds,
	super *ZExt_Superblock, rightsSuper, leftsSuper **ZExt_Superblock, partsegs *[]PartSeg) {

	best := w.pickNode(w, ts, bbox, super)

	if best == nil {

		Log.Panic("Couldn't pick nodeline!\n")
	}

	if partsegs != nil {
		w.GetPartSegs(ts, best, partsegs)
	}

	c := &ZExt_IntersectionContext{
		psx: best.StartVertex.X,
		psy: best.StartVertex.Y,
		pex: best.EndVertex.X,
		pey: best.EndVertex.Y,
	}
	c.pdx = c.psx - c.pex
	c.pdy = c.psy - c.pey

	w.SetNodeCoords(best, bbox, c)

	w.DivideSegsActual(ts, rs, ls, bbox, best, c, super, rightsSuper, leftsSuper)
}

func (w *ZExt_NodesWork) DivideSegsActual(ts *ZExt_NodeSeg, rs **ZExt_NodeSeg, ls **ZExt_NodeSeg, bbox *NodeBounds, best *ZExt_NodeSeg, c *ZExt_IntersectionContext, super *ZExt_Superblock, rightsSuper, leftsSuper **ZExt_Superblock) {

	var rights, lefts, strights, stlefts *ZExt_NodeSeg

	w.dismissChildrenToSuperblockPool(super)
	*rightsSuper = w.getNewSuperblock(super)
	*leftsSuper = w.getNewSuperblock(super)
	(*rightsSuper).SetBounds(bbox)
	(*leftsSuper).SetBounds(bbox)
	w.returnSuperblockToPool(super)

	asector := -1

	tmpsNext := ts.next
	for tmps := ts; tmps != nil; tmps = tmpsNext {
		if asector == -1 {
			asector = int(tmps.sector)
		} else if asector != int(tmps.sector) {
			asector = -2
		}

		addToRs := [2]*ZExt_NodeSeg{nil, nil}
		addToLs := [2]*ZExt_NodeSeg{nil, nil}
		if tmps != best && tmps != best.partner {

			w.CategoriseAndMaybeDivideSeg(addToRs[:], addToLs[:], c, &tmps)
		} else {

			addToRs[0] = best
			addToLs[0] = best.partner
			if best.partner != nil {

				tmps.partner.partner = nil
				tmps.partner = nil

				tmps = tmps.next

			}
		}
		tmpsNext = tmps.next

		ZExt_addSegToSide(addToRs[:], &rights, &strights)
		ZExt_addSegToSide(addToLs[:], &lefts, &stlefts)
	}

	if strights == nil {

		Log.Panic("Should have had partition at the right side (hardcoded to go there).\n")
	}

	if stlefts == nil {

		if asector > 0 {
			w.mlog.Verbose(1, "No left side, moving partition into left side (single sector #%d)\n",
				asector)
		} else {
			if config.VerbosityLevel >= 3 {

				sectorsHit := make(map[uint16]bool)
				for tmps := ts; tmps != nil; tmps = tmps.next {
					sectorsHit[tmps.sector] = true
				}
				s := ""
				for sector := range sectorsHit {
					s += "," + strconv.Itoa(int(sector))
				}
				if s != "" {
					s = s[1:]
				}
				w.mlog.Verbose(1, "No left side, moving partition into left side (sectors %s)\n", s)
			} else {
				w.mlog.Verbose(1, "No left side, moving partition into left side (multiple sectors in this node)\n")
			}
		}
		lefts = best
		stlefts = best
		var prev *ZExt_NodeSeg
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

		stlefts.next = nil
		prev.next = nil

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

func ZExt_addSegToSide(segToAdd []*ZExt_NodeSeg, thisside, stThisSide **ZExt_NodeSeg) {
	if segToAdd[0] != nil {

		segToAdd[0].next = nil
		if *thisside == nil {
			*thisside = segToAdd[0]
			*stThisSide = segToAdd[0]

		} else {
			(*thisside).next = segToAdd[0]
			*thisside = segToAdd[0]
		}

		if segToAdd[1] != nil {
			segToAdd[1].next = nil
			(*thisside).next = segToAdd[1]
			*thisside = segToAdd[1]
		}
	}
}

func (w *ZExt_NodesWork) CategoriseAndMaybeDivideSeg(addToRs []*ZExt_NodeSeg, addToLs []*ZExt_NodeSeg, c *ZExt_IntersectionContext, tmps **ZExt_NodeSeg) bool {

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

		if atmps.flags&SEG_FLAG_PRECIOUS != 0 {

			w.totals.preciousSplit++
		}
		x, y := c.computeIntersection()
		newVertex := w.AddVertex(x, y)
		news := new(ZExt_NodeSeg)
		atmps.alias = 0
		if w.sidenessCache.maxKnownAlias > 0 {
			if ZExt_getGlobalFlip(atmps) {
				atmps.flags |= SEG_FLAG_GLOBAL_FLIP
			} else {
				atmps.flags &= ^uint8(SEG_FLAG_GLOBAL_FLIP)
			}
		}
		atmps.flags |= SEG_FLAG_NOCACHE
		*news = *atmps
		atmps.next = news
		news.StartVertex = newVertex
		atmps.EndVertex = newVertex
		news.Offset = uint16(ZExt_splitDist(w.lines, news))
		w.totals.segSplits++
		if atmps.partner != nil {
			if atmps.partner.flags&SEG_FLAG_PRECIOUS != 0 {
				w.totals.preciousSplit++
			}
			w.totals.segSplits++
			atmps.partner.alias = 0
			if w.sidenessCache.maxKnownAlias > 0 {
				if ZExt_getGlobalFlip(atmps.partner) {
					atmps.partner.flags |= SEG_FLAG_GLOBAL_FLIP
				} else {
					atmps.partner.flags &= ^uint8(SEG_FLAG_GLOBAL_FLIP)
				}
			}
			atmps.partner.flags |= SEG_FLAG_NOCACHE

			newVertex2 := newVertex
			news2 := new(ZExt_NodeSeg)
			*news2 = *(atmps.partner)
			news2.StartVertex = newVertex2
			atmps.partner.EndVertex = newVertex2
			news2.Offset = uint16(ZExt_splitDist(w.lines, news2))

			oldpartner := atmps.partner
			atmps.partner = news2
			news2.partner = atmps
			oldpartner.partner = news
			news.partner = oldpartner
			oldpartner.next = news2
			news.next = oldpartner

			*tmps = news2
		} else {

			*tmps = news
		}

		w.recomputeSegs(atmps, news)
		if atmps.partner != nil {
			w.recomputeSegs(news.partner, atmps.partner)
		}

		if val&32 != 0 {
			addToLs[0] = atmps
			addToLs[1] = atmps.partner
		}
		if val&64 != 0 {
			addToRs[0] = atmps
			addToRs[1] = atmps.partner
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

		if val&34 != 0 {

			addToLs[0] = atmps
			addToLs[1] = atmps.partner
		}
		if val&68 != 0 {

			addToRs[0] = atmps
			addToRs[1] = atmps.partner
		}
		if (val&1 != 0) && (val&16 != 0) {

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
			*tmps = atmps.partner
			if partnersBreakUp {

				atmps.partner.partner = nil
				atmps.partner = nil
			}
		}
		return false
	}
}

func (w *ZExt_NodesWork) recomputeSegs(originSeg, newSeg *ZExt_NodeSeg) {

	originNewLen := int(newSeg.Offset) - int(originSeg.Offset)
	originSeg.len = ZNumber(originNewLen)
	newSeg.len = newSeg.len - ZNumber(originNewLen)

	oldAngle := originSeg.Angle
	w.recomputeOneSeg(originSeg)
	w.recomputeOneSeg(newSeg)
	if oldAngle != originSeg.Angle || oldAngle != newSeg.Angle {

		w.mlog.Verbose(3, "Angle changed after splitting seg:  %d -> (%d ; %d)\n",
			oldAngle, originSeg.Angle, newSeg.Angle)
	}
}

func (w *ZExt_NodesWork) recomputeOneSeg(s *ZExt_NodeSeg) {
	s.pex = s.EndVertex.X
	s.psx = s.StartVertex.X
	s.pdx = s.pex - s.psx
	s.pey = s.EndVertex.Y
	s.psy = s.StartVertex.Y
	s.pdy = s.pey - s.psy
	s.perp = s.pdx*s.psy - s.psx*s.pdy
	w.updateSegLenBetter(s)
	bamEffect := w.lines.GetBAMEffect(s.Linedef)
	if bamEffect.Action == BAM_REPLACE {
		s.Angle = bamEffect.Value
	} else {

		s.Angle = int16(ZExt_computeAngle(s.pdx, s.pdy))
		if bamEffect.Action == BAM_ADDITIVE {
			s.Angle += bamEffect.Value
		}
	}
	horizon := w.lines.IsHorizonEffect(s.Linedef)
	if horizon {

		isFront := s.getFlip() == 0
		sdef := w.lines.GetSidedefIndex(s.Linedef, isFront)
		s.Angle += int16(float64(w.sides[sdef].XOffset) * float64(65536.0/360.0))
	}
}

func ZExt_CreateNode(w *ZExt_NodesWork, ts *ZExt_NodeSeg, bbox *NodeBounds, super *ZExt_Superblock) *NodeInProcess {
	res := new(NodeInProcess)
	var rights *ZExt_NodeSeg
	var lefts *ZExt_NodeSeg
	var rightsSuper *ZExt_Superblock
	var leftsSuper *ZExt_Superblock

	w.totals.numNodes++
	w.DivideSegs(ts, &rights, &lefts, bbox, super, &rightsSuper, &leftsSuper, nil)

	super = nil
	res.X = int16(w.nodeX)
	res.Y = int16(w.nodeY)
	res.Dx = int16(w.nodeDx)
	res.Dy = int16(w.nodeDy)

	leftBox := ZExt_FindLimits(lefts)
	res.Lbox[BB_TOP] = int16(leftBox.Ymax)
	res.Lbox[BB_BOTTOM] = int16(leftBox.Ymin)
	res.Lbox[BB_LEFT] = int16(leftBox.Xmin)
	res.Lbox[BB_RIGHT] = int16(leftBox.Xmax)
	state := w.isItConvex(lefts)
	if state == CONVEX_SUBSECTOR {
		res.nextL = nil
		res.LChild = w.CreateSSector(lefts) | SSECTOR_DEEP_MASK
		w.returnSuperblockToPool(leftsSuper)
	} else if state == NONCONVEX_ONESECTOR {
		res.nextL = w.createNodeSS(w, lefts, leftBox, leftsSuper)
		res.LChild = 0
	} else {
		if config.VerbosityLevel >= 1 && w.IsConvexMultiSector(lefts) {
			w.dumpMultiSectorSegs(lefts)
		}
		res.nextL = ZExt_CreateNode(w, lefts, leftBox, leftsSuper)
		res.LChild = 0
	}
	leftsSuper = nil

	rightBox := ZExt_FindLimits(rights)
	res.Rbox[BB_TOP] = int16(rightBox.Ymax)
	res.Rbox[BB_BOTTOM] = int16(rightBox.Ymin)
	res.Rbox[BB_LEFT] = int16(rightBox.Xmin)
	res.Rbox[BB_RIGHT] = int16(rightBox.Xmax)
	state = w.isItConvex(rights)
	if state == CONVEX_SUBSECTOR {
		res.nextR = nil
		res.RChild = w.CreateSSector(rights) | SSECTOR_DEEP_MASK
		w.returnSuperblockToPool(rightsSuper)
	} else if state == NONCONVEX_ONESECTOR {
		res.nextR = w.createNodeSS(w, rights, rightBox, rightsSuper)
		res.RChild = 0
	} else {
		if config.VerbosityLevel >= 1 && w.IsConvexMultiSector(rights) {
			w.dumpMultiSectorSegs(rights)
		}
		res.nextR = ZExt_CreateNode(w, rights, rightBox, rightsSuper)
		res.RChild = 0
	}
	rightsSuper = nil

	return res
}

func (w *ZExt_NodesWork) dumpMultiSectorSegs(ts *ZExt_NodeSeg) {
	if config.VerbosityLevel < 1 {
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

func (n ZNumber) ToFixed16Dot16() int32 {
	return int32(float64(n) * FIXED16DOT16_MULTIPLIER)
}

func (s *ZExt_NodeSeg) getFlip() int16 {
	if s.flags&SEG_FLAG_FLIP != 0 {
		return 1
	}
	return 0
}

func ZExt_suppressErrorDueToExtraLogImport(ts *ZExt_NodeSeg) {

	log.Panic("Function that was not supposed to be called was called.\n")
}

func ZExt_PickNodeFuncFromOption(userOption int) ZExt_PickNodeFunc {
	switch userOption {
	case PICKNODE_TRADITIONAL:
		{
			return ZExt_PickNode_traditional
		}
	case PICKNODE_VISPLANE:
		{
			return ZExt_PickNode_visplaneKillough
		}
	case PICKNODE_VISPLANE_ADV:
		{
			return ZExt_PickNode_visplaneVigilant
		}
	case PICKNODE_MAELSTROM:
		{
			return ZExt_PickNode_maelstrom
		}
	case PICKNODE_ZENLIKE:
		{
			return ZExt_PickNode_ZennodeDepth
		}
	default:
		{
			Log.Panic("Invalid argument\n")
			return nil
		}
	}
}

func ZExt_CreateNodeSSFromOption(userOption int) ZExt_CreateNodeSSFunc {
	if userOption == PICKNODE_VISPLANE_ADV {
		return ZExt_CreateNodeForSingleSector
	}
	return ZExt_CreateNode
}

func ZExt_StkCreateNodeSSFromOption(userOption int) ZExt_StkCreateNodeSSFunc {
	if userOption == PICKNODE_VISPLANE_ADV {
		return ZExt_StkCreateNodeForSingleSector
	}
	return ZExt_StkCreateNode
}

func ZExt_StkSingleSectorDivisorFromOption(userOption int) ZExt_SingleSectorDivisorFunc {
	if userOption == PICKNODE_VISPLANE_ADV {
		return ZExt_VigilantSingleSectorDivisor
	}
	return ZExt_DefaultSingleSectorDivisor
}

func ZExt_createSegs(lines WriteableLines, sidedefs []Sidedef,
	linesToIgnore []bool, extNode bool) ([]*ZExt_NodeSeg, []ZExt_NodeVertex) {
	res := make([]*ZExt_NodeSeg, 0, 65536)
	var rootCs, lastCs *ZExt_NodeSeg

	var cull *Culler
	if config.CullInvisibleSegs != CULL_SEGS_DONT {

		cull = new(Culler)

		if config.CullInvisibleSegs == CULL_SEGS_SLOPPY {
			cull.SetMode(CREATE_SEGS_SLOPPY, sidedefs)
		} else {
			cull.SetMode(CREATE_SEGS, sidedefs)
		}
		cull.SetWriteableLines(lines)
	}

	myVertices := make([]ZExt_NodeVertex, lines.GetVerticesCount())
	l := lines.Len()
	for i := uint16(0); i < l; i++ {
		if linesToIgnore != nil && linesToIgnore[i] {
			continue
		}
		x1, y1, x2, y2 := lines.GetAllXY(i)
		vidx1, vidx2 := lines.GetLinedefVertices(i)
		ZExt_storeNodeVertex(myVertices, x1, y1, uint32(vidx1))
		ZExt_storeNodeVertex(myVertices, x2, y2, uint32(vidx2))
		if x1 == x2 && y1 == y2 {
			continue
		}

		culled := cull.AddLine(i)
		if lines.IsDoNotRender(i) {
			Log.Verbose(1, "Linedef %d will not be rendered because it was either tagged so (tag 998) or assigned an action with numeric code 1086.\n",
				i)
			continue
		}

		if culled {
			continue
		} else {

			firstSdef := lines.GetSidedefIndex(i, true)
			secondSdef := lines.GetSidedefIndex(i, false)
			ZExt_addSegsPerLine(myVertices, i, lines, vidx1, vidx2, firstSdef, secondSdef,
				&rootCs, &lastCs, sidedefs, &res, extNode)
		}

	}

	cull.Analyze()

	unculled := false
	for cull.SpewBack() {
		line := cull.GetLine()
		vidx1, vidx2 := lines.GetLinedefVertices(line)
		firstSdef := lines.GetSidedefIndex(line, true)
		secondSdef := lines.GetSidedefIndex(line, false)
		if cull.GetMode() == CREATE_SEGS_SLOPPY {

			Log.Verbose(1, "Linedef %d will be rendered after all, I think sector %d may be self-referencing.\n",
				line, sidedefs[firstSdef].Sector)
		} else {
			Log.Verbose(1, "Linedef %d will be rendered after all - used to create self-referencing effect for sector %d.\n",
				line, sidedefs[firstSdef].Sector)
		}

		ZExt_addSegsPerLine(myVertices, line, lines, vidx1, vidx2, firstSdef, secondSdef,
			&rootCs, &lastCs, sidedefs, &res, extNode)
		unculled = true
	}

	if unculled {

		ZExt_restoreSegOrder(res)
	}

	return res, myVertices
}

func ZExt_addSegsPerLine(myVertices []ZExt_NodeVertex, i uint16, lines WriteableLines, vidx1, vidx2 int,
	firstSdef, secondSdef uint16, rootCs **ZExt_NodeSeg, lastCs **ZExt_NodeSeg, sidedefs []Sidedef,
	res *[]*ZExt_NodeSeg, extNode bool) {
	var lcs, rcs *ZExt_NodeSeg
	horizon := lines.IsHorizonEffect(i)
	action := lines.GetAction(i)
	bamEffect := lines.GetBAMEffect(i)
	preciousLine := lines.IsTaggedPrecious(i)

	if firstSdef != SIDEDEF_NONE {
		if !bytes.Equal(sidedefs[firstSdef].LoName[:], []byte("BSPNOSEG")) {

			if action == 1085 {
				Log.Verbose(1, "Will not create seg for front sidedef of linedef %d - instructed so by action's numeric code set to 1085 on linedef.\n",
					i)
			} else {
				sdef := &(sidedefs[firstSdef])
				lcs = ZExt_addSeg(myVertices, i, vidx1, vidx2, rootCs,
					sdef, lastCs, horizon, bamEffect,
					preciousLine && !lines.SectorIgnorePrecious(sdef.Sector))
				*res = append(*res, lcs)
			}
		} else {

			Log.Verbose(1, "Will not create seg for FRONT sidedef of linedef %d (because BSPNOSEG was used as lower texture)\n", i)
		}
	} else {
		Log.Printf("Warning: linedef %d doesn't have front sidedef\n", i)
	}
	if secondSdef != SIDEDEF_NONE {
		if !bytes.Equal(sidedefs[firstSdef].LoName[:], []byte("BSPNOSEG")) {

			if action == 1084 {
				Log.Verbose(1, "Will not create seg for BACK sidedef of linedef %d - instructed so by action's numeric code set to 1084 on linedef.\n",
					i)
			} else {
				sdef := &(sidedefs[secondSdef])
				rcs = ZExt_addSeg(myVertices, i, vidx2, vidx1, rootCs,
					sdef, lastCs, horizon, bamEffect,
					preciousLine && !lines.SectorIgnorePrecious(sdef.Sector))
				rcs.flags |= SEG_FLAG_FLIP
				*res = append(*res, rcs)
			}
		} else {

			Log.Verbose(1, "Will not create seg for back sidedef of linedef %d (because BSPNOSEG was used as lower texture)\n", i)
		}
	} else if uint16(lines.GetFlags(i))&LF_TWOSIDED != 0 {
		Log.Printf("Warning: linedef %d is marked as 2-sided but doesn't have back sidedef\n", i)
	}
	if extNode && (horizon || bamEffect.Action != BAM_NOSPECIAL) {

		Log.Verbose(1, "Linedef %d has BAM or horizon effect specified, but it cannot be supported in Zdoom nodes format and so will be ignored.\n",
			i)
	}
	if lcs != nil && rcs != nil {

		lcs.partner = rcs
		rcs.partner = lcs
	}
}

func ZExt_storeNodeVertex(vs []ZExt_NodeVertex, x, y int, idx uint32) {
	vs[idx] = ZExt_NodeVertex{
		X:   ZNumber(x),
		Y:   ZNumber(y),
		idx: idx,
	}
}

func ZExt_addSeg(vs []ZExt_NodeVertex, i uint16, vidx1, vidx2 int, rootCs **ZExt_NodeSeg, sdef *Sidedef, lastCs **ZExt_NodeSeg, horizon bool, bamEffect BAMEffect,
	precious bool) *ZExt_NodeSeg {
	s := new(ZExt_NodeSeg)
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
	s.len = ZNumber(flen)
	s.Offset = 0
	if precious {
		s.flags |= SEG_FLAG_PRECIOUS
	}

	if bamEffect.Action == BAM_REPLACE {

		s.Angle = bamEffect.Value
	} else {

		s.Angle = int16(ZExt_computeAngle(s.pdx, s.pdy))

		if bamEffect.Action == BAM_ADDITIVE {
			s.Angle += bamEffect.Value
		}
	}

	if horizon {
		s.Angle += int16(float64(sdef.XOffset) * float64(65536.0/360.0))
	}
	return s
}

func ZExt_restoreSegOrder(segs []*ZExt_NodeSeg) {
	sort.Sort(ZExt_InitialSegOrderByLinedef(segs))
	for i, x := range segs {
		if i < len(segs)-1 {
			x.next = segs[i+1]
		} else {
			x.next = nil
		}
	}

	for _, x := range segs {
		if x.partner != nil && x.next != x.partner && x.partner.next != x {
			x.partner = nil
			Log.Verbose(1, "restoreSegOrder: partner was not referencing a neighbor seg!\n")
		}
	}
}

type ZExt_InitialSegOrderByLinedef []*ZExt_NodeSeg

func (x ZExt_InitialSegOrderByLinedef) Len() int { return len(x) }
func (x ZExt_InitialSegOrderByLinedef) Less(i, j int) bool {

	return (x[i].Linedef < x[j].Linedef) ||
		(x[i].Linedef == x[j].Linedef && x[i].getFlip() < x[j].getFlip())
}
func (x ZExt_InitialSegOrderByLinedef) Swap(i, j int) { x[i], x[j] = x[j], x[i] }

func (ni *NodesInput) ZExt_applySectorEquivalence(allSegs []*ZExt_NodeSeg, sectorEquiv []uint16) {
	for i, _ := range allSegs {
		allSegs[i].secEquiv = sectorEquiv[allSegs[i].sector]
	}
}

func (w *ZExt_NodesWork) doInitialSuperblocks(rootBox *NodeBounds) *ZExt_Superblock {
	ret := w.newSuperblockNoProto()
	ret.nwlink = w
	ret.SetBounds(rootBox)
	for _, seg := range w.allSegs {
		ret.AddSegToSuper(seg)
	}
	return ret
}

func (w *ZExt_NodesWork) GetInitialStateClone() *ZExt_NodesWork {
	newW := new(ZExt_NodesWork)
	*newW = *w

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

	newW.vertices = make([]ZExt_NodeVertex, len(w.vertices))
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
	newW.incidental = make([]ZExt_IntVertexPairC, 0)

	newW.nonVoidCache = make(map[int]ZExt_NonVoidPerAlias)
	newW.blocksHit = make([]ZExt_BlocksHit, 0)
	if newW.zenScores != nil {
		newW.zenScores = make([]ZExt_DepthScoreBundle, 0, cap(w.zenScores))
	}

	newW.qallocSupers = nil
	newW.stkExtra = nil

	if w.parts != nil {
		newW.parts = make([]*ZExt_NodeSeg, 0)
	}

	if newW.zdoomVertexHeader != nil {
		newW.zdoomVertexHeader = new(ZdoomNode_VertexHeader)
		*newW.zdoomVertexHeader = *w.zdoomVertexHeader
	}

	newW.lines = w.lines.Clone()

	newW.allSegs = make([]*ZExt_NodeSeg, len(w.allSegs))
	for i := 0; i < len(newW.allSegs); i++ {
		newW.allSegs[i] = new(ZExt_NodeSeg)
		*(newW.allSegs[i]) = *(w.allSegs[i])

		newW.allSegs[i].nextInSuper = nil
		newW.allSegs[i].block = nil

		newW.allSegs[i].StartVertex = &(newW.vertices[w.allSegs[i].StartVertex.idx])
		newW.allSegs[i].EndVertex = &(newW.vertices[w.allSegs[i].EndVertex.idx])
	}

	for i := 0; i < len(newW.allSegs); i++ {
		if i < len(newW.allSegs)-1 {
			newW.allSegs[i].next = newW.allSegs[i+1]
		} else {
			newW.allSegs[i].next = nil
		}
		if w.allSegs[i].partner != nil {

			if i > 0 && w.allSegs[i].partner == w.allSegs[i-1] {
				newW.allSegs[i].partner = newW.allSegs[i-1]
			} else {
				newW.allSegs[i].partner = newW.allSegs[i+1]
			}
		}
	}

	return newW
}

type ZExt_VertexMap struct {
	w          *ZExt_NodesWork
	Grid       [][]*FloatVertex
	Snapshot   []int
	BlocksWide int
	BlocksTall int
	MinX, MinY float64
	MaxX, MaxY float64
}

func ZExt_CreateVertexMap(w *ZExt_NodesWork, minx, miny, maxx, maxy int) *ZExt_VertexMap {

	minx = minx - VMAP_SAFE_MARGIN
	miny = miny - VMAP_SAFE_MARGIN
	maxx = maxx + VMAP_SAFE_MARGIN
	maxy = maxy + VMAP_SAFE_MARGIN

	vm := &ZExt_VertexMap{
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

func (vm *ZExt_VertexMap) Clone() *ZExt_VertexMap {
	if vm == nil {
		return nil
	}
	newVm := &ZExt_VertexMap{}
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

func (vm *ZExt_VertexMap) GetBlock(x, y float64) int {

	ret := int(uint((x-vm.MinX)*FIXED16DOT16_MULTIPLIER)>>VMAP_BLOCK_SHIFT +
		(uint((y-vm.MinY)*FIXED16DOT16_MULTIPLIER)>>VMAP_BLOCK_SHIFT)*
			uint(vm.BlocksWide))
	if ret < 0 || ret >= len(vm.Grid) {
		vm.w.mlog.Verbose(1, "Vertex map index out of range, source values: x=%f, y=%f xmin,ymin=(%f,%f) xmax,ymax=(%f,%f)\n",
			x, y, vm.MinX, vm.MinY, vm.MaxX, vm.MaxY)

		return 0
	}
	return ret
}

func (vm *ZExt_VertexMap) SelectVertexExact(x, y float64, id int) *FloatVertex {
	block := &(vm.Grid[vm.GetBlock(x, y)])
	for _, it := range *block {
		if it.X == x && it.Y == y {
			return it
		}
	}
	return vm.insertVertex(x, y, id)
}

func (vm *ZExt_VertexMap) SelectVertexClose(x, y float64) *FloatVertex {
	block := &(vm.Grid[vm.GetBlock(x, y)])
	for _, it := range *block {
		if math.Abs(it.X-x) < VERTEX_EPSILON &&
			math.Abs(it.Y-y) < VERTEX_EPSILON {
			return it
		}
	}
	return vm.insertVertex(x, y, -1)
}

func (vm *ZExt_VertexMap) insertVertex(x, y float64, id int) *FloatVertex {

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

func (vm *ZExt_VertexMap) RestoreOrBeginSnapshot() {
	if vm.Snapshot == nil {

		vm.Snapshot = make([]int, len(vm.Grid))
		for i, it := range vm.Grid {
			vm.Snapshot[i] = len(it)
		}
	} else {

		for i, it := range vm.Grid {
			vm.Grid[i] = it[:vm.Snapshot[i]]
		}
	}
}

func ZExt_PopulateVertexMap(vm *ZExt_VertexMap, allSegs []*ZExt_NodeSeg) {
	for _, seg := range allSegs {
		vm.SelectVertexExact(float64(seg.psx), float64(seg.psy),
			int(seg.StartVertex.idx))
		vm.SelectVertexExact(float64(seg.pex), float64(seg.pey),
			int(seg.EndVertex.idx))
	}
}

func ZExt_PopulateVertexMapFromLines(vm *ZExt_VertexMap, lines AbstractLines) {
	l := int(lines.Len())
	for i := 0; i < l; i++ {
		x1, x2, y1, y2 := lines.GetAllXY(uint16(i))
		vm.SelectVertexExact(float64(x1), float64(y1), i)
		vm.SelectVertexExact(float64(x2), float64(y2), i)
	}
}

func ZExt_PopulateVertexCache(cache map[SimpleVertex]int, allSegs []*ZExt_NodeSeg) {
	for _, it := range allSegs {
		rec := SimpleVertex{int(it.StartVertex.X), int(it.StartVertex.Y)}
		cache[rec] = int(it.StartVertex.idx)
		rec = SimpleVertex{int(it.EndVertex.X), int(it.EndVertex.Y)}
		cache[rec] = int(it.EndVertex.idx)
	}
}

func (w *ZExt_NodesWork) emptyNodesLumps() {
	w.deepNodes = nil
	w.deepSegs = nil
	w.deepSubsectors = nil
	w.nodes = nil
	w.segs = nil
	w.subsectors = nil
}

func (w *ZExt_NodesWork) lastSubsectorOverflows(maxSegIndex uint16) bool {
	l := len(w.subsectors)
	firstSeg := int(w.subsectors[l-1].FirstSeg)
	segCnt := int(w.subsectors[l-1].SegCount)
	if len(w.segs) > (firstSeg + segCnt) {

		return true
	}
	if firstSeg > int(maxSegIndex) {
		return true
	}
	return false
}

func (w *ZExt_NodesWork) fitSegsToTarget(maxSegIndex uint16, dryRun bool) (bool, int) {
	newMaxSegIndex := uint32(len(w.segs)) - w.totals.maxSegCountInSubsector
	if newMaxSegIndex > uint32(maxSegIndex) {

		return false, 0
	}

	if dryRun {

		return true, -1
	}

	biggestSubsector := -1
	for i := len(w.subsectors) - 1; i >= 0; i-- {
		if uint32(w.subsectors[i].SegCount) == w.totals.maxSegCountInSubsector {
			biggestSubsector = i
		}
	}
	if biggestSubsector == -1 {
		Log.Panic("Couldn't find subsector whose seg count matches computed maximum. (programmer error)\n")
	}

	newAddr := uint16(0)
	if biggestSubsector > 0 {
		newAddr = w.subsectors[biggestSubsector-1].FirstSeg +
			w.subsectors[biggestSubsector-1].SegCount
	}

	pivot := w.subsectors[biggestSubsector].FirstSeg
	pivot2 := pivot + w.subsectors[biggestSubsector].SegCount

	w.segs = append(w.segs[:pivot], append(w.segs[pivot2:], w.segs[pivot:pivot2]...)...)
	for i := biggestSubsector + 1; i < len(w.subsectors); i++ {
		w.subsectors[i].FirstSeg = newAddr
		newAddr += w.subsectors[i].SegCount
	}
	w.subsectors[biggestSubsector].FirstSeg = uint16(newMaxSegIndex)
	return true, biggestSubsector
}

func (w *ZExt_NodesWork) reverseDeepNodes(node *NodeInProcess) uint32 {
	if w.deepNodes == nil {
		w.deepNodes = make([]DeepNode, w.totals.numNodes)
		w.nreverse = 0
	}
	if config.StraightNodes {

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

func (w *ZExt_NodesWork) convertDeepNodesStraight(node *NodeInProcess, idx uint32) uint32 {
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

type ZExt_SegMinorBundle struct {
	seg   *ZExt_NodeSeg
	minor MinorCosts
}

func ZExt_PickNode_traditional(w *ZExt_NodesWork, ts *ZExt_NodeSeg, bbox *NodeBounds,
	super *ZExt_Superblock) *ZExt_NodeSeg {
	best := ts
	bestcost := int(INITIAL_BIG_COST)
	bestMinors := MinorCosts{
		PreciousSplit: int(INITIAL_BIG_COST),
	}
	cnt := 0
	if w.parts != nil {
		w.parts = w.parts[:0]
	}

	for part := ts; part != nil; part = part.next {
		cnt++
	}
	var previousPart *ZExt_NodeSeg

	w.segAliasObj.UnvisitAll()

	for part := ts; part != nil; part = part.next {
		if part.partner != nil && part.partner == previousPart {

			continue
		}
		if part.alias != 0 {
			if w.segAliasObj.MarkAndRecall(part.alias) {

				continue
			}
		} else {

			part.alias = w.segAliasObj.Generate()

		}
		previousPart = part
		cost := 0
		tot := 0
		diff := cnt
		minors := MinorCosts{
			PreciousSplit: 0,
		}

		if w.diagonalPenalty != 0 && part.pdx != 0 && part.pdy != 0 {
			cost += w.diagonalPenalty
		}

		prune := w.evalPartitionWorker_Traditional(super, part, &tot, &diff,
			&cost, bestcost, &minors)
		if prune {
			continue
		}

		diff -= tot
		if diff < 0 {
			diff = -diff
		}

		if (tot + cnt) > diff {

			cost += diff
			if w.parts == nil {
				if cost < bestcost || (cost == bestcost &&
					minorIsBetter_Precious(minors, bestMinors)) {

					bestcost = cost
					best = part
					bestMinors = minors
				}
			} else {
				strictlyBetter := false
				if cost < bestcost {
					strictlyBetter = true
				} else if cost == bestcost {
					if minorIsBetter_Precious(minors, bestMinors) {
						strictlyBetter = true
					} else if !minorIsBetter_Precious(bestMinors, minors) {

						w.parts = append(w.parts, part)
					}
				}

				if strictlyBetter {
					bestcost = cost
					best = part
					bestMinors = minors
					w.parts = w.parts[:0]
					w.parts = append(w.parts, part)
				}
			}
		}

	}

	return best
}

func (w *ZExt_NodesWork) evalPartitionWorker_Traditional(block *ZExt_Superblock, part *ZExt_NodeSeg, tot, diff, cost *int, bestcost int, minors *MinorCosts) bool {
forblock:

	num := ZExt_BoxOnLineSide(block, part)
	if num < 0 {

		*diff -= 2 * block.realNum
		return false
	} else if num > 0 {

		return false
	}

	for check := block.segs; check != nil; check = check.nextInSuper {

		leftside := false
		a := part.pdy*check.psx - part.pdx*check.psy + part.perp
		b := part.pdy*check.pex - part.pdx*check.pey + part.perp
		if ZDiffSign(a, b) {
			if (a != 0) && (b != 0) {
				if w.PassingTooClose(part, check, cost, minors) {
					return true
				}

				l := check.len

				d := ZNumber((ZWideNumber(l) * ZWideNumber(a)) / (ZWideNumber(a) - ZWideNumber(b)))
				if d >= 2 {

					if check.flags&SEG_FLAG_PRECIOUS != 0 {

						if part.pdx != 0 && part.pdy != 0 {
							*cost += w.pickNodeFactor * PRECIOUS_MULTIPLY * 2
						} else {
							*cost += w.pickNodeFactor * PRECIOUS_MULTIPLY
						}
						minors.PreciousSplit++
					}

					*cost += w.pickNodeFactor
					if *cost > bestcost {

						return true
					}
					(*tot)++
				} else {
					if ZExt_checkPorn1(l, d, check.pdx, part.pdx, check.pdy, part.pdy, b) {
						leftside = true
					}
				}
			} else {

				if (check.flags&SEG_FLAG_PRECIOUS != 0) &&
					!w.PartIsPolyobjSide(part, check) {
					if part.pdx != 0 && part.pdy != 0 {
						*cost += w.pickNodeFactor * PRECIOUS_MULTIPLY * 2
					} else {
						*cost += w.pickNodeFactor * PRECIOUS_MULTIPLY
					}
					minors.PreciousSplit++
				}
				leftside = true
			}
		} else if a <= 0 {
			if a != 0 {
				leftside = true
			} else if b == 0 {

				check.alias = part.alias
				if check.pdx*part.pdx+check.pdy*part.pdy < 0 {
					leftside = true
				}
			}
		}
		if leftside {
			*diff -= 2
		}
	}

	if block.subs[0] != nil {
		if w.evalPartitionWorker_Traditional(block.subs[0], part, tot, diff,
			cost, bestcost, minors) {
			return true
		}
	}

	if block.subs[1] != nil {
		block = block.subs[1]
		goto forblock
	}

	return false
}

func ZExt_checkPorn1(l, d, cpdx, ppdx, cpdy, ppdy, b ZNumber) bool {
	if (l - d) < 2 {
		return cpdx*ppdx+cpdy*ppdy < 0
	} else {
		return b < 0
	}
}

func ZExt_PickNode_visplaneKillough(w *ZExt_NodesWork, ts *ZExt_NodeSeg, bbox *NodeBounds,
	super *ZExt_Superblock) *ZExt_NodeSeg {
	best := ts
	bestcost := int(INITIAL_BIG_COST)
	bestMinors := MinorCosts{
		PreciousSplit: int(INITIAL_BIG_COST),
	}
	cnt := 0
	if w.parts != nil {
		w.parts = w.parts[:0]
	}

	for part := ts; part != nil; part = part.next {
		cnt++
	}

	var previousPart *ZExt_NodeSeg

	w.segAliasObj.UnvisitAll()

	for part := ts; part != nil; part = part.next {
		if part.partner != nil && part.partner == previousPart {

			continue
		}
		if part.alias != 0 {
			if w.segAliasObj.MarkAndRecall(part.alias) {

				continue
			}
		} else {

			part.alias = w.segAliasObj.Generate()

		}
		previousPart = part
		cost := 0
		tot := 0
		slen := ZNumber(0)
		diff := cnt
		minors := MinorCosts{
			PreciousSplit: 0,
		}

		w.sectorHits[0] = 0
		sectorCount := len(w.sectorHits)
		for j := 1; j < sectorCount; j = j << 1 {
			copy(w.sectorHits[j:], w.sectorHits[:j])
		}

		w.blocksHit = w.blocksHit[:0]
		prune := w.evalPartitionWorker_VisplaneKillough(super, part, &tot, &diff,
			&cost, bestcost, &slen, &minors)
		if prune {
			continue
		}

		diff -= tot
		if diff < 0 {
			diff = -diff
		}

		if tot+cnt <= diff {
			continue
		}

		for _, hitRecord := range w.blocksHit {
			hitRecord.block.MarkSectorsHit(w.sectorHits, hitRecord.mask)
		}

		diff = 0
		tot = 0
		flat := 0
		for ; tot < len(w.sectorHits); tot++ {
			switch w.sectorHits[tot] {
			case 1:
				{
					diff++
				}
			case 2:
				{
					diff--

				}
			}
			if w.sectorHits[tot] != 0 {
				flat++
			}
		}

		if diff < 0 {
			diff = -diff
		}

		if flat > 1 && w.diagonalPenalty != 0 && (part.pdx != 0 && part.pdy != 0) {
			cost += w.diagonalPenalty
		}
		cost += diff
		if cost > bestcost || (cost == bestcost &&
			w.parts == nil && !minorIsBetter_Precious(minors, bestMinors)) {
			continue
		}

		l := ZExt_GetPartitionLength_LegacyWay(part, bbox)

		if slen < l {
			cost += w.pickNodeFactor
			if cost > bestcost || (cost == bestcost &&
				w.parts == nil && !minorIsBetter_Precious(minors, bestMinors)) {
				continue
			}
		}

		if w.parts == nil {

			bestcost = cost
			best = part
			bestMinors = minors

		} else {

			strictlyBetter := false
			if cost < bestcost {
				strictlyBetter = true
			} else {
				if minorIsBetter_Precious(minors, bestMinors) {

					strictlyBetter = true
				} else if !minorIsBetter_Precious(bestMinors, minors) {

					w.parts = append(w.parts, part)
				}
			}
			if strictlyBetter {
				bestcost = cost
				best = part
				bestMinors = minors
				w.parts = w.parts[:0]
				w.parts = append(w.parts, part)
			}

		}
	}
	return best
}

func (w *ZExt_NodesWork) evalPartitionWorker_VisplaneKillough(block *ZExt_Superblock, part *ZExt_NodeSeg, tot, diff, cost *int, bestcost int, slen *ZNumber,
	minors *MinorCosts) bool {

forblock:

	num := ZExt_BoxOnLineSide(block, part)
	if num < 0 {

		*diff -= 2 * block.realNum
		w.blocksHit = append(w.blocksHit, ZExt_BlocksHit{
			block: block,
			mask:  uint8(1),
		})
		return false
	} else if num > 0 {

		w.blocksHit = append(w.blocksHit, ZExt_BlocksHit{
			block: block,
			mask:  uint8(2),
		})
		return false
	}

	for check := block.segs; check != nil; check = check.nextInSuper {

		leftside := false
		a := part.pdy*check.psx - part.pdx*check.psy + part.perp
		b := part.pdy*check.pex - part.pdx*check.pey + part.perp
		mask := uint8(2)
		if ZDiffSign(a, b) {
			if (a != 0) && (b != 0) {
				if w.PassingTooClose(part, check, cost, minors) {
					return true
				}

				l := check.len

				d := ZNumber((ZWideNumber(l) * ZWideNumber(a)) / (ZWideNumber(a) - ZWideNumber(b)))
				if d >= 2 {

					if check.flags&SEG_FLAG_PRECIOUS != 0 {

						if part.pdx != 0 && part.pdy != 0 {
							*cost += w.pickNodeFactor * PRECIOUS_MULTIPLY * 2
						} else {
							*cost += w.pickNodeFactor * PRECIOUS_MULTIPLY
						}
						minors.PreciousSplit++
					}

					*cost += w.pickNodeFactor
					if *cost > bestcost {

						return true
					}
					(*tot)++
					mask = uint8(4)
				} else if ZExt_checkPorn1(l, d, check.pdx, part.pdx, check.pdy, part.pdy, b) {
					leftside = true
				}
			} else {

				if (check.flags&SEG_FLAG_PRECIOUS != 0) &&
					!w.PartIsPolyobjSide(part, check) {
					if part.pdx != 0 && part.pdy != 0 {
						*cost += w.pickNodeFactor * PRECIOUS_MULTIPLY * 2
					} else {
						*cost += w.pickNodeFactor * PRECIOUS_MULTIPLY
					}
					minors.PreciousSplit++
				}
				leftside = true
			}
		} else if a <= 0 {
			if a != 0 {
				leftside = true
			} else if b == 0 {

				check.alias = part.alias
				*slen += check.len
				if check.pdx*part.pdx+check.pdy*part.pdy < 0 {
					leftside = true
				}
			}
		}
		if leftside {
			*diff -= 2
			mask = uint8(1)
		}
		w.sectorHits[check.sector] |= mask
	}

	if block.subs[0] != nil {
		if w.evalPartitionWorker_VisplaneKillough(block.subs[0], part, tot, diff,
			cost, bestcost, slen, minors) {
			return true
		}
	}

	if block.subs[1] != nil {
		block = block.subs[1]
		goto forblock
	}

	return false
}

func ZExt_PickNode_visplaneVigilant(w *ZExt_NodesWork, ts *ZExt_NodeSeg, bbox *NodeBounds,
	super *ZExt_Superblock) *ZExt_NodeSeg {
	best := ts
	var bestFallback *ZExt_NodeSeg
	executed := false
	bestcost := int(INITIAL_BIG_COST)
	bestMinors := MinorCosts{
		PreciousSplit: int(INITIAL_BIG_COST),
		SegsSplit:     int(INITIAL_BIG_COST),
		SectorsSplit:  int(INITIAL_BIG_COST),
		Unmerged:      int(INITIAL_BIG_COST),
	}
	var parts []ZExt_SegMinorBundle
	if w.multipart {
		parts = make([]ZExt_SegMinorBundle, 0)
	}
	cnt := 0
	if w.parts != nil {
		w.parts = w.parts[:0]

	}

	for part := ts; part != nil; part = part.next {
		cnt++
	}

	var previousPart *ZExt_NodeSeg

	w.segAliasObj.UnvisitAll()

	for part := ts; part != nil; part = part.next {
		if part.partner != nil && part.partner == previousPart {

			continue
		}
		if part.alias != 0 {
			if w.segAliasObj.MarkAndRecall(part.alias) {

				continue
			}
		} else {

			part.alias = w.segAliasObj.Generate()

		}
		previousPart = part
		cost := 0
		minors := MinorCosts{
			PreciousSplit: 0,
			SegsSplit:     0,
			SectorsSplit:  0,
			Unmerged:      0,
		}
		tot := 0
		slen := ZNumber(0)
		diff := cnt

		w.sectorHits[0] = 0
		hitArrayLen := len(w.sectorHits)
		for j := 1; j < hitArrayLen; j = j << 1 {
			copy(w.sectorHits[j:], w.sectorHits[:j])
		}

		w.incidental = w.incidental[:0]

		w.blocksHit = w.blocksHit[:0]
		hasLeft := false
		prune := w.evalPartitionWorker_VisplaneVigilant(super, part, &tot, &diff,
			&cost, bestcost, &slen, &hasLeft, &minors)
		if prune {
			continue
		}

		if bestFallback == nil {
			bestFallback = part
		}

		diff -= tot
		if diff < 0 {
			diff = -diff
		}

		if tot+cnt <= diff {
			continue
		}

		for _, hitRecord := range w.blocksHit {
			hitRecord.block.MarkSecEquivsHit(w.sectorHits, hitRecord.mask)
		}

		diff = 0
		tot = 0

		flat := 0

		unmerged := 0
		for ; tot < len(w.sectorHits); tot++ {
			switch w.sectorHits[tot] {
			case 1:
				{
					diff++
				}
			case 2:
				{
					diff--
				}
			}
			if w.sectorHits[tot] >= 3 {
				minors.SectorsSplit++
			}
			if w.sectorHits[tot] >= 4 {
				unmerged++
			}
			if w.sectorHits[tot] != 0 {
				flat++
			}
		}

		if diff < 0 {
			diff = -diff
		}

		if unmerged > 0 {

			unmerged--
		}

		cost += diff
		if config.PenalizeSectorSplits && !w.multipart {

			cost += unmerged * w.pickNodeFactor
		} else {

			minors.Unmerged = unmerged
		}
		if (cost > bestcost) || (cost == bestcost && w.parts == nil &&
			!w.minorIsBetter(minors, bestMinors)) {
			continue
		}

		if flat > 1 {

			if w.diagonalPenalty != 0 && (part.pdx != 0 && part.pdy != 0) {
				cost += w.diagonalPenalty
				if (cost > bestcost) || (cost == bestcost && w.parts == nil &&
					!w.minorIsBetter(minors, bestMinors)) {
					continue
				}
			}

			l := w.GetPartitionLength_VigilantWay(part, bbox)
			if l == 0 {

				l = ZExt_GetFullPartitionLength(w, part, bbox)
				w.mlog.Verbose(2, "Recomputing partition length the old way, because I got zero length doing it the new way.\n")
			}

			collinearSegs := ZExt_IntCollinearVertexPairCByCoord(w.incidental)

			sort.Sort(collinearSegs)

			slen = slen - collinearSegs.GetOverlapsLength()
			if slen < 0 {

				w.mlog.Verbose(2, "Oops, got negative length after removing overlaps! Must have overflowed somewhere.\n")
				slen = 0
			}

			l = l / 2

			if slen < l {
				cost += w.pickNodeFactor
				if (cost > bestcost) || (cost == bestcost && w.parts == nil &&
					!w.minorIsBetter(minors, bestMinors)) {
					continue
				}
			}
		}

		if !hasLeft && w.VigilantGuard_IsBadPartition(part, ts, cnt) {
			cost += w.pickNodeFactor * ONESIDED_MULTIPLY
			if (cost > bestcost) || (cost == bestcost && w.parts == nil &&
				!w.minorIsBetter(minors, bestMinors)) {
				continue
			}
		}

		if w.parts == nil || parts != nil {

			if parts != nil {
				if bestcost != cost {

					parts = parts[:0]
				}
				parts = append(parts, ZExt_SegMinorBundle{
					seg:   part,
					minor: minors,
				})
			}
			bestcost = cost
			bestMinors = minors
			best = part
			executed = true

		} else {
			strictlyBetter := false
			if cost < bestcost {
				strictlyBetter = true
			} else {
				if w.minorIsBetter(minors, bestMinors) {
					strictlyBetter = true
				} else if !w.minorIsBetter(bestMinors, minors) {

					if minors.SectorsSplit > 0 || minors.SegsSplit > 0 {
						w.parts = append(w.parts, part)
					}
				}
			}
			if strictlyBetter {
				bestcost = cost
				bestMinors = minors
				best = part
				executed = true
				w.parts = w.parts[:0]
				w.parts = append(w.parts, part)
			}
		}
	}
	w.incidental = w.incidental[:0]
	if len(parts) > 1 {

		depthScores := ZExt_ZenSegMinorToDepthScores(parts)
		newSectorHits := make([]uint8, len(w.sectors))
		w.ZenComputeScores(super, depthScores, newSectorHits, w.depthArtifacts)
		ZExt_ZenPickBestScore(depthScores)
		if depthScores[0].scoreSeg != VERY_BAD_SCORE {
			best = depthScores[0].seg
			if w.parts != nil {
				w.parts = append(w.parts, best)
			}
			if w.parts != nil || config.VerbosityLevel >= 4 {

				tst1, tst2, tst3, tst4 := depthScores[0].preciousSplit,
					depthScores[0].scoreTotal,
					depthScores[0].equivSplit,
					depthScores[0].segSplit
				if tst3 > 0 || tst4 > 0 {

					track := 1
					for i := 1; i < len(depthScores); i++ {
						if tst1 == depthScores[i].preciousSplit &&
							tst2 == depthScores[i].scoreTotal &&
							tst3 == depthScores[i].equivSplit &&
							tst4 == depthScores[i].segSplit {
							if w.parts != nil {
								w.parts = append(w.parts, depthScores[i].seg)
							}
							track++
						} else {
							break
						}
					}
					if track > 1 {
						w.mlog.Verbose(4, "ZEN Ambiguity equal rank for %d records \n", track)

					}
				}
			}
		}
	}
	if !executed && bestFallback != nil {

		best = bestFallback
	}
	return best
}

func (w *ZExt_NodesWork) evalPartitionWorker_VisplaneVigilant(block *ZExt_Superblock, part *ZExt_NodeSeg, tot, diff, cost *int, bestcost int, slen *ZNumber,
	hasLeft *bool, minors *MinorCosts) bool {

forblock:

	num := ZExt_BoxOnLineSide(block, part)
	if num < 0 {

		*hasLeft = true
		*diff -= 2 * block.realNum
		w.blocksHit = append(w.blocksHit, ZExt_BlocksHit{
			block: block,
			mask:  uint8(1),
		})
		return false
	} else if num > 0 {

		w.blocksHit = append(w.blocksHit, ZExt_BlocksHit{
			block: block,
			mask:  uint8(2),
		})
		return false
	}

	for check := block.segs; check != nil; check = check.nextInSuper {

		leftside := false
		mask := uint8(2)
		a := part.pdy*check.psx - part.pdx*check.psy + part.perp
		b := part.pdy*check.pex - part.pdx*check.pey + part.perp
		if ZDiffSign(a, b) {
			if (a != 0) && (b != 0) {
				if w.PassingTooClose(part, check, cost, minors) {
					return true
				}

				l := check.len

				d := ZNumber((ZWideNumber(l) * ZWideNumber(a)) / (ZWideNumber(a) - ZWideNumber(b)))
				if d >= 2 {

					if check.flags&SEG_FLAG_PRECIOUS != 0 {

						if part.pdx != 0 && part.pdy != 0 {
							*cost += w.pickNodeFactor * PRECIOUS_MULTIPLY * 2
						} else {
							*cost += w.pickNodeFactor * PRECIOUS_MULTIPLY
						}
						minors.PreciousSplit++
					}

					*cost += w.pickNodeFactor
					if *cost > bestcost {

						return true
					}
					(*tot)++
					minors.SegsSplit++
					mask = uint8(4)
				} else {
					if ZExt_checkPorn1(l, d, check.pdx, part.pdx, check.pdy, part.pdy, b) {
						leftside = true
					}
				}
			} else {

				if (check.flags&SEG_FLAG_PRECIOUS != 0) &&
					!w.PartIsPolyobjSide(part, check) {
					if part.pdx != 0 && part.pdy != 0 {
						*cost += w.pickNodeFactor * PRECIOUS_MULTIPLY * 2
					} else {
						*cost += w.pickNodeFactor * PRECIOUS_MULTIPLY
					}
					minors.PreciousSplit++
				}
				leftside = true
			}
		} else if a <= 0 {
			if a != 0 {
				leftside = true
			} else if b == 0 {

				if check.alias != part.alias && ZExt_vetAliasTransfer2(part, check) {
					check.alias = part.alias
				}
				*slen += check.len

				w.incidental = append(w.incidental, check.toIntVertexPairC())
				if check.pdx*part.pdx+check.pdy*part.pdy < 0 {
					leftside = true
				}
			}
		}

		if leftside {
			*diff -= 2
			mask = uint8(1)
		}

		w.sectorHits[check.secEquiv] |= mask
	}

	if block.subs[0] != nil {
		if w.evalPartitionWorker_VisplaneVigilant(block.subs[0], part, tot, diff,
			cost, bestcost, slen, hasLeft, minors) {
			return true
		}
	}

	if block.subs[1] != nil {
		block = block.subs[1]
		goto forblock
	}

	return false
}

func (w *ZExt_NodesWork) VigilantGuard_IsBadPartition(part, ts *ZExt_NodeSeg, cnt int) bool {

	c := &ZExt_IntersectionContext{
		psx: part.psx,
		psy: part.psy,
		pex: part.pex,
		pey: part.pey,
	}
	c.pdx = c.psx - c.pex
	c.pdy = c.psy - c.pey
	tot := 0
	diff := cnt

	for check := ts; check != nil; check = check.next {

		leftside := false
		c.lsx = check.psx
		c.lsy = check.psy
		c.lex = check.pex
		c.ley = check.pey
		val := w.doLinesIntersect(c)
		if ((val&2 != 0) && (val&64 != 0)) || ((val&4 != 0) && (val&32 != 0)) {
			tot++
			return false
		} else {
			if check == part || check == part.partner {
				leftside = check == part.partner
				if leftside {
					return false
				}
			} else {
				if val&34 != 0 {

					leftside = true
					return false
				}
				if (val&1 != 0) && (val&16 != 0) {
					if check.pdx*part.pdx+check.pdy*part.pdy < 0 {
						leftside = true
						return false
					}
				}
			}
		}
		if leftside {
			diff -= 2
			return false
		}
	}

	diff -= tot
	if diff < 0 {
		diff = -diff
	}

	return tot+cnt <= diff
}

func ZExt_GetPartitionLength_LegacyWay(part *ZExt_NodeSeg, bbox *NodeBounds) ZNumber {
	var l ZNumber
	if part.pdx == 0 {
		l = ZNumber(bbox.Ymax - bbox.Ymin)
	} else if part.pdy == 0 {
		l = ZNumber(bbox.Xmax - bbox.Xmin)
	} else {
		t1 := (float64(part.psx) - float64(bbox.Xmax)) / float64(part.pdx)
		t2 := (float64(part.psx) - float64(bbox.Xmin)) / float64(part.pdx)
		t3 := (float64(part.psy) - float64(bbox.Ymax)) / float64(part.pdy)
		t4 := (float64(part.psy) - float64(bbox.Ymin)) / float64(part.pdy)
		if part.pdx > 0 {
			t1, t2 = t2, t1
		}
		if part.pdy > 0 {
			t3, t4 = t4, t3
		}
		if t1 > t3 {
			t1 = t3
		}
		if t2 < t4 {
			t2 = t4
		}
		l = ZNumber((t1 - t2) * float64(part.len))
	}
	return l
}

func ZExt_GetFullPartitionLength(w *ZExt_NodesWork, part *ZExt_NodeSeg, bbox *NodeBounds) ZNumber {
	var l ZNumber
	if part.pdx == 0 {
		l = ZNumber(bbox.Ymax - bbox.Ymin)
	} else if part.pdy == 0 {
		l = ZNumber(bbox.Xmax - bbox.Xmin)
	} else {
		var c FloatIntersectionContext
		partSegCoords := part.toVertexPairC()
		c.psx = partSegCoords.StartVertex.X
		c.psy = partSegCoords.StartVertex.Y
		c.pex = partSegCoords.EndVertex.X
		c.pey = partSegCoords.EndVertex.Y
		c.pdx = c.pex - c.psx
		c.pdy = c.pey - c.psy
		contextStart, contextEnd, _ := ZExt_PartitionInBoundary(w, part, &c, bbox.Xmax,
			bbox.Ymax, bbox.Xmin, bbox.Ymin, partSegCoords)
		if contextStart == nil {

			return ZExt_GetPartitionLength_LegacyWay(part, bbox)
		}
		fullXDiff := contextStart.X - contextEnd.X
		fullYDiff := contextEnd.Y - contextStart.Y
		l = ZNumber(math.Round(math.Sqrt(fullXDiff*fullXDiff + fullYDiff*fullYDiff)))
	}
	return l
}

func (w *ZExt_NodesWork) GetPartitionLength_VigilantWay(part *ZExt_NodeSeg, bbox *NodeBounds) ZNumber {

	if part.alias == 0 {

		w.mlog.Verbose(2, "What? Alias should not be zero here. Falling back to old way of computing partition length. (Programmer error)")
		return ZExt_GetFullPartitionLength(w, part, bbox)
	}
	nonVoidStruc, ok := w.nonVoidCache[part.alias]
	if !ok {

		nonVoidStruc = w.ComputeNonVoid(part)
		w.nonVoidCache[part.alias] = nonVoidStruc
	}
	if !nonVoidStruc.success {

		return ZExt_GetFullPartitionLength(w, part, bbox)
	}

	contextStart, contextEnd, _ := ZExt_PartitionInBoundary(w, part,
		&(nonVoidStruc.c), bbox.Xmax, bbox.Ymax, bbox.Xmin, bbox.Ymin,
		nonVoidStruc.partSegCoords)
	if contextStart == nil || contextEnd == nil {

		w.mlog.Verbose(2, "This cannot be! Got so far but now failing (got all the segments of line on the map to see when it goes through the void and when it does not, but failed to determine the edges of line touching the current node's bounding box)\n")
		return ZExt_GetFullPartitionLength(w, part, bbox)
	}

	if contextStart.equalToWithEpsilon(contextEnd) {
		w.mlog.Verbose(2, "Partition line seems to have zero length inside the node box (%v,%v)-(%v,%v) in (%v,%v,%v,%v) yielded (%v,%v)-(%v,%v).\n",
			part.psx, part.psy, part.pex, part.pey,
			bbox.Xmax, bbox.Ymax, bbox.Xmin, bbox.Ymin,
			contextStart.X, contextStart.Y, contextEnd.X, contextEnd.Y)
		return 0
	}

	nonVoid := nonVoidStruc.data

	hitStart := -1
	hitStop := -1

	for i := 0; i < len(nonVoid); i++ {

		if AreOverlapping(contextStart, contextEnd, nonVoid[i].StartVertex,
			nonVoid[i].EndVertex) {
			hitStart = i
			break
		}
	}
	for i := len(nonVoid) - 1; i >= 0; i-- {

		if AreOverlapping(contextStart, contextEnd, nonVoid[i].StartVertex,
			nonVoid[i].EndVertex) {
			hitStop = i
			break
		}
	}
	if hitStart < 0 || hitStop < 0 {

		w.mlog.Verbose(2, "to below: %s", nonVoidStruc.original.toString())
		w.mlog.Verbose(2, "More dropouts! %v %v %s\n  ...... %d [%s-%s]\n",
			hitStart, hitStop,
			ZExt_CollinearVertexPairCByCoord(nonVoid).toString(), part.Linedef,
			contextStart.toString(), contextEnd.toString())
		return ZExt_GetFullPartitionLength(w, part, bbox)
	}

	L := ZNumber(0)
	for i := hitStart; i <= hitStop; i++ {
		thisStart := nonVoid[i].StartVertex
		thisEnd := nonVoid[i].EndVertex

		precomputedInvalid := false
		if i == hitStart {
			if !VertexPairCOrdering(contextStart, nonVoid[i].StartVertex, false) {
				thisStart = contextStart
			}
			precomputedInvalid = true
		}
		if i == hitStop {
			if VertexPairCOrdering(contextEnd, nonVoid[i].EndVertex, false) {
				thisEnd = contextEnd
			}
			precomputedInvalid = true
		}
		if !precomputedInvalid {
			L = L + nonVoid[i].len
			if nonVoid[i].len == 0 {
				w.mlog.Verbose(2, "non-void interval computed with zero length (precomputed)\n")
			} else if nonVoid[i].len < 0 {
				w.mlog.Verbose(2, "what? non-void interval computed with NEGATIVE length (precomputed)\n")
			}
		} else {
			dx := thisEnd.X - thisStart.X
			dy := thisEnd.Y - thisStart.Y

			l0 := ZNumber(math.Sqrt(dx*dx + dy*dy))
			L = L + l0
			if l0 == 0 {

			} else if l0 < 0 {
				w.mlog.Verbose(2, "what? sqrt yieled NEGATIVE value after cast?\n")
			}
		}
	}
	if L == 0 {
		w.mlog.Verbose(2, "returning 0 (sad)\n ........... %d %s ........ range (%d,%d) - (%d, %d) hit: %d,%d \n",
			part.Linedef, ZExt_CollinearVertexPairCByCoord(nonVoid).toString(),
			bbox.Xmin, bbox.Ymin, bbox.Xmax, bbox.Ymax,
			hitStart, hitStop)
	}
	return L
}

func ZExt_subPickNode_fast(w *ZExt_NodesWork, ts *ZExt_NodeSeg, bbox *NodeBounds,
	super *ZExt_Superblock, cnt int) *ZExt_NodeSeg {
	var previousPart *ZExt_NodeSeg
	var bestH, bestV *ZExt_NodeSeg
	oldDistH := ZNumber(-1)
	oldDistV := ZNumber(-1)
	if w.parts != nil {
		w.parts = w.parts[:0]
	}

	midX := (bbox.Xmax + bbox.Xmin) >> 1
	midY := (bbox.Ymax + bbox.Ymin) >> 1

	w.segAliasObj.UnvisitAll()

	for part := ts; part != nil; part = part.next {
		if part.partner != nil && part.partner == previousPart {
			continue
		}
		if part.alias != 0 {
			if w.segAliasObj.MarkAndRecall(part.alias) {
				continue
			}
		}
		previousPart = part
		if part.pdy == 0 {
			if bestH == nil {
				bestH = part
				oldDistH = (part.psy - ZNumber(midY)).Abs()
			} else {
				newDistH := (part.psy - ZNumber(midY)).Abs()
				if newDistH < oldDistH {
					bestH = part
					oldDistH = newDistH
				}
			}
		} else if part.pdx == 0 {
			if bestV == nil {
				bestV = part
				oldDistV = (part.psx - ZNumber(midX)).Abs()
			} else {
				newDistV := (part.psx - ZNumber(midX)).Abs()
				if newDistV < oldDistV {
					bestV = part
					oldDistV = newDistV
				}
			}
		}
	}

	if bestH == nil && bestV == nil {
		return nil
	}

	var best *ZExt_NodeSeg
	bestcost := int(INITIAL_BIG_COST)

	for i := 0; i <= 1; i++ {
		var part *ZExt_NodeSeg
		if i == 0 {
			part = bestH
		} else {
			part = bestV
		}
		if part == nil {
			continue
		}
		cost := 0
		tot := 0
		diff := cnt
		prune := w.evalPartitionWorker_Maelstrom(super, part, &tot, &diff,
			&cost, bestcost)
		if prune {
			continue
		}

		diff -= tot
		if diff < 0 {
			diff = -diff
		}

		if (tot + cnt) > diff {
			cost += diff
			if cost < bestcost {

				bestcost = cost
				best = part
				if w.parts != nil {
					w.parts = w.parts[:0]
					w.parts = append(w.parts, part)
				}
			} else if cost == bestcost {
				if w.parts != nil {
					w.parts = append(w.parts, part)
				}
			}
		}
	}
	return best
}

func ZExt_PickNode_maelstrom(w *ZExt_NodesWork, ts *ZExt_NodeSeg, bbox *NodeBounds,
	super *ZExt_Superblock) *ZExt_NodeSeg {
	cnt := 0
	for part := ts; part != nil; part = part.next {
		cnt++
	}
	if cnt >= SEG_FAST_THRESHHOLD {
		best := ZExt_subPickNode_fast(w, ts, bbox, super, cnt)
		if best != nil {
			return best
		}
	}

	return ZExt_PickNode_traditional(w, ts, bbox, super)
}

func (w *ZExt_NodesWork) evalPartitionWorker_Maelstrom(block *ZExt_Superblock, part *ZExt_NodeSeg, tot, diff, cost *int, bestcost int) bool {

forblock:

	num := ZExt_BoxOnLineSide(block, part)
	if num < 0 {

		*diff -= 2 * block.realNum
		return false
	} else if num > 0 {

		return false
	}

	for check := block.segs; check != nil; check = check.nextInSuper {

		leftside := false
		a := part.pdy*check.psx - part.pdx*check.psy + part.perp
		b := part.pdy*check.pex - part.pdx*check.pey + part.perp
		if ZDiffSign(a, b) {
			if (a != 0) && (b != 0) {
				if w.PassingTooClose(part, check, cost, nil) {
					return true
				}

				l := check.len

				d := ZNumber((ZWideNumber(l) * ZWideNumber(a)) / (ZWideNumber(a) - ZWideNumber(b)))
				if d >= 2 {

					if check.flags&SEG_FLAG_PRECIOUS != 0 {
						return true
					}

					*cost += w.pickNodeFactor
					if *cost > bestcost {

						return true
					}
					(*tot)++
				} else if ZExt_checkPorn1(l, d, check.pdx, part.pdx, check.pdy, part.pdy, b) {
					leftside = true
				}
			} else {

				if (check.flags&SEG_FLAG_PRECIOUS != 0) &&
					!w.PartIsPolyobjSide(part, check) {

					return true
				}
				leftside = true
			}
		} else if a <= 0 {
			if a != 0 {
				leftside = true
			} else if b == 0 {

				check.alias = part.alias
				if check.pdx*part.pdx+check.pdy*part.pdy < 0 {
					leftside = true
				}
			}
		}
		if leftside {
			*diff -= 2
		}
	}

	if block.subs[0] != nil {
		if w.evalPartitionWorker_Maelstrom(block.subs[0], part, tot, diff,
			cost, bestcost) {
			return true
		}
	}

	if block.subs[1] != nil {
		block = block.subs[1]
		goto forblock
	}

	return false
}

func (w *ZExt_NodesWork) PartIsPolyobjSide(part, check *ZExt_NodeSeg) bool {
	lines := w.lines.GetAllPolyobjLines(check.Linedef)
	if lines == nil {
		return false
	}
	c := &ZExt_IntersectionContext{
		psx: part.psx,
		psy: part.psy,
		pdx: part.pdx,
		pdy: part.pdy,
		pex: part.pex,
		pey: part.pey,
	}
	for _, line := range lines {
		x1, x2, y1, y2 := w.lines.GetAllXY(line)
		c.lsx = ZNumber(x1)
		c.lsy = ZNumber(x2)
		c.lex = ZNumber(y1)
		c.ley = ZNumber(y2)
		val := w.doLinesIntersect(c)
		if (val&1 != 0) && (val&16 != 0) {
			return true
		}
	}
	return false
}

func ZExt_PickNode_ZennodeDepth(w *ZExt_NodesWork, ts *ZExt_NodeSeg, bbox *NodeBounds,
	super *ZExt_Superblock) *ZExt_NodeSeg {
	best := ts
	if w.parts != nil {
		w.parts = w.parts[:0]
	}

	var previousPart *ZExt_NodeSeg

	w.segAliasObj.UnvisitAll()
	w.zenScores = w.zenScores[:0]
	var c ZExt_IntersectionContext

	for part := ts; part != nil; part = part.next {
		if part.partner != nil && part.partner == previousPart {

			continue
		}
		if part.alias != 0 {
			if w.segAliasObj.MarkAndRecall(part.alias) {

				continue
			}
		} else {

			part.alias = w.segAliasObj.Generate()

		}
		previousPart = part
		cost := 0
		tot := 0
		slen := ZNumber(0)

		w.zenScores = append(w.zenScores, ZExt_DepthScoreBundle{})
		bundle := &(w.zenScores[len(w.zenScores)-1])
		minorsDummy := MinorCosts{}
		bundle.seg = part

		w.sectorHits[0] = 0
		sectorCount := len(w.sectorHits)
		for j := 1; j < sectorCount; j = j << 1 {
			copy(w.sectorHits[j:], w.sectorHits[:j])
		}

		c.psx = part.psx
		c.psy = part.psy
		c.pex = part.pex
		c.pey = part.pey
		c.pdx = c.psx - c.pex
		c.pdy = c.psy - c.pey

		w.blocksHit = w.blocksHit[:0]
		inter := &ZenIntermediary{}
		prune := w.evalPartitionWorker_ZennodeDepth(super, part, &c, &cost,
			&slen, bundle, inter, &minorsDummy)
		prune = prune ||
			inter.segR == 0 ||
			(inter.segR == 1 && inter.segL == 0)
		if prune {
			w.zenScores = w.zenScores[:len(w.zenScores)-1]
			continue
		}

		for _, hitRecord := range w.blocksHit {
			hitRecord.block.MarkSectorsHit(w.sectorHits, hitRecord.mask)
		}

		flat := 0
		for tot = 0; tot < len(w.sectorHits); tot++ {
			switch w.sectorHits[tot] {
			case 0x0F:
				{
					inter.sectorL++
					flat++
				}
			case 0xF0:
				{
					inter.sectorR++
					flat++

				}
			case 0xFF:
				{
					inter.sectorS++
					flat++
				}
			}
		}

		if flat > 1 && w.diagonalPenalty != 0 && (part.pdx != 0 && part.pdy != 0) {
			bundle.diagonalFactor++
		}

		ZExt_scoreIntermediate(bundle, inter, w.depthArtifacts)

		best = part
	}
	if len(w.zenScores) > 0 {
		ZExt_ZenPickBestScore(w.zenScores)
		best = w.zenScores[0].seg
		if w.parts != nil {
			w.parts = append(w.parts, best)
		}
		if w.parts != nil || config.VerbosityLevel >= 4 {

			tst1, tst2, tst3, tst4 := w.zenScores[0].preciousSplit,
				w.zenScores[0].scoreTotal,
				w.zenScores[0].equivSplit,
				w.zenScores[0].segSplit
			track := 1
			for i := 1; i < len(w.zenScores); i++ {
				if tst1 == w.zenScores[i].preciousSplit &&
					tst2 == w.zenScores[i].scoreTotal &&
					tst3 == w.zenScores[i].equivSplit &&
					tst4 == w.zenScores[i].segSplit {
					if w.parts != nil {
						w.parts = append(w.parts, w.zenScores[i].seg)
					}
					track++
				} else {
					break
				}
			}
			if track > 1 {
				w.mlog.Verbose(4, "ZEN Ambiguity equal rank for %d records \n", track)
			}
		}
	}
	return best
}

func (w *ZExt_NodesWork) evalPartitionWorker_ZennodeDepth(block *ZExt_Superblock, part *ZExt_NodeSeg, c *ZExt_IntersectionContext, cost *int, slen *ZNumber,
	bundle *ZExt_DepthScoreBundle, inter *ZenIntermediary, minors *MinorCosts) bool {

forblock:

	num := ZExt_BoxOnLineSide(block, part)
	if num < 0 {

		inter.segL += block.realNum
		w.blocksHit = append(w.blocksHit, ZExt_BlocksHit{
			block: block,
			mask:  uint8(0x0F),
		})
		return false
	} else if num > 0 {

		inter.segR += block.realNum
		w.blocksHit = append(w.blocksHit, ZExt_BlocksHit{
			block: block,
			mask:  uint8(0xF0),
		})
		return false
	}

	for check := block.segs; check != nil; check = check.nextInSuper {

		leftside := false
		mask := uint8(0xF0)
		if check == part || check == part.partner {

			leftside = check == part.partner
			if leftside {
				inter.segL++
			} else {
				inter.segR++
			}
		} else {
			val := w.WhichSideCached(part, check, c)
			if val == SIDENESS_INTERSECT {
				if w.PassingTooClose(part, check, cost, nil) {
					return true
				}

				inter.segS++
				mask = uint8(0xFF)
				if check.flags&SEG_FLAG_PRECIOUS != 0 {
					bundle.preciousSplit++
				}
			} else {

				checkPrecious := false
				if val == SIDENESS_LEFT {

					leftside = true
					inter.segL++
					checkPrecious = true
				}
				if val == SIDENESS_RIGHT {

					inter.segR++
					checkPrecious = true
				}
				if checkPrecious && (check.flags&SEG_FLAG_PRECIOUS != 0) &&
					ZExt_passingThrough(part, check) &&
					!w.PartIsPolyobjSide(part, check) {

					bundle.preciousSplit++
				}
				if val == SIDENESS_COLLINEAR {

					if check.alias != part.alias && ZExt_vetAliasTransfer(c) {
						check.alias = part.alias
					}
					*slen += check.len
					if check.pdx*part.pdx+check.pdy*part.pdy < 0 {
						leftside = true
						inter.segL++
					} else {
						inter.segR++
					}
				}
			}
		}
		if leftside {
			mask = uint8(0x0F)
		}

		w.sectorHits[check.sector] |= mask
	}

	if block.subs[0] != nil {
		if w.evalPartitionWorker_ZennodeDepth(block.subs[0], part, c,
			cost, slen, bundle, inter, minors) {
			return true
		}
	}

	if block.subs[1] != nil {
		block = block.subs[1]
		goto forblock
	}

	return false
}

func ZExt_passingThrough(part, check *ZExt_NodeSeg) bool {
	a := part.pdy*check.psx - part.pdx*check.psy + part.perp
	b := part.pdy*check.pex - part.pdx*check.pey + part.perp
	if ZDiffSign(a, b) {
		if a != 0 && b != 0 {

		} else {
			return true
		}
	}
	return false
}

func (w *ZExt_NodesWork) ComputeNonVoid(part *ZExt_NodeSeg) ZExt_NonVoidPerAlias {

	blXMin := int(w.solidMap.header.XMin)
	blXMax := w.solidMap.XMax
	blYMin := int(w.solidMap.header.YMin)
	blYMax := w.solidMap.YMax

	partSegCoords := w.SegOrLineToVertexPairC(part)
	var c FloatIntersectionContext
	c.psx = partSegCoords.StartVertex.X
	c.psy = partSegCoords.StartVertex.Y
	c.pex = partSegCoords.EndVertex.X
	c.pey = partSegCoords.EndVertex.Y
	c.pdx = c.pex - c.psx
	c.pdy = c.pey - c.psy

	partStart, partEnd, bside := ZExt_PartitionInBoundary(w, part, &c, blXMax,
		blYMax, blXMin, blYMin, partSegCoords)
	if partStart == nil || partEnd == nil {

		return ZExt_NonVoidPerAlias{
			success: false,
		}
	}

	w.dgVertexMap.RestoreOrBeginSnapshot()
	ipsx := int(math.Round(partStart.X))
	ipsy := int(math.Round(partStart.Y))
	ipex := int(math.Round(partEnd.X))
	ipey := int(math.Round(partEnd.Y))

	partStart = w.dgVertexMap.SelectVertexClose(partStart.X, partStart.Y)
	partEnd = w.dgVertexMap.SelectVertexClose(partEnd.X, partEnd.Y)

	setContextFromBlockSide(&c, bside, blXMax, blYMax, blXMin, blYMin,
		MOVE_SAFE_MARGIN)
	partOrient := c.getIntersectionPoint_InfiniteLines()
	if partOrient == nil {
		w.mlog.Verbose(2, "Failed to compute a special vertex for clockwise-triangle checking.\n")
	}

	pts := CollinearOrientedVertices(make([]OrientedVertex, 0))
	if w.blockity == nil {
		w.blockity = GetBlockityLines(w.solidMap)
	}
	iter := w.blockity

	iter.SetContext(ipsx, ipsy, ipex, ipey)
	cntIncident := 0
	for iter.NextLine() {
		aline := iter.GetLine()
		lsx, lsy, lex, ley := w.lines.GetAllXY(aline)
		if lsx == lex && lsy == ley {
			continue
		}
		c.lsx = float64(lsx)
		c.lsy = float64(lsy)
		c.lex = float64(lex)
		c.ley = float64(ley)
		pt1, pt2 := c.getIntersectionOrIndicence()
		if pt1 != nil {
			RoundToFixed1616(pt1)
			pt1 = w.dgVertexMap.SelectVertexClose(pt1.X, pt1.Y)
			if pt2 != nil {

				RoundToFixed1616(pt2)
				pt2 = w.dgVertexMap.SelectVertexClose(pt2.X, pt2.Y)
				var ov1, ov2 OrientedVertex
				if VertexPairCOrdering(pt1, pt2, false) {
					ov1.v = pt1
					ov1.left = false
					ov2.v = pt2
					ov2.left = true
				} else {
					ov1.v = pt2
					ov1.left = false
					ov2.v = pt1
					ov2.left = true
				}
				pts = append(pts, ov1)
				pts = append(pts, ov2)
				cntIncident++
			} else {

				ov := OrientedVertex{
					v: pt1,
					left: IsClockwiseTriangle(&FloatVertex{X: c.lsx, Y: c.lsy},
						&FloatVertex{X: c.lex, Y: c.ley}, partOrient),
				}
				pts = append(pts, ov)
			}
		}

	}

	if len(pts) < 2 {

		w.mlog.Verbose(2, "Failed to produce a solid hits array: %d items (need at least two)\n",
			len(pts))
		return ZExt_NonVoidPerAlias{
			success: false,
		}
	}
	sort.Sort(pts)
	startBound := VertexPairCOrdering(partStart, pts[0].v, false)
	endBound := VertexPairCOrdering(pts[len(pts)-1].v, partEnd, false)
	if !startBound || !endBound {
		tailStr := ""
		if !startBound {
			tailStr = fmt.Sprintf("(%f, %f) != (%f, %f)",
				pts[0].v.X, pts[0].v.Y,
				partStart.X, partStart.Y)
		}
		if !endBound {
			if tailStr != "" {
				tailStr = "s " + tailStr + " e "
			}
			tailStr += fmt.Sprintf("(%f, %f) != (%f, %f)",
				pts[len(pts)-1].v.X, pts[len(pts)-1].v.Y,
				partEnd.X, partEnd.Y)
		}
		w.mlog.Verbose(2, "Failed to produce a solid hits array for partition line %d: %t %t %s.\n",
			part.Linedef, startBound, endBound, tailStr)

		return ZExt_NonVoidPerAlias{
			success: false,
		}
	}

	ovStart := OrientedVertex{
		v:    partStart,
		left: true,
	}
	ovEnd := OrientedVertex{
		v:    partEnd,
		left: false,
	}
	ptsOld := CollinearOrientedVertices(make([]OrientedVertex, 0))
	for _, it := range pts {
		ptsOld = append(ptsOld, OrientedVertex{
			v:    it.v,
			left: it.left,
		})
	}

	pts_fix := CollinearOrientedVertices(make([]OrientedVertex, 0))
	pts_fix = append(pts_fix, ovStart)
	for _, it := range pts {
		pts_fix = append(pts_fix, OrientedVertex{
			v:    it.v,
			left: it.left,
		})
	}
	pts_fix = append(pts_fix, ovEnd)
	pts = pts_fix
	pts.Coalesce()

	fluger := true
	for i := 2; i < len(pts)-2; i++ {
		fluger = fluger && (pts[i].left != pts[i-1].left)
	}
	if !fluger {

		w.mlog.Verbose(2, "... for the next line - old content: %s",
			ptsOld.toString())
		w.mlog.Verbose(2, "Sanity check failed! Evaluated partition line %d (%v,%v)-(%v,%v) doesn't consistently go in/out of the void when crossing solid lines (incidence count: %d). %s\n",
			part.Linedef, part.psx, part.psy, part.pex, part.pey, cntIncident, pts.toString())

		if !config.PersistThroughInsanity {
			return ZExt_NonVoidPerAlias{
				success: false,
			}
		}
	}

	nonVoid := make([]ZExt_VertexPairC, 0)
	for i := 1; i < len(pts); i++ {
		if pts[i-1].left || !pts[i].left {
			continue
		}
		dx := pts[i].v.X - pts[i-1].v.X
		dy := pts[i].v.Y - pts[i-1].v.Y
		nonVoid = append(nonVoid, ZExt_VertexPairC{
			StartVertex: pts[i-1].v,
			EndVertex:   pts[i].v,
			len:         ZNumber(math.Sqrt(dx*dx + dy*dy)),
		})
	}

	if len(nonVoid) == 0 {
		w.mlog.Verbose(2, "part %d trace produced ZERO non-void intervals\n",
			part.Linedef)
	}

	return ZExt_NonVoidPerAlias{
		data: nonVoid,

		success:       len(nonVoid) > 0,
		c:             c,
		partSegCoords: partSegCoords,
		original:      ptsOld,
	}
}

func ZExt_PartitionInBoundary(w *ZExt_NodesWork, part *ZExt_NodeSeg, c *FloatIntersectionContext,
	blXMax, blYMax, blXMin, blYMin int, partSegCoords ZExt_VertexPairC) (*FloatVertex, *FloatVertex, int) {

	partStart := new(FloatVertex)
	partEnd := new(FloatVertex)
	var side [2]int
	if part.pdx == 0 {

		partStart.Y = float64(blYMax)
		partStart.X = partSegCoords.StartVertex.X
		partEnd.Y = float64(blYMin)
		partEnd.X = partStart.X
		side[0] = SIDE_TOP_HORIZONTAL
		side[1] = SIDE_BOTTOM_HORIZONTAL
	} else if part.pdy == 0 {

		partStart.X = float64(blXMin)
		partStart.Y = partSegCoords.StartVertex.Y
		partEnd.X = float64(blXMax)
		partEnd.Y = partStart.Y
		side[0] = SIDE_LEFT_VERTICAL
		side[1] = SIDE_RIGHT_VERTICAL
	} else {

		linesTried := 0
		intersectPoints := make([]*FloatVertex, 0)
		for len(intersectPoints) < 2 && linesTried < 4 {
			added := false
			setContextFromBlockSide(c, linesTried, blXMax, blYMax, blXMin,
				blYMin, 0)
			v := c.getIntersectionPoint_InfiniteLines()
			if v != nil && CheckBoundsForIntersectPoint(v, blXMax, blYMax, blXMin,
				blYMin) {
				intersectPoints, added = appendNoDuplicates(intersectPoints,
					v)
			}
			if added {
				sideIdx := len(intersectPoints) - 1
				if sideIdx < 2 {
					side[sideIdx] = linesTried
				} else {
					w.mlog.Verbose(2, "More than 2 intersection points between line and a box - error.\n")
					return nil, nil, 0
				}
			}
			linesTried++
		}
		if len(intersectPoints) < 2 {

			w.mlog.Verbose(2, "Couldn't determine point of intersection between partition line and solid internal blockmap bounding box (%d, %d). Falling back to legacy way of measuring length.\n",
				len(intersectPoints), linesTried)
			w.mlog.Verbose(2, "part from linedef %d!%d+%d: (%v %v) - (%v %v) bbox: (%v %v) - (%v %v)\n",
				part.Linedef, part.getFlip(), part.Offset, c.psx, c.psy, c.pex,
				c.pey, blXMin, blYMax, blXMax, blYMin)
			for i := 0; i < len(intersectPoints); i++ {
				w.mlog.Verbose(2, "Intersection#%d: (%v, %v)",
					i, intersectPoints[i].X, intersectPoints[i].Y)
			}

			return nil, nil, 0
		}
		if VertexPairCOrdering(intersectPoints[0], intersectPoints[1],
			false) {
			partStart, partEnd = intersectPoints[0], intersectPoints[1]
		} else {
			partStart, partEnd = intersectPoints[1], intersectPoints[0]
			side[0], side[1] = side[1], side[0]
		}
	}
	return partStart, partEnd, side[0]
}

type ZExt_NonVoidPerAlias struct {
	data          []ZExt_VertexPairC
	success       bool
	c             FloatIntersectionContext
	partSegCoords ZExt_VertexPairC
	original      CollinearOrientedVertices
}

func (v *ZExt_NodeVertex) toFloatVertex() *FloatVertex {
	return &FloatVertex{
		Id: int(v.idx),
		X:  float64(v.X),
		Y:  float64(v.Y),
	}
}

type ZExt_VertexPairC struct {
	StartVertex *FloatVertex
	EndVertex   *FloatVertex
	len         ZNumber
}

func (s *ZExt_NodeSeg) toVertexPairC() ZExt_VertexPairC {

	sv := s.StartVertex.toFloatVertex()
	ev := s.EndVertex.toFloatVertex()
	if VertexPairCOrdering(sv, ev, false) {
		return ZExt_VertexPairC{
			StartVertex: sv,
			EndVertex:   ev,
			len:         s.len,
		}
	} else {
		return ZExt_VertexPairC{
			StartVertex: ev,
			EndVertex:   sv,
			len:         s.len,
		}
	}
}

type ZExt_CollinearVertexPairCByCoord []ZExt_VertexPairC

func (x ZExt_CollinearVertexPairCByCoord) toString() string {
	if len(x) == 0 {
		return "{EMPTY!}"
	}
	s := ""
	for i := 0; i < len(x); i++ {
		s += fmt.Sprintf("; [%s-%s]", x[i].StartVertex.toString(), x[i].EndVertex.toString())
	}
	return s
}

func ZExt_CreateNodeForSingleSector(w *ZExt_NodesWork, ts *ZExt_NodeSeg, bbox *NodeBounds,
	super *ZExt_Superblock) *NodeInProcess {
	res := new(NodeInProcess)
	var rights *ZExt_NodeSeg
	var lefts *ZExt_NodeSeg
	var rightsSuper *ZExt_Superblock
	var leftsSuper *ZExt_Superblock

	w.totals.numNodes++
	w.DivideSegsForSingleSector(ts, &rights, &lefts, bbox, super, &rightsSuper,
		&leftsSuper, nil)
	super = nil
	res.X = int16(w.nodeX)
	res.Y = int16(w.nodeY)
	res.Dx = int16(w.nodeDx)
	res.Dy = int16(w.nodeDy)

	leftBox := ZExt_FindLimits(lefts)
	res.Lbox[BB_TOP] = int16(leftBox.Ymax)
	res.Lbox[BB_BOTTOM] = int16(leftBox.Ymin)
	res.Lbox[BB_LEFT] = int16(leftBox.Xmin)
	res.Lbox[BB_RIGHT] = int16(leftBox.Xmax)
	if w.isItConvex(lefts) == CONVEX_SUBSECTOR {
		res.nextL = nil
		res.LChild = w.CreateSSector(lefts) | SSECTOR_DEEP_MASK
		w.returnSuperblockToPool(leftsSuper)
	} else {
		res.nextL = ZExt_CreateNodeForSingleSector(w, lefts, leftBox, leftsSuper)
		res.LChild = 0
	}

	rightBox := ZExt_FindLimits(rights)
	res.Rbox[BB_TOP] = int16(rightBox.Ymax)
	res.Rbox[BB_BOTTOM] = int16(rightBox.Ymin)
	res.Rbox[BB_LEFT] = int16(rightBox.Xmin)
	res.Rbox[BB_RIGHT] = int16(rightBox.Xmax)
	if w.isItConvex(rights) == CONVEX_SUBSECTOR {
		res.nextR = nil
		res.RChild = w.CreateSSector(rights) | SSECTOR_DEEP_MASK
		w.returnSuperblockToPool(rightsSuper)
	} else {
		res.nextR = ZExt_CreateNodeForSingleSector(w, rights, rightBox, rightsSuper)
		res.RChild = 0
	}

	return res
}

func (w *ZExt_NodesWork) DivideSegsForSingleSector(ts *ZExt_NodeSeg, rs **ZExt_NodeSeg, ls **ZExt_NodeSeg, bbox *NodeBounds, super *ZExt_Superblock, rightsSuper,
	leftsSuper **ZExt_Superblock, partsegs *[]PartSeg) {

	best := ZExt_PickNode_SingleSector(w, ts, bbox, super)

	if best == nil {
		panic("Couldn't pick nodeline!")
	}

	if partsegs != nil {
		w.GetPartSegs(ts, best, partsegs)
	}

	c := &ZExt_IntersectionContext{
		psx: best.StartVertex.X,
		psy: best.StartVertex.Y,
		pex: best.EndVertex.X,
		pey: best.EndVertex.Y,
	}
	c.pdx = c.psx - c.pex
	c.pdy = c.psy - c.pey

	w.SetNodeCoords(best, bbox, c)

	w.DivideSegsActual(ts, rs, ls, bbox, best, c, super, rightsSuper, leftsSuper)
}

func ZExt_PickNode_SingleSector(w *ZExt_NodesWork, ts *ZExt_NodeSeg, bbox *NodeBounds,
	super *ZExt_Superblock) *ZExt_NodeSeg {
	best := ts
	bestcost := int(INITIAL_BIG_COST)
	cnt := 0
	if w.parts != nil {
		w.parts = w.parts[:0]
	}

	for part := ts; part != nil; part = part.next {
		cnt++
	}

	var previousPart *ZExt_NodeSeg

	w.segAliasObj.UnvisitAll()
	for part := ts; part != nil; part = part.next {
		if part.partner != nil && part.partner == previousPart {

			continue
		}
		if part.alias != 0 {
			if w.segAliasObj.MarkAndRecall(part.alias) {

				continue
			}
		} else {

			part.alias = w.segAliasObj.Generate()

		}
		previousPart = part
		cost := 0
		tot := 0
		diff := cnt

		c := &ZExt_IntersectionContext{
			psx: part.StartVertex.X,
			psy: part.StartVertex.Y,
			pex: part.EndVertex.X,
			pey: part.EndVertex.Y,
		}
		c.pdx = c.psx - c.pex
		c.pdy = c.psy - c.pey
		leftcnt := 0
		rightcnt := 0
		prune := false

		for check := ts; check != nil; check = check.next {

			leftside := false
			c.lsx = check.StartVertex.X
			c.lsy = check.StartVertex.Y
			c.lex = check.EndVertex.X
			c.ley = check.EndVertex.Y
			val := w.doLinesIntersect(c)
			if ((val&2 != 0) && (val&64 != 0)) || ((val&4 != 0) && (val&32 != 0)) {

				cost += PICKNODE_FACTOR << 1
				if cost >= bestcost {
					prune = true
					break
				}
				tot++
				leftcnt++
				rightcnt++
			} else {
				if check == part || check == part.partner {
					leftside = check == part.partner
					if leftside {
						check.alias = part.alias
						leftcnt++
					} else {
						rightcnt++
					}
				} else {
					if val&34 != 0 {

						leftside = true
						leftcnt++
					}
					if val&68 != 0 {

						rightcnt++
					}
					if (val&1 != 0) && (val&16 != 0) {
						if check.alias != part.alias && ZExt_vetAliasTransfer(c) {
							check.alias = part.alias
						}
						if check.pdx*part.pdx+check.pdy*part.pdy < 0 {
							leftside = true
							leftcnt++
						} else {
							rightcnt++
						}
					}
				}
			}
			if leftside {
				diff -= 2
			}
		}
		if prune {
			continue
		}

		if rightcnt == 0 || (rightcnt == 1 && leftcnt == 0) {
			continue
		}

		if leftcnt == 0 {

			cost += PICKNODE_FACTOR
		}

		diff -= tot
		if diff < 0 {
			diff = -diff
		}

		cost += diff
		if cost < bestcost {

			bestcost = cost
			best = part
			if w.parts != nil {
				w.parts = w.parts[:0]
				w.parts = append(w.parts, part)
			}
		} else if cost == bestcost && w.parts != nil {
			w.parts = append(w.parts, part)
		}
	}

	return best
}

type ZExt_MTPWorker_Input struct {
	workData    *ZExt_NodesWork
	pickSegIdx  int
	ts          *ZExt_NodeSeg
	bbox        *NodeBounds
	pseudoSuper *ZExt_Superblock
	id          int
}

type ZExt_MTPWorker_Result struct {
	workData *ZExt_NodesWork
	bspTree  *NodeInProcess
	id       int
}

func ZExt_MTPSentinel_MakeBestBSPTree(w *ZExt_NodesWork, bbox *NodeBounds,
	super *ZExt_Superblock, rootChoiceMethod int) (*NodeInProcess, int) {
	Log.Printf("Nodes builder: info: you have selected multi-tree mode with bruteforce for root partition only.\n")
	Log.Printf("Multi-tree modes take significant time to compute, and also have rather high memory consumption.\n")

	rootSegCandidates := ZExt_MTPSentinel_GetRootSegCandidates(w.allSegs,
		rootChoiceMethod)

	workerCount := int(config.NodeThreads)

	if workerCount == 0 {
		workerCount = runtime.NumCPU()
		if workerCount > MAX_MTP_WORKERS {
			workerCount = MAX_MTP_WORKERS
		}
	}

	if workerCount > len(rootSegCandidates) {
		workerCount = len(rootSegCandidates)
		Log.Printf("Limiting number of threads for multi-tree to %d, because only %d linedefs to try.\n",
			workerCount, workerCount)
	}

	Log.Printf("Multi-tree generator will use %d CPUs\n", workerCount)

	workerChans := make([]chan ZExt_MTPWorker_Input, workerCount)
	workerReplyChans := make([]chan ZExt_MTPWorker_Result, workerCount)

	if config.SpeedTree {

		for i := 0; i < workerCount; i++ {
			workerChans[i] = make(chan ZExt_MTPWorker_Input)
			workerReplyChans[i] = make(chan ZExt_MTPWorker_Result)

			go ZExt_MTPWorker_SpeedGenerateBSPTrees(workerChans[i], workerReplyChans[i])
		}
	} else {

		for i := 0; i < workerCount; i++ {
			workerChans[i] = make(chan ZExt_MTPWorker_Input)
			workerReplyChans[i] = make(chan ZExt_MTPWorker_Result)

			go ZExt_MTPWorker_GenerateBSPTrees(workerChans[i], workerReplyChans[i])
		}
	}

	pseudoSuper := super.DerivePseudo()

	for i := 0; i < workerCount; i++ {
		clonedWorkData, clonedBbox := ZExt_MTPSentinel_Clone(w, bbox)
		workerChans[i] <- ZExt_MTPWorker_Input{
			workData:    clonedWorkData,
			pickSegIdx:  rootSegCandidates[i],
			ts:          clonedWorkData.allSegs[0],
			bbox:        clonedBbox,
			id:          i,
			pseudoSuper: pseudoSuper,
		}
	}
	lastFedIdx := workerCount - 1

	branches := make([]reflect.SelectCase, workerCount)
	branchIdx := make([]int, workerCount)
	NA := reflect.Value{}
	for i := 0; i < workerCount; i++ {
		branches[i] = reflect.SelectCase{
			Dir:  reflect.SelectRecv,
			Chan: reflect.ValueOf(workerReplyChans[i]),
			Send: NA,
		}
		branchIdx[i] = i
	}
	treesDone := 0
	maxTrees := len(rootSegCandidates)

	var bestResult *ZExt_MTPWorker_Result
	for len(branches) > 0 {
		chi, recv, recvOk := reflect.Select(branches)
		if !recvOk {
			branches, branchIdx = MTPSentinel_DeleteBranch(branches, branchIdx, chi)
			continue
		}
		treesDone++
		res := (recv.Interface()).(ZExt_MTPWorker_Result)
		resp := new(ZExt_MTPWorker_Result)
		*resp = res

		if lastFedIdx < len(rootSegCandidates)-1 {
			lastFedIdx++
			clonedWorkData, clonedBbox := ZExt_MTPSentinel_Clone(w, bbox)
			workerChans[branchIdx[chi]] <- ZExt_MTPWorker_Input{
				workData:    clonedWorkData,
				pickSegIdx:  rootSegCandidates[lastFedIdx],
				ts:          clonedWorkData.allSegs[0],
				bbox:        clonedBbox,
				id:          lastFedIdx,
				pseudoSuper: pseudoSuper,
			}
		} else {

			close(workerChans[branchIdx[chi]])
		}

		if bestResult == nil || ZExt_MTP_IsBSPTreeBetter(bestResult, resp) {
			bestResult = resp
		}
		Log.Printf("Multi-tree: processed %d/%d trees\n", treesDone, maxTrees)
	}
	Log.Printf("Multi-tree finished processing trees (%d out of %d - should be equal)", treesDone,
		lastFedIdx+1)

	oldLines := w.lines
	*w = *(bestResult.workData)
	oldLines.AssignFrom(w.lines)
	return bestResult.bspTree, treesDone
}

func ZExt_IsBSPTreeBetter(oldWD *ZExt_NodesWork, oldNIP *NodeInProcess,
	newWD *ZExt_NodesWork, newNIP *NodeInProcess) int {

	oldBad := oldWD.totals.preciousSplit > 0
	newBad := newWD.totals.preciousSplit > 0
	if oldBad && !newBad {
		return 1
	} else if !oldBad && newBad {
		return -1
	}

	oldSsectorsCnt := oldWD.totals.numSSectors
	newSSectorsCnt := newWD.totals.numSSectors
	if newSSectorsCnt < oldSsectorsCnt {
		return 1
	} else if newSSectorsCnt > oldSsectorsCnt {
		return -1
	}

	oldLeft, oldRight := getBSPHeights(oldNIP)
	newLeft, newRight := getBSPHeights(newNIP)
	oldDiff := oldRight - oldLeft
	if oldDiff < 0 {
		oldDiff = -oldDiff
	}
	newDiff := newRight - newLeft
	if newDiff < 0 {
		newDiff = -newDiff
	}

	if newDiff < oldDiff {
		return 1
	} else if newDiff > oldDiff {
		return -1
	}

	oldMaxHeight := oldLeft
	if oldRight > oldMaxHeight {
		oldMaxHeight = oldRight
	}
	newMaxHeight := newLeft
	if newRight > newMaxHeight {
		newMaxHeight = newRight
	}

	if newMaxHeight < oldMaxHeight {
		return 1
	} else if newMaxHeight > oldMaxHeight {
		return -1
	}

	oldSegCnt := oldWD.totals.numSegs
	newSegCnt := newWD.totals.numSegs
	if newSegCnt < oldSegCnt {
		return 1
	} else if newSegCnt > oldSegCnt {
		return -1
	}
	return 0
}

func ZExt_MTP_IsBSPTreeBetter(oldResult *ZExt_MTPWorker_Result, newResult *ZExt_MTPWorker_Result) bool {
	ir := ZExt_IsBSPTreeBetter(oldResult.workData, oldResult.bspTree,
		newResult.workData, newResult.bspTree)
	if ir > 0 {
		return true
	} else if ir < 0 {
		return false
	}

	if newResult.id < oldResult.id {
		return true
	}
	return false
}

func ZExt_MTPSentinel_Clone(w *ZExt_NodesWork, bbox *NodeBounds) (*ZExt_NodesWork, *NodeBounds) {
	clonedWorkData := w.GetInitialStateClone()
	clonedWorkData.mlog = CreateMiniLogger()
	clonedBbox := new(NodeBounds)
	*clonedBbox = *bbox
	return clonedWorkData, clonedBbox
}

func ZExt_MTPSentinel_GetRootSegCandidates(allSegs []*ZExt_NodeSeg, rootChoiceMethod int) []int {
	res := make([]int, 0, len(allSegs))
	for i, seg := range allSegs {
		if i > 0 && seg.partner == allSegs[i-1] {

			continue
		}

		if seg.partner == nil && (rootChoiceMethod&MROOT_ONESIDED == 0) {
			continue
		}

		if seg.partner != nil && (rootChoiceMethod&MROOT_TWOSIDED == 0) {
			continue
		}
		res = append(res, i)
	}

	alias := make([]bool, len(res))
	for i := 0; i < len(res); i++ {
		if alias[i] {
			continue
		}
		part := allSegs[res[i]]
		for j := i + 1; j < len(res); j++ {
			if alias[j] {
				continue
			}
			check := allSegs[res[j]]
			a := part.pdy*check.psx - part.pdx*check.psy + part.perp
			b := part.pdy*check.pex - part.pdx*check.pey + part.perp
			if a == 0 && b == 0 {

				alias[j] = true
			}
		}
	}

	res2 := make([]int, 0, len(res))
	for i := 0; i < len(res); i++ {
		if alias[i] {
			continue
		}
		res2 = append(res2, res[i])
	}
	Log.Printf("Multi-tree: originally %d linedefs to try for root, but only %d would produce unique trees. (This is normal)",
		len(res), len(res2))
	return res2
}

func ZExt_MTPWorker_GenerateBSPTrees(input <-chan ZExt_MTPWorker_Input, replyTo chan<- ZExt_MTPWorker_Result) {
	for permutation := range input {
		tree := ZExt_MTP_CreateRootNode(permutation.workData,
			permutation.ts, permutation.bbox, permutation.pseudoSuper,
			permutation.pickSegIdx)

		replyTo <- ZExt_MTPWorker_Result{
			workData: permutation.workData,
			bspTree:  tree,
			id:       permutation.id,
		}
	}
	close(replyTo)
}

func ZExt_MTPWorker_SpeedGenerateBSPTrees(input <-chan ZExt_MTPWorker_Input, replyTo chan<- ZExt_MTPWorker_Result) {

	var qallocSupers *ZExt_Superblock
	bhPool := sync.Pool{
		New: func() interface{} {
			return make([]ZExt_BlocksHit, 0)
		},
	}

	for permutation := range input {

		if qallocSupers != nil {
			permutation.workData.qallocSupers = qallocSupers
			permutation.workData.blocksHit = bhPool.Get().([]ZExt_BlocksHit)
		}

		tree := ZExt_MTP_CreateRootNode(permutation.workData,
			permutation.ts, permutation.bbox, permutation.pseudoSuper,
			permutation.pickSegIdx)

		qallocSupers = permutation.workData.qallocSupers
		permutation.workData.qallocSupers = nil
		blocksHit := permutation.workData.blocksHit[:0]
		permutation.workData.blocksHit = nil
		bhPool.Put(blocksHit)

		replyTo <- ZExt_MTPWorker_Result{
			workData: permutation.workData,
			bspTree:  tree,
			id:       permutation.id,
		}
	}
	close(replyTo)
}

func ZExt_MTP_CreateRootNode(w *ZExt_NodesWork, ts *ZExt_NodeSeg, bbox *NodeBounds,
	pseudoSuper *ZExt_Superblock, pickSegIdx int) *NodeInProcess {
	res := new(NodeInProcess)
	var rights *ZExt_NodeSeg
	var lefts *ZExt_NodeSeg
	var rightsSuper *ZExt_Superblock
	var leftsSuper *ZExt_Superblock

	w.totals.numNodes++

	w.MTP_DivideSegs(ts, &rights, &lefts, bbox, pseudoSuper, &rightsSuper,
		&leftsSuper, w.allSegs[pickSegIdx])
	res.X = int16(w.nodeX)
	res.Y = int16(w.nodeY)
	res.Dx = int16(w.nodeDx)
	res.Dy = int16(w.nodeDy)

	leftBox := ZExt_FindLimits(lefts)
	res.Lbox[BB_TOP] = int16(leftBox.Ymax)
	res.Lbox[BB_BOTTOM] = int16(leftBox.Ymin)
	res.Lbox[BB_LEFT] = int16(leftBox.Xmin)
	res.Lbox[BB_RIGHT] = int16(leftBox.Xmax)
	state := w.isItConvex(lefts)
	if state == CONVEX_SUBSECTOR {
		res.nextL = nil
		res.LChild = w.CreateSSector(lefts) | SSECTOR_DEEP_MASK
		w.returnSuperblockToPool(leftsSuper)
	} else if state == NONCONVEX_ONESECTOR {
		res.nextL = w.createNodeSS(w, lefts, leftBox, leftsSuper)
		res.LChild = 0
	} else {
		res.nextL = ZExt_CreateNode(w, lefts, leftBox, leftsSuper)
		res.LChild = 0
	}
	leftsSuper = nil

	rightBox := ZExt_FindLimits(rights)
	res.Rbox[BB_TOP] = int16(rightBox.Ymax)
	res.Rbox[BB_BOTTOM] = int16(rightBox.Ymin)
	res.Rbox[BB_LEFT] = int16(rightBox.Xmin)
	res.Rbox[BB_RIGHT] = int16(rightBox.Xmax)
	state = w.isItConvex(rights)
	if state == CONVEX_SUBSECTOR {
		res.nextR = nil
		res.RChild = w.CreateSSector(rights) | SSECTOR_DEEP_MASK
		w.returnSuperblockToPool(rightsSuper)
	} else if state == NONCONVEX_ONESECTOR {
		res.nextR = w.createNodeSS(w, rights, rightBox, rightsSuper)
		res.RChild = 0
	} else {
		res.nextR = ZExt_CreateNode(w, rights, rightBox, rightsSuper)
		res.RChild = 0
	}
	rightsSuper = nil
	return res
}

func (w *ZExt_NodesWork) MTP_DivideSegs(ts *ZExt_NodeSeg, rs **ZExt_NodeSeg, ls **ZExt_NodeSeg, bbox *NodeBounds, pseudoSuper *ZExt_Superblock, rightsSuper, leftsSuper **ZExt_Superblock, chosen *ZExt_NodeSeg) {
	best := chosen

	c := &ZExt_IntersectionContext{
		psx: best.StartVertex.X,
		psy: best.StartVertex.Y,
		pex: best.EndVertex.X,
		pey: best.EndVertex.Y,
	}
	c.pdx = c.psx - c.pex
	c.pdy = c.psy - c.pey

	w.SetNodeCoords(best, bbox, c)

	w.DivideSegsActual(ts, rs, ls, bbox, best, c, pseudoSuper, rightsSuper, leftsSuper)
}

func ZExt_zoneAllocSupers(sz int, pseudoSuper *ZExt_Superblock) *ZExt_Superblock {
	zone := make([]ZExt_Superblock, sz)
	dummy := ZExt_NodesWork{}
	dummySeg := &ZExt_NodeSeg{}
	for i, _ := range zone {

		zone[i].segs = dummySeg

		zone[i].InitSectorsIfNeeded(pseudoSuper)
		dummy.returnSuperblockToPool(&(zone[i]))
	}
	return dummy.qallocSupers
}

func (w *ZExt_NodesWork) AddVertex(x, y ZNumber) *ZExt_NodeVertex {
	v := w.vertexMap.SelectVertexClose(float64(x), float64(y))
	if v.Id != -1 {
		w.vertexExists++
		return &(w.vertices[v.Id])
	}

	idx := len(w.vertices)
	v.Id = idx

	w.vertices = append(w.vertices, ZExt_NodeVertex{
		X:   ZNumber(v.X),
		Y:   ZNumber(v.Y),
		idx: uint32(idx),
	})
	return &(w.vertices[idx])
}

func (w *ZExt_NodesWork) CreateSSector(tmps *ZExt_NodeSeg) uint32 {
	subsectorIdx := uint32(len(w.zdoomSubsectors))
	oldNumSegs := uint32(len(w.zdoomSegs))

	w.totals.numSSectors++
	var currentCount uint32
	for ; tmps != nil; tmps = tmps.next {
		tmps.block = nil
		tmps.nextInSuper = nil
		w.zdoomSegs = append(w.zdoomSegs, ZdoomNode_Seg{
			StartVertex: uint32(tmps.StartVertex.idx),
			EndVertex:   uint32(tmps.EndVertex.idx),
			Linedef:     tmps.Linedef,
			Flip:        byte(tmps.getFlip()),
		})
	}
	currentCount = uint32(len(w.zdoomSegs)) - oldNumSegs
	if currentCount > w.totals.maxSegCountInSubsector {
		w.totals.maxSegCountInSubsector = currentCount
	}
	w.totals.numSegs += currentCount
	w.zdoomSubsectors = append(w.zdoomSubsectors, currentCount)
	return subsectorIdx
}

func ZExt_PointOnLineSide(part *ZExt_NodeSeg, x, y int) int {
	perp := ZExt_UtilPerpDist_Float64(part, float64(x), float64(y))
	ab, sgn := Float64AbsAndSign(perp)
	if ab <= DIST_EPSILON {
		return 0
	}
	if sgn {
		return -1
	}
	return +1
}

func ZExt_UtilPerpDist_Float64(part *ZExt_NodeSeg, x, y float64) float64 {
	return (x*float64(part.pdy) - y*float64(part.pdx) +
		float64(part.perp)) / float64(part.len)
}

func (w *ZExt_NodesWork) PassingTooClose(part, check *ZExt_NodeSeg, cost *int,
	minors *MinorCosts) bool {
	frac := ZExt_InterceptVector(part, check)
	if frac < 0.001 || frac > 0.999 {
		x := float64(check.psx)
		y := float64(check.psy)
		x = math.FMA(frac, float64(check.pex)-x, x)
		y = math.FMA(frac, float64(check.pey)-y, y)
		if math.Abs(x-float64(check.psx)) <= VERTEX_EPSILON &&
			math.Abs(y-float64(check.psy)) <= VERTEX_EPSILON {
			return true
		}
		if math.Abs(x-float64(check.pex)) <= VERTEX_EPSILON &&
			math.Abs(y-float64(check.pey)) <= VERTEX_EPSILON {
			return true
		}

	}
	return false
}

func ZExt_InterceptVector(part, check *ZExt_NodeSeg) float64 {
	return InterceptVectorCoord(float64(part.psx), float64(part.psy),
		float64(part.pdx), float64(part.pdy),
		float64(check.psx), float64(check.psy), float64(check.pdx),
		float64(check.pdy))
}

func (c *ZExt_IntersectionContext) computeIntersection() (ZNumber, ZNumber) {
	frac := InterceptVectorCoord(float64(c.psx), float64(c.psy), float64(c.pdx),
		float64(c.pdy), float64(c.lsx), float64(c.lsy), float64(c.lex-c.lsx),
		float64(c.ley-c.lsy))

	newx := float64(c.lsx)
	newy := float64(c.lsy)
	newx = math.FMA(frac, float64(c.lex)-newx, newx)
	newy = math.FMA(frac, float64(c.ley)-newy, newy)

	return ZNumber(newx), ZNumber(newy)
}

func ZExt_doLinesIntersectStandard(c *ZExt_IntersectionContext) uint8 {
	dx2 := c.psx - c.lsx
	dy2 := c.psy - c.lsy
	dx3 := c.psx - c.lex
	dy3 := c.psy - c.ley

	a := c.pdy*dx2 - c.pdx*dy2
	b := c.pdy*dx3 - c.pdx*dy3
	if ZDiffSign(a, b) && (a != 0) && (b != 0) {
		x, y := c.computeIntersection()
		dx2 = c.lsx - x
		dy2 = c.lsy - y
		if dx2 == 0 && dy2 == 0 {
			a = 0
		} else {

			cmp := float64(a) * float64(a) / float64(c.pdx*c.pdx+c.pdy*c.pdy)
			if cmp < SIDE_EPSILON*SIDE_EPSILON {
				a = 0
			}
		}
		dx3 = c.lex - x
		dy3 = c.ley - y
		if dx3 == 0 && dy3 == 0 {
			b = 0
		} else {

			cmp := float64(b) * float64(b) / float64(c.pdx*c.pdx+c.pdy*c.pdy)
			if cmp < SIDE_EPSILON*SIDE_EPSILON {
				b = 0
			}
		}

	}

	var val uint8

	if a == 0 {
		val = val | 16
	} else if a < 0 {
		val = val | 32
	} else {
		val = val | 64
	}

	if b == 0 {
		val = val | 1
	} else if b < 0 {
		val = val | 2
	} else {
		val = val | 4
	}

	return val
}

func (w *ZExt_NodesWork) SetNodeCoords(part *ZExt_NodeSeg, bbox *NodeBounds,
	c *ZExt_IntersectionContext) {

	x1, y1, x2, y2 := w.lines.GetAllXY(part.Linedef)
	if part.getFlip() != 0 {
		w.nodeX = x2
		w.nodeY = y2
		w.nodeDx = x1 - x2
		w.nodeDy = y1 - y2
	} else {
		w.nodeX = x1
		w.nodeY = y1
		w.nodeDx = x2 - x1
		w.nodeDy = y2 - y1
	}
	if w.nodeDx <= 32767 && w.nodeDy <= 32767 && w.nodeDx >= -32768 &&
		w.nodeDy >= -32768 {
		return
	}
	oldDx := w.nodeDx
	oldDy := w.nodeDy
	oldX := w.nodeX
	oldY := w.nodeY
	psx, psy := part.StartVertex.X, part.StartVertex.Y
	pex, pey := part.EndVertex.X, part.EndVertex.Y
	pexc, peyc := pex.Ceil(), pey.Ceil()
	w.nodeX = int(psx)
	w.nodeY = int(psy)
	w.nodeDx = pexc - w.nodeX
	w.nodeDy = peyc - w.nodeY

	if float64(w.nodeX) == float64(psx) && float64(w.nodeY) == float64(psy) &&
		float64(pexc) == float64(pex) && float64(peyc) == float64(pey) {

		if w.nodeDx <= 32767 && w.nodeDy <= 32767 && w.nodeDx >= -32768 &&
			w.nodeDy >= -32768 {

			Log.Verbose(1, "I avoided overflow in node values by chosing seg coords instead of linedef coords (line too long). Source linedef: %d\n",
				part.Linedef)
			return
		}
	}

	w.nodeDx = oldDx
	w.nodeDy = oldDy
	w.nodeX = oldX
	w.nodeY = oldY
	XEnd := w.nodeX + w.nodeDx
	YEnd := w.nodeY + w.nodeDy

	dd := GCD(Abs(w.nodeDx), Abs(w.nodeDy))
	if dd != 1 {
		w.nodeDx = w.nodeDx / dd
		w.nodeDy = w.nodeDy / dd
		if dd > 2 {

			pCenter := ProjectBoxCenterOntoLine(bbox, w.nodeX, w.nodeY, XEnd,
				YEnd)
			dcx := int(pCenter.X) - w.nodeX
			dcy := int(pCenter.Y) - w.nodeY
			if w.nodeDx == 0 && w.nodeDy == 0 {

				Log.Verbose(1, "Really strange happenings - partition line is a point\n")
				return
			}
			if (Sign(dcx) != Sign(w.nodeDx)) ||
				(Sign(dcy) != Sign(w.nodeDy)) {
				Log.Verbose(2, "Signs don't match (move after scale op cancelled): dcx,dcy=(%d,%d) dx,dy=(%d,%d)\n",
					dcx, dcy, w.nodeDx, w.nodeDy)
				return
			}
			var d2 int
			if Abs(w.nodeDx) > Abs(w.nodeDy) {
				d2 = dcx / w.nodeDx
			} else {
				d2 = dcy / w.nodeDy
			}
			w.nodeX += d2 * w.nodeDx
			w.nodeY += d2 * w.nodeDy
		}
	}

	if w.nodeDx <= 32767 && w.nodeDy <= 32767 && w.nodeDx >= -32768 &&
		w.nodeDy >= -32768 {
		Log.Verbose(1, "Prevented partition line coords overflow (from segment of linedef %d).\n",
			part.Linedef)
		return
	}

	Log.Verbose(1, "Overflow: partition line DX=%d, DY=%d (from linedef %d) can not be represented correctly due to (-32768,32767) signed int16 range limit in format. Parts of map will not be rendered correctly in any port.\n",
		w.nodeDx, w.nodeDy, part.Linedef)
}

func (w *ZExt_NodesWork) updateSegLenBetter(s *ZExt_NodeSeg) {
	dy := float64(s.pey - s.psy)
	dx := float64(s.pex - s.psx)
	s.len = ZNumber(math.Sqrt(dx*dx + dy*dy))
}

func (w *ZExt_NodesWork) SegOrLineToVertexPairC(part *ZExt_NodeSeg) ZExt_VertexPairC {
	x1, y1, x2, y2 := w.lines.GetAllXY(part.Linedef)
	sv := &FloatVertex{
		X: float64(x1),
		Y: float64(y1),
	}
	ev := &FloatVertex{
		X: float64(x2),
		Y: float64(y2),
	}
	dx := float64(x2 - x1)
	dy := float64(y2 - y1)
	l := ZNumber(math.Sqrt(dx*dx + dy*dy))
	if VertexPairCOrdering(sv, ev, false) {
		return ZExt_VertexPairC{
			StartVertex: sv,
			EndVertex:   ev,
			len:         l,
		}
	} else {
		return ZExt_VertexPairC{
			StartVertex: ev,
			EndVertex:   sv,
			len:         l,
		}
	}
}

func ZExt_vetAliasTransfer(c *ZExt_IntersectionContext) bool {
	return true
}

func ZExt_vetAliasTransfer2(part, check *ZExt_NodeSeg) bool {
	if check.len >= 4 {
		return true
	}
	c := &ZExt_IntersectionContext{
		psx: part.psx,
		psy: part.psy,
		pex: part.pex,
		pey: part.pey,
	}
	c.pdx = c.psx - c.pex
	c.pdy = c.psy - c.pey
	c.lsx = check.psx
	c.lsy = check.psy
	c.lex = check.pex
	c.ley = check.pey
	val := ZExt_doLinesIntersectStandard(c)
	return (val&1 != 0) && (val&16 != 0)
}

func (w *ZExt_NodesWork) upgradeToDeep() {

}

func (w *ZExt_NodesWork) tooManySegsCantFix(dryRun bool) bool {
	return false
}

func (w *ZExt_NodesWork) getZdoomNodesBytes() []byte {
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

func (w *ZExt_NodesWork) reverseNodes(node *NodeInProcess) uint32 {

	return uint32(0)
}

func (w *ZExt_NodesWork) convertNodesStraight(node *NodeInProcess, idx uint32) uint32 {

	return uint32(0)
}

type ZExt_Superblock struct {
	parent *ZExt_Superblock

	x1, y1 int
	x2, y2 int

	subs [2]*ZExt_Superblock

	realNum int

	segs *ZExt_NodeSeg

	sectors   []uint16
	secEquivs []uint16

	secMap map[uint16]struct{}
	nwlink *ZExt_NodesWork
}

type ZExt_BlocksHit struct {
	block *ZExt_Superblock
	mask  uint8
}

func (s *ZExt_Superblock) SuperIsLeaf() bool {
	return (s.x2-s.x1) <= 256 && (s.y2-s.y1) <= 256
}

func (s *ZExt_Superblock) AddSegToSuper(seg *ZExt_NodeSeg) {
	if seg == nil {
		return
	}
	block := s
	for {
		var p1, p2 bool
		var child int
		xMid := (block.x1 + block.x2) >> 1
		yMid := (block.y1 + block.y2) >> 1

		block.realNum++

		if block.sectors != nil {
			sec := seg.sector
			if !block.markAndRecall(block.sectors, sec) {
				block.sectors = append(block.sectors, sec)
			}
		} else if block.secEquivs != nil {
			sec := seg.secEquiv
			if !block.markAndRecall(block.secEquivs, sec) {
				block.secEquivs = append(block.secEquivs, sec)
			}
		}
		if block.SuperIsLeaf() {

			seg.nextInSuper = block.segs
			seg.block = block
			block.segs = seg
			return
		}
		if block.x2-block.x1 >= block.y2-block.y1 {

			p1 = seg.StartVertex.X >= ZNumber(xMid)
			p2 = seg.EndVertex.X >= ZNumber(xMid)
		} else {

			p1 = seg.StartVertex.Y >= ZNumber(yMid)
			p2 = seg.EndVertex.Y >= ZNumber(yMid)
		}

		if p1 && p2 {
			child = 1
		} else if !p1 && !p2 {
			child = 0
		} else {

			seg.nextInSuper = block.segs
			seg.block = block
			block.segs = seg
			return
		}

		if block.subs[child] == nil {
			sub := s.nwlink.getNewSuperblock(s)
			block.subs[child] = sub
			sub.parent = block

			if block.x2-block.x1 >= block.y2-block.y1 {
				if child == 1 {
					sub.x1 = xMid
				} else {
					sub.x1 = block.x1
				}
				sub.y1 = block.y1

				if child == 1 {
					sub.x2 = block.x2
				} else {
					sub.x2 = xMid
				}
				sub.y2 = block.y2
			} else {
				sub.x1 = block.x1
				if child == 1 {
					sub.y1 = yMid
				} else {
					sub.y1 = block.y1
				}

				sub.x2 = block.x2
				if child == 1 {
					sub.y2 = block.y2
				} else {
					sub.y2 = yMid
				}
			}
		}
		block = block.subs[child]
	}
}

func (s *ZExt_Superblock) InitSectorsIfNeeded(template *ZExt_Superblock) {
	if template.sectors != nil {
		s.sectors = make([]uint16, 0)
	}
	if template.secEquivs != nil {
		s.secEquivs = make([]uint16, 0)
	}
}

func (s *ZExt_Superblock) SetBounds(box *NodeBounds) {
	dx := (box.Xmax - box.Xmin + 127) >> BLOCK_BITS
	dy := (box.Ymax - box.Ymin + 127) >> BLOCK_BITS

	s.x1 = box.Xmin
	s.x2 = box.Xmin + (RoundPOW2(dx) << BLOCK_BITS)
	s.y1 = box.Ymin
	s.y2 = box.Ymin + (RoundPOW2(dy) << BLOCK_BITS)
}

func (s *ZExt_Superblock) MarkSectorsHit(sectorsHit []uint8, mask uint8) {
	for _, sector := range s.sectors {
		sectorsHit[sector] |= mask
	}
}

func (s *ZExt_Superblock) MarkSecEquivsHit(sectorsHit []uint8, mask uint8) {
	for _, secEquiv := range s.secEquivs {
		sectorsHit[secEquiv] |= mask
	}
}

func (s *ZExt_Superblock) MarkSectorsHitNoCached(sectorsHit []uint8, mask uint8) {
	for seg := s.segs; seg != nil; seg = seg.nextInSuper {
		sectorsHit[seg.sector] |= mask
	}

	for num := 0; num < 2; num++ {
		if s.subs[num] == nil {
			continue
		}

		s.subs[num].MarkSectorsHitNoCached(sectorsHit, mask)
	}
}

func (s *ZExt_Superblock) DerivePseudo() *ZExt_Superblock {
	res := &ZExt_Superblock{
		parent: nil,

		subs: [2]*ZExt_Superblock{nil, nil},
		segs: nil,
	}
	res.InitSectorsIfNeeded(s)

	res.x1 = cap(s.secEquivs) + cap(s.sectors)
	return res
}

func ZExt_UtilPerpDist(part *ZExt_NodeSeg, x, y int) int64 {
	return ((int64(x)*int64(part.pdy) - int64(y)*int64(part.pdx) +
		int64(part.perp)) << DIST_SHIFT) / int64(part.len)
}

func ZExt_BoxOnLineSide(box *ZExt_Superblock, part *ZExt_NodeSeg) int {
	x1 := box.x1 - MARGIN_LEN
	y1 := box.y1 - MARGIN_LEN
	x2 := box.x2 + MARGIN_LEN
	y2 := box.y2 + MARGIN_LEN

	var p1, p2 int

	if part.pdx == 0 {
		if ZNumber(x1) > part.psx {
			p1 = +1
		} else {
			p1 = -1
		}
		if ZNumber(x2) > part.psx {
			p2 = +1
		} else {
			p2 = -1
		}
		if part.pdy < 0 {
			p1 = -p1
			p2 = -p2
		}
	} else if part.pdy == 0 {
		if ZNumber(y1) < part.psy {
			p1 = +1
		} else {
			p1 = -1
		}
		if ZNumber(y2) < part.psy {
			p2 = +1
		} else {
			p2 = -1
		}

		if part.pdx < 0 {
			p1 = -p1
			p2 = -p2
		}
	} else if ZWideNumber(part.pdx)*ZWideNumber(part.pdy) > 0 {
		p1 = ZExt_PointOnLineSide(part, x1, y2)
		p2 = ZExt_PointOnLineSide(part, x2, y1)
	} else {
		p1 = ZExt_PointOnLineSide(part, x1, y1)
		p2 = ZExt_PointOnLineSide(part, x2, y2)
	}

	if p1 == p2 {
		return p1
	}
	return 0
}

func (block *ZExt_Superblock) markAndRecall(arr []uint16, sec uint16) bool {
	if len(arr) <= 4 {

		for _, chk := range arr {
			if chk == sec {
				return true
			}
		}
		return false
	}

	if block.secMap == nil {

		block.secMap = make(map[uint16]struct{}, 64)
		for _, chk := range arr {
			block.secMap[chk] = struct{}{}
		}
	}
	if _, ex := block.secMap[sec]; !ex {
		block.secMap[sec] = struct{}{}
		return false
	}
	return true
}

func (w *ZExt_NodesWork) getNewSuperblock(template *ZExt_Superblock) *ZExt_Superblock {
	if w.qallocSupers == nil {
		ret := &ZExt_Superblock{}
		ret.InitSectorsIfNeeded(template)
		ret.nwlink = w
		return ret
	}
	ret := w.qallocSupers
	w.qallocSupers = w.qallocSupers.subs[0]
	ret.subs[0] = nil
	ret.nwlink = w
	if ret.sectors != nil {
		ret.sectors = ret.sectors[:0]
	}
	if ret.secEquivs != nil {
		ret.secEquivs = ret.secEquivs[:0]
	}
	return ret
}

func (w *ZExt_NodesWork) returnSuperblockToPool(block *ZExt_Superblock) {
	if block.segs == nil {

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

	block.subs[0] = w.qallocSupers
	w.qallocSupers = block
}

func (w *ZExt_NodesWork) dismissChildrenToSuperblockPool(block *ZExt_Superblock) {
	if block.segs == nil {

		return
	}
	for num := 0; num < 2; num++ {
		if block.subs[num] == nil {
			continue
		}
		w.returnSuperblockToPool(block.subs[num])
		block.subs[num] = nil
	}
}

func (w *ZExt_NodesWork) newSuperblockNoProto() *ZExt_Superblock {
	ret := &ZExt_Superblock{}
	if w.pickNodeUser == PICKNODE_VISPLANE || w.pickNodeUser == PICKNODE_ZENLIKE {
		ret.sectors = make([]uint16, 0)
	} else if w.pickNodeUser == PICKNODE_VISPLANE_ADV {
		ret.secEquivs = make([]uint16, 0)
	}
	return ret
}

type ZExt_DepthScoreBundle struct {
	seg            *ZExt_NodeSeg
	preciousSplit  int
	equivSplit     int
	segSplit       int
	diagonalFactor int
	scoreSeg       int
	scoreSector    int
	scoreTotal     int
}

type ZExt_DepthScoresBySeg []ZExt_DepthScoreBundle

func (x ZExt_DepthScoresBySeg) Len() int { return len(x) }
func (x ZExt_DepthScoresBySeg) Less(i, j int) bool {
	if x[i].scoreSeg < x[j].scoreSeg {
		return false
	}
	if x[i].scoreSeg > x[j].scoreSeg {
		return true
	}
	if x[i].scoreSector < x[j].scoreSector {
		return false
	}
	if x[i].scoreSector > x[j].scoreSeg {
		return true
	}
	return x[i].seg.Linedef < x[j].seg.Linedef
}
func (x ZExt_DepthScoresBySeg) Swap(i, j int) { x[i], x[j] = x[j], x[i] }

type ZExt_DepthScoresBySector []ZExt_DepthScoreBundle

func (x ZExt_DepthScoresBySector) Len() int { return len(x) }
func (x ZExt_DepthScoresBySector) Less(i, j int) bool {
	if x[i].scoreSector < x[j].scoreSector {
		return false
	}
	if x[i].scoreSector > x[j].scoreSeg {
		return true
	}
	if x[i].scoreSeg < x[j].scoreSeg {
		return false
	}
	if x[i].scoreSeg > x[j].scoreSeg {
		return true
	}
	return x[i].seg.Linedef < x[j].seg.Linedef
}
func (x ZExt_DepthScoresBySector) Swap(i, j int) { x[i], x[j] = x[j], x[i] }

type ZExt_DepthScoresByTotal []ZExt_DepthScoreBundle

func (x ZExt_DepthScoresByTotal) Len() int { return len(x) }
func (x ZExt_DepthScoresByTotal) Less(i, j int) bool {
	if x[i].preciousSplit < x[j].preciousSplit {
		return true
	} else if x[i].preciousSplit > x[j].preciousSplit {
		return false
	}

	if x[i].preciousSplit > 0 {

		axisAlignedI := x[i].seg.pdx == 0 || x[i].seg.pdy == 0
		axisAlignedJ := x[j].seg.pdx == 0 || x[j].seg.pdy == 0
		if axisAlignedI && !axisAlignedJ {
			return true
		}
		if axisAlignedJ && !axisAlignedI {
			return false
		}
	}

	if x[i].scoreTotal < x[j].scoreTotal {
		return true
	} else if x[i].scoreTotal > x[j].scoreTotal {
		return false
	}

	if x[i].equivSplit < x[j].equivSplit {
		return true
	} else if x[i].equivSplit > x[j].equivSplit {
		return false
	}

	if x[i].segSplit < x[j].segSplit {
		return true
	} else if x[i].segSplit > x[j].segSplit {
		return false
	}

	if x[i].diagonalFactor < x[j].diagonalFactor {
		return true
	} else if x[i].diagonalFactor > x[j].diagonalFactor {
		return false
	}

	return x[i].seg.Linedef < x[j].seg.Linedef
}
func (x ZExt_DepthScoresByTotal) Swap(i, j int) { x[i], x[j] = x[j], x[i] }

func ZExt_ZenPickBestScore(sc []ZExt_DepthScoreBundle) {
	sort.Sort(ZExt_DepthScoresBySeg(sc))
	rank := 0
	for i := 0; i < len(sc); i++ {
		sc[i].scoreTotal = rank
		if i < len(sc)-1 && sc[i].scoreSeg != sc[i+1].scoreSeg {
			rank++
		}
	}
	sort.Sort(ZExt_DepthScoresBySector(sc))
	rank = 0
	for i := 0; i < len(sc); i++ {
		sc[i].scoreTotal += rank
		if i < len(sc)-1 && sc[i].scoreSector != sc[i+1].scoreSector {
			rank++
		}
	}
	sort.Sort(ZExt_DepthScoresByTotal(sc))

}

func (w *ZExt_NodesWork) ZenComputeScores(super *ZExt_Superblock, sc []ZExt_DepthScoreBundle, sectorHits []uint8, depthArtifacts bool) {
	var c ZExt_IntersectionContext
	for i, _ := range sc {
		inter := ZenIntermediary{
			segL:    0,
			segR:    0,
			segS:    0,
			sectorL: 0,
			sectorR: 0,
			sectorS: 0,
		}

		sectorHits[0] = 0
		hitArrayLen := len(sectorHits)
		for j := 1; j < hitArrayLen; j = j << 1 {
			copy(sectorHits[j:], sectorHits[:j])
		}

		part := sc[i].seg
		c.psx = part.psx
		c.psy = part.psy
		c.pex = part.pex
		c.pey = part.pey
		c.pdx = c.psx - c.pex
		c.pdy = c.psy - c.pey
		w.evalPartitionWorker_Zen(super, &(sc[i]), &inter, sectorHits, &c)
		for j := 0; j < len(sectorHits); j++ {
			switch sectorHits[j] {
			case 0x0F:
				{
					inter.sectorL++
				}
			case 0xF0:
				{
					inter.sectorR++
				}
			case 0xFF:
				{
					inter.sectorS++
				}
			}
		}

		ZExt_scoreIntermediate(&(sc[i]), &inter, depthArtifacts)
	}
}

func ZExt_scoreIntermediate(rec *ZExt_DepthScoreBundle, inter *ZenIntermediary,
	depthArtifacts bool) {

	rec.scoreSeg = (inter.segL + inter.segS) * (inter.segR + inter.segS)
	rec.scoreSector = (inter.sectorL + inter.sectorS) * (inter.sectorR + inter.sectorS)

	if rec.scoreSeg == 0 {

		rec.scoreSeg = VERY_BAD_SCORE
		rec.scoreSector = VERY_BAD_SCORE
		return
	}

	if inter.segS > 0 {

		tmp := ZEN_X1 * inter.segS
		if ZEN_X2 < tmp {
			rec.scoreSeg = ZEN_X2 * rec.scoreSeg / tmp
		}
		if depthArtifacts {

			rec.scoreSeg -= (ZEN_X3*inter.segS*(inter.segS/3) +
				ZEN_X4) * inter.segS
		} else {
			rec.scoreSeg -= (ZEN_X3*inter.segS + ZEN_X4) * inter.segS
		}
	} else {

		if config.EffectiveSecondary != SECONDARY_PRIORITY_SUBSECTORS {
			rec.scoreSeg = 0x7FFFFFFF - Abs(inter.segL-
				inter.segR)
		}
	}

	if depthArtifacts {

		if inter.sectorS > 0 {
			tmp := ZEN_X1 * inter.sectorS
			if ZEN_X2 < tmp {
				rec.scoreSector = ZEN_X2 * rec.scoreSector / tmp
			}

			rec.scoreSector -= (ZEN_X3*inter.sectorS + ZEN_X4) * inter.segS
		} else {

			if config.EffectiveSecondary != SECONDARY_PRIORITY_SEGS {
				rec.scoreSector = 0x7FFFFFFF - Abs(inter.sectorL-
					inter.sectorR)
			}
		}
	} else {

		if inter.sectorS > 0 {
			tmp := ZEN_Y1 * inter.sectorS
			if ZEN_Y2 < tmp {
				rec.scoreSector = ZEN_Y2 * rec.scoreSector / tmp
			}

			rec.scoreSector -= (ZEN_Y3*inter.sectorS + ZEN_Y4) * inter.sectorS
		} else {

			if config.EffectiveSecondary != SECONDARY_PRIORITY_SEGS {
				rec.scoreSector = 0x7FFFFFFF - Abs(inter.sectorL-
					inter.sectorR)
			}
		}
	}
}

func (w *ZExt_NodesWork) evalPartitionWorker_Zen(block *ZExt_Superblock, rec *ZExt_DepthScoreBundle, intermediate *ZenIntermediary, sectorHits []uint8,
	c *ZExt_IntersectionContext) {
	part := rec.seg
	num := ZExt_BoxOnLineSide(block, part)
	if num < 0 {

		intermediate.segL += block.realNum
		block.MarkSectorsHitNoCached(sectorHits, uint8(0x0F))
		return
	} else if num > 0 {

		intermediate.segR += block.realNum
		block.MarkSectorsHitNoCached(sectorHits, uint8(0xF0))
		return
	}

	for check := block.segs; check != nil; check = check.nextInSuper {

		leftside := false
		mask := uint8(0xF0)
		c.lsx = check.StartVertex.X
		c.lsy = check.StartVertex.Y
		c.lex = check.EndVertex.X
		c.ley = check.EndVertex.Y
		val := w.doLinesIntersect(c)
		if ((val&2 != 0) && (val&64 != 0)) || ((val&4 != 0) && (val&32 != 0)) {

			intermediate.segS++
			mask = uint8(0xFF)
		} else {
			if check == part || check == part.partner {

				leftside = check == part.partner
				if leftside {
					intermediate.segL++
				} else {
					intermediate.segR++
				}
			} else {
				if val&34 != 0 {

					leftside = true
					intermediate.segL++
				}
				if val&68 != 0 {

					intermediate.segR++
				}
				if (val&1 != 0) && (val&16 != 0) {

					if check.pdx*part.pdx+check.pdy*part.pdy < 0 {
						leftside = true
						intermediate.segL++
					} else {
						intermediate.segR++
					}
				}
			}
		}
		if leftside {
			mask = uint8(0x0F)
		}

		sectorHits[check.sector] |= mask
	}

	for num := 0; num < 2; num++ {
		if block.subs[num] == nil {
			continue
		}

		w.evalPartitionWorker_Zen(block.subs[num], rec, intermediate,
			sectorHits, c)
	}

}

func ZExt_ZenSegMinorToDepthScores(input []ZExt_SegMinorBundle) []ZExt_DepthScoreBundle {
	res := make([]ZExt_DepthScoreBundle, len(input))
	for i, entry := range input {
		res[i].seg = entry.seg
		res[i].preciousSplit = entry.minor.PreciousSplit
		res[i].scoreSeg = 0
		res[i].scoreSector = 0
		res[i].scoreTotal = 0
		res[i].equivSplit = entry.minor.SectorsSplit
		res[i].segSplit = entry.minor.SegsSplit
	}
	return res
}

func (log *MyLogger) ZExt_DumpSegs(ts *ZExt_NodeSeg) {
	if !config.DumpSegsFlag || ts == nil {
		return
	}

	allSector := ts.sector
	log.segs.WriteString(fmt.Sprintf("Sector #%d:\n", allSector))
	for tmps := ts; tmps != nil; tmps = tmps.next {
		log.segs.WriteString(fmt.Sprintf(
			"  Linedef: %d Flip: %d (%v,%v) - (%v, %v)",
			tmps.Linedef, tmps.getFlip(), tmps.StartVertex.X, tmps.StartVertex.Y,
			tmps.EndVertex.X, tmps.EndVertex.Y))
		if tmps.sector != allSector {

			log.segs.WriteString(fmt.Sprintf(" BAD! Sector = %d\n", tmps.sector))
		} else {
			log.segs.WriteString("\n")
		}
	}
}

func (mlog *MiniLogger) ZExt_DumpSegs(ts *ZExt_NodeSeg) {
	if mlog == nil {
		Log.ZExt_DumpSegs(ts)
	}
	if !config.DumpSegsFlag || ts == nil {
		return
	}

	allSector := ts.sector
	mlog.segs.WriteString(fmt.Sprintf("Sector #%d:\n", allSector))
	for tmps := ts; tmps != nil; tmps = tmps.next {
		mlog.segs.WriteString(fmt.Sprintf(
			"  Linedef: %d Flip: %d (%v,%v) - (%v, %v)",
			tmps.Linedef, tmps.getFlip(), tmps.StartVertex.X, tmps.StartVertex.Y,
			tmps.EndVertex.X, tmps.EndVertex.Y))
		if tmps.sector != allSector {

			mlog.segs.WriteString(fmt.Sprintf(" BAD! Sector = %d\n", tmps.sector))
		} else {
			mlog.segs.WriteString("\n")
		}
	}
}

func ZExt_IntPartitionInBoundary(part *ZExt_NodeSeg, c *ZExt_IntersectionContext, blXMax, blYMax, blXMin, blYMin int, partSegCoords ZExt_IntVertexPairC) (*ZExt_IntOrientedVertex, *ZExt_IntOrientedVertex) {

	var partStart, partEnd ZExt_IntOrientedVertex
	partStart.v = new(ZExt_NodeVertex)
	partEnd.v = new(ZExt_NodeVertex)
	if part.pdx == 0 {

		partStart.v.Y = ZNumber(blYMax)
		partStart.v.X = partSegCoords.StartVertex.X
		partEnd.v.Y = ZNumber(blYMin)
		partEnd.v.X = partStart.v.X
	} else if part.pdy == 0 {

		partStart.v.X = ZNumber(blXMin)
		partStart.v.Y = partSegCoords.StartVertex.Y
		partEnd.v.X = ZNumber(blXMax)
		partEnd.v.Y = partStart.v.Y
	} else {

		linesTried := 0
		intersectPoints := make([]ZExt_IntOrientedVertex, 0)
		for len(intersectPoints) < 2 && linesTried < 4 {
			switch linesTried {
			case 0:
				{

					c.lsx = ZNumber(blXMin)
					c.lsy = ZNumber(blYMax)
					c.lex = c.lsx
					c.ley = ZNumber(blYMin)
					v := c.intGetIntersectionPoint_InfiniteLines()
					if v != nil && ZExt_intTskCheckBounds(v, blXMax, blYMax, blXMin,
						blYMin) {
						intersectPoints = ZExt_intAppendNoDuplicates(intersectPoints,
							ZExt_IntOrientedVertex{
								v: v,
							})
					}
				}
			case 1:
				{

					c.lsx = ZNumber(blXMax)
					c.lsy = ZNumber(blYMax)
					c.lex = c.lsx
					c.ley = ZNumber(blYMin)
					v := c.intGetIntersectionPoint_InfiniteLines()
					if v != nil && ZExt_intTskCheckBounds(v, blXMax, blYMax, blXMin,
						blYMin) {
						intersectPoints = ZExt_intAppendNoDuplicates(intersectPoints,
							ZExt_IntOrientedVertex{
								v: v,
							})
					}
				}
			case 2:
				{

					c.lsx = ZNumber(blXMin)
					c.lsy = ZNumber(blYMax)
					c.lex = ZNumber(blXMax)
					c.ley = c.lsy
					v := c.intGetIntersectionPoint_InfiniteLines()
					if v != nil && ZExt_intTskCheckBounds(v, blXMax, blYMax, blXMin,
						blYMin) {
						intersectPoints = ZExt_intAppendNoDuplicates(intersectPoints,
							ZExt_IntOrientedVertex{
								v: v,
							})
					}
				}
			case 3:
				{

					c.lsx = ZNumber(blXMin)
					c.lsy = ZNumber(blYMin)
					c.lex = ZNumber(blXMax)
					c.ley = c.lsy
					v := c.intGetIntersectionPoint_InfiniteLines()
					if v != nil && ZExt_intTskCheckBounds(v, blXMax, blYMax, blXMin,
						blYMin) {
						intersectPoints = ZExt_intAppendNoDuplicates(intersectPoints,
							ZExt_IntOrientedVertex{
								v: v,
							})
					}
				}
			}
			linesTried++
		}
		if len(intersectPoints) < 2 {

			Log.Verbose(2, "Couldn't determine point of intersection between partition line and solid internal blockmap bounding box (%d, %d). Falling back to legacy way of measuring length.\n",
				len(intersectPoints), linesTried)
			Log.Verbose(2, "part from linedef %d!%d+%d: (%v %v) - (%v %v) bbox: (%v %v) - (%v %v)\n",
				part.Linedef, part.getFlip(), part.Offset, c.psx, c.psy, c.pex,
				c.pey, blXMin, blYMax, blXMax, blYMin)
			for i := 0; i < len(intersectPoints); i++ {
				Log.Verbose(2, "Intersection#%d: %s",
					i, intersectPoints[i].toString())
			}

			return nil, nil
		}
		if ZExt_IntVertexPairCOrdering(intersectPoints[0].v, intersectPoints[1].v,
			false) {
			partStart, partEnd = intersectPoints[0], intersectPoints[1]
		} else {
			partStart, partEnd = intersectPoints[1], intersectPoints[0]
		}
	}
	return &partStart, &partEnd
}

func ZExt_intAppendNoDuplicates(x []ZExt_IntOrientedVertex, v ZExt_IntOrientedVertex) []ZExt_IntOrientedVertex {
	if len(x) == 0 || !x[0].equalTo(&v) {
		return append(x, v)
	}
	return x
}

func (c *ZExt_IntersectionContext) intGetIntersectionPoint_InfiniteLines() *ZExt_NodeVertex {
	ldx := float64(c.lex - c.lsx)
	ldy := float64(c.ley - c.lsy)
	d := float64(c.pdy)*ldx - ldy*float64(c.pdx)
	if d == 0.0 {
		return nil
	}
	c1 := float64(c.pdy)*float64(c.psx) - float64(c.pdx)*float64(c.psy)
	c2 := ldy*float64(c.lsx) - ldx*float64(c.lsy)
	id := 1 / d
	return &ZExt_NodeVertex{
		X: ZNumber(int(math.Round((ldx*c1 - float64(c.pdx)*c2) * id))),
		Y: ZNumber(int(math.Round((ldy*c1 - float64(c.pdy)*c2) * id))),
	}
}

func ZExt_intTskCheckBounds(v *ZExt_NodeVertex, blXMax, blYMax, blXMin, blYMin int) bool {
	return v.X <= ZNumber(blXMax) && v.X >= ZNumber(blXMin) &&
		v.Y <= ZNumber(blYMax) && v.Y >= ZNumber(blYMin)
}

func (c *ZExt_IntersectionContext) intGetIntersectionOrIndicence() (*ZExt_NodeVertex, *ZExt_NodeVertex) {
	ldx := float64(c.lex - c.lsx)
	ldy := float64(c.ley - c.lsy)
	d := float64(c.pdy)*ldx - ldy*float64(c.pdx)
	if d == 0.0 {
		ptmp := c.pdx*c.psy - c.psx*c.pdy
		a := c.pdy*c.lsx - c.pdx*c.lsy + ptmp
		b := c.pdy*c.lex - c.pdx*c.ley + ptmp
		if a == 0 && b == 0 {

			return &ZExt_NodeVertex{
					X: c.lsx,
					Y: c.lsy,
				}, &ZExt_NodeVertex{
					X: c.lex,
					Y: c.ley,
				}
		} else {

			return nil, nil
		}
	}
	c1 := float64(c.pdy)*float64(c.psx) - float64(c.pdx)*float64(c.psy)
	c2 := ldy*float64(c.lsx) - ldx*float64(c.lsy)
	id := 1 / d
	v := &ZExt_NodeVertex{
		X: ZNumber(int(math.Round((ldx*c1 - float64(c.pdx)*c2) * id))),
		Y: ZNumber(int(math.Round((ldy*c1 - float64(c.pdy)*c2) * id))),
	}

	if v != nil {
		var blXmax, blXmin, blYmax, blYmin int
		if c.lsx < c.lex {
			blXmax = int(c.lex)
			blXmin = int(c.lsx)
		} else {
			blXmax = int(c.lsx)
			blXmin = int(c.lex)
		}
		if c.lsy < c.ley {
			blYmax = int(c.ley)
			blYmin = int(c.lsy)
		} else {
			blYmax = int(c.lsy)
			blYmin = int(c.ley)
		}
		if ZExt_intTskCheckBounds(v, blXmax, blYmax, blXmin, blYmin) {
			return v, nil
		}
	}

	return nil, nil
}

type ZExt_IntVertexPairC struct {
	StartVertex *ZExt_NodeVertex
	EndVertex   *ZExt_NodeVertex
	len         ZNumber
}

func (s *ZExt_NodeSeg) toIntVertexPairC() ZExt_IntVertexPairC {

	if ZExt_IntVertexPairCOrdering(s.StartVertex, s.EndVertex, false) {
		return ZExt_IntVertexPairC{
			StartVertex: s.StartVertex,
			EndVertex:   s.EndVertex,
			len:         s.len,
		}
	} else {
		return ZExt_IntVertexPairC{
			StartVertex: s.EndVertex,
			EndVertex:   s.StartVertex,
			len:         s.len,
		}
	}
}

func ZExt_IntVertexPairCOrdering(a, b *ZExt_NodeVertex, strictLess bool) bool {

	if a.X == b.X {
		if a.Y < b.Y {
			return false
		} else {
			return !strictLess || a.Y > b.Y
		}
	} else if a.X < b.X {
		return true
	} else {
		return false
	}
}

type ZExt_IntCollinearVertexPairCByCoord []ZExt_IntVertexPairC

func (x ZExt_IntCollinearVertexPairCByCoord) Len() int { return len(x) }
func (x ZExt_IntCollinearVertexPairCByCoord) Less(i, j int) bool {

	return ZExt_IntVertexPairCOrdering(x[i].StartVertex, x[j].StartVertex, true)
}
func (x ZExt_IntCollinearVertexPairCByCoord) Swap(i, j int) { x[i], x[j] = x[j], x[i] }

func (x ZExt_IntCollinearVertexPairCByCoord) GetOverlapsLength() ZNumber {

	overlapLength := ZNumber(0)
	for i := 1; i < len(x); i++ {
		prevStart := x[i-1].StartVertex
		prevEnd := x[i-1].EndVertex
		curStart := x[i].StartVertex
		if ZExt_IntVertexPairCOrdering(curStart, prevEnd, true) &&
			ZExt_IntVertexPairCOrdering(prevStart, curStart, false) {

			dx := prevEnd.X - curStart.X
			dy := prevEnd.Y - curStart.Y
			overlapLength += ZNumber(math.Sqrt(float64(dx*dx) + float64(dy*dy)))
		}
	}
	return overlapLength
}

func (x ZExt_IntCollinearVertexPairCByCoord) toString() string {
	if len(x) == 0 {
		return "{EMPTY!}"
	}
	s := ""
	for i := 0; i < len(x); i++ {
		s += fmt.Sprintf("; [(%v;%v)-(%v;%v)]", x[i].StartVertex.X, x[i].StartVertex.Y,
			x[i].EndVertex.X, x[i].EndVertex.Y)
	}
	return s
}

type ZExt_IntOrientedVertex struct {
	v    *ZExt_NodeVertex
	left bool
}

func (ov1 *ZExt_IntOrientedVertex) equalTo(ov2 *ZExt_IntOrientedVertex) bool {
	if ov1 == nil || ov2 == nil {
		return false
	}
	if ov1.v == nil || ov2.v == nil {
		return false
	}
	return ov1.v.X == ov2.v.X && ov1.v.Y == ov2.v.Y && ov1.left == ov2.left
}

type ZExt_IntCollinearOrientedVertices []ZExt_IntOrientedVertex

func (x ZExt_IntCollinearOrientedVertices) Len() int { return len(x) }
func (x ZExt_IntCollinearOrientedVertices) Less(i, j int) bool {

	if x[i].v.X == x[j].v.X && x[i].v.Y == x[j].v.Y {
		if i == len(x)-1 {

			return false
		}
		return x[i].left && !x[j].left
	}
	return ZExt_IntVertexPairCOrdering(x[i].v, x[j].v, true)
}
func (x ZExt_IntCollinearOrientedVertices) Swap(i, j int) { x[i], x[j] = x[j], x[i] }

func (x ZExt_IntCollinearOrientedVertices) toString() string {
	s := "["
	for i := 0; i < len(x); i++ {
		s = s + fmt.Sprintf(",%s", x[i].toString())
	}
	if len(s) > 1 {
		s = "[" + s[2:]
	}
	return s + "]"
}

func (x *ZExt_IntOrientedVertex) toString() string {
	if x == nil {
		return "nil"
	}
	str := "RIGHT"
	if x.left {
		str = "LEFT"
	}
	return fmt.Sprintf("%s(%v,%v)", str, x.v.X, x.v.Y)
}

func ZExt_IntIsClockwiseTriangle(p1, p2, p3 *ZExt_NodeVertex) bool {
	sgn := (p2.X-p1.X)*(p3.Y-p1.Y) - (p2.Y-p1.Y)*(p3.X-p1.X)
	return sgn < 0
}

func (w *ZExt_NodesWork) buildSidenessCache(rootSeg *ZExt_NodeSeg, super *ZExt_Superblock) {
	start := time.Now()
	Log.Printf("[nodes] Building cache for sideness\n")
	lineCount := w.precomputeAliasesForCache(rootSeg, super)
	w.sidenessCache.maxKnownAlias = w.segAliasObj.maxAlias
	w.sidenessCache.colCount = int(lineCount)
	w.sidenessCache.data = make([]uint8,
		(w.sidenessCache.maxKnownAlias*w.sidenessCache.colCount)>>2+1)
	ZExt_setSidenessIdxForAll(super)
	w.computeAndLockSidenessCache(rootSeg, super)
	Log.Printf("[nodes] sideness cache took %s\n", time.Since(start))
}

func (w *ZExt_NodesWork) precomputeAliasesForCache(ts *ZExt_NodeSeg, super *ZExt_Superblock) int {
	var previousPart *ZExt_NodeSeg
	w.segAliasObj.UnvisitAll()
	var c ZExt_IntersectionContext
	lines := make(map[uint16]bool)
	for part := ts; part != nil; part = part.next {
		if part.partner != nil && part.partner == previousPart {

			continue
		}
		lines[part.Linedef] = true
		if part.alias != 0 {
			if w.segAliasObj.MarkAndRecall(part.alias) {

				continue
			}
		} else {

			part.alias = w.segAliasObj.Generate()

		}
		previousPart = part
		c.psx = part.psx
		c.psy = part.psy
		c.pex = part.pex
		c.pey = part.pey
		c.pdx = c.psx - c.pex
		c.pdy = c.psy - c.pey
		w.evalComputeAliasesWorker(super, part, &c)
	}
	cntLines := 0
	for range lines {
		cntLines++
	}
	return cntLines
}

func (w *ZExt_NodesWork) evalComputeAliasesWorker(block *ZExt_Superblock, part *ZExt_NodeSeg, c *ZExt_IntersectionContext) {

forblock:

	num := ZExt_BoxOnLineSide(block, part)
	if num < 0 {
		return
	} else if num > 0 {
		return
	}

	for check := block.segs; check != nil; check = check.nextInSuper {
		c.lsx = check.psx
		c.lsy = check.psy
		c.lex = check.pex
		c.ley = check.pey
		val := w.doLinesIntersect(c)
		if ((val&2 != 0) && (val&64 != 0)) || ((val&4 != 0) && (val&32 != 0)) {
		} else {
			if check == part || check == part.partner {
			} else {
				if (val&1 != 0) && (val&16 != 0) {

					if check.alias != part.alias && ZExt_vetAliasTransfer(c) {
						check.alias = part.alias
					}
				}
			}
		}
	}

	if block.subs[0] != nil {
		w.evalComputeAliasesWorker(block.subs[0], part, c)
	}

	if block.subs[1] != nil {
		block = block.subs[1]
		goto forblock
	}
}

func ZExt_setSidenessIdxForAll(super *ZExt_Superblock) {
	remap := make(map[uint16]int)
	gen := 0
	ZExt_evalSidenessIdx(super, remap, &gen)
}

func ZExt_evalSidenessIdx(block *ZExt_Superblock, remap map[uint16]int, gen *int) {
	for check := block.segs; check != nil; check = check.nextInSuper {
		val, ok := remap[check.Linedef]
		if !ok {
			val = *gen
			(*gen)++
			remap[check.Linedef] = val
		}
		check.sidenessIdx = val
	}
	for num := 0; num < 2; num++ {
		if block.subs[num] == nil {
			continue
		}

		ZExt_evalSidenessIdx(block.subs[num], remap, gen)
	}
}

func (w *ZExt_NodesWork) computeAndLockSidenessCache(ts *ZExt_NodeSeg, super *ZExt_Superblock) {
	if w.sidenessCache.readOnly {
		Log.Panic("Can't compute sideness cache - already locked (read only) (programmer error)")
		return
	}
	if w.sidenessCache.maxKnownAlias > 0 {
		var previousPart *ZExt_NodeSeg
		w.segAliasObj.UnvisitAll()
		var c ZExt_IntersectionContext
		for part := ts; part != nil; part = part.next {
			if ZExt_getGlobalFlip(part) {
				part.flags |= SEG_FLAG_GLOBAL_FLIP
			}
			if part.partner != nil && part.partner == previousPart {

				continue
			}
			if part.alias != 0 {
				if w.segAliasObj.MarkAndRecall(part.alias) {

					continue
				}
			} else {

				part.alias = w.segAliasObj.Generate()

			}
			previousPart = part
			c.psx = part.psx
			c.psy = part.psy
			c.pex = part.pex
			c.pey = part.pey
			c.pdx = c.psx - c.pex
			c.pdy = c.psy - c.pey
			w.evalComputeSidenessCache(super, part, &c)
		}
		w.segAliasObj.UnvisitAll()
	}
	w.sidenessCache.readOnly = true
}

func (w *ZExt_NodesWork) evalComputeSidenessCache(block *ZExt_Superblock, part *ZExt_NodeSeg, c *ZExt_IntersectionContext) {

forblock:

	num := ZExt_BoxOnLineSide(block, part)
	if num < 0 {

		w.storeEntireBlockSideness(block, part, SIDENESS_LEFT)
		return
	} else if num > 0 {

		w.storeEntireBlockSideness(block, part, SIDENESS_RIGHT)
		return
	}

	for check := block.segs; check != nil; check = check.nextInSuper {
		w.computeAndCacheSideness(part, check, c)
	}

	if block.subs[0] != nil {
		w.evalComputeSidenessCache(block.subs[0], part, c)
	}

	if block.subs[1] != nil {
		block = block.subs[1]
		goto forblock
	}
}

func (w *ZExt_NodesWork) storeEntireBlockSideness(block *ZExt_Superblock, part *ZExt_NodeSeg, sideness uint8) {

forblock:
	for check := block.segs; check != nil; check = check.nextInSuper {
		w.storeSidenessDirectly(part, check, sideness)
	}

	if block.subs[0] != nil {
		w.storeEntireBlockSideness(block.subs[0], part, sideness)
	}

	if block.subs[1] != nil {
		block = block.subs[1]
		goto forblock
	}
}

func (w *ZExt_NodesWork) WhichSideCached(part, check *ZExt_NodeSeg, c *ZExt_IntersectionContext) uint8 {
	if part.alias == 0 || part.alias > w.sidenessCache.maxKnownAlias ||
		(check.flags&SEG_FLAG_NOCACHE != 0) {

		return w.WhichSideInternal(check, c)
	}
	negate := part.flags&SEG_FLAG_GLOBAL_FLIP != 0

	cell := (part.alias-1)*w.sidenessCache.colCount + check.sidenessIdx
	mov := (cell & 0x3) << 1
	cell >>= 2
	raw := w.sidenessCache.data[cell]
	raw = raw >> mov & 0x3
	if negate {
		return flipVal(raw)
	}
	return raw
}

func (w *ZExt_NodesWork) computeAndCacheSideness(part, check *ZExt_NodeSeg, c *ZExt_IntersectionContext) {
	if part.alias == 0 || part.alias > w.sidenessCache.maxKnownAlias ||
		(check.flags&SEG_FLAG_NOCACHE != 0) {
		return
	}
	cell := (part.alias-1)*w.sidenessCache.colCount + check.sidenessIdx
	mov := (cell & 0x3) << 1
	cell >>= 2
	raw := w.sidenessCache.data[cell]
	subraw := raw >> mov & 0x3
	if subraw != 0 {
		return
	}
	newRaw := w.WhichSideInternal(check, c)
	negate := part.flags&SEG_FLAG_GLOBAL_FLIP != 0
	if negate {
		newRaw = flipVal(newRaw)
	}
	newRaw = newRaw << mov
	w.sidenessCache.data[cell] = raw | newRaw
}

func (w *ZExt_NodesWork) storeSidenessDirectly(part, check *ZExt_NodeSeg, sideness uint8) {
	if part.alias == 0 || part.alias > w.sidenessCache.maxKnownAlias ||
		(check.flags&SEG_FLAG_NOCACHE != 0) {
		return
	}
	cell := (part.alias-1)*w.sidenessCache.colCount + check.sidenessIdx
	mov := (cell & 0x3) << 1
	cell >>= 2
	raw := w.sidenessCache.data[cell]
	subraw := raw >> mov & 0x3
	if subraw != 0 {
		return
	}
	newRaw := sideness
	negate := part.flags&SEG_FLAG_GLOBAL_FLIP != 0
	if negate {
		newRaw = flipVal(newRaw)
	}
	newRaw = newRaw << mov
	w.sidenessCache.data[cell] = raw | newRaw
}

func ZExt_getGlobalFlip(part *ZExt_NodeSeg) bool {
	return ZExt_IntVertexPairCOrdering(part.StartVertex, part.EndVertex, false)
}

func (w *ZExt_NodesWork) WhichSideInternal(check *ZExt_NodeSeg, c *ZExt_IntersectionContext) uint8 {
	c.lsx = check.psx
	c.lsy = check.psy
	c.lex = check.pex
	c.ley = check.pey
	val := w.doLinesIntersect(c)
	if ((val&2 != 0) && (val&64 != 0)) || ((val&4 != 0) && (val&32 != 0)) {
		return SIDENESS_INTERSECT
	}
	if (val&1 != 0) && (val&16 != 0) {
		return SIDENESS_COLLINEAR
	}
	if val&34 != 0 {
		return SIDENESS_LEFT
	}
	if val&68 != 0 {
		return SIDENESS_RIGHT
	}
	return 0
}

type ZExt_RearrangeTracker struct {
	totals             *NodesTotals
	vertices           []ZExt_NodeVertex
	subsectors         []SubSector
	segs               []Seg
	deepSubsectors     []DeepSubSector
	deepSegs           []DeepSeg
	zdoomSubsectors    []uint32
	zdoomSegs          []ZdoomNode_Seg
	verticeRenumbering []int
	cntVerts           int
}

func (w *ZExt_NodesWork) RearrangeBSPVanilla(node *NodeInProcess) {
	if w.vertexExists > 0 {

		Log.Printf("RearrangeBSP: cancelled (vertex deduplication was in effect, correct rearrangement is not supported in this case)\n")
		return
	}
	if len(w.segs) > 32767 || len(w.subsectors) > 32767 {

		Log.Printf("RearrangeBSP: cancelled (segs / subsectors are out of guaranteed vanilla range).\n")
		return
	}
	track := &ZExt_RearrangeTracker{
		totals:     &NodesTotals{},
		segs:       make([]Seg, 0, cap(w.segs)),
		subsectors: make([]SubSector, 0, cap(w.subsectors)),
		vertices:   make([]ZExt_NodeVertex, 0, cap(w.vertices)),
	}

	if !w.identifyLinedefVertices(track) {
		Log.Printf("RearrangeBSP: cancelled because of incorrect linedef->vertice references\n")
		return
	}

	w.rearrangeVanilla(node, track)
	if w.totals.numSegs != track.totals.numSegs ||
		w.totals.numSSectors != track.totals.numSSectors ||
		track.cntVerts != w.lines.GetVerticesCount() {
		Log.Panic("RearrangeBSP: BSP structures length not identical after rearrangement segs:%d:%d ssectors:%d:%d verts:%d:%d\n",
			w.totals.numSegs, track.totals.numSegs,
			w.totals.numSSectors, track.totals.numSSectors,
			track.cntVerts, w.lines.GetVerticesCount())
	}
	w.segs = track.segs
	w.subsectors = track.subsectors
	w.vertices = track.vertices
	w.lines.RearrangeVertices(track.verticeRenumbering)
	Log.Printf("RearrangeBSP: done\n")
}

func (w *ZExt_NodesWork) rearrangeVanilla(node *NodeInProcess, track *ZExt_RearrangeTracker) {
	w.rearrangeVertices(node, track)
	if node.nextL != nil {
		w.rearrangeVanilla(node.nextL, track)
	} else {
		ssector := &(w.subsectors[node.LChild & ^w.SsectorMask])
		node.LChild = w.putSubsector(ssector, track) | w.SsectorMask
	}
	if node.nextR != nil {
		w.rearrangeVanilla(node.nextR, track)
	} else {
		ssector := &(w.subsectors[node.RChild & ^w.SsectorMask])
		node.RChild = w.putSubsector(ssector, track) | w.SsectorMask
	}
}

func (w *ZExt_NodesWork) rearrangeVertices(node *NodeInProcess, track *ZExt_RearrangeTracker) {
	ex, ok := w.stkExtra[node]
	if !ok {
		Log.Panic("rearrangeVertices: failed to retrieve extra information on node-in-process structure.\n")
	}

	for i := ex.vstart; i < ex.vend; i++ {
		if track.verticeRenumbering[i] >= 0 {
			Log.Panic("vertex already numbered\n")
		}
		track.verticeRenumbering[i] = track.cntVerts
		track.cntVerts++
	}
}

func (w *ZExt_NodesWork) putSubsector(ssector *SubSector, track *ZExt_RearrangeTracker) uint32 {
	idx := track.totals.numSSectors
	track.totals.numSSectors++
	v := w.putSegs(ssector, track)
	track.subsectors = append(track.subsectors, v)
	return uint32(idx)
}

func (w *ZExt_NodesWork) putSegs(orig *SubSector, track *ZExt_RearrangeTracker) SubSector {
	firstSeg := track.totals.numSegs
	for _, seg := range w.segs[orig.FirstSeg : orig.FirstSeg+orig.SegCount] {
		seg2 := seg
		w.renumberSegVertices(&seg2, track)
		track.segs = append(track.segs, seg2)
		track.totals.numSegs++
	}
	return SubSector{
		FirstSeg: uint16(firstSeg),
		SegCount: orig.SegCount,
	}
}

func (w *ZExt_NodesWork) renumberSegVertices(seg *Seg, track *ZExt_RearrangeTracker) {
	s := int(seg.StartVertex)
	e := int(seg.EndVertex)
	if track.verticeRenumbering[s] < 0 || track.verticeRenumbering[e] < 0 {
		Log.Panic("vertex was not renumbered at proper place (s: %d->%d, e: %d->%d)\n",
			s, track.verticeRenumbering[s], e, track.verticeRenumbering[e])
	}
	seg.StartVertex = uint16(track.verticeRenumbering[s])
	seg.EndVertex = uint16(track.verticeRenumbering[e])
}

func (w *ZExt_NodesWork) identifyLinedefVertices(track *ZExt_RearrangeTracker) bool {
	lcount := w.lines.Len()
	numVerts := len(w.vertices)
	translate := make([]int, numVerts)
	track.cntVerts = 0
	for i, _ := range translate {
		translate[i] = -1
	}
	for i := uint16(0); i < lcount; i++ {
		s, e := w.lines.GetLinedefVertices(i)
		if s >= numVerts || e >= numVerts {
			Log.Error("identifyLinedefVertices: Linedef %d has vertex out of range\n", i)
			return false
		}

		if translate[s] < 0 {
			track.cntVerts++
		}
		translate[s] = s

		if translate[e] < 0 {
			track.cntVerts++
		}
		translate[e] = e
	}

	for i := 0; i < track.cntVerts; i++ {
		if translate[i] != i {

			Log.Error("identifyLinedefVertices: failed assertion that vertices from linedefs must precede vertices from segs\n")
			return false
		}
	}
	for i := track.cntVerts; i < numVerts; i++ {
		if translate[i] != -1 {
			Log.Error("identifyLinedefVertices: wtf (programmer error)\n")
			return false
		}
	}
	Log.Printf("debug: cntVerts (original) == %d\n", track.cntVerts)
	track.verticeRenumbering = translate
	return true
}

func (w *ZExt_NodesWork) RearrangeBSPDeep(node *NodeInProcess) {
	if w.vertexExists > 0 {
		Log.Printf("RearrangeBSP: cancelled (vertex deduplication was in effect, correct rearrangement is not supported in this case)\n")
		return
	}

	track := &ZExt_RearrangeTracker{
		totals:         &NodesTotals{},
		deepSegs:       make([]DeepSeg, 0, cap(w.deepSegs)),
		deepSubsectors: make([]DeepSubSector, 0, cap(w.deepSubsectors)),
		vertices:       make([]ZExt_NodeVertex, 0, cap(w.vertices)),
	}
	if !w.identifyLinedefVertices(track) {
		Log.Printf("RearrangeBSP: cancelled because of incorrect linedef->vertice references\n")
		return
	}
	w.rearrangeDeep(node, track)
	if w.totals.numSegs != track.totals.numSegs ||
		w.totals.numSSectors != track.totals.numSSectors ||
		track.cntVerts != w.lines.GetVerticesCount() {
		Log.Panic("RearrangeBSP: BSP structures length not identical after rearrangement segs:%d:%d ssectors:%d:%d verts:%d:%d\n",
			w.totals.numSegs, track.totals.numSegs,
			w.totals.numSSectors, track.totals.numSSectors,
			track.cntVerts, w.lines.GetVerticesCount())
	}
	w.deepSegs = track.deepSegs
	w.deepSubsectors = track.deepSubsectors
	w.vertices = track.vertices
	w.lines.RearrangeVertices(track.verticeRenumbering)
	Log.Printf("RearrangeBSP: done\n")
}

func (w *ZExt_NodesWork) rearrangeDeep(node *NodeInProcess, track *ZExt_RearrangeTracker) {
	w.rearrangeVertices(node, track)
	if node.nextL != nil {
		w.rearrangeDeep(node.nextL, track)
	} else {
		ssector := &(w.deepSubsectors[node.LChild & ^w.SsectorMask])
		node.LChild = w.putDeepSubsector(ssector, track) | w.SsectorMask
	}
	if node.nextR != nil {
		w.rearrangeDeep(node.nextR, track)
	} else {
		ssector := &(w.deepSubsectors[node.RChild & ^w.SsectorMask])
		node.RChild = w.putDeepSubsector(ssector, track) | w.SsectorMask
	}
}

func (w *ZExt_NodesWork) putDeepSubsector(ssector *DeepSubSector,
	track *ZExt_RearrangeTracker) uint32 {
	idx := track.totals.numSSectors
	track.totals.numSSectors++
	v := w.putDeepSegs(ssector, track)
	track.deepSubsectors = append(track.deepSubsectors, v)
	return uint32(idx)
}

func (w *ZExt_NodesWork) putDeepSegs(orig *DeepSubSector,
	track *ZExt_RearrangeTracker) DeepSubSector {
	firstSeg := track.totals.numSegs
	view := w.deepSegs[orig.FirstSeg : orig.FirstSeg+uint32(orig.SegCount)]
	for _, seg := range view {
		seg2 := seg
		w.renumberDeepSegVertices(&seg2, track)
		track.deepSegs = append(track.deepSegs, seg2)
		track.totals.numSegs++
	}
	return DeepSubSector{
		FirstSeg: uint32(firstSeg),
		SegCount: orig.SegCount,
	}
}

func (w *ZExt_NodesWork) renumberDeepSegVertices(seg *DeepSeg,
	track *ZExt_RearrangeTracker) {
	s := int(seg.StartVertex)
	e := int(seg.EndVertex)
	if track.verticeRenumbering[s] < 0 || track.verticeRenumbering[e] < 0 {
		Log.Panic("vertex was not renumbered at proper place\n")
	}
	seg.StartVertex = uint32(track.verticeRenumbering[s])
	seg.EndVertex = uint32(track.verticeRenumbering[e])
}

func (w *ZExt_NodesWork) RearrangeBSPExtended(node *NodeInProcess) {
	if w.vertexExists > 0 {
		Log.Printf("RearrangeBSP: cancelled (vertex deduplication was in effect, correct rearrangement is not supported in this case)\n")
		return
	}

	track := &ZExt_RearrangeTracker{
		totals:          &NodesTotals{},
		zdoomSegs:       make([]ZdoomNode_Seg, 0, cap(w.zdoomSegs)),
		zdoomSubsectors: make([]uint32, 0, cap(w.zdoomSubsectors)),
	}
	w.initTranslateVectorForExtended(track)
	w.rearrangeExtended(node, track)
	if w.totals.numSegs != track.totals.numSegs ||
		w.totals.numSSectors != track.totals.numSSectors ||
		uint32(track.cntVerts) != (uint32(len(w.vertices))-
			w.zdoomVertexHeader.ReusedOriginalVertices) {
		Log.Panic("RearrangeBSP: BSP structures length not identical after rearrangement segs:%d:%d ssectors:%d:%d verts:%d:%d\n",
			w.totals.numSegs, track.totals.numSegs,
			w.totals.numSSectors, track.totals.numSSectors,
			track.cntVerts, uint32(len(w.vertices))-
				w.zdoomVertexHeader.ReusedOriginalVertices)
	}
	w.zdoomSegs = track.zdoomSegs
	w.zdoomSubsectors = track.zdoomSubsectors
	w.reallyRearrangeExtendedVertices(track.verticeRenumbering)
	Log.Printf("RearrangeBSP: done\n")
}

func (w *ZExt_NodesWork) rearrangeExtended(node *NodeInProcess, track *ZExt_RearrangeTracker) {
	w.rearrangeExtendedVertices(node, track)
	if node.nextL != nil {
		w.rearrangeExtended(node.nextL, track)
	} else {
		ssector := node.LChild & ^w.SsectorMask
		node.LChild = w.putExtendedSubsector(ssector, track) | w.SsectorMask
	}
	if node.nextR != nil {
		w.rearrangeExtended(node.nextR, track)
	} else {
		ssector := node.RChild & ^w.SsectorMask
		node.RChild = w.putExtendedSubsector(ssector, track) | w.SsectorMask
	}
}

func (w *ZExt_NodesWork) putExtendedSubsector(ssector uint32,
	track *ZExt_RearrangeTracker) uint32 {
	idx := track.totals.numSSectors
	track.totals.numSSectors++
	v := w.putExtendedSegs(ssector, track)
	track.zdoomSubsectors = append(track.zdoomSubsectors, v)
	return uint32(idx)
}

func (w *ZExt_NodesWork) putExtendedSegs(orig uint32,
	track *ZExt_RearrangeTracker) uint32 {
	oldFirstSeg := getFirstSegExtended(w.zdoomSubsectors, orig)
	view := w.zdoomSegs[oldFirstSeg : oldFirstSeg+w.zdoomSubsectors[orig]]
	a := uint32(0)
	for _, seg := range view {
		seg2 := seg
		w.renumberExtendedSegVertices(&seg2, track)
		track.zdoomSegs = append(track.zdoomSegs, seg2)
		track.totals.numSegs++
		a++
	}
	return w.zdoomSubsectors[orig]
}

func (w *ZExt_NodesWork) renumberExtendedSegVertices(seg *ZdoomNode_Seg,
	track *ZExt_RearrangeTracker) {
	reused := w.zdoomVertexHeader.ReusedOriginalVertices
	if seg.StartVertex >= reused {
		s := seg.StartVertex - reused
		if track.verticeRenumbering[s] < 0 {
			Log.Panic("vertex was not renumbered at proper place\n")
		}
		seg.StartVertex = uint32(track.verticeRenumbering[s]) + reused
	}
	if seg.EndVertex >= reused {
		e := seg.EndVertex - reused
		if track.verticeRenumbering[e] < 0 {
			Log.Panic("vertex was not renumbered at proper place\n")
		}
		seg.EndVertex = uint32(track.verticeRenumbering[e]) + reused
	}
}

func (w *ZExt_NodesWork) initTranslateVectorForExtended(track *ZExt_RearrangeTracker) {
	numVerts := uint32(len(w.vertices) -
		int(w.zdoomVertexHeader.ReusedOriginalVertices))
	translate := make([]int, numVerts)
	track.cntVerts = 0
	for i, _ := range translate {
		translate[i] = -1
	}
	track.verticeRenumbering = translate
}

func (w *ZExt_NodesWork) rearrangeExtendedVertices(node *NodeInProcess,
	track *ZExt_RearrangeTracker) {
	ex, ok := w.stkExtra[node]
	if !ok {
		Log.Panic("rearrangeExtendedVertices: failed to retrieve extra information on node-in-process structure.\n")
	}
	reused := w.zdoomVertexHeader.ReusedOriginalVertices
	dstart := ex.vstart - int64(reused)
	dend := ex.vend - int64(reused)
	for i := dstart; i < dend; i++ {
		if track.verticeRenumbering[i] >= 0 {
			Log.Panic("vertex already numbered\n")
		}
		track.verticeRenumbering[i] = track.cntVerts
		track.cntVerts++
	}
}

func (w *ZExt_NodesWork) reallyRearrangeExtendedVertices(translate []int) {
	reused := w.zdoomVertexHeader.ReusedOriginalVertices
	if len(translate) != (len(w.vertices) - int(reused)) {
		Log.Panic("reallyRearrangeExtendedVertices: failed translation (length don't match) %d != %d",
			len(translate), len(w.vertices)-int(reused))
	}
	newVertices := make([]ZExt_NodeVertex, len(w.vertices))
	for i, _ := range w.vertices[:reused] {
		newVertices[i] = w.vertices[i]
	}
	for i, _ := range w.vertices[reused:] {
		newVertices[uint32(translate[i])+reused] =
			w.vertices[uint32(i)+reused]
	}
	w.vertices = newVertices
}

type ZExt_StkQueue struct {
	tasks      []ZExt_StkQueueTask
	depthLimit int
	cur        int

	tmp *ZExt_StkQueueTask
}

type ZExt_StkQueueTask struct {
	action       int
	num          int
	parentNum    int
	isRightChild bool
	depth        int
	seg          *ZExt_NodeSeg
	box          *NodeBounds
	super        *ZExt_Superblock
	leftChild    int
	rightChild   int
	parts        []PartSeg
	node         *NodeInProcess
}

func ZExt_StkEntryPoint(w *ZExt_NodesWork, ts *ZExt_NodeSeg, bbox *NodeBounds, super *ZExt_Superblock) *NodeInProcess {
	w.stkExtra = make(map[*NodeInProcess]StkNodeExtraData, 0)
	w.parts = make([]*ZExt_NodeSeg, 0)

	queue := &ZExt_StkQueue{}

	queue.depthLimit = 7
	queue.Enqueue(STK_QUEUE_REGULARNODE, ts, bbox, super, false)

	return ZExt_StkCreateNode(w, ts, bbox, super, queue)
}

func ZExt_StkCreateNode(w *ZExt_NodesWork, ts *ZExt_NodeSeg, bbox *NodeBounds,
	super *ZExt_Superblock, queue *ZExt_StkQueue) *NodeInProcess {
	b := true
	var firstRes *NodeInProcess
	task := queue.Dequeue()
	for b {
		singleSectorMode := false
		if task != nil {
			ts = task.seg
			bbox = task.box
			super = task.super
			singleSectorMode = task.action == STK_QUEUE_SINGLE_NONCONVEX
		} else {
			b = false
		}
		res := new(NodeInProcess)
		if firstRes == nil {
			firstRes = res
		}
		var rights *ZExt_NodeSeg
		var lefts *ZExt_NodeSeg
		var rightsSuper *ZExt_Superblock
		var leftsSuper *ZExt_Superblock

		w.totals.numNodes++

		partsegs := make([]PartSeg, 0)

		vstart := int64(len(w.vertices))
		if singleSectorMode {
			w.stkDivisorSS(w, ts, &rights, &lefts, bbox, super, &rightsSuper,
				&leftsSuper, &partsegs)
		} else {
			w.DivideSegs(ts, &rights, &lefts, bbox, super, &rightsSuper,
				&leftsSuper, &partsegs)
		}
		vend := int64(len(w.vertices))
		super = nil
		res.X = int16(w.nodeX)
		res.Y = int16(w.nodeY)
		res.Dx = int16(w.nodeDx)
		res.Dy = int16(w.nodeDy)

		w.stkExtra[res] = StkNodeExtraData{
			vstart: vstart,
			vend:   vend,
			parts:  partsegs,
		}

		if task != nil {
			queue.SetResult(res, partsegs)
		}

		leftBox := ZExt_FindLimits(lefts)
		res.Lbox[BB_TOP] = int16(leftBox.Ymax)
		res.Lbox[BB_BOTTOM] = int16(leftBox.Ymin)
		res.Lbox[BB_LEFT] = int16(leftBox.Xmin)
		res.Lbox[BB_RIGHT] = int16(leftBox.Xmax)
		state := w.isItConvex(lefts)
		if state == CONVEX_SUBSECTOR {
			res.nextL = nil
			res.LChild = w.CreateSSector(lefts) | SSECTOR_DEEP_MASK
			w.returnSuperblockToPool(leftsSuper)
		} else if state == NONCONVEX_ONESECTOR {
			res.LChild = 0
			if !b || !queue.Enqueue(STK_QUEUE_SINGLE_NONCONVEX, lefts, leftBox,
				leftsSuper, false) {
				res.nextL = w.stkCreateNodeSS(w, lefts, leftBox, leftsSuper, nil)
			}
		} else {
			res.LChild = 0
			if config.VerbosityLevel >= 1 && w.IsConvexMultiSector(lefts) {
				w.dumpMultiSectorSegs(lefts)
			}
			if !b || !queue.Enqueue(STK_QUEUE_REGULARNODE, lefts, leftBox,
				leftsSuper, false) {
				res.nextL = ZExt_StkCreateNode(w, lefts, leftBox, leftsSuper, nil)
			}
		}
		leftsSuper = nil

		rightBox := ZExt_FindLimits(rights)
		res.Rbox[BB_TOP] = int16(rightBox.Ymax)
		res.Rbox[BB_BOTTOM] = int16(rightBox.Ymin)
		res.Rbox[BB_LEFT] = int16(rightBox.Xmin)
		res.Rbox[BB_RIGHT] = int16(rightBox.Xmax)
		state = w.isItConvex(rights)
		if state == CONVEX_SUBSECTOR {
			res.nextR = nil
			res.RChild = w.CreateSSector(rights) | SSECTOR_DEEP_MASK
			w.returnSuperblockToPool(rightsSuper)
		} else if state == NONCONVEX_ONESECTOR {
			res.RChild = 0
			if !b || !queue.Enqueue(STK_QUEUE_SINGLE_NONCONVEX, rights, rightBox,
				rightsSuper, true) {
				res.nextR = w.stkCreateNodeSS(w, rights, rightBox, rightsSuper, nil)
			}
		} else {
			res.RChild = 0
			if config.VerbosityLevel >= 1 && w.IsConvexMultiSector(rights) {
				w.dumpMultiSectorSegs(rights)
			}
			if !b || !queue.Enqueue(STK_QUEUE_REGULARNODE, rights, rightBox,
				rightsSuper, true) {
				res.nextR = ZExt_StkCreateNode(w, rights, rightBox, rightsSuper, nil)
			}
		}
		rightsSuper = nil
		task = queue.Dequeue()
		b = task != nil
	}
	return firstRes
}

func ZExt_StkCreateNodeForSingleSector(w *ZExt_NodesWork, ts *ZExt_NodeSeg, bbox *NodeBounds,
	super *ZExt_Superblock, queue *ZExt_StkQueue) *NodeInProcess {
	res := new(NodeInProcess)
	var rights *ZExt_NodeSeg
	var lefts *ZExt_NodeSeg
	var rightsSuper *ZExt_Superblock
	var leftsSuper *ZExt_Superblock

	w.totals.numNodes++
	partsegs := make([]PartSeg, 0)
	vstart := int64(len(w.vertices))
	w.DivideSegsForSingleSector(ts, &rights, &lefts, bbox, super, &rightsSuper,
		&leftsSuper, &partsegs)
	vend := int64(len(w.vertices))
	super = nil
	res.X = int16(w.nodeX)
	res.Y = int16(w.nodeY)
	res.Dx = int16(w.nodeDx)
	res.Dy = int16(w.nodeDy)
	w.stkExtra[res] = StkNodeExtraData{
		vstart: vstart,
		vend:   vend,
		parts:  partsegs,
	}

	leftBox := ZExt_FindLimits(lefts)
	res.Lbox[BB_TOP] = int16(leftBox.Ymax)
	res.Lbox[BB_BOTTOM] = int16(leftBox.Ymin)
	res.Lbox[BB_LEFT] = int16(leftBox.Xmin)
	res.Lbox[BB_RIGHT] = int16(leftBox.Xmax)
	if w.isItConvex(lefts) == CONVEX_SUBSECTOR {
		res.nextL = nil
		res.LChild = w.CreateSSector(lefts) | SSECTOR_DEEP_MASK
		w.returnSuperblockToPool(leftsSuper)
	} else {
		res.nextL = ZExt_StkCreateNodeForSingleSector(w, lefts, leftBox, leftsSuper, queue)
		res.LChild = 0
	}

	rightBox := ZExt_FindLimits(rights)
	res.Rbox[BB_TOP] = int16(rightBox.Ymax)
	res.Rbox[BB_BOTTOM] = int16(rightBox.Ymin)
	res.Rbox[BB_LEFT] = int16(rightBox.Xmin)
	res.Rbox[BB_RIGHT] = int16(rightBox.Xmax)
	if w.isItConvex(rights) == CONVEX_SUBSECTOR {
		res.nextR = nil
		res.RChild = w.CreateSSector(rights) | SSECTOR_DEEP_MASK
		w.returnSuperblockToPool(rightsSuper)
	} else {
		res.nextR = ZExt_StkCreateNodeForSingleSector(w, rights, rightBox, rightsSuper, queue)
		res.RChild = 0
	}

	return res
}

func ZExt_VigilantSingleSectorDivisor(w *ZExt_NodesWork, ts *ZExt_NodeSeg, rs **ZExt_NodeSeg, ls **ZExt_NodeSeg, bbox *NodeBounds, super *ZExt_Superblock, rightsSuper,
	leftsSuper **ZExt_Superblock, partsegs *[]PartSeg) {

	w.DivideSegsForSingleSector(ts, rs, ls, bbox, super, rightsSuper,
		leftsSuper, partsegs)
}

func ZExt_DefaultSingleSectorDivisor(w *ZExt_NodesWork, ts *ZExt_NodeSeg, rs **ZExt_NodeSeg, ls **ZExt_NodeSeg, bbox *NodeBounds, super *ZExt_Superblock, rightsSuper,
	leftsSuper **ZExt_Superblock, partsegs *[]PartSeg) {

	w.DivideSegs(ts, rs, ls, bbox, super, rightsSuper, leftsSuper, partsegs)
}

func (q *ZExt_StkQueue) lastDequeued() *ZExt_StkQueueTask {
	if len(q.tasks) == 0 || q.cur == 0 {
		return nil
	}
	return &(q.tasks[q.cur-1])
}

func (q *ZExt_StkQueue) depthChanged() {

}

func (q *ZExt_StkQueue) getDepth() int {
	if len(q.tasks) == 0 {
		return -1
	}
	return q.tasks[len(q.tasks)-1].depth
}

func (q *ZExt_StkQueue) Enqueue(action int, seg *ZExt_NodeSeg, box *NodeBounds,
	super *ZExt_Superblock, isRightChild bool) bool {
	if q == nil {

		return false
	}
	if q.depthLimit != -1 && q.getDepth()+1 > q.depthLimit {
		return false
	}
	num := len(q.tasks)
	q.tasks = append(q.tasks, ZExt_StkQueueTask{
		action: action,
		num:    num,

		isRightChild: isRightChild,

		seg:        seg,
		box:        box,
		super:      super,
		leftChild:  -1,
		rightChild: -1,
		parts:      nil,
	})
	parent := q.lastDequeued()
	nu := &(q.tasks[len(q.tasks)-1])
	if parent != nil {
		nu.parentNum = parent.num
		nu.depth = parent.depth + 1
		if isRightChild {
			parent.rightChild = num
		} else {
			parent.leftChild = num
		}
	} else {

		nu.parentNum = -1
		nu.depth = 0
	}
	return true
}

func (q *ZExt_StkQueue) Dequeue() *ZExt_StkQueueTask {
	if q == nil {
		return nil
	}
	if len(q.tasks) == 0 {
		return nil
	}
	if q.cur == len(q.tasks) {
		q.tmp = nil
		return nil
	}
	if q.tmp == nil {
		q.tmp = &ZExt_StkQueueTask{}

	} else {
		oldDepth := q.tmp.depth
		newDepth := q.tasks[q.cur].depth
		if oldDepth != newDepth {
			q.depthChanged()
		}
	}
	*(q.tmp) = q.tasks[q.cur]
	q.cur++
	return q.tmp
}

func (q *ZExt_StkQueue) SetResult(node *NodeInProcess, parts []PartSeg) {
	de := q.lastDequeued()
	de.node = node
	if de.parentNum >= 0 {
		if de.isRightChild {
			q.tasks[de.parentNum].node.nextR = node
		} else {
			q.tasks[de.parentNum].node.nextL = node
		}
	}

	de.parts = parts
}
func init() {
	ZNodesGenerator = ZExt_NodesGenerator
}
