// Code generated from other source files. DO NOT EDIT.
// Copyright (C) 2022, VigilantDoomer
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
	"math"
	"reflect"
	"runtime"
	"sort"
	"strconv"
	"time"
)

type ZExt_PickNodeFunc func(*ZExt_NodesWork, *ZExt_NodeSeg, *NodeBounds, *ZExt_Superblock) *ZExt_NodeSeg

type ZExt_CreateNodeSSFunc func(*ZExt_NodesWork, *ZExt_NodeSeg, *NodeBounds, *ZExt_Superblock) *NodeInProcess

type ZExt_NodesWork struct {
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
	sectorHits      []uint8
	incidental      []ZExt_VertexPairC
	solidMap        *Blockmap
	nonVoidCache    map[int]ZExt_NonVoidPerAlias
	blockity        *BlockityLines
	deepNodes       []DeepNode
	deepSubsectors  []DeepSubSector
	deepSegs        []DeepSeg
	SsectorMask     uint32
	blocksHit       []ZExt_BlocksHit
	diagonalPenalty int
	pickNodeFactor  int
	pickNodeUser    int
	minorIsBetter   MinorIsBetterFunc
	multipart       bool
	depthArtifacts  bool
	nodeType        int

	zdoomVertexHeader *ZdoomNode_VertexHeader
	zdoomVertices     []ZdoomNode_Vertex
	zdoomSubsectors   []uint32
	zdoomSegs         []ZdoomNode_Seg
}

type ZExt_NodeVertex struct {
	X   ZNumber
	Y   ZNumber
	idx uint32
}

type ZExt_NodeSeg struct {
	StartVertex        *ZExt_NodeVertex
	EndVertex          *ZExt_NodeVertex
	Angle              int16
	Linedef            uint16
	Flip               int16
	Offset             uint16
	next               *ZExt_NodeSeg
	partner            *ZExt_NodeSeg
	psx, psy, pex, pey ZNumber
	pdx, pdy, perp     ZNumber
	len                ZNumber
	sector             uint16
	secEquiv           uint16
	alias              int
	block              *ZExt_Superblock
	nextInSuper        *ZExt_NodeSeg
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
		},
		vertices:        intVertices,
		segAliasObj:     new(SegAliasHolder),
		pickNode:        ZExt_PickNodeFuncFromOption(input.pickNodeUser),
		createNodeSS:    ZExt_CreateNodeSSFromOption(input.pickNodeUser),
		sectorHits:      make([]byte, whichLen),
		incidental:      make([]ZExt_VertexPairC, 0),
		solidMap:        solidMap,
		nonVoidCache:    make(map[int]ZExt_NonVoidPerAlias),
		SsectorMask:     ssectorMask,
		blocksHit:       make([]ZExt_BlocksHit, 0),
		diagonalPenalty: input.diagonalPenalty,
		pickNodeFactor:  input.pickNodeFactor,
		pickNodeUser:    input.pickNodeUser,
		minorIsBetter:   MinorCmpFuncFromOption(input.minorIsBetterUser),
		multipart:       input.minorIsBetterUser == MINOR_CMP_DEPTH,
		depthArtifacts:  input.depthArtifacts,
		nodeType:        input.nodeType,
	}
	if input.nodeType == NODETYPE_ZDOOM_EXTENDED ||
		input.nodeType == NODETYPE_ZDOOM_COMPRESSED {
		workData.zdoomVertexHeader = &ZdoomNode_VertexHeader{
			ReusedOriginalVertices: uint32(input.lines.GetVerticesCount()),
		}
	}
	workData.segAliasObj.Init()
	initialSuper := workData.doInitialSuperblocks(rootBox)

	var rootNode *NodeInProcess
	if input.multiTreeMode == MULTITREE_NOTUSED {

		rootNode = ZExt_CreateNode(&workData, rootSeg, rootBox, initialSuper)
	} else if input.multiTreeMode == MULTITREE_ROOT_ONLY {

		rootNode = ZExt_MTPSentinel_MakeBestBSPTree(&workData, rootBox, initialSuper,
			input.specialRootMode)
	} else {
		Log.Panic("Multi-tree variant not implemented.\n")
	}

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
	Log.Printf("Height of left and right subtrees = (%d,%d)\n", hLeft, hRight)
	if input.nodeType == NODETYPE_VANILLA {
		workData.reverseNodes(rootNode)
	} else {

		workData.reverseDeepNodes(rootNode)
	}
	Log.Printf("Max seg count in subsector: %d\n", workData.totals.maxSegCountInSubsector)

	if uint32(workData.totals.numSSectors)&workData.SsectorMask == workData.SsectorMask {
		Log.Error("Number of subsectors is too large (%d) for this format. Lumps NODES, SEGS, SSECTORS will be emptied.\n",
			workData.totals.numSSectors)
		workData.emptyNodesLumps()
	} else if workData.tooManySegsCantFix() {
		Log.Error("Number of segs is too large (%d) for this format. Lumps NODES, SEGS, SSECTORS will be emptied.\n",
			len(workData.segs))
		workData.emptyNodesLumps()
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
	} else {
		if workData.deepNodes == nil {

			input.nodesChan <- NodesResult{}
		} else {

			input.nodesChan <- NodesResult{
				rawNodes: workData.getZdoomNodesBytes(),
			}
		}
	}
	Log.Printf("Nodes took %s\n", time.Since(start))
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
	default:
		{
			panic("Invalid argument")
		}
	}
}

func ZExt_CreateNodeSSFromOption(userOption int) ZExt_CreateNodeSSFunc {
	if userOption == PICKNODE_VISPLANE_ADV {
		return ZExt_CreateNodeForSingleSector
	}
	return ZExt_CreateNode
}

func (w *ZExt_NodesWork) emptyNodesLumps() {
	w.deepNodes = nil
	w.deepSegs = nil
	w.deepSubsectors = nil
	w.nodes = nil
	w.segs = nil
	w.subsectors = nil
}

func (w *ZExt_NodesWork) tooManySegsCantFix() bool {

	if w.segs == nil {
		return false
	}
	l := len(w.subsectors)
	if l == 0 {
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

		} else {
			Log.Printf("You almost exceeded vanilla addressable seg limit in SSECTORS - managed to reorder segs to avoid that. Changed subsector: %d\n",
				pivotSsector)
		}
	}

	return false
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

func (w *ZExt_NodesWork) fitSegsToTarget(maxSegIndex uint16) (bool, int) {
	newMaxSegIndex := uint32(len(w.segs)) - w.totals.maxSegCountInSubsector
	if newMaxSegIndex > uint32(maxSegIndex) {

		return false, 0
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

	if firstSdef != SIDEDEF_NONE {
		if !bytes.Equal(sidedefs[firstSdef].LoName[:], []byte("BSPNOSEG")) {

			if action == 1085 {
				Log.Verbose(1, "Will not create seg for front sidedef of linedef %d - instructed so by action's numeric code set to 1085 on linedef.\n",
					i)
			} else {
				lcs = ZExt_addSeg(myVertices, i, vidx1, vidx2, rootCs,
					&(sidedefs[firstSdef]), lastCs, horizon, bamEffect)
				lcs.Flip = 0
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
				rcs = ZExt_addSeg(myVertices, i, vidx2, vidx1, rootCs,
					&(sidedefs[secondSdef]), lastCs, horizon, bamEffect)
				rcs.Flip = 1
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

func ZExt_addSeg(vs []ZExt_NodeVertex, i uint16, vidx1, vidx2 int, rootCs **ZExt_NodeSeg, sdef *Sidedef, lastCs **ZExt_NodeSeg, horizon bool, bamEffect BAMEffect) *ZExt_NodeSeg {
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

func ZExt_computeAngle(dx, dy ZNumber) int {
	w := math.Atan2(float64(dy), float64(dx)) * float64(65536.0/(math.Pi*2))

	if w < 0 {
		w = 65536.0 + w
	}

	return int(w)
}

func ZExt_restoreSegOrder(segs []*ZExt_NodeSeg) {
	sort.Sort(ZExt_InitialSegOrderByLinedef(segs))
	for i, x := range segs {
		if i < len(segs)-1 {
			x.next = segs[i+1]
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
		(x[i].Linedef == x[j].Linedef && x[i].Flip < x[j].Flip)
}
func (x ZExt_InitialSegOrderByLinedef) Swap(i, j int) { x[i], x[j] = x[j], x[i] }

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
			r.Xmin = int(fv.X)
		}
		if fv.X > ZNumber(r.Xmax) {
			r.Xmax = fv.X.Ceil()
		}
		if fv.Y < ZNumber(r.Ymin) {
			r.Ymin = int(fv.Y)
		}
		if fv.Y > ZNumber(r.Ymax) {
			r.Ymax = fv.Y.Ceil()
		}

		if tv.X < ZNumber(r.Xmin) {
			r.Xmin = int(tv.X)
		}
		if tv.X > ZNumber(r.Xmax) {
			r.Xmax = tv.X.Ceil()
		}
		if tv.Y < ZNumber(r.Ymin) {
			r.Ymin = int(tv.Y)
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

	if seg.Flip == 0 {

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

		Log.Printf("Trouble in splitDist %d!%d %f,%f\n", seg.Linedef, seg.Flip, dx, dy)
	}
	t := math.Sqrt((dx * dx) + (dy * dy))
	return int(t)
}

func (c *ZExt_IntersectionContext) computeIntersection() (ZNumber, ZNumber) {
	dx := c.pex - c.psx
	dy := c.pey - c.psy
	dx2 := c.lex - c.lsx
	dy2 := c.ley - c.lsy

	if dx == 0 && dy == 0 {

		Log.Printf("Trouble in computeIntersection dx,dy\n")
	}
	if dx2 == 0 && dy2 == 0 {

		Log.Printf("Trouble in computeIntersection dx2,dy2\n")
	}

	l2 := dx2.Trunc(math.Sqrt(float64(dx2)*float64(dx2) + float64(dy2)*float64(dy2)))

	a := float64(dx)
	b := float64(dy)
	a2 := float64(dx2) / l2
	b2 := float64(dy2) / l2
	d := b*a2 - a*b2
	if d != 0 {
		w := ((a * float64(c.lsy-c.psy)) + (b * float64(c.psx-c.lsx))) / d

		a = float64(c.lsx) + (a2 * w)
		b = float64(c.lsy) + (b2 * w)
		return ZRoundToPrecision(a), ZRoundToPrecision(b)
	} else {

		return c.lsx, c.lsy
	}
}

func (c *ZExt_IntersectionContext) doLinesIntersect() uint8 {
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
			if l < ZWideNumber(4) {

				a = 0
			}
		}
		dx3 = c.lex - x
		dy3 = c.ley - y
		if dx3 == 0 && dy3 == 0 {
			b = 0
		} else {
			l := ZWideNumber(dx3)*ZWideNumber(dx3) + ZWideNumber(dy3)*ZWideNumber(dy3)
			if l < ZWideNumber(4) {
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
	if ts.Flip != 0 {
		sector = w.sides[w.lines.GetSidedefIndex(ts.Linedef, false)].Sector
	} else {
		sector = w.sides[w.lines.GetSidedefIndex(ts.Linedef, true)].Sector
	}
	if w.sectors[sector].Tag < 900 {
		line := ts.next
		for line != nil {
			var csector uint16
			if line.Flip != 0 {
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
				val := c.doLinesIntersect()
				if val&34 != 0 {
					if nonConvexityMode == NONCONVEX_ONESECTOR {
						Log.ZExt_DumpSegs(ts)

					}
					return nonConvexityMode
				}
			}
		}
	}

	return CONVEX_SUBSECTOR
}

func (w *ZExt_NodesWork) DivideSegs(ts *ZExt_NodeSeg, rs **ZExt_NodeSeg, ls **ZExt_NodeSeg, bbox *NodeBounds,
	super *ZExt_Superblock, rightsSuper, leftsSuper **ZExt_Superblock) {

	best := w.pickNode(w, ts, bbox, super)

	if best == nil {

		panic("Couldn't pick nodeline!")
	}

	w.nodeX = int(best.StartVertex.X)
	w.nodeY = int(best.StartVertex.Y)
	w.nodeDx = best.EndVertex.X.Ceil() - w.nodeX
	w.nodeDy = best.EndVertex.Y.Ceil() - w.nodeY

	c := &ZExt_IntersectionContext{
		psx: best.StartVertex.X,
		psy: best.StartVertex.Y,
		pex: best.EndVertex.X,
		pey: best.EndVertex.Y,
	}
	c.pdx = c.psx - c.pex
	c.pdy = c.psy - c.pey
	w.DivideSegsActual(ts, rs, ls, bbox, best, c, super, rightsSuper, leftsSuper)
}

func (w *ZExt_NodesWork) DivideSegsActual(ts *ZExt_NodeSeg, rs **ZExt_NodeSeg, ls **ZExt_NodeSeg, bbox *NodeBounds, best *ZExt_NodeSeg, c *ZExt_IntersectionContext, super *ZExt_Superblock, rightsSuper, leftsSuper **ZExt_Superblock) {

	var rights, lefts, strights, stlefts *ZExt_NodeSeg

	*rightsSuper = ZExt_NewSuperBlock()
	*leftsSuper = ZExt_NewSuperBlock()
	(*rightsSuper).SetBounds(bbox)
	(*leftsSuper).SetBounds(bbox)
	(*rightsSuper).InitSectorsIfNeeded(super)
	(*leftsSuper).InitSectorsIfNeeded(super)

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
				if tmps == best {
					tmps = tmps.next
				}
			}
		}
		tmpsNext = tmps.next

		ZExt_addSegToSide(addToRs[:], &rights, &strights)
		ZExt_addSegToSide(addToLs[:], &lefts, &stlefts)
	}

	if strights == nil {

		panic("Should have had partition at the right side (hardcoded to go there).")
	}

	if stlefts == nil {

		if asector > 0 {
			Log.Verbose(1, "No left side, moving partition into left side (single sector #%d)\n",
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
				Log.Verbose(1, "No left side, moving partition into left side (sectors %s)\n", s)
			} else {
				Log.Verbose(1, "No left side, moving partition into left side (multiple sectors in this node)\n")
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
		panic("Partnership was not kept up to date!")
	}
	c.lsx = atmps.StartVertex.X
	c.lsy = atmps.StartVertex.Y
	c.lex = atmps.EndVertex.X
	c.ley = atmps.EndVertex.Y
	val := c.doLinesIntersect()
	if ((val&2 != 0) && (val&64 != 0)) || ((val&4 != 0) && (val&32 != 0)) {

		x, y := c.computeIntersection()
		newVertex := w.AddVertex(x, y)
		news := new(ZExt_NodeSeg)
		atmps.alias = 0
		*news = *atmps
		atmps.next = news
		news.StartVertex = newVertex
		atmps.EndVertex = newVertex
		news.Offset = uint16(ZExt_splitDist(w.lines, news))
		w.totals.segSplits++
		if atmps.partner != nil {
			w.totals.segSplits++
			atmps.partner.alias = 0

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

		Log.Verbose(3, "Angle changed after splitting seg:  %d -> (%d ; %d)",
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

		isFront := s.Flip == 0
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
	w.DivideSegs(ts, &rights, &lefts, bbox, super, &rightsSuper, &leftsSuper)
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
		res.LChild = w.CreateSSector(lefts) | w.SsectorMask
	} else if state == NONCONVEX_ONESECTOR {
		res.nextL = w.createNodeSS(w, lefts, leftBox, leftsSuper)
		res.LChild = 0
	} else {
		res.nextL = ZExt_CreateNode(w, lefts, leftBox, leftsSuper)
		res.LChild = 0
	}

	rightBox := ZExt_FindLimits(rights)
	res.Rbox[BB_TOP] = int16(rightBox.Ymax)
	res.Rbox[BB_BOTTOM] = int16(rightBox.Ymin)
	res.Rbox[BB_LEFT] = int16(rightBox.Xmin)
	res.Rbox[BB_RIGHT] = int16(rightBox.Xmax)
	state = w.isItConvex(rights)
	if state == CONVEX_SUBSECTOR {
		res.nextR = nil
		res.RChild = w.CreateSSector(rights) | w.SsectorMask
	} else if state == NONCONVEX_ONESECTOR {
		res.nextR = w.createNodeSS(w, rights, rightBox, rightsSuper)
		res.RChild = 0
	} else {
		res.nextR = ZExt_CreateNode(w, rights, rightBox, rightsSuper)
		res.RChild = 0
	}

	return res
}

func (w *ZExt_NodesWork) reverseNodes(node *NodeInProcess) uint32 {
	if w.nodes == nil {
		w.nodes = make([]Node, w.totals.numNodes)
		w.nreverse = 0
	}
	if config.StraightNodes {

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

func (w *ZExt_NodesWork) convertNodesStraight(node *NodeInProcess, idx uint32) uint32 {
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

func (ni *NodesInput) ZExt_applySectorEquivalence(allSegs []*ZExt_NodeSeg, sectorEquiv []uint16) {
	for i, _ := range allSegs {
		allSegs[i].secEquiv = sectorEquiv[allSegs[i].sector]
	}
}

func (w *ZExt_NodesWork) doInitialSuperblocks(rootBox *NodeBounds) *ZExt_Superblock {
	ret := ZExt_NewSuperBlock()
	ret.SetBounds(rootBox)
	if w.pickNodeUser == PICKNODE_VISPLANE {
		ret.sectors = make([]uint16, 0)
		ret.secMap = make(map[uint16]bool)
	} else if w.pickNodeUser == PICKNODE_VISPLANE_ADV {
		ret.secEquivs = make([]uint16, 0)
		ret.secMap = make(map[uint16]bool)
	}
	for _, seg := range w.allSegs {
		ret.AddSegToSuper(seg)
	}
	return ret
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

func (n ZNumber) ToFixed16Dot16() int32 {
	return int32(float64(n) * FIXED16DOT16_MULTIPLIER)
}

func (w *ZExt_NodesWork) GetInitialStateClone() *ZExt_NodesWork {
	newW := new(ZExt_NodesWork)
	*newW = *w

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

	newW.segAliasObj = new(SegAliasHolder)
	newW.segAliasObj.Init()
	newW.sectorHits = make([]byte, len(w.sectorHits))
	newW.incidental = make([]ZExt_VertexPairC, 0)

	newW.nonVoidCache = make(map[int]ZExt_NonVoidPerAlias)
	newW.blocksHit = make([]ZExt_BlocksHit, 0)

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
			if cost < bestcost || (cost == bestcost &&
				minorIsBetter_Precious(minors, bestMinors)) {

				bestcost = cost
				best = part
				bestMinors = minors
			}
		}

	}

	return best
}

func (w *ZExt_NodesWork) evalPartitionWorker_Traditional(block *ZExt_Superblock, part *ZExt_NodeSeg, tot, diff, cost *int, bestcost int, minors *MinorCosts) bool {

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

				l := check.len

				d := ZNumber((ZWideNumber(l) * ZWideNumber(a)) / (ZWideNumber(a) - ZWideNumber(b)))
				if d >= 2 {

					if w.lines.IsTaggedPrecious(check.Linedef) {

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

				if w.lines.IsTaggedPrecious(check.Linedef) {
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

	for num := 0; num < 2; num++ {
		if block.subs[num] == nil {
			continue
		}

		if w.evalPartitionWorker_Traditional(block.subs[num], part, tot, diff,
			cost, bestcost, minors) {
			return true
		}
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
			!minorIsBetter_Precious(minors, bestMinors)) {
			continue
		}

		l := ZExt_GetPartitionLength_LegacyWay(part, bbox)

		if slen < l {
			cost += w.pickNodeFactor
			if cost > bestcost || (cost == bestcost &&
				!minorIsBetter_Precious(minors, bestMinors)) {
				continue
			}
		}

		bestcost = cost
		best = part
		bestMinors = minors
	}
	return best
}

func (w *ZExt_NodesWork) evalPartitionWorker_VisplaneKillough(block *ZExt_Superblock, part *ZExt_NodeSeg, tot, diff, cost *int, bestcost int, slen *ZNumber,
	minors *MinorCosts) bool {

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

				l := check.len

				d := ZNumber((ZWideNumber(l) * ZWideNumber(a)) / (ZWideNumber(a) - ZWideNumber(b)))
				if d >= 2 {

					if w.lines.IsTaggedPrecious(check.Linedef) {

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

				if w.lines.IsTaggedPrecious(check.Linedef) {
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

	for num := 0; num < 2; num++ {
		if block.subs[num] == nil {
			continue
		}

		if w.evalPartitionWorker_VisplaneKillough(block.subs[num], part, tot, diff,
			cost, bestcost, slen, minors) {
			return true
		}
	}

	return false
}

func ZExt_PickNode_visplaneVigilant(w *ZExt_NodesWork, ts *ZExt_NodeSeg, bbox *NodeBounds,
	super *ZExt_Superblock) *ZExt_NodeSeg {
	best := ts
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
		if (cost > bestcost) || (cost == bestcost && !w.minorIsBetter(minors, bestMinors)) {
			continue
		}

		if flat > 1 {

			if w.diagonalPenalty != 0 && (part.pdx != 0 && part.pdy != 0) {
				cost += w.diagonalPenalty
				if (cost > bestcost) || (cost == bestcost && !w.minorIsBetter(minors, bestMinors)) {
					continue
				}
			}

			l := w.GetPartitionLength_VigilantWay(part, bbox)
			if l == 0 {

				l = ZExt_GetFullPartitionLength(part, bbox)
				Log.Verbose(2, "Recomputing partition length the old way, because I got zero length doing it the new way.\n")
			}

			collinearSegs := ZExt_CollinearVertexPairCByCoord(w.incidental)

			sort.Sort(collinearSegs)

			slen = slen - collinearSegs.GetOverlapsLength()
			if slen < 0 {

				Log.Verbose(2, "Oops, got negative length after removing overlaps! Must have overflowed somewhere.\n")
				slen = 0
			}

			l = l / 2

			if slen < l {
				cost += w.pickNodeFactor
				if (cost > bestcost) || (cost == bestcost && !w.minorIsBetter(minors, bestMinors)) {
					continue
				}
			}
		}

		if !hasLeft && ZExt_VigilantGuard_IsBadPartition(part, ts, cnt) {
			cost += w.pickNodeFactor * ONESIDED_MULTIPLY
			if (cost > bestcost) || (cost == bestcost && !w.minorIsBetter(minors, bestMinors)) {
				continue
			}
		}

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
	}
	w.incidental = w.incidental[:0]
	if len(parts) > 1 {

		depthScores := ZExt_ZenSegMinorToDepthScores(parts)
		newSectorHits := make([]uint8, len(w.sectors))
		ZExt_ZenComputeScores(super, depthScores, newSectorHits, w.depthArtifacts)
		ZExt_ZenPickBestScore(depthScores)
		if depthScores[0].scoreSeg != VERY_BAD_SCORE {
			best = depthScores[0].seg
			tst1, tst2, tst3, tst4 := depthScores[0].preciousSplit,
				depthScores[0].scoreTotal,
				depthScores[0].equivSplit,
				depthScores[0].segSplit
			track := 1
			for i := 1; i < len(depthScores); i++ {
				if tst1 == depthScores[i].preciousSplit &&
					tst2 == depthScores[i].scoreTotal &&
					tst3 == depthScores[i].equivSplit &&
					tst4 == depthScores[i].segSplit {
					track++
				} else {
					break
				}
			}
			if track > 1 {

				Log.Verbose(4, "ZEN Ambiguity equal rank for %d records \n", track)
			}
		}
	}
	return best
}

func (w *ZExt_NodesWork) evalPartitionWorker_VisplaneVigilant(block *ZExt_Superblock, part *ZExt_NodeSeg, tot, diff, cost *int, bestcost int, slen *ZNumber,
	hasLeft *bool, minors *MinorCosts) bool {

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

				l := check.len

				d := ZNumber((ZWideNumber(l) * ZWideNumber(a)) / (ZWideNumber(a) - ZWideNumber(b)))
				if d >= 2 {

					if w.lines.IsTaggedPrecious(check.Linedef) {

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
				} else if ZExt_checkPorn1(l, d, check.pdx, part.pdx, check.pdy, part.pdy, b) {
					leftside = true
				}
			} else {

				if w.lines.IsTaggedPrecious(check.Linedef) {
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

				w.incidental = append(w.incidental, check.toVertexPairC())
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

	for num := 0; num < 2; num++ {
		if block.subs[num] == nil {
			continue
		}

		if w.evalPartitionWorker_VisplaneVigilant(block.subs[num], part, tot, diff,
			cost, bestcost, slen, hasLeft, minors) {
			return true
		}
	}

	return false
}

func ZExt_VigilantGuard_IsBadPartition(part, ts *ZExt_NodeSeg, cnt int) bool {

	c := &ZExt_IntersectionContext{
		psx: part.StartVertex.X,
		psy: part.StartVertex.Y,
		pex: part.EndVertex.X,
		pey: part.EndVertex.Y,
	}
	c.pdx = c.psx - c.pex
	c.pdy = c.psy - c.pey
	tot := 0
	diff := cnt

	for check := ts; check != nil; check = check.next {

		leftside := false
		c.lsx = check.StartVertex.X
		c.lsy = check.StartVertex.Y
		c.lex = check.EndVertex.X
		c.ley = check.EndVertex.Y
		val := c.doLinesIntersect()
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

func ZExt_GetFullPartitionLength(part *ZExt_NodeSeg, bbox *NodeBounds) ZNumber {
	var l ZNumber
	if part.pdx == 0 {
		l = ZNumber(bbox.Ymax - bbox.Ymin)
	} else if part.pdy == 0 {
		l = ZNumber(bbox.Xmax - bbox.Xmin)
	} else {
		var c ZExt_IntersectionContext
		partSegCoords := part.toVertexPairC()
		c.psx = partSegCoords.StartVertex.X
		c.psy = partSegCoords.StartVertex.Y
		c.pex = partSegCoords.EndVertex.X
		c.pey = partSegCoords.EndVertex.Y
		c.pdx = c.pex - c.psx
		c.pdy = c.pey - c.psy
		contextStart, contextEnd := ZExt_PartitionInBoundary(part, &c, bbox.Xmax,
			bbox.Ymax, bbox.Xmin, bbox.Ymin, partSegCoords)
		if contextStart == nil {

			return ZExt_GetPartitionLength_LegacyWay(part, bbox)
		}
		fullXDiff := float64(contextStart.v.X) - float64(contextEnd.v.X)
		fullYDiff := float64(contextEnd.v.Y) - float64(contextStart.v.Y)
		l = ZNumber(math.Round(math.Sqrt(fullXDiff*fullXDiff + fullYDiff*fullYDiff)))
	}
	return l
}

func (w *ZExt_NodesWork) GetPartitionLength_VigilantWay(part *ZExt_NodeSeg, bbox *NodeBounds) ZNumber {

	if part.alias == 0 {

		Log.Verbose(2, "What? Alias should not be zero here. Falling back to old way of computing partition length. (Programmer error)")
		return ZExt_GetFullPartitionLength(part, bbox)
	}
	nonVoidStruc, ok := w.nonVoidCache[part.alias]
	if !ok {

		nonVoidStruc = w.ComputeNonVoid(part)
		w.nonVoidCache[part.alias] = nonVoidStruc
	}
	if !nonVoidStruc.success {
		return ZExt_GetFullPartitionLength(part, bbox)
	}

	contextStart, contextEnd := ZExt_PartitionInBoundary(part, &(nonVoidStruc.c), bbox.Xmax, bbox.Ymax, bbox.Xmin, bbox.Ymin,
		nonVoidStruc.partSegCoords)
	if contextStart == nil || contextEnd == nil {

		Log.Verbose(2, "This cannot be! Got so far but now failing (got all the segments of line on the map to see when it goes through the void and when it does not, but failed to determine the edges of line touching the current node's bounding box)\n")
		return ZExt_GetFullPartitionLength(part, bbox)
	}

	if contextStart.equalTo(contextEnd) {
		Log.Verbose(2, "Partition line seems to have zero length inside the node box (%v,%v)-(%v,%v) in (%v,%v,%v,%v) yielded (%v,%v)-(%v,%v).\n",
			part.StartVertex.X, part.StartVertex.Y, part.EndVertex.X, part.EndVertex.Y,
			bbox.Xmax, bbox.Ymax, bbox.Xmin, bbox.Ymin,
			contextStart.v.X, contextStart.v.Y, contextEnd.v.X, contextEnd.v.Y)
		return 0
	}

	nonVoid := nonVoidStruc.data

	hitStart := -1
	hitStop := -1

	for i := 0; i < len(nonVoid); i++ {

		if ZExt_AreOverlapping(contextStart.v, contextEnd.v, nonVoid[i].StartVertex,
			nonVoid[i].EndVertex) {
			hitStart = i
			break
		}
	}
	for i := len(nonVoid) - 1; i >= 0; i-- {

		if ZExt_AreOverlapping(contextStart.v, contextEnd.v, nonVoid[i].StartVertex,
			nonVoid[i].EndVertex) {
			hitStop = i
			break
		}
	}
	if hitStart < 0 || hitStop < 0 {

		Log.Verbose(2, nonVoidStruc.original.toString())
		Log.Verbose(2, "More dropouts! %v %v %s [%s-%s]\n", hitStart, hitStop,
			ZExt_CollinearVertexPairCByCoord(nonVoid).toString(), contextStart.toString(),
			contextEnd.toString())
		return ZExt_GetFullPartitionLength(part, bbox)
	}

	L := ZNumber(0)
	for i := hitStart; i <= hitStop; i++ {
		thisStart := nonVoid[i].StartVertex
		thisEnd := nonVoid[i].EndVertex

		precomputedInvalid := false
		if i == hitStart {
			if !ZExt_VertexPairCOrdering(contextStart.v, nonVoid[i].StartVertex, false) {
				thisStart = contextStart.v
			}
			precomputedInvalid = true
		}
		if i == hitStop {
			if ZExt_VertexPairCOrdering(contextEnd.v, nonVoid[i].EndVertex, false) {
				thisEnd = contextEnd.v
			}
			precomputedInvalid = true
		}
		if !precomputedInvalid {
			L = L + nonVoid[i].len
		} else {
			dx := float64(thisEnd.X - thisStart.X)
			dy := float64(thisEnd.Y - thisStart.Y)
			L = L + ZNumber(math.Sqrt(dx*dx+dy*dy))
		}
	}
	return L
}

func ZExt_subPickNode_fast(w *ZExt_NodesWork, ts *ZExt_NodeSeg, bbox *NodeBounds,
	super *ZExt_Superblock, cnt int) *ZExt_NodeSeg {
	var previousPart *ZExt_NodeSeg
	var bestH, bestV *ZExt_NodeSeg
	oldDistH := ZNumber(-1)
	oldDistV := ZNumber(-1)

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

				l := check.len

				d := ZNumber((ZWideNumber(l) * ZWideNumber(a)) / (ZWideNumber(a) - ZWideNumber(b)))
				if d >= 2 {

					if w.lines.IsTaggedPrecious(check.Linedef) {
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

	for num := 0; num < 2; num++ {
		if block.subs[num] == nil {
			continue
		}

		if w.evalPartitionWorker_Maelstrom(block.subs[num], part, tot, diff,
			cost, bestcost) {
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

	partSegCoords := part.toVertexPairC()
	var c ZExt_IntersectionContext
	c.psx = partSegCoords.StartVertex.X
	c.psy = partSegCoords.StartVertex.Y
	c.pex = partSegCoords.EndVertex.X
	c.pey = partSegCoords.EndVertex.Y
	c.pdx = c.pex - c.psx
	c.pdy = c.pey - c.psy

	partStart, partEnd := ZExt_PartitionInBoundary(part, &c, blXMax, blYMax, blXMin,
		blYMin, partSegCoords)
	if partStart == nil || partEnd == nil {

		return ZExt_NonVoidPerAlias{
			success: false,
		}
	}

	partStart.left = true
	partEnd.left = false

	pts := ZExt_CollinearOrientedVertices(make([]ZExt_OrientedVertex, 1))
	pts[0] = *partStart
	if w.blockity == nil {
		w.blockity = GetBlockityLines(w.solidMap)
	}
	iter := w.blockity

	iter.SetContext(int(partStart.v.X), int(partStart.v.Y), int(partEnd.v.X), int(partEnd.v.Y))
	for iter.NextLine() {
		aline := iter.GetLine()
		lsx, lsy, lex, ley := w.lines.GetAllXY(aline)
		c.lsx = ZNumber(lsx)
		c.lsy = ZNumber(lsy)
		c.lex = ZNumber(lex)
		c.ley = ZNumber(ley)
		if c.lsx == c.lex && c.lsy == c.ley {
			continue
		}
		pt1, pt2 := c.getIntersectionOrIndicence()
		if pt1 != nil {
			if pt2 != nil {

				var ov1, ov2 ZExt_OrientedVertex
				if ZExt_VertexPairCOrdering(pt1, pt2, false) {
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
			} else {

				ov := ZExt_OrientedVertex{
					v: pt1,
					left: ZExt_IsClockwiseTriangle(&ZExt_NodeVertex{X: c.lsx, Y: c.lsy},
						&ZExt_NodeVertex{X: c.lex, Y: c.ley}, partStart.v),
				}
				pts = append(pts, ov)
			}
		}
	}

	pts = append(pts, *partEnd)

	sort.Stable(pts)
	if pts[0] != *partStart || pts[len(pts)-1] != *partEnd {
		Log.Verbose(2, "Failed to produce a solid hits array %t %t (%v, %v) != (%v, %v).\n",
			pts[0] != *partStart, pts[len(pts)-1] != *partEnd,
			pts[len(pts)-1].v.X, pts[len(pts)-1].v.Y,
			partEnd.v.X, partEnd.v.Y)

		return ZExt_NonVoidPerAlias{
			success: false,
		}
	}

	ptsOld := append(ZExt_CollinearOrientedVertices([]ZExt_OrientedVertex{}), pts...)
	if !pts.Coalesce() {

		Log.Verbose(2, "Coalesce fail at %s\n", ptsOld.toString())
		return ZExt_NonVoidPerAlias{
			success: false,
		}
	}

	fluger := true
	for i := 2; i < len(pts)-2; i++ {
		fluger = fluger && (pts[i].left != pts[i-1].left)
	}
	if !fluger {

		Log.Verbose(2, "Sanity check failed! Evaluated partition line (%v,%v)-(%v,%v) doesn't consistently go in/out of the void when crossing solid lines. %s\n",
			part.psx, part.psy, part.pex, part.pey, pts.toString())
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
		dx := float64(pts[i].v.X - pts[i-1].v.X)
		dy := float64(pts[i].v.Y - pts[i-1].v.Y)
		nonVoid = append(nonVoid, ZExt_VertexPairC{
			StartVertex: pts[i-1].v,
			EndVertex:   pts[i].v,
			len:         ZNumber(math.Sqrt(dx*dx + dy*dy)),
		})
	}

	return ZExt_NonVoidPerAlias{
		data: nonVoid,

		success:       len(nonVoid) > 0,
		c:             c,
		partSegCoords: partSegCoords,
		original:      ptsOld,
	}
}

func ZExt_PartitionInBoundary(part *ZExt_NodeSeg, c *ZExt_IntersectionContext, blXMax, blYMax, blXMin, blYMin int, partSegCoords ZExt_VertexPairC) (*ZExt_OrientedVertex, *ZExt_OrientedVertex) {

	var partStart, partEnd ZExt_OrientedVertex
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
		intersectPoints := make([]ZExt_OrientedVertex, 0)
		for len(intersectPoints) < 2 && linesTried < 4 {
			switch linesTried {
			case 0:
				{

					c.lsx = ZNumber(blXMin)
					c.lsy = ZNumber(blYMax)
					c.lex = c.lsx
					c.ley = ZNumber(blYMin)
					v := c.getIntersectionPoint_InfiniteLines()
					if v != nil && ZExt_tskCheckBounds(v, blXMax, blYMax, blXMin,
						blYMin) {
						intersectPoints = ZExt_appendNoDuplicates(intersectPoints,
							ZExt_OrientedVertex{
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
					v := c.getIntersectionPoint_InfiniteLines()
					if v != nil && ZExt_tskCheckBounds(v, blXMax, blYMax, blXMin,
						blYMin) {
						intersectPoints = ZExt_appendNoDuplicates(intersectPoints,
							ZExt_OrientedVertex{
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
					v := c.getIntersectionPoint_InfiniteLines()
					if v != nil && ZExt_tskCheckBounds(v, blXMax, blYMax, blXMin,
						blYMin) {
						intersectPoints = ZExt_appendNoDuplicates(intersectPoints,
							ZExt_OrientedVertex{
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
					v := c.getIntersectionPoint_InfiniteLines()
					if v != nil && ZExt_tskCheckBounds(v, blXMax, blYMax, blXMin,
						blYMin) {
						intersectPoints = ZExt_appendNoDuplicates(intersectPoints,
							ZExt_OrientedVertex{
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
				part.Linedef, part.Flip, part.Offset, c.psx, c.psy, c.pex,
				c.pey, blXMin, blYMax, blXMax, blYMin)
			for i := 0; i < len(intersectPoints); i++ {
				Log.Verbose(2, "Intersection#%d: %s",
					i, intersectPoints[i].toString())
			}

			return nil, nil
		}
		if ZExt_VertexPairCOrdering(intersectPoints[0].v, intersectPoints[1].v,
			false) {
			partStart, partEnd = intersectPoints[0], intersectPoints[1]
		} else {
			partStart, partEnd = intersectPoints[1], intersectPoints[0]
		}
	}
	return &partStart, &partEnd
}

func ZExt_appendNoDuplicates(x []ZExt_OrientedVertex, v ZExt_OrientedVertex) []ZExt_OrientedVertex {
	if len(x) == 0 || !x[0].equalTo(&v) {
		return append(x, v)
	}
	return x
}

func (c *ZExt_IntersectionContext) getIntersectionPoint_InfiniteLines() *ZExt_NodeVertex {
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

func ZExt_tskCheckBounds(v *ZExt_NodeVertex, blXMax, blYMax, blXMin, blYMin int) bool {
	return v.X <= ZNumber(blXMax) && v.X >= ZNumber(blXMin) &&
		v.Y <= ZNumber(blYMax) && v.Y >= ZNumber(blYMin)
}

func (c *ZExt_IntersectionContext) getIntersectionOrIndicence() (*ZExt_NodeVertex, *ZExt_NodeVertex) {
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
		if ZExt_tskCheckBounds(v, blXmax, blYmax, blXmin, blYmin) {
			return v, nil
		}
	}

	return nil, nil
}

type ZExt_VertexPairC struct {
	StartVertex *ZExt_NodeVertex
	EndVertex   *ZExt_NodeVertex
	len         ZNumber
}

func (s *ZExt_NodeSeg) toVertexPairC() ZExt_VertexPairC {

	if ZExt_VertexPairCOrdering(s.StartVertex, s.EndVertex, false) {
		return ZExt_VertexPairC{
			StartVertex: s.StartVertex,
			EndVertex:   s.EndVertex,
			len:         s.len,
		}
	} else {
		return ZExt_VertexPairC{
			StartVertex: s.EndVertex,
			EndVertex:   s.StartVertex,
			len:         s.len,
		}
	}
}

func ZExt_VertexPairCOrdering(a, b *ZExt_NodeVertex, strictLess bool) bool {

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

func ZExt_ProjectsLeftToVPCOrdering(a, b *ZExt_NodeVertex) bool {
	if a.X == b.X {
		if a.Y < b.Y {
			return false
		} else {
			return true
		}
	} else if a.X < b.X {
		return false
	} else {
		return true
	}
}

type ZExt_CollinearVertexPairCByCoord []ZExt_VertexPairC

func (x ZExt_CollinearVertexPairCByCoord) Len() int { return len(x) }
func (x ZExt_CollinearVertexPairCByCoord) Less(i, j int) bool {

	return ZExt_VertexPairCOrdering(x[i].StartVertex, x[j].StartVertex, true)
}
func (x ZExt_CollinearVertexPairCByCoord) Swap(i, j int) { x[i], x[j] = x[j], x[i] }

func (x ZExt_CollinearVertexPairCByCoord) GetOverlapsLength() ZNumber {

	overlapLength := ZNumber(0)
	for i := 1; i < len(x); i++ {
		prevStart := x[i-1].StartVertex
		prevEnd := x[i-1].EndVertex
		curStart := x[i].StartVertex
		if ZExt_VertexPairCOrdering(curStart, prevEnd, true) &&
			ZExt_VertexPairCOrdering(prevStart, curStart, false) {

			dx := prevEnd.X - curStart.X
			dy := prevEnd.Y - curStart.Y
			overlapLength += ZNumber(math.Sqrt(float64(dx*dx) + float64(dy*dy)))
		}
	}
	return overlapLength
}

func ZExt_AreOverlapping(seg1Start, seg1End, seg2Start, seg2End *ZExt_NodeVertex) bool {
	return (ZExt_VertexPairCOrdering(seg1Start, seg2Start, false) &&
		ZExt_VertexPairCOrdering(seg2Start, seg1End, false)) ||
		(ZExt_VertexPairCOrdering(seg1Start, seg2End, false) &&
			ZExt_VertexPairCOrdering(seg2End, seg1End, false)) ||
		(ZExt_VertexPairCOrdering(seg2Start, seg1Start, false) &&
			ZExt_VertexPairCOrdering(seg1Start, seg2End, false) &&
			ZExt_VertexPairCOrdering(seg2Start, seg1End, false) &&
			ZExt_VertexPairCOrdering(seg1End, seg2End, false))
}

func (x ZExt_CollinearVertexPairCByCoord) toString() string {
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

type ZExt_OrientedVertex struct {
	v    *ZExt_NodeVertex
	left bool
}

func (ov1 *ZExt_OrientedVertex) equalTo(ov2 *ZExt_OrientedVertex) bool {
	if ov1 == nil || ov2 == nil {
		return false
	}
	if ov1.v == nil || ov2.v == nil {
		return false
	}
	return ov1.v.X == ov2.v.X && ov1.v.Y == ov2.v.Y && ov1.left == ov2.left
}

type ZExt_CollinearOrientedVertices []ZExt_OrientedVertex

func (x ZExt_CollinearOrientedVertices) Len() int { return len(x) }
func (x ZExt_CollinearOrientedVertices) Less(i, j int) bool {

	if x[i].v.X == x[j].v.X && x[i].v.Y == x[j].v.Y {
		if i == len(x)-1 {

			return false
		}
		return x[i].left && !x[j].left
	}
	return ZExt_VertexPairCOrdering(x[i].v, x[j].v, true)
}
func (x ZExt_CollinearOrientedVertices) Swap(i, j int) { x[i], x[j] = x[j], x[i] }

func (x ZExt_CollinearOrientedVertices) toString() string {
	s := "["
	for i := 0; i < len(x); i++ {
		s = s + fmt.Sprintf(",%s", x[i].toString())
	}
	if len(s) > 1 {
		s = "[" + s[2:]
	}
	return s + "]"
}

func (x *ZExt_CollinearOrientedVertices) Coalesce() bool {
	allOk := true
	l := len(*x)
	for i := 1; i < l; i++ {

		if (*x)[i].valuesEqualWithEpsilon(&((*x)[i-1])) {
			p, hasDominant, dominantDir := x.seekPastEqualsAndNoteSign(&((*x)[i-1]), i)
			dropLen := p - i + 1
			dropStart := i

			hasDominant = hasDominant && !x.matchToPrev(i, p, dominantDir)
			if hasDominant {

				(*x)[i-1].left = dominantDir
			} else {

				if (i > 1) && (p < l-1) {
					dropLen++
					dropStart--
				}
			}
			l = l - dropLen
			for j := dropStart; j < l; j++ {
				(*x)[j] = (*x)[j+dropLen]
			}
			i = dropStart - 1
			*x = (*x)[:l]
		}
	}
	a := (*x)[:l]
	*x = a
	return allOk
}

func (x *ZExt_CollinearOrientedVertices) matchToPrev(i, p int, dominantDir bool) bool {

	if i-2 > 0 {
		prevDir := (*x)[i-2].left
		if prevDir == dominantDir {

			for j := i - 1; j <= p; j++ {
				if (*x)[j].left != dominantDir {
					return true
					break
				}
			}
		}
	}
	return false
}

func (ov1 *ZExt_OrientedVertex) valuesEqualWithEpsilon(ov2 *ZExt_OrientedVertex) bool {
	if ov1 == nil || ov2 == nil {
		return false
	}
	if ov1.v == nil || ov2.v == nil {
		return false
	}
	XequalWithEpsilon := Abs(int(ov1.v.X-ov2.v.X)) <= 1
	YequalWithEpsilon := Abs(int(ov1.v.Y-ov2.v.Y)) <= 1
	return XequalWithEpsilon && YequalWithEpsilon
}

func (x *ZExt_CollinearOrientedVertices) seekPastEqualsAndNoteSign(ov *ZExt_OrientedVertex, startPos int) (int, bool, bool) {
	l := len(*x)
	a := startPos
	leftCnt := 0
	rightCnt := 0
	if ov.left {
		leftCnt++
	} else {
		rightCnt++
	}
	for i := startPos; i < l; i++ {
		if !((*x)[i].valuesEqualWithEpsilon(ov)) {
			break
		}
		if (*x)[i].left {
			leftCnt++
		} else {
			rightCnt++
		}
	}
	hasDominant := leftCnt != rightCnt
	dominantDir := hasDominant && (leftCnt > rightCnt)
	return a, hasDominant, dominantDir
}

func (x *ZExt_OrientedVertex) toString() string {
	if x == nil {
		return "nil"
	}
	str := "RIGHT"
	if x.left {
		str = "LEFT"
	}
	return fmt.Sprintf("%s(%v,%v)", str, x.v.X, x.v.Y)
}

type ZExt_NonVoidPerAlias struct {
	data          []ZExt_VertexPairC
	success       bool
	c             ZExt_IntersectionContext
	partSegCoords ZExt_VertexPairC
	original      ZExt_CollinearOrientedVertices
}

func ZExt_IsClockwiseTriangle(p1, p2, p3 *ZExt_NodeVertex) bool {
	sgn := (p2.X-p1.X)*(p3.Y-p1.Y) - (p2.Y-p1.Y)*(p3.X-p1.X)
	return sgn < 0
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
		&leftsSuper)
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
		res.LChild = w.CreateSSector(lefts) | w.SsectorMask
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
		res.RChild = w.CreateSSector(rights) | w.SsectorMask
	} else {
		res.nextR = ZExt_CreateNodeForSingleSector(w, rights, rightBox, rightsSuper)
		res.RChild = 0
	}

	return res
}

func (w *ZExt_NodesWork) DivideSegsForSingleSector(ts *ZExt_NodeSeg, rs **ZExt_NodeSeg, ls **ZExt_NodeSeg, bbox *NodeBounds, super *ZExt_Superblock, rightsSuper,
	leftsSuper **ZExt_Superblock) {

	best := ZExt_PickNode_SingleSector(w, ts, bbox, super)

	if best == nil {
		panic("Couldn't pick nodeline!")
	}

	w.nodeX = int(best.StartVertex.X)
	w.nodeY = int(best.StartVertex.Y)
	w.nodeDx = int(best.EndVertex.X) - w.nodeX
	w.nodeDy = int(best.EndVertex.Y) - w.nodeY

	c := &ZExt_IntersectionContext{
		psx: best.StartVertex.X,
		psy: best.StartVertex.Y,
		pex: best.EndVertex.X,
		pey: best.EndVertex.Y,
	}
	c.pdx = c.psx - c.pex
	c.pdy = c.psy - c.pey
	w.DivideSegsActual(ts, rs, ls, bbox, best, c, super, rightsSuper, leftsSuper)
}

func ZExt_PickNode_SingleSector(w *ZExt_NodesWork, ts *ZExt_NodeSeg, bbox *NodeBounds,
	super *ZExt_Superblock) *ZExt_NodeSeg {
	best := ts
	bestcost := int(INITIAL_BIG_COST)
	cnt := 0

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
			val := c.doLinesIntersect()
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
					if val&64 != 0 {

						rightcnt++
					}
					if (val&1 != 0) && (val&16 != 0) {
						check.alias = part.alias
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
	super *ZExt_Superblock, rootChoiceMethod int) *NodeInProcess {
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
	for i := 0; i < workerCount; i++ {
		workerChans[i] = make(chan ZExt_MTPWorker_Input)
		workerReplyChans[i] = make(chan ZExt_MTPWorker_Result)

		go ZExt_MTPWorker_GenerateBSPTrees(workerChans[i], workerReplyChans[i])
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
	return bestResult.bspTree
}

func ZExt_MTP_IsBSPTreeBetter(oldResult *ZExt_MTPWorker_Result, newResult *ZExt_MTPWorker_Result) bool {

	oldSsectorsCnt := oldResult.workData.totals.numSSectors
	newSSectorsCnt := newResult.workData.totals.numSSectors
	if newSSectorsCnt < oldSsectorsCnt {
		return true
	} else if newSSectorsCnt > oldSsectorsCnt {
		return false
	}

	oldLeft, oldRight := getBSPHeights(oldResult.bspTree)
	newLeft, newRight := getBSPHeights(newResult.bspTree)
	oldDiff := oldRight - oldLeft
	if oldDiff < 0 {
		oldDiff = -oldDiff
	}
	newDiff := newRight - newLeft
	if newDiff < 0 {
		newDiff = -newDiff
	}

	if newDiff < oldDiff {
		return true
	} else if newDiff > oldDiff {
		return false
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
		return true
	} else if newMaxHeight > oldMaxHeight {
		return false
	}

	oldSegCnt := oldResult.workData.totals.numSegs
	newSegCnt := newResult.workData.totals.numSegs
	if newSegCnt < oldSegCnt {
		return true
	} else if newSegCnt > oldSegCnt {
		return false
	}

	if newResult.id < oldResult.id {
		return true
	}
	return false
}

func ZExt_MTPSentinel_Clone(w *ZExt_NodesWork, bbox *NodeBounds) (*ZExt_NodesWork, *NodeBounds) {
	clonedWorkData := w.GetInitialStateClone()
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
		res.LChild = w.CreateSSector(lefts) | w.SsectorMask
	} else if state == NONCONVEX_ONESECTOR {
		res.nextL = w.createNodeSS(w, lefts, leftBox, leftsSuper)
		res.LChild = 0
	} else {
		res.nextL = ZExt_CreateNode(w, lefts, leftBox, leftsSuper)
		res.LChild = 0
	}

	rightBox := ZExt_FindLimits(rights)
	res.Rbox[BB_TOP] = int16(rightBox.Ymax)
	res.Rbox[BB_BOTTOM] = int16(rightBox.Ymin)
	res.Rbox[BB_LEFT] = int16(rightBox.Xmin)
	res.Rbox[BB_RIGHT] = int16(rightBox.Xmax)
	state = w.isItConvex(rights)
	if state == CONVEX_SUBSECTOR {
		res.nextR = nil
		res.RChild = w.CreateSSector(rights) | w.SsectorMask
	} else if state == NONCONVEX_ONESECTOR {
		res.nextR = w.createNodeSS(w, rights, rightBox, rightsSuper)
		res.RChild = 0
	} else {
		res.nextR = ZExt_CreateNode(w, rights, rightBox, rightsSuper)
		res.RChild = 0
	}
	return res
}

func (w *ZExt_NodesWork) MTP_DivideSegs(ts *ZExt_NodeSeg, rs **ZExt_NodeSeg, ls **ZExt_NodeSeg, bbox *NodeBounds, pseudoSuper *ZExt_Superblock, rightsSuper, leftsSuper **ZExt_Superblock, chosen *ZExt_NodeSeg) {
	best := chosen

	w.nodeX = int(best.StartVertex.X)
	w.nodeY = int(best.StartVertex.Y)
	w.nodeDx = best.EndVertex.X.Ceil() - w.nodeX
	w.nodeDy = best.EndVertex.Y.Ceil() - w.nodeY

	c := &ZExt_IntersectionContext{
		psx: best.StartVertex.X,
		psy: best.StartVertex.Y,
		pex: best.EndVertex.X,
		pey: best.EndVertex.Y,
	}
	c.pdx = c.psx - c.pex
	c.pdy = c.psy - c.pey
	w.DivideSegsActual(ts, rs, ls, bbox, best, c, pseudoSuper, rightsSuper, leftsSuper)
}

func (w *ZExt_NodesWork) AddVertex(x, y ZNumber) *ZExt_NodeVertex {
	idx := len(w.vertices)

	w.vertices = append(w.vertices, ZExt_NodeVertex{
		X:   x,
		Y:   y,
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
		w.zdoomSegs = append(w.zdoomSegs, ZdoomNode_Seg{
			StartVertex: uint32(tmps.StartVertex.idx),
			EndVertex:   uint32(tmps.EndVertex.idx),
			Linedef:     tmps.Linedef,
			Flip:        byte(tmps.Flip),
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

func (log *MyLogger) ZExt_DumpSegs(ts *ZExt_NodeSeg) {
	if !config.DumpSegsFlag || ts == nil {
		return
	}

	allSector := ts.sector
	log.segs.WriteString(fmt.Sprintf("Sector #%d:\n", allSector))
	for tmps := ts; tmps != nil; tmps = tmps.next {
		log.segs.WriteString(fmt.Sprintf(
			"  Linedef: %d Flip: %d (%f,%f) - (%f, %f)",
			tmps.Linedef, tmps.Flip, tmps.StartVertex.X, tmps.StartVertex.Y,
			tmps.EndVertex.X, tmps.EndVertex.Y))
		if tmps.sector != allSector {

			log.segs.WriteString(fmt.Sprintf(" BAD! Sector = %d\n", tmps.sector))
		} else {
			log.segs.WriteString("\n")
		}
	}
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

	secMap map[uint16]bool
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
			if !block.secMap[sec] {
				block.sectors = append(block.sectors, sec)
				block.secMap[sec] = true
			}
		} else if block.secEquivs != nil {
			sec := seg.secEquiv
			if !block.secMap[sec] {
				block.secEquivs = append(block.secEquivs, sec)
				block.secMap[sec] = true
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
			sub := ZExt_NewSuperBlock()
			block.subs[child] = sub
			sub.parent = block
			sub.InitSectorsIfNeeded(block)

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

func ZExt_NewSuperBlock() *ZExt_Superblock {
	return &ZExt_Superblock{}
}

func (s *ZExt_Superblock) InitSectorsIfNeeded(template *ZExt_Superblock) {
	if template.sectors != nil {
		s.sectors = make([]uint16, 0)
	}
	if template.secEquivs != nil {
		s.secEquivs = make([]uint16, 0)
	}
	if template.secMap != nil {
		s.secMap = make(map[uint16]bool)
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
		x1:     s.x1,
		y1:     s.x1,
		x2:     s.x2,
		y2:     s.y2,
		subs:   [2]*ZExt_Superblock{nil, nil},
		segs:   nil,
	}
	res.InitSectorsIfNeeded(s)
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
	} else if int64(part.pdx)*int64(part.pdy) > 0 {
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

type ZExt_DepthScoreBundle struct {
	seg           *ZExt_NodeSeg
	preciousSplit int
	equivSplit    int
	segSplit      int
	scoreSeg      int
	scoreSector   int
	scoreTotal    int
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

	return x[i].seg.Linedef < x[j].seg.Linedef
}
func (x ZExt_DepthScoresByTotal) Swap(i, j int) { x[i], x[j] = x[j], x[i] }

func ZExt_ZenPickBestScore(sc []ZExt_DepthScoreBundle) {
	sort.Sort(ZExt_DepthScoresBySeg(sc))
	rank := 0
	for i := 0; i < len(sc); i++ {
		sc[i].scoreSeg = rank
		if i < len(sc)-1 && sc[i].scoreSeg != sc[i+1].scoreSeg {
			rank++
		}
	}
	sort.Sort(ZExt_DepthScoresBySector(sc))
	rank = 0
	for i := 0; i < len(sc); i++ {
		sc[i].scoreSector += rank
		if i < len(sc)-1 && sc[i].scoreSector != sc[i+1].scoreSector {
			rank++
		}
	}
	sort.Sort(ZExt_DepthScoresByTotal(sc))

}

func ZExt_ZenComputeScores(super *ZExt_Superblock, sc []ZExt_DepthScoreBundle, sectorHits []uint8, depthArtifacts bool) {
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

		ZExt_evalPartitionWorker_Zen(super, &(sc[i]), &inter, sectorHits)
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

		sc[i].scoreSeg = (inter.segL + inter.segS) * (inter.segR + inter.segS)
		sc[i].scoreSector = (inter.sectorL + inter.sectorS) * (inter.sectorR + inter.sectorS)

		if sc[i].scoreSeg == 0 {

			sc[i].scoreSeg = VERY_BAD_SCORE
			sc[i].scoreSector = VERY_BAD_SCORE
			continue
		}

		if inter.segS > 0 {

			tmp := ZEN_X1 * inter.segS
			if ZEN_X2 < tmp {
				sc[i].scoreSeg = ZEN_X2 * sc[i].scoreSeg / tmp
			}
			if depthArtifacts {

				sc[i].scoreSeg -= (ZEN_X3*inter.segS*(inter.segS/3) +
					ZEN_X4) * inter.segS
			} else {
				sc[i].scoreSeg -= (ZEN_X3*inter.segS + ZEN_X4) * inter.segS
			}
		} else {

			sc[i].scoreSeg = 0x7FFFFFFF - Abs(inter.segL-
				inter.segR)
		}

		if depthArtifacts {

			if inter.sectorS > 0 {
				tmp := ZEN_X1 * inter.sectorS
				if ZEN_X2 < tmp {
					sc[i].scoreSector = ZEN_X2 * sc[i].scoreSector / tmp
				}

				sc[i].scoreSector -= (ZEN_X3*inter.sectorS + ZEN_X4) * inter.segS
			} else {

				sc[i].scoreSector = 0x7FFFFFFF - Abs(inter.sectorL-
					inter.sectorR)
			}
		} else {

			if inter.sectorS > 0 {
				tmp := ZEN_Y1 * inter.sectorS
				if ZEN_Y2 < tmp {
					sc[i].scoreSector = ZEN_Y2 * sc[i].scoreSector / tmp
				}

				sc[i].scoreSector -= (ZEN_Y3*inter.sectorS + ZEN_Y4) * inter.sectorS
			} else {

				sc[i].scoreSector = 0x7FFFFFFF - Abs(inter.sectorL-
					inter.sectorR)
			}
		}
	}
}

func ZExt_evalPartitionWorker_Zen(block *ZExt_Superblock, rec *ZExt_DepthScoreBundle, intermediate *ZenIntermediary, sectorHits []uint8) {
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
		c := &ZExt_IntersectionContext{
			psx: part.StartVertex.X,
			psy: part.StartVertex.Y,
			pex: part.EndVertex.X,
			pey: part.EndVertex.Y,
		}
		c.pdx = c.psx - c.pex
		c.pdy = c.psy - c.pey
		c.lsx = check.StartVertex.X
		c.lsy = check.StartVertex.Y
		c.lex = check.EndVertex.X
		c.ley = check.EndVertex.Y
		val := c.doLinesIntersect()
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
				if val&64 != 0 {

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

		ZExt_evalPartitionWorker_Zen(block.subs[num], rec, intermediate,
			sectorHits)
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

func init() {
	ZNodesGenerator = ZExt_NodesGenerator
}
