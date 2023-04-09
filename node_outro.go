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
	"encoding/binary"
)

// node_outro.go contains functions called after BSP tree is built (and in
// case of multi-tree modes, the best one is chosen), but needs to be written
// in the way lump format specifies, and perhaps fit under limits

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
