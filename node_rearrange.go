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

// This unit attempts to fix order of certain structures in BSP tree built
// partially breadth-first with stknode.go, so SSECTORS, SEGS, VERTEXES lumps
// could match byte-for-byte results obtained by building BSP tree the
// traditional depth-first way. Uses are:
// 1. Determinism in hard multi-tree mode when number of threads > 1
// TODO determinism in multi-tree mode is better guaranteed by some other
// mechanism. It is too hard to write this stuff to work in all cases, like
// segs having to move under limit, or Zdoom extended/compressed nodes with
// their float grid that might indeed select coordinates depending on order
// 2. Verification via building wad in single-tree mode with --stknode debug
// parameter and without it, both with determinism - file should match
// 3. Avoid slowdowns in game engine caused by irregular layout of structures

// FIXME currently, cannot succeed and is aborted if duplicate vertices
// were prevented from occuring. It needs to succeed 100% of the time in order
// for determinism + hard multi-tree setting combination to work

// NOTE hard multi-tree itself is not yet implemented, however, it being
// completed requires the above FIXME note to be addressed

// TODO might be impossible to guarantee determinism for hard multi-tree with
// multiple threads for Zdoom extended and compressed nodes format

type RearrangeTracker struct {
	totals             *NodesTotals
	vertices           []NodeVertex
	subsectors         []SubSector
	firstSegs          []int // used instead of subsectors[i].FirstSeg
	segs               []Seg // game format for actual SEGS lump
	deepSubsectors     []DeepSubSector
	deepSegs           []DeepSeg
	zdoomSubsectors    []uint32
	zdoomSegs          []ZdoomNode_Seg
	verticeRenumbering []int // index is original index, value is new index
	cntVerts           int
	vertexCache        map[SimpleVertex]int
	vertexMap          *VertexMap
}

// RearrangeBSPVanilla tries to arrange the BSP tree built with this method
// so that the wad output matches what the traditional method (recursion) would
// produce. Only for levels within vanilla seg and subsector count limits,
// though
func (w *NodesWork) RearrangeBSPVanilla(node *NodeInProcess,
	pristineVertexCache map[SimpleVertex]int,
	pristineVertexMap *VertexMap) {

	track := &RearrangeTracker{
		totals:      &NodesTotals{},
		segs:        make([]Seg, 0, cap(w.segs)),
		subsectors:  make([]SubSector, 0, cap(w.subsectors)),
		firstSegs:   make([]int, len(w.subsectors)),
		vertices:    make([]NodeVertex, 0, cap(w.vertices)),
		vertexCache: pristineVertexCache,
		vertexMap:   pristineVertexMap,
	}
	if !w.identifyLinedefVertices(track) {
		Log.Printf("RearrangeBSP: cancelled because of incorrect linedef->vertice references\n")
		return
	}
	firstSeg := 0
	for i := range w.subsectors {
		track.firstSegs[i] = firstSeg
		firstSeg += int(w.subsectors[i].SegCount)
	}
	w.rearrangeVertices(node, track)
	// Recursively arrange subsectors, segs and vertices created specifically
	// for segs according to node tree traversal order, which should match how
	// these structures would be arranged by default CreateNode function
	// originating from BSP v5.2
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

func (w *NodesWork) rearrangeVanilla(node *NodeInProcess, track *RearrangeTracker) {
	if node.nextL != nil {
		w.rearrangeVanilla(node.nextL, track)
	} else {
		ssidx := node.LChild & ^SSECTOR_DEEP_MASK
		ssector := &(w.subsectors[ssidx])
		node.LChild = w.putSubsector(ssector, track, ssidx) | SSECTOR_DEEP_MASK
	}
	if node.nextR != nil {
		w.rearrangeVanilla(node.nextR, track)
	} else {
		ssidx := node.RChild & ^SSECTOR_DEEP_MASK
		ssector := &(w.subsectors[ssidx])
		node.RChild = w.putSubsector(ssector, track, ssidx) | SSECTOR_DEEP_MASK
	}
}

func (w *NodesWork) rearrangeVertices(node *NodeInProcess, track *RearrangeTracker) {
	ex, ok := w.stkExtra[node]
	if !ok {
		Log.Panic("rearrangeVertices: failed to retrieve extra information on node-in-process structure.\n")
	}
	for i := ex.vstart; i < ex.vend; i++ {
		v := w.vertexSink[i]
		if track.verticeRenumbering[v] >= 0 {
			continue
		}
		siv := SimpleVertex{
			X: int(w.vertices[v].X),
			Y: int(w.vertices[v].Y),
		}

		v2, ex := track.vertexCache[siv]
		if ex {
			track.verticeRenumbering[v] = v2
		} else {
			track.vertexCache[siv] = track.cntVerts
			track.verticeRenumbering[v] = track.cntVerts
			track.cntVerts++
			track.vertices = append(track.vertices, w.vertices[v])
		}
	}

	if node.nextL != nil {
		w.rearrangeVertices(node.nextL, track)
	}
	if node.nextR != nil {
		w.rearrangeVertices(node.nextR, track)
	}
}

func (w *NodesWork) putSubsector(ssector *SubSector, track *RearrangeTracker,
	ssIdx uint32) uint32 {
	idx := track.totals.numSSectors
	track.totals.numSSectors++
	v := w.putSegs(ssector, track, ssIdx)
	track.subsectors = append(track.subsectors, v)
	return uint32(idx)
}

func (w *NodesWork) putSegs(orig *SubSector, track *RearrangeTracker,
	ssIdx uint32) SubSector {
	firstSeg := track.totals.numSegs
	for _, seg := range w.segs[track.firstSegs[ssIdx] : track.firstSegs[ssIdx]+
		int(orig.SegCount)] {
		seg2 := seg // copy struct data - will be changed
		w.renumberSegVertices(&seg2, track)
		track.segs = append(track.segs, seg2)
		track.totals.numSegs++
	}
	return SubSector{
		FirstSeg: uint16(firstSeg),
		SegCount: orig.SegCount,
	}
}

func (w *NodesWork) renumberSegVertices(seg *Seg, track *RearrangeTracker) {
	s := int(seg.StartVertex)
	e := int(seg.EndVertex)
	if track.verticeRenumbering[s] < 0 || track.verticeRenumbering[e] < 0 {
		Log.Panic("vertex was not renumbered at proper place (s: %d->%d, e: %d->%d)\n",
			s, track.verticeRenumbering[s], e, track.verticeRenumbering[e])
	}
	seg.StartVertex = uint16(track.verticeRenumbering[s])
	seg.EndVertex = uint16(track.verticeRenumbering[e])
}

func (w *NodesWork) identifyLinedefVertices(track *RearrangeTracker) bool {
	lcount := w.lines.Len()
	numVerts := len(w.vertices)
	translate := make([]int, numVerts)
	track.cntVerts = 0
	for i, _ := range translate { // Unmark all vertices
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
		translate[s] = s // translate to self

		if translate[e] < 0 { // s might be == e so it has to be done in this order
			track.cntVerts++
		}
		translate[e] = e // translate to self
	}
	// track.cntVerts now contains only linedef vertices
	for i := 0; i < track.cntVerts; i++ {
		if translate[i] != i {
			// NOTE we are expecting all of them to precede newly created vertices!
			// (WritableLines.PruneUnusedVertices() and then the nodebuilding process
			// is obligated to provide such guarantee)
			// Otherwise we'll have a GRAVE error
			// Let's cancel gracefully, though
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

	for i := 0; i < track.cntVerts; i++ {
		track.vertices = append(track.vertices, w.vertices[i])
	}
	return true
}

// RearrangeBSPDeep - see RearrangeBSPVanilla's description
func (w *NodesWork) RearrangeBSPDeep(node *NodeInProcess,
	pristineVertexCache map[SimpleVertex]int,
	pristineVertexMap *VertexMap) {
	// FIXME Mixing uint32 and int for indexing vertices
	// Investigate impact - might need to build map that requires vertice indices
	// larger than int32, in 32-bit address space, maybe we have trouble
	// earlier?
	track := &RearrangeTracker{
		totals:         &NodesTotals{},
		deepSegs:       make([]DeepSeg, 0, cap(w.deepSegs)),
		deepSubsectors: make([]DeepSubSector, 0, cap(w.deepSubsectors)),
		vertices:       make([]NodeVertex, 0, cap(w.vertices)),
		vertexCache:    pristineVertexCache,
		vertexMap:      pristineVertexMap,
	}
	if !w.identifyLinedefVertices(track) {
		Log.Printf("RearrangeBSP: cancelled because of incorrect linedef->vertice references\n")
		return
	}
	w.rearrangeVertices(node, track)
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

func (w *NodesWork) rearrangeDeep(node *NodeInProcess, track *RearrangeTracker) {
	if node.nextL != nil {
		w.rearrangeDeep(node.nextL, track)
	} else {
		ssector := &(w.deepSubsectors[node.LChild & ^SSECTOR_DEEP_MASK])
		node.LChild = w.putDeepSubsector(ssector, track) | SSECTOR_DEEP_MASK
	}
	if node.nextR != nil {
		w.rearrangeDeep(node.nextR, track)
	} else {
		ssector := &(w.deepSubsectors[node.RChild & ^SSECTOR_DEEP_MASK])
		node.RChild = w.putDeepSubsector(ssector, track) | SSECTOR_DEEP_MASK
	}
}

func (w *NodesWork) putDeepSubsector(ssector *DeepSubSector,
	track *RearrangeTracker) uint32 {
	idx := track.totals.numSSectors
	track.totals.numSSectors++
	v := w.putDeepSegs(ssector, track)
	track.deepSubsectors = append(track.deepSubsectors, v)
	return uint32(idx)
}

func (w *NodesWork) putDeepSegs(orig *DeepSubSector,
	track *RearrangeTracker) DeepSubSector {
	firstSeg := track.totals.numSegs
	view := w.deepSegs[orig.FirstSeg : orig.FirstSeg+uint32(orig.SegCount)]
	for _, seg := range view {
		seg2 := seg // copy struct data - will be changed
		w.renumberDeepSegVertices(&seg2, track)
		track.deepSegs = append(track.deepSegs, seg2)
		track.totals.numSegs++
	}
	return DeepSubSector{
		FirstSeg: uint32(firstSeg),
		SegCount: orig.SegCount,
	}
}

func (w *NodesWork) renumberDeepSegVertices(seg *DeepSeg,
	track *RearrangeTracker) {
	s := int(seg.StartVertex)
	e := int(seg.EndVertex)
	if track.verticeRenumbering[s] < 0 || track.verticeRenumbering[e] < 0 {
		Log.Panic("vertex was not renumbered at proper place\n")
	}
	seg.StartVertex = uint32(track.verticeRenumbering[s])
	seg.EndVertex = uint32(track.verticeRenumbering[e])
}

// RearrangeBSPExtended - see RearrangeBSPVanilla's description
// Attention to programmers refactoring stuff: when RearrangeBSPExtended is
// called, the zdoomVertexHeader.ReusedOriginalVertices does contain actualized
// value, but zdoomVertexHeader.NumExtendedVertices does not (is set to zero
// instead) and is only set *much* later, when extended nodes are about to get
// written
func (w *NodesWork) RearrangeBSPExtended(node *NodeInProcess,
	pristineVertexCache map[SimpleVertex]int,
	pristineVertexMap *VertexMap) {
	// FIXME Mixing uint32 and int for indexing vertices
	// Investigate impact - might need to build map that requires vertice indices
	// larger than int32, in 32-bit address space, maybe we have trouble
	// earlier?
	track := &RearrangeTracker{
		totals:          &NodesTotals{},
		zdoomSegs:       make([]ZdoomNode_Seg, 0, cap(w.zdoomSegs)),
		zdoomSubsectors: make([]uint32, 0, cap(w.zdoomSubsectors)),
		vertexCache:     pristineVertexCache,
		vertexMap:       pristineVertexMap,
		vertices:        make([]NodeVertex, 0, cap(w.vertices)),
	}
	w.initTranslateVectorForExtended(track)
	w.rearrangeExtendedVertices(node, track)
	w.rearrangeExtended(node, track)
	if w.totals.numSegs != track.totals.numSegs ||
		w.totals.numSSectors != track.totals.numSSectors ||
		uint32(track.cntVerts) != uint32(len(track.vertices)) {
		Log.Panic("RearrangeBSP: BSP structures length not identical after rearrangement segs:%d:%d ssectors:%d:%d verts:%d:%d\n",
			w.totals.numSegs, track.totals.numSegs,
			w.totals.numSSectors, track.totals.numSSectors,
			track.cntVerts, uint32(len(track.vertices)))
	}
	w.zdoomSegs = track.zdoomSegs
	w.zdoomSubsectors = track.zdoomSubsectors
	w.reallyRearrangeExtendedVertices(track.verticeRenumbering, track)
	Log.Printf("RearrangeBSP: done\n")
}

func (w *NodesWork) rearrangeExtended(node *NodeInProcess, track *RearrangeTracker) {
	if node.nextL != nil {
		w.rearrangeExtended(node.nextL, track)
	} else {
		ssector := node.LChild & ^SSECTOR_DEEP_MASK
		node.LChild = w.putExtendedSubsector(ssector, track) | SSECTOR_DEEP_MASK
	}
	if node.nextR != nil {
		w.rearrangeExtended(node.nextR, track)
	} else {
		ssector := node.RChild & ^SSECTOR_DEEP_MASK
		node.RChild = w.putExtendedSubsector(ssector, track) | SSECTOR_DEEP_MASK
	}
}

func (w *NodesWork) putExtendedSubsector(ssector uint32,
	track *RearrangeTracker) uint32 {
	idx := track.totals.numSSectors
	track.totals.numSSectors++
	v := w.putExtendedSegs(ssector, track)
	track.zdoomSubsectors = append(track.zdoomSubsectors, v)
	return uint32(idx)
}

func (w *NodesWork) putExtendedSegs(orig uint32,
	track *RearrangeTracker) uint32 {
	oldFirstSeg := getFirstSegExtended(w.zdoomSubsectors, orig)
	view := w.zdoomSegs[oldFirstSeg : oldFirstSeg+w.zdoomSubsectors[orig]]
	a := uint32(0)
	for _, seg := range view {
		seg2 := seg // copy, will be changed
		w.renumberExtendedSegVertices(&seg2, track)
		track.zdoomSegs = append(track.zdoomSegs, seg2)
		track.totals.numSegs++
		a++
	}
	return w.zdoomSubsectors[orig]
}

func (w *NodesWork) renumberExtendedSegVertices(seg *ZdoomNode_Seg,
	track *RearrangeTracker) {
	reused := w.zdoomVertexHeader.ReusedOriginalVertices
	if seg.StartVertex >= reused {
		s := seg.StartVertex
		if track.verticeRenumbering[s] < 0 {
			Log.Panic("vertex was not renumbered at proper place\n")
		}
		seg.StartVertex = uint32(track.verticeRenumbering[s])
	}
	if seg.EndVertex >= reused {
		e := seg.EndVertex
		if track.verticeRenumbering[e] < 0 {
			Log.Panic("vertex was not renumbered at proper place\n")
		}
		seg.EndVertex = uint32(track.verticeRenumbering[e])
	}
}

func (w *NodesWork) initTranslateVectorForExtended(track *RearrangeTracker) {
	numVerts := uint32(len(w.vertices))
	translate := make([]int, numVerts)
	track.cntVerts = 0
	for i, _ := range translate { // Unmark all vertices
		translate[i] = -1
	}
	track.verticeRenumbering = translate
	for i := 0; i < int(w.zdoomVertexHeader.ReusedOriginalVertices); i++ {
		track.vertices = append(track.vertices, w.vertices[i])
		track.cntVerts++
		translate[i] = i
	}

}

func (w *NodesWork) rearrangeExtendedVertices(node *NodeInProcess,
	track *RearrangeTracker) {
	ex, ok := w.stkExtra[node]
	if !ok {
		Log.Panic("rearrangeExtendedVertices: failed to retrieve extra information on node-in-process structure.\n")
	}

	for i := ex.vstart; i < ex.vend; i++ {
		v := w.vertexSink[i]
		vv := w.vertices[v]
		// definitely not SelectVertexClose, that stuff fails to even have
		// the same vertice count on iwad maps as compared to non-stknode
		// But it is not necessary SelectVertexExact would always deliver
		// byte-for-byte matching output against non-stknode, either, just have
		// not run into that case yet
		fv := track.vertexMap.SelectVertexExact(float64(vv.X), float64(vv.Y),
			track.cntVerts)
		if fv.Id != track.cntVerts {
			track.verticeRenumbering[v] = fv.Id
		} else {
			nv := track.cntVerts
			track.verticeRenumbering[v] = nv
			track.vertices = append(track.vertices, NodeVertex{
				X:   Number(fv.X),
				Y:   Number(fv.Y),
				idx: uint32(nv),
			})
			track.cntVerts++
		}
	}

	if node.nextL != nil {
		w.rearrangeExtendedVertices(node.nextL, track)
	}
	if node.nextR != nil {
		w.rearrangeExtendedVertices(node.nextR, track)
	}
}

func (w *NodesWork) reallyRearrangeExtendedVertices(translate []int,
	track *RearrangeTracker) {
	reused := w.zdoomVertexHeader.ReusedOriginalVertices
	if len(translate) != len(w.vertices) {
		Log.Panic("reallyRearrangeExtendedVertices: failed translation (length don't match) %d != %d",
			len(translate), len(w.vertices))
	}
	newVertices := track.vertices
	for i, _ := range w.vertices[:reused] {
		newVertices[i] = w.vertices[i]
	}
	for i, _ := range w.vertices[reused:] {
		newVertices[uint32(translate[i])] =
			w.vertices[uint32(i)+reused]
	}
	w.vertices = newVertices
}
