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
	segs               []Seg // game format for actual SEGS lump
	deepSubsectors     []DeepSubSector
	deepSegs           []DeepSeg
	zdoomSubsectors    []uint32
	zdoomSegs          []ZdoomNode_Seg
	verticeRenumbering []int // index is original index, value is new index
	cntVerts           int
}

// RearrangeBSPVanilla tries to arrange the BSP tree built with this method
// so that the wad output matches what the traditional method (recursion) would
// produce, in deterministic mode. Only for levels within vanilla seg and
// subsector count limits, though
// This is for debug purposes mainly
func (w *NodesWork) RearrangeBSPVanilla(node *NodeInProcess) {
	if w.vertexExists > 0 {
		// doom.wad, doom2.wad, hr2final.wad all have at least one affected map
		// On some maps, rearrangement would succeed even with duplicate
		// vertices prevention measure taking effect beforehand. However, when
		// it doesn't, it would either trigger a panic, or produce broken map,
		// so until algorithm is thorougly adapted to the existence of vertex
		// deduplication, this limitation is to be enforced.
		Log.Printf("RearrangeBSP: cancelled (vertex deduplication was in effect, correct rearrangement is not supported in this case)\n")
		return
	}
	if len(w.segs) > 32767 || len(w.subsectors) > 32767 {
		// Don't risk rearrangement breaking what could otherwise work
		// Also, fields may contain incorrect values if limit is exceeded
		Log.Printf("RearrangeBSP: cancelled (segs / subsectors are out of guaranteed vanilla range).\n")
		return
	}
	track := &RearrangeTracker{
		totals:     &NodesTotals{},
		segs:       make([]Seg, 0, cap(w.segs)),
		subsectors: make([]SubSector, 0, cap(w.subsectors)),
		vertices:   make([]NodeVertex, 0, cap(w.vertices)),
	}

	if !w.identifyLinedefVertices(track) {
		Log.Printf("RearrangeBSP: cancelled because of incorrect linedef->vertice references\n")
		return
	}
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

func (w *NodesWork) rearrangeVertices(node *NodeInProcess, track *RearrangeTracker) {
	ex, ok := w.stkExtra[node]
	if !ok {
		Log.Panic("rearrangeVertices: failed to retrieve extra information on node-in-process structure.\n")
	}
	//Log.Printf("Range [%d-%d]\n", ex.vstart, ex.vend)
	for i := ex.vstart; i < ex.vend; i++ {
		if track.verticeRenumbering[i] >= 0 {
			Log.Panic("vertex already numbered\n")
		}
		track.verticeRenumbering[i] = track.cntVerts
		track.cntVerts++
	}
}

func (w *NodesWork) putSubsector(ssector *SubSector, track *RearrangeTracker) uint32 {
	idx := track.totals.numSSectors
	track.totals.numSSectors++
	v := w.putSegs(ssector, track)
	track.subsectors = append(track.subsectors, v)
	return uint32(idx)
}

func (w *NodesWork) putSegs(orig *SubSector, track *RearrangeTracker) SubSector {
	firstSeg := track.totals.numSegs
	for _, seg := range w.segs[orig.FirstSeg : orig.FirstSeg+orig.SegCount] {
		seg2 := seg // copy, will be changed
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
	return true
}

// RearrangeBSPDeep - see RearrangeBSPVanilla's description
func (w *NodesWork) RearrangeBSPDeep(node *NodeInProcess) {
	if w.vertexExists > 0 {
		Log.Printf("RearrangeBSP: cancelled (vertex deduplication was in effect, correct rearrangement is not supported in this case)\n")
		return
	}
	// FIXME Mixing uint32 and int for indexing vertices
	// Investigate impact - might need to build map that requires vertice indices
	// larger than int32, in 32-bit address space, maybe we have trouble
	// earlier?
	track := &RearrangeTracker{
		totals:         &NodesTotals{},
		deepSegs:       make([]DeepSeg, 0, cap(w.deepSegs)),
		deepSubsectors: make([]DeepSubSector, 0, cap(w.deepSubsectors)),
		vertices:       make([]NodeVertex, 0, cap(w.vertices)),
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

func (w *NodesWork) rearrangeDeep(node *NodeInProcess, track *RearrangeTracker) {
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
		seg2 := seg // copy, will be changed
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
func (w *NodesWork) RearrangeBSPExtended(node *NodeInProcess) {
	if w.vertexExists > 0 {
		Log.Printf("RearrangeBSP: cancelled (vertex deduplication was in effect, correct rearrangement is not supported in this case)\n")
		return
	}
	// FIXME Mixing uint32 and int for indexing vertices
	// Investigate impact - might need to build map that requires vertice indices
	// larger than int32, in 32-bit address space, maybe we have trouble
	// earlier?
	track := &RearrangeTracker{
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

func (w *NodesWork) rearrangeExtended(node *NodeInProcess, track *RearrangeTracker) {
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

func (w *NodesWork) initTranslateVectorForExtended(track *RearrangeTracker) {
	numVerts := uint32(len(w.vertices) -
		int(w.zdoomVertexHeader.ReusedOriginalVertices))
	translate := make([]int, numVerts)
	track.cntVerts = 0
	for i, _ := range translate { // Unmark all vertices
		translate[i] = -1
	}
	track.verticeRenumbering = translate
}

func (w *NodesWork) rearrangeExtendedVertices(node *NodeInProcess,
	track *RearrangeTracker) {
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

func (w *NodesWork) reallyRearrangeExtendedVertices(translate []int) {
	reused := w.zdoomVertexHeader.ReusedOriginalVertices
	if len(translate) != (len(w.vertices) - int(reused)) {
		Log.Panic("reallyRearrangeExtendedVertices: failed translation (length don't match) %d != %d",
			len(translate), len(w.vertices)-int(reused))
	}
	newVertices := make([]NodeVertex, len(w.vertices))
	for i, _ := range w.vertices[:reused] {
		newVertices[i] = w.vertices[i]
	}
	for i, _ := range w.vertices[reused:] {
		newVertices[uint32(translate[i])+reused] =
			w.vertices[uint32(i)+reused]
	}
	w.vertices = newVertices
}
