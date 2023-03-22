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

/*import (
	"strconv"
)*/

// This unit contains algorithm to build BSP tree breadth-first (or in mixed
// manner) rather than depth-first, to make efficient (=done early) workload
// distribution possible for hard multi-tree. It also contains adjacent
// functionality such as *optional* reordering of the resulting tree, which can
// be used in tests to ensure algorithm correctness

// Stk stands for "stUck", as in being stuck to give a name to it. It is also a
// reference to how difficult it was to conceptualize this

// Multi-tree hard needs to evaluate partition candidates at the top of the
// tree as early as possible, to give other threads material to work on
// different branches of the tree as early as possible. This can be made
// possible if breadth-first traversal is implemented for top levels of the
// tree, until a certain depth is reached, before switching to depth-first
// algorithm

// Via debug parameters, this can be used on single-tree also. This option
// is useful to verify new algorithm's correctness by byte-to-byte comparison
// against single-tree built normally with depth-first algorithm. But since the
// order of building a tree does influence the order of all rendering
// primitives, an explicit reordering pass must be applied to the resulting tree
// so that it would actually match the order produced by depth-first algorithm.
// This is not always easily done (see RearrangeBSPVanilla)

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

// StkNodeExtraData contains information associated with NodeInProcess struct,
// but held separately so that classic node creation algorithm does not allocate
// memory for what it does not need.
// However, it is not easy to glue the two together in memory-efficient manner,
// which means this is additional source of increased memory consumption for
// multi-tree (as of time of this writing, the glue is map indexed by pointer
// keys, that is *NodeInProcess keys, and StkNodeExtraData values. Maps consume
// noticably more memory than array, so need to figure out another way at some
// point)
type StkNodeExtraData struct {
	// [vstart:vend), vstart inclusive, vend NOT, denote range of vertices
	// created at DivideSegs on this node directly (child nodes are not
	// counted). There is at least one vertex if vend > vstart, zero otherwise
	vstart, vend int64
	// parts are available partitions that could have been used as a divisor
	// for this node (instead of the one actually used) - only relevant to
	// multi-tree
	parts []PartSeg
}

// StkQueue grows when new things enqueued, but doesn't remove values when
// dequeued. Only cursor is moved on dequeue
type StkQueue struct {
	tasks      []StkQueueTask
	depthLimit int
	cur        int // cursor (points at task not yet processed)

	tmp *StkQueueTask // persistent pointer, but value to be accessed through it is changed on every call to .Dequeue() method - to a copy of dequeued value

	// TODO uplink. It will be queue's responsibility to accumulate data to send
	// to multitree_hard sentinel during "early report" stages
	// It will do so when processing the next task of depth being different
	// from original
	// Of course, for that StkCreateNode* will have not only to Dequeue/Enqueue,
	// but also SetResult() (for last dequeued)
	// Past the "early report" phase, uplink receives data only at the very end,
	// which will be done outside of Queue (although it might need to peruse
	// Queue's data on what was sent)
}

type StkQueueTask struct {
	action       int         // action to take on this task (maybe it requires non-convex single sector special treatment?)
	num          int         // index of self in queue
	parentNum    int         // index of parent node in queue (-1 for root)
	isRightChild bool        // whether it's parent left or right subnode
	depth        int         // depth in tree relative to root (root is at 0)
	seg          *NodeSeg    // seg chain to process
	box          *NodeBounds // box to process
	super        *Superblock // superblock to process
	leftChild    int
	rightChild   int
	parts        []PartSeg // which partitions multi-tree scheduler can choose for this node
	node         *NodeInProcess
}

type PartSeg struct {
	Linedef   uint16
	Occurence int // assuming segs have consistent order, which occurence of it to use as partition
}

const ( // enumeration for StkQueueTask.action - notably, ready convex sectors are not queued
	STK_QUEUE_REGULARNODE      = iota // for multi-sector (non-convex)
	STK_QUEUE_SINGLE_NONCONVEX        // for single sector (non-convex)
)

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

// StkEntryPoint is where the node tree building starts
func StkEntryPoint(w *NodesWork, ts *NodeSeg, bbox *NodeBounds, super *Superblock) *NodeInProcess {
	w.stkExtra = make(map[*NodeInProcess]StkNodeExtraData, 0)
	w.parts = make([]*NodeSeg, 0) // FIXME debug - it should not be created here, I think. Then again...

	queue := &StkQueue{}
	// if making parameter for depthLimit, 0 should mean auto, -1 means
	// unbounded, value above 0 are specific and values below -1 are undefined
	// behavior (yet) or downright verbotten
	//queue.depthLimit = -1
	queue.depthLimit = 7 // TODO depth limit must be a parameter (debug single-tree mode) or a calculated value (multi-tree hard)
	queue.Enqueue(STK_QUEUE_REGULARNODE, ts, bbox, super, false)

	return StkCreateNode(w, ts, bbox, super, queue)
}

// StkCreateNode begins with doing things breadth first, until Queue no longer
// accepts any more things, then it switches to classic recursion depth first
// If passed empty queue, depth first is used
func StkCreateNode(w *NodesWork, ts *NodeSeg, bbox *NodeBounds,
	super *Superblock, queue *StkQueue) *NodeInProcess {
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
		var rights *NodeSeg
		var lefts *NodeSeg
		var rightsSuper *Superblock
		var leftsSuper *Superblock
		// Divide node in two
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
		super = nil // NOTE after DivideSegs return, super may no longer be valid
		res.X = int16(w.nodeX)
		res.Y = int16(w.nodeY)
		res.Dx = int16(w.nodeDx)
		res.Dy = int16(w.nodeDy)
		w.stkExtra[res] = StkNodeExtraData{
			vstart: vstart,
			vend:   vend,
			parts:  partsegs,
		}
		/* Can't work here correctly now - width cap defaults to 2
		if len(partsegs) > 1 && config.VerbosityLevel >= 1 { // reference to global: config
			// todo also beware of ps.Occurence == -1 red flag, where Linedef
			// field is not valid and set to 0. Although len(partsegs) will be
			// 1 in this case, meaning this path is not reached, but something
			// to keep in mind when refactoring
			s := ""
			for _, ps := range partsegs {
				s += "," + strconv.Itoa(int(ps.Linedef))
			}
			w.mlog.Verbose(1, "maxwidth = %d (%s)\n", len(partsegs), s)
		}*/
		if task != nil {
			queue.SetResult(res, partsegs)
		}

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
			res.LChild = 0
			if !b || !queue.Enqueue(STK_QUEUE_SINGLE_NONCONVEX, lefts, leftBox,
				leftsSuper, false) {
				res.nextL = w.stkCreateNodeSS(w, lefts, leftBox, leftsSuper, nil)
			}
		} else {
			res.LChild = 0
			if config.VerbosityLevel >= 1 && w.IsConvexMultiSector(lefts) { // reference to global: config
				w.dumpMultiSectorSegs(lefts)
			}
			if !b || !queue.Enqueue(STK_QUEUE_REGULARNODE, lefts, leftBox,
				leftsSuper, false) {
				res.nextL = StkCreateNode(w, lefts, leftBox, leftsSuper, nil)
			}
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
			res.RChild = 0
			if !b || !queue.Enqueue(STK_QUEUE_SINGLE_NONCONVEX, rights, rightBox,
				rightsSuper, true) {
				res.nextR = w.stkCreateNodeSS(w, rights, rightBox, rightsSuper, nil)
			}
		} else {
			res.RChild = 0
			if config.VerbosityLevel >= 1 && w.IsConvexMultiSector(rights) { // reference to global: config
				w.dumpMultiSectorSegs(rights)
			}
			if !b || !queue.Enqueue(STK_QUEUE_REGULARNODE, rights, rightBox,
				rightsSuper, true) {
				res.nextR = StkCreateNode(w, rights, rightBox, rightsSuper, nil)
			}
		}
		rightsSuper = nil
		task = queue.Dequeue()
		b = task != nil
	}
	return firstRes
}

// original was in convexity.go
// Queue is not needed here (YET - that is, at the time the comment was written
// - might change!), but I have to make this match the signature of
// StkCreateNode anyway so both are assignable to a single callback of
// StkCreateNodeSSFunc type
func StkCreateNodeForSingleSector(w *NodesWork, ts *NodeSeg, bbox *NodeBounds,
	super *Superblock, queue *StkQueue) *NodeInProcess {
	res := new(NodeInProcess)
	var rights *NodeSeg
	var lefts *NodeSeg
	var rightsSuper *Superblock
	var leftsSuper *Superblock
	// Divide node in two
	w.totals.numNodes++
	partsegs := make([]PartSeg, 0)
	vstart := int64(len(w.vertices))
	w.DivideSegsForSingleSector(ts, &rights, &lefts, bbox, super, &rightsSuper,
		&leftsSuper, &partsegs)
	vend := int64(len(w.vertices))
	super = nil // NOTE after DivideSegs return, super may no longer be valid
	res.X = int16(w.nodeX)
	res.Y = int16(w.nodeY)
	res.Dx = int16(w.nodeDx)
	res.Dy = int16(w.nodeDy)
	w.stkExtra[res] = StkNodeExtraData{
		vstart: vstart,
		vend:   vend,
		parts:  partsegs,
	}

	// These will form the left box
	leftBox := FindLimits(lefts)
	res.Lbox[BB_TOP] = int16(leftBox.Ymax)
	res.Lbox[BB_BOTTOM] = int16(leftBox.Ymin)
	res.Lbox[BB_LEFT] = int16(leftBox.Xmin)
	res.Lbox[BB_RIGHT] = int16(leftBox.Xmax)
	if w.isItConvex(lefts) == CONVEX_SUBSECTOR {
		res.nextL = nil
		res.LChild = w.CreateSSector(lefts) | w.SsectorMask
		w.returnSuperblockToPool(leftsSuper)
	} else { // only NONCONVEX_ONESECTOR can be here
		res.nextL = StkCreateNodeForSingleSector(w, lefts, leftBox, leftsSuper, queue)
		res.LChild = 0
	}

	// These will form the right box
	rightBox := FindLimits(rights)
	res.Rbox[BB_TOP] = int16(rightBox.Ymax)
	res.Rbox[BB_BOTTOM] = int16(rightBox.Ymin)
	res.Rbox[BB_LEFT] = int16(rightBox.Xmin)
	res.Rbox[BB_RIGHT] = int16(rightBox.Xmax)
	if w.isItConvex(rights) == CONVEX_SUBSECTOR {
		res.nextR = nil
		res.RChild = w.CreateSSector(rights) | w.SsectorMask
		w.returnSuperblockToPool(rightsSuper)
	} else { // only NONCONVEX_ONESECTOR can be here
		res.nextR = StkCreateNodeForSingleSector(w, rights, rightBox, rightsSuper, queue)
		res.RChild = 0
	}

	return res
}

// wrapper over (*NodesWork).DivideSegsForSingleSector - assignable to a callback
func VigilantSingleSectorDivisor(w *NodesWork, ts *NodeSeg, rs **NodeSeg,
	ls **NodeSeg, bbox *NodeBounds, super *Superblock, rightsSuper,
	leftsSuper **Superblock, partsegs *[]PartSeg) {
	// call forwarded
	w.DivideSegsForSingleSector(ts, rs, ls, bbox, super, rightsSuper,
		leftsSuper, partsegs)
}

// wrapper over (*NodesWork).DivideSegs - assignable to a callback
func DefaultSingleSectorDivisor(w *NodesWork, ts *NodeSeg, rs **NodeSeg,
	ls **NodeSeg, bbox *NodeBounds, super *Superblock, rightsSuper,
	leftsSuper **Superblock, partsegs *[]PartSeg) {
	// call forwarded
	w.DivideSegs(ts, rs, ls, bbox, super, rightsSuper, leftsSuper, partsegs)
}

func (q *StkQueue) lastDequeued() *StkQueueTask {
	if len(q.tasks) == 0 || q.cur == 0 {
		return nil
	}
	return &(q.tasks[q.cur-1])
}

func (q *StkQueue) depthChanged() {
	// TODO must report parts to uplink channel (earlyRep)
	// Note that records total below would include enqueued but not processed
	// items. So yes, at depth 0 we have 3 records total, 1 of them (root) was
	// done when depthChanged is called, the other two are NOT yet done, their
	// partition candidates are yet unknown
	// TODO also StkQueue doesn't have access to mlog currently
	//q.mlog.Printf("Done depth %d - %d records total\n", q.tmp.depth, len(q.tasks))
}

func (q *StkQueue) getDepth() int {
	if len(q.tasks) == 0 {
		return -1
	}
	return q.tasks[len(q.tasks)-1].depth
}

func (q *StkQueue) Enqueue(action int, seg *NodeSeg, box *NodeBounds,
	super *Superblock, isRightChild bool) bool {
	if q == nil {
		// doubt anyone would call this on empty queue actually, but if they
		// do, this will function as queue that reached its limit anyway
		return false
	}
	if q.depthLimit != -1 && q.getDepth()+1 > q.depthLimit {
		return false // limit reached, refused to queue
	}
	num := len(q.tasks)
	q.tasks = append(q.tasks, StkQueueTask{
		action: action,
		num:    num,
		// parentNum - set later
		isRightChild: isRightChild,
		// depth - set later
		seg:        seg,
		box:        box,
		super:      super,
		leftChild:  -1,
		rightChild: -1,
		parts:      nil, // only known after dequeueing and running DivideSegs*
	})
	parent := q.lastDequeued()
	nu := &(q.tasks[len(q.tasks)-1]) // our newly created item
	if parent != nil {
		nu.parentNum = parent.num
		nu.depth = parent.depth + 1
		if isRightChild {
			parent.rightChild = num
		} else {
			parent.leftChild = num
		}
	} else {
		// Inserting root
		nu.parentNum = -1
		nu.depth = 0
	}
	return true
}

// Dequeue returns a shallow copy of the record the caller must NOT modify.
// Also, the returned pointer is reused by the queue - subsequent calls to
// Dequeue will return the same pointer, but values behind it will CHANGE after
// each call to Dequeue
// NOTE So while using write access on the returned record won't do anything
// good, it can do a lot of bad, and will corrupt data, and if the data needs
// to be retained for reading even, the caller must still deep copy everything
// it wants retained
func (q *StkQueue) Dequeue() *StkQueueTask {
	if q == nil {
		return nil
	}
	if len(q.tasks) == 0 {
		return nil
	}
	if q.cur == len(q.tasks) { // limit reached
		q.tmp = nil
		return nil
	}
	if q.tmp == nil {
		q.tmp = &StkQueueTask{}
		// don't call depthChanged - they are only beginning to build root
		// node, don't know their candidates yet
	} else {
		oldDepth := q.tmp.depth
		newDepth := q.tasks[q.cur].depth
		if oldDepth != newDepth {
			q.depthChanged()
		}
	}
	*(q.tmp) = q.tasks[q.cur] // clobber q.tmp with new value
	q.cur++
	return q.tmp
}

func (q *StkQueue) SetResult(node *NodeInProcess, parts []PartSeg) {
	de := q.lastDequeued()
	de.node = node
	if de.parentNum >= 0 {
		if de.isRightChild {
			q.tasks[de.parentNum].node.nextR = node
		} else {
			q.tasks[de.parentNum].node.nextL = node
		}
	}
	// TODO when no more things enqueued, might need to send something still?
	de.parts = parts
}
