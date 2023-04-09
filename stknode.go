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
// distribution possible for hard multi-tree. This, however, results in
// subsectors, segs and vertices being placed in different order (a
// different unit - node_rearrange.go - has provision to reorder the results to
// match results of tree built depth-first).

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
// This is not always easily done (see node_rearrange.go, RearrangeBSPVanilla)

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
		// TODO instead of map, these extra data should be represented as
		// "binary tree stored as an array". Then, when working NodeInProcess
		// tree in DFS-order, these should be traversed simultaneously, a
		// corresponding mapping obtained.
		// The only problem with such approach would arise when linguortals have
		// to be supported - although extra data for those can be stored in the
		// description of linguortal injections that would contain corresponding
		// NodeInProcess'es (of linguortals)
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
