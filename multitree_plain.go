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
	"reflect"
	"runtime"
)

// multitree_plain.go - bruteforce solution for root node only

// each thread works on each own deep copy
// they communicate the result structure to the main thread when they're done
// main thread keeps best structure based on some linear comparison like
// number of subsectors/nodes and bsp height
// Number of subsectors can be checked real quick, ditto with number of segs
// (totals field of NodesWork)
// Number of threads (goroutines) can be a configured parameter, or derived
// automatically from number of cores. Will just throw every core at them,
// regardless of whether blockmap / reject are being built

// Options:
// 1. Default root node selection (single tree) - covered elsewhere, not in this
// file
// 2. Try every one sided linedef as a possible root partition
// 3. Try every two sided linedef as a possible root partition
// 4. Try every linedef as a possible root partition
// compare the results, use "best" one

type MTPWorker_Input struct {
	workData    *NodesWork
	pickSegIdx  int
	ts          *NodeSeg
	bbox        *NodeBounds
	pseudoSuper *Superblock
	id          int
}

type MTPWorker_Result struct {
	workData *NodesWork
	bspTree  *NodeInProcess
	id       int
}

// To avoid running out of memory, there is a limit on the number of workers
// that can be designated in auto-mode (but this limit will be ignored if user
// tells us an explicit number of workers to use)
// The limit assumes people use multi-tree on vanilla maps, which might not be
// true in practice
const MAX_MTP_WORKERS = 16

// Parameter w is also mutated by this call. Changes to bbox and super are
// undefined
func MTPSentinel_MakeBestBSPTree(w *NodesWork, bbox *NodeBounds,
	super *Superblock, rootChoiceMethod int) *NodeInProcess {
	Log.Printf("Nodes builder: info: you have selected multi-tree mode with bruteforce for root partition only.\n")
	Log.Printf("Multi-tree modes take significant time to compute, and also have rather high memory consumption.\n")
	// Which segs will be used as starting point?
	rootSegCandidates := MTPSentinel_GetRootSegCandidates(w.allSegs,
		rootChoiceMethod)

	// How many threads to create
	workerCount := int(config.NodeThreads) // global config
	// If not explicitly specified, default to number of cores, but apply limit
	if workerCount == 0 { // auto mode
		workerCount = runtime.NumCPU()
		if workerCount > MAX_MTP_WORKERS {
			workerCount = MAX_MTP_WORKERS
		}
	} // no limit applied if explicitly specified - intentional

	// Is there enough segs to use as a starting choice for every thread?
	// If not, must reduce even if value was user-supplied
	if workerCount > len(rootSegCandidates) { // more cores than segs to use as root
		workerCount = len(rootSegCandidates)
		Log.Printf("Limiting number of threads for multi-tree to %d, because only %d linedefs to try.\n",
			workerCount, workerCount)
	}

	Log.Printf("Multi-tree generator will use %d CPUs\n", workerCount)

	// To communicate with threads, channels are needed
	workerChans := make([]chan MTPWorker_Input, workerCount)       // input data for each thread
	workerReplyChans := make([]chan MTPWorker_Result, workerCount) // output result from each thread
	for i := 0; i < workerCount; i++ {
		workerChans[i] = make(chan MTPWorker_Input)
		workerReplyChans[i] = make(chan MTPWorker_Result)
		// Spawn workers (they will be fed data later)
		go MTPWorker_GenerateBSPTrees(workerChans[i], workerReplyChans[i])
	}

	// pseudoSuper is a superblock containing only metadata used to create new
	// superblocks: whether to initialize secEquivs and sector fields, etc.
	// It can be shared between all tree workers. Avoids having to clone or
	// rebuild the original with new seg references (which is expensive), and
	// guards against possible future code change bringing in race condition
	// (say, if DivideSegsActual begins to access seg stuff, but multitree
	// code is not updated accordingly) - such shortsighted code change would
	// cause null pointer access instead, signaling that superblock of root node
	// in multi-tree was not meant to be accessed in this capacity
	pseudoSuper := super.DerivePseudo()

	// At start, feed each channel/thread exactly once
	for i := 0; i < workerCount; i++ {
		clonedWorkData, clonedBbox := MTPSentinel_Clone(w, bbox)
		workerChans[i] <- MTPWorker_Input{
			workData:    clonedWorkData,
			pickSegIdx:  rootSegCandidates[i],
			ts:          clonedWorkData.allSegs[0],
			bbox:        clonedBbox,
			id:          i,
			pseudoSuper: pseudoSuper,
		}
	}
	lastFedIdx := workerCount - 1

	// Then, we'll refeed threads as we retrieve data, using reflection
	// until we run out of supplies, then we start closing input channels,
	// whereas when result channels get closed, we remove them

	// These branches are per output only
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

	// Now peek results of workers as they come, feeding next input to every
	// worker that delivers result until no more inputs to try
	var bestResult *MTPWorker_Result
	for len(branches) > 0 {
		chi, recv, recvOk := reflect.Select(branches)
		if !recvOk { // channel closed
			branches, branchIdx = MTPSentinel_DeleteBranch(branches, branchIdx, chi)
			continue
		}
		treesDone++
		res := (recv.Interface()).(MTPWorker_Result)
		resp := new(MTPWorker_Result)
		*resp = res
		// Reseed worker with next seg to try as root
		if lastFedIdx < len(rootSegCandidates)-1 {
			lastFedIdx++
			clonedWorkData, clonedBbox := MTPSentinel_Clone(w, bbox)
			workerChans[branchIdx[chi]] <- MTPWorker_Input{
				workData:    clonedWorkData,
				pickSegIdx:  rootSegCandidates[lastFedIdx],
				ts:          clonedWorkData.allSegs[0],
				bbox:        clonedBbox,
				id:          lastFedIdx,
				pseudoSuper: pseudoSuper,
			}
		} else {
			// No more segs to try
			close(workerChans[branchIdx[chi]])
		}
		// Compare against previous best
		if bestResult == nil || MTP_IsBSPTreeBetter(bestResult, resp) {
			bestResult = resp
		}
		Log.Printf("Multi-tree: processed %d/%d trees\n", treesDone, maxTrees)
	}
	Log.Printf("Multi-tree finished processing trees (%d out of %d - should be equal)", treesDone,
		lastFedIdx+1)
	// Splice workData struct with one that is associated with bestResult
	oldLines := w.lines // !!! level.go retains reference to old w.lines
	*w = *(bestResult.workData)
	oldLines.AssignFrom(w.lines) // so must clobber the data in old w.lines with the new one
	return bestResult.bspTree
}

// Really banal bsp tree comparison. Tries to minimize subsector count first,
// then bsp tree height difference, then bsp tree height itself, then seg count,
// after that, if all things still equal, tree that came from earlier seg wins
// NOTE also added check for precious splits
func MTP_IsBSPTreeBetter(oldResult *MTPWorker_Result, newResult *MTPWorker_Result) bool {
	// Prefer trees where no precious segs or polyobj sector borders were split
	// But, don't compare numbers itself as a lot of Hexen maps have setups that
	// deviate from "convex sector" ideal and are not getting broken by those
	// splits really
	oldBad := oldResult.workData.totals.preciousSplit > 0
	newBad := newResult.workData.totals.preciousSplit > 0
	if oldBad && !newBad {
		return true
	} else if !oldBad && newBad {
		return false
	}
	// Compare subsector count
	oldSsectorsCnt := oldResult.workData.totals.numSSectors
	newSSectorsCnt := newResult.workData.totals.numSSectors
	if newSSectorsCnt < oldSsectorsCnt {
		return true
	} else if newSSectorsCnt > oldSsectorsCnt {
		return false
	}

	// Subsector count equal
	// Let's see BSP height
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
	// Smaller difference in BSP height wins
	if newDiff < oldDiff {
		return true
	} else if newDiff > oldDiff {
		return false
	}

	// Difference was equal, let's see the height itself
	oldMaxHeight := oldLeft
	if oldRight > oldMaxHeight {
		oldMaxHeight = oldRight
	}
	newMaxHeight := newLeft
	if newRight > newMaxHeight {
		newMaxHeight = newRight
	}
	// Smaller maximum BSP height wins
	if newMaxHeight < oldMaxHeight {
		return true
	} else if newMaxHeight > oldMaxHeight {
		return false
	}

	// Even maximum height was equal, wtf
	// Ok, let's minimize seg count then
	oldSegCnt := oldResult.workData.totals.numSegs
	newSegCnt := newResult.workData.totals.numSegs
	if newSegCnt < oldSegCnt {
		return true
	} else if newSegCnt > oldSegCnt {
		return false
	}

	// All things equal, smaller id (tree produced by seg that should have
	// been tried earlier in single-threaded mode) wins. This makes the process
	// deterministic
	if newResult.id < oldResult.id {
		return true
	}
	return false
}

func getBSPHeights(rootNode *NodeInProcess) (int, int) {
	hLeft := 0
	hRight := 0
	if rootNode.nextL != nil {
		hLeft = HeightOfNodes(rootNode.nextL) + 1
	}
	if rootNode.nextR != nil {
		hRight = HeightOfNodes(rootNode.nextR) + 1
	}
	return hLeft, hRight
}

// Because of stateful nature of node workers, must clone all used records
func MTPSentinel_Clone(w *NodesWork, bbox *NodeBounds) (*NodesWork, *NodeBounds) {
	clonedWorkData := w.GetInitialStateClone()
	clonedBbox := new(NodeBounds)
	*clonedBbox = *bbox
	return clonedWorkData, clonedBbox
}

func MTPSentinel_DeleteBranch(branches []reflect.SelectCase, branchIdx []int,
	chi int) ([]reflect.SelectCase, []int) {
	for i := chi; i < (len(branches) - 1); i++ {
		branches[i] = branches[i+1]
		branchIdx[i] = branchIdx[i+1]
	}
	return branches[:(len(branches) - 1)], branchIdx[:(len(branchIdx) - 1)]
}

// This returns all segs to be tried as partition choice for root node,
// according to the user choice and with only one seg per alias
func MTPSentinel_GetRootSegCandidates(allSegs []*NodeSeg, rootChoiceMethod int) []int {
	res := make([]int, 0, len(allSegs))
	for i, seg := range allSegs {
		if i > 0 && seg.partner == allSegs[i-1] {
			// For 2-sided linedefs, only first seg of two is tried, the second
			// is dismissed regardless of the mode
			continue
		}
		// If not choosing segs from one-sided lines, then all segs without
		// partner are assumed to have come from one-sided lines and thus
		// skipped. Warning: no render instructions can actually remove seg
		// from one side of two-sided line
		if seg.partner == nil && (rootChoiceMethod&MROOT_ONESIDED == 0) {
			continue
		}
		// If not choosing segs from two-sided lines, then all segs that have
		// partner are assumed to have come from two-sided lines and thus skipped
		// There should not be any no problem with this assumption
		if seg.partner != nil && (rootChoiceMethod&MROOT_TWOSIDED == 0) {
			continue
		}
		res = append(res, i)
	}

	// Segs that would produce same partition line as other segs need to
	// be eliminated so that only one of such seg group (alias) is retained
	alias := make([]bool, len(res))
	for i := 0; i < len(res); i++ {
		if alias[i] { // already eliminated
			continue
		}
		part := allSegs[res[i]]
		for j := i + 1; j < len(res); j++ {
			if alias[j] { // already eliminated
				continue
			}
			check := allSegs[res[j]]
			a := part.pdy*check.psx - part.pdx*check.psy + part.perp
			b := part.pdy*check.pex - part.pdx*check.pey + part.perp
			if a == 0 && b == 0 {
				// co-linear, must be an alias then
				alias[j] = true // mark for elimination
			}
		}
	}

	// Now produce the final array without all the chaff
	res2 := make([]int, 0, len(res))
	for i := 0; i < len(res); i++ {
		if alias[i] { // skip this
			continue
		}
		res2 = append(res2, res[i]) // copy good one
	}
	Log.Printf("Multi-tree: originally %d linedefs to try for root, but only %d would produce unique trees. (This is normal)",
		len(res), len(res2))
	return res2
}

func MTPWorker_GenerateBSPTrees(input <-chan MTPWorker_Input, replyTo chan<- MTPWorker_Result) {
	for permutation := range input {
		tree := MTP_CreateRootNode(permutation.workData,
			permutation.ts, permutation.bbox, permutation.pseudoSuper,
			permutation.pickSegIdx)

		replyTo <- MTPWorker_Result{
			workData: permutation.workData,
			bspTree:  tree,
			id:       permutation.id,
		}
	}
	close(replyTo)
}

func MTP_CreateRootNode(w *NodesWork, ts *NodeSeg, bbox *NodeBounds,
	pseudoSuper *Superblock, pickSegIdx int) *NodeInProcess {
	res := new(NodeInProcess)
	var rights *NodeSeg
	var lefts *NodeSeg
	var rightsSuper *Superblock
	var leftsSuper *Superblock
	// Divide node in two
	w.totals.numNodes++
	// New DivideSegs variant - uses provided seg as root seg
	w.MTP_DivideSegs(ts, &rights, &lefts, bbox, pseudoSuper, &rightsSuper,
		&leftsSuper, w.allSegs[pickSegIdx])
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
	} else if state == NONCONVEX_ONESECTOR {
		res.nextL = w.createNodeSS(w, lefts, leftBox, leftsSuper)
		res.LChild = 0
	} else {
		res.nextL = CreateNode(w, lefts, leftBox, leftsSuper)
		res.LChild = 0
	}

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
	} else if state == NONCONVEX_ONESECTOR {
		res.nextR = w.createNodeSS(w, rights, rightBox, rightsSuper)
		res.RChild = 0
	} else {
		res.nextR = CreateNode(w, rights, rightBox, rightsSuper)
		res.RChild = 0
	}
	return res
}

func (w *NodesWork) MTP_DivideSegs(ts *NodeSeg, rs **NodeSeg, ls **NodeSeg,
	bbox *NodeBounds, pseudoSuper *Superblock, rightsSuper, leftsSuper **Superblock,
	chosen *NodeSeg) {
	best := chosen

	w.nodeX = int(best.StartVertex.X)
	w.nodeY = int(best.StartVertex.Y)
	w.nodeDx = best.EndVertex.X.Ceil() - w.nodeX
	w.nodeDy = best.EndVertex.Y.Ceil() - w.nodeY

	// Partition line coords
	c := &IntersectionContext{
		psx: best.StartVertex.X,
		psy: best.StartVertex.Y,
		pex: best.EndVertex.X,
		pey: best.EndVertex.Y,
	}
	c.pdx = c.psx - c.pex
	c.pdy = c.psy - c.pey
	w.DivideSegsActual(ts, rs, ls, bbox, best, c, pseudoSuper, rightsSuper, leftsSuper)
}
