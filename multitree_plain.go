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

import (
	"reflect"
	"runtime"
	"sync"
	"time"
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

// MTPForeignInput is a special action that MTPSentinel is requested to do by other
// parts of VigilantBSP
type MTPForeignInput struct {
	Action        int
	LineIdx       uint16
	linesForZdoom WriteableLines
	start         time.Time
	input         *NodesInput
	solidMap      *Blockmap
	rootFunc      CreateRootNodeForPick
	reportStr     string
	announceStr   string
}

// preparation for... something. Used to be hardcoded to MTP_CreateRootNode
// ok, --stknode now already uses it since it can be now used with plain multi-tree,
// but it is originally intended for mega-tree, and will indeed be used there once
// they are implemented
type CreateRootNodeForPick func(w *NodesWork, ts *NodeSeg, bbox *NodeBounds,
	pseudoSuper *Superblock, pickSegIdx int) *NodeInProcess

const MTP_FOREIGN_EXTRADATA = 0
const MTP_FOREIGN_THIS_ONE_LINEDEF = 1
const MTP_FOREIGN_CUSTOM_LABEL = 2

// To avoid running out of memory, there is a limit on the number of workers
// that can be designated in auto-mode (but this limit will be ignored if user
// tells us an explicit number of workers to use)
// The limit assumes people use multi-tree on vanilla maps, which might not be
// true in practice
const MAX_MTP_WORKERS = 16

// Parameter w is also mutated by this call. Changes to bbox and super are
// undefined
// Second return value is tree count
func MTPSentinel_MakeBestBSPTree(w *NodesWork, bbox *NodeBounds,
	super *Superblock, rootChoiceMethod int, oldNodeType int,
	foreign *MTPForeignInput) (*NodeInProcess, int) {

	rootFunc := MTP_CreateRootNode
	if foreign != nil && foreign.rootFunc != nil { // assign
		rootFunc = foreign.rootFunc
	}

	reportStr := "Multi-tree: processed %d/%d trees\n"
	announceStr := "Multi-tree crunch threads are launched -- waiting for completion reports.\n"

	if foreign == nil || foreign.Action == MTP_FOREIGN_EXTRADATA {
		Log.Printf("Nodes builder: info: you have selected multi-tree mode with bruteforce for root partition only.\n")
		Log.Printf("Multi-tree modes take significant time to compute, and also have rather high memory consumption.\n")
	} else {
		switch foreign.Action {
		case MTP_FOREIGN_EXTRADATA:
			Log.Panic("MTP_FOREIGN_EXTRADATA was supposed to be handled in another branch (programmer error)\n")
		case MTP_FOREIGN_THIS_ONE_LINEDEF:
			rootNode := MTP_OneTree(w, w.allSegs[0], bbox, super, foreign.LineIdx,
				rootFunc)
			return rootNode, 1
		case MTP_FOREIGN_CUSTOM_LABEL:
			reportStr = foreign.reportStr
			announceStr = foreign.announceStr
		default:
			Log.Panic("Unknown foreign action for MTPSentinel: %d (programmer error)\n", foreign.Action)
		}
	}

	// Which segs will be used as starting point?
	var rootSegCandidates []int
	if config.Roots == 0 { // reference to global: config
		rootSegCandidates = MTPSentinel_GetRootSegCandidates(w.allSegs,
			rootChoiceMethod)
	} else {
		// code tree will pick from global config directly the needed things
		// not a good API, admittedly
		rootSegCandidates = MTP_ZenRootEnumerate(w, bbox)
	}
	// whether vanilla nodes format is required or preferred, AND we should be
	// choosing best tree among specifically vanilla whenever any vanilla-compatible
	// tree exists
	vanillaBias := w.nodeType == NODETYPE_VANILLA && !config.Ableist // reference to global: config
	// only bias in favor of best tree to run in vanilla engine with specifically
	// SIGNED indices limits when target is strictly vanilla no compromises allowed
	// (the default nodes format is the only such target)
	strictVanillaBias := vanillaBias && oldNodeType == NODETYPE_VANILLA

	var input2 *NodesInput
	var nodesChan chan NodesResult
	if w.nodeType == NODETYPE_VANILLA &&
		(oldNodeType == NODETYPE_VANILLA_OR_ZEXTENDED ||
			oldNodeType == NODETYPE_VANILLA_OR_ZCOMPRESSED) {
		Log.Verbose(1, "Setting up a fallback to Zdoom nodes generator, in case no trees fit in vanilla\n")
		input2 = &NodesInput{}
		*input2 = *foreign.input
		input2.lines = foreign.linesForZdoom
		input2.nodeType = oldNodeType
		input2.bcontrol = nil   // catch unexpected calls to it
		input2.bgenerator = nil // catch unexpected calls to it
		nodesChan = make(chan NodesResult)
		input2.nodesChan = nodesChan
		input2.solidMap = foreign.solidMap
	}

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
		Log.Printf("Limiting number of threads for multi-tree to %d, because only %d linedefs to try for root.\n",
			workerCount, workerCount)
	}

	Log.Printf("Multi-tree generator will use %d CPUs\n", workerCount)

	// To communicate with threads, channels are needed
	workerChans := make([]chan MTPWorker_Input, workerCount)       // input data for each thread
	workerReplyChans := make([]chan MTPWorker_Result, workerCount) // output result from each thread

	if config.SpeedTree { // reference to global: config
		// Accelerated path
		for i := 0; i < workerCount; i++ {
			workerChans[i] = make(chan MTPWorker_Input)
			workerReplyChans[i] = make(chan MTPWorker_Result)
			// Spawn FAST but MEMORY HUNGRY workers
			go MTPWorker_SpeedGenerateBSPTrees(workerChans[i], workerReplyChans[i],
				rootFunc)
		}
	} else {
		// Non-accelerated path
		for i := 0; i < workerCount; i++ {
			workerChans[i] = make(chan MTPWorker_Input)
			workerReplyChans[i] = make(chan MTPWorker_Result)
			// Spawn workers (they will be fed data later)
			go MTPWorker_GenerateBSPTrees(workerChans[i], workerReplyChans[i],
				rootFunc)
		}
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
		clonedWorkData, clonedBbox := MultiTree_Clone(w, bbox)
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

	Log.Printf(announceStr) // ... crunch threads are launched -- waiting for completion reports.

	// Now peek results of workers as they come, feeding next input to every
	// worker that delivers result until no more inputs to try
	var bestResult, bestVanillaResult, bestStrictVanillaResult *MTPWorker_Result
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
			clonedWorkData, clonedBbox := MultiTree_Clone(w, bbox)
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
		if vanillaBias {
			// If only some trees can be represented in vanilla nodes format, must
			// track those alongside absolute best, because user specified target is
			// either vanilla or "prefer vanilla, fallback to advanced format"
			if bestResult == nil || MTP_IsBSPTreeBetter(bestResult, resp) {
				bestResult = resp
				if !resp.workData.isUnsignedOverflow() {
					bestVanillaResult = resp
				}
			} else if !resp.workData.isUnsignedOverflow() &&
				(bestVanillaResult == nil ||
					MTP_IsBSPTreeBetter(bestVanillaResult, resp)) {
				// compare against previous vanilla best
				bestVanillaResult = resp
			}
			if strictVanillaBias {
				if !resp.workData.isVanillaSignedOverflow() {
					if bestStrictVanillaResult == nil ||
						MTP_IsBSPTreeBetter(bestStrictVanillaResult, resp) {
						bestStrictVanillaResult = resp
					}
				}
			}
		} else {
			// straightforward, for non-vanilla node format targets or ableist mode
			if bestResult == nil || MTP_IsBSPTreeBetter(bestResult, resp) {
				bestResult = resp
			}
		}

		Log.Printf(reportStr, treesDone, maxTrees)
	}

	oldBestResult := bestResult
	if bestStrictVanillaResult != nil {
		bestResult = bestStrictVanillaResult
	} else if bestVanillaResult != nil {
		bestResult = bestVanillaResult
	}

	Log.Printf("Finished processing trees (%d out of %d - should be equal)", treesDone,
		lastFedIdx+1)

	// Assess compromises made
	if vanillaBias && bestVanillaResult == nil {
		Log.Printf("No trees could fit under vanilla format.\n")
	} else if bestVanillaResult != nil && bestVanillaResult.id != oldBestResult.id {
		Log.Printf("I've chosen a tree representable in vanilla nodes format, though a better tree existed outside vanilla format limits.\n")
	}
	if bestStrictVanillaResult != nil && bestStrictVanillaResult.id != bestVanillaResult.id {
		Log.Printf("I've chosen a tree that satisfies signed integer cap of vanilla engine, though a better tree existed for limit-removing ports that allow unsigned integers.\n")
	}

	if (oldNodeType == NODETYPE_VANILLA_OR_ZEXTENDED ||
		oldNodeType == NODETYPE_VANILLA_OR_ZCOMPRESSED) && !isZDoomNodes() {
		// can't rely on bestVanillaResult, as vanillaBias may have been disabled by
		// user
		if bestResult.workData.isUnsignedOverflow() {
			// Limits exceeded, must upgrade
			Log.Printf("(multi-tree) vanilla nodes format overflowed for ALL trees, will run Zdoom nodes format generator for a root seg that corresponded to best tree\n")
			// print intermediary timer, it might hang around the screen for a while
			// since we are obviously building a large level
			Log.Printf("   time spent on trees prior: %s\n", time.Since(foreign.start))
			foreign.input.lines.AssignFrom(input2.lines)
			li := new(uint16)
			*li = w.allSegs[rootSegCandidates[bestResult.id]].Linedef
			input2.linedefForMTP = li
			go ZNodesGenerator(input2)
			// this interception is because timer is only output from here
			nodeResult := <-nodesChan
			foreign.input.nodesChan <- nodeResult
			// we already sent result through channel, will return
			return MTP_nilAndPrintTimer(foreign.start, treesDone+1, oldNodeType), 0
		}
	}

	// Splice workData struct with one that is associated with bestResult
	oldLines := w.lines // !!! level.go retains reference to old w.lines
	*w = *(bestResult.workData)
	oldLines.AssignFrom(w.lines) // so must clobber the data in old w.lines with the new one
	w.lines = oldLines           // make sure it points to the actual, so that --stknode works
	return bestResult.bspTree, treesDone
}

func MTP_nilAndPrintTimer(start time.Time, treeCount, oldNodeType int) *NodeInProcess {
	switch promoteNodeType(oldNodeType) {
	case NODETYPE_ZDOOM_EXTENDED:
		Log.Printf("I have switched to ZDoom extended nodes format to avoid overflow.\n")
	case NODETYPE_ZDOOM_COMPRESSED:
		Log.Printf("I have switched to ZDoom compressed nodes format to avoid overflow.\n")
	}
	dur := time.Since(start)
	avg := time.Duration(int64(dur) / int64(treeCount))
	Log.Printf("Nodes took %s (avg %s per tree)\n", dur, avg)
	return nil
}

// Really banal bsp tree comparison. Tries to minimize subsector count first,
// then bsp tree height difference, then bsp tree height itself, then seg count
// NOTE also added check for precious splits
func IsBSPTreeBetter(oldWD *NodesWork, oldNIP *NodeInProcess,
	newWD *NodesWork, newNIP *NodeInProcess) int {
	// Prefer trees where no precious segs or polyobj sector borders were split
	// But, don't compare numbers itself as a lot of Hexen maps have setups that
	// deviate from "convex sector" ideal and are not getting broken by those
	// splits really
	oldBad := oldWD.totals.preciousSplit > 0
	newBad := newWD.totals.preciousSplit > 0
	if oldBad && !newBad {
		return 1
	} else if !oldBad && newBad {
		return -1
	}
	// Compare subsector count
	oldSsectorsCnt := oldWD.totals.numSSectors
	newSSectorsCnt := newWD.totals.numSSectors
	if newSSectorsCnt < oldSsectorsCnt {
		return 1
	} else if newSSectorsCnt > oldSsectorsCnt {
		return -1
	}

	// Subsector count equal
	// Let's see BSP height
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
	// Smaller difference in BSP height wins
	if newDiff < oldDiff {
		return 1
	} else if newDiff > oldDiff {
		return -1
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
		return 1
	} else if newMaxHeight > oldMaxHeight {
		return -1
	}

	// Even maximum height was equal, wtf
	// Ok, let's minimize seg count then
	oldSegCnt := oldWD.totals.numSegs
	newSegCnt := newWD.totals.numSegs
	if newSegCnt < oldSegCnt {
		return 1
	} else if newSegCnt > oldSegCnt {
		return -1
	}
	return 0
}

// BSP tree comparison for plain multitree. If both trees are equally good, tree
// that came from earlier seg wins (used to ensure determinism)
func MTP_IsBSPTreeBetter(oldResult *MTPWorker_Result, newResult *MTPWorker_Result) bool {
	ir := IsBSPTreeBetter(oldResult.workData, oldResult.bspTree,
		newResult.workData, newResult.bspTree)
	if ir > 0 {
		return true
	} else if ir < 0 {
		return false
	}
	// else got ir = 0
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
func MultiTree_Clone(w *NodesWork, bbox *NodeBounds) (*NodesWork, *NodeBounds) {
	clonedWorkData := w.GetInitialStateClone()
	clonedWorkData.mlog = CreateMiniLogger()
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

// The non-accelerated version of multitree_plain worker goroutine
func MTPWorker_GenerateBSPTrees(input <-chan MTPWorker_Input,
	replyTo chan<- MTPWorker_Result, rootFunc CreateRootNodeForPick) {
	for permutation := range input {
		tree := rootFunc(permutation.workData, permutation.ts, permutation.bbox,
			permutation.pseudoSuper, permutation.pickSegIdx)

		replyTo <- MTPWorker_Result{
			workData: permutation.workData,
			bspTree:  tree,
			id:       permutation.id,
		}
	}
	close(replyTo)
}

// The speedy variant of multitree_plain worker goroutine.
// sync.Pool acts as a stabilizer, decreasing memory consumption from factor
// of 6x greater than non-speed variant to 3x or less. This is necessary to
// prevent worse performance on certain inputs (usually the more complex)
// compared to non-speed version, even if the price is that friendlier inputs
// get lesser speed improvements than without Pool.
// It seems too big memory use alone can defeat optimization, sync.Pool prevents
// that
func MTPWorker_SpeedGenerateBSPTrees(input <-chan MTPWorker_Input,
	replyTo chan<- MTPWorker_Result, rootFunc CreateRootNodeForPick) {

	var qallocSupers *Superblock
	bhPool := sync.Pool{
		New: func() interface{} {
			return make([]BlocksHit, 0)
		},
	}

	// Loop over actual tasks
	for permutation := range input {

		// Reuse special structures from previous tasks
		if qallocSupers != nil {
			permutation.workData.qallocSupers = qallocSupers
			permutation.workData.blocksHit = bhPool.Get().([]BlocksHit)
		}

		// Build BSP tree for this task
		tree := rootFunc(permutation.workData, permutation.ts, permutation.bbox,
			permutation.pseudoSuper, permutation.pickSegIdx)

		// Make special structures reusable for future tasks. Was not exactly
		// easy to make gc cooperative, so any changes to this code must be
		// checked against regressions
		qallocSupers = permutation.workData.qallocSupers
		permutation.workData.qallocSupers = nil
		blocksHit := permutation.workData.blocksHit[:0]
		permutation.workData.blocksHit = nil
		bhPool.Put(blocksHit)

		replyTo <- MTPWorker_Result{
			workData: permutation.workData,
			bspTree:  tree,
			id:       permutation.id,
		}
	}
	close(replyTo)
}

// pseudoSuper parameter may be a pseudo, but may be also a normal root superblock
// must satisfy CreateRootNodeForPick calling conventions
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
		res.LChild = w.CreateSSector(lefts) | SSECTOR_DEEP_MASK
		w.returnSuperblockToPool(leftsSuper)
	} else if state == NONCONVEX_ONESECTOR {
		res.nextL = w.createNodeSS(w, lefts, leftBox, leftsSuper)
		res.LChild = 0
	} else {
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
		res.RChild = w.CreateSSector(rights) | SSECTOR_DEEP_MASK
		w.returnSuperblockToPool(rightsSuper)
	} else if state == NONCONVEX_ONESECTOR {
		res.nextR = w.createNodeSS(w, rights, rightBox, rightsSuper)
		res.RChild = 0
	} else {
		res.nextR = CreateNode(w, rights, rightBox, rightsSuper)
		res.RChild = 0
	}
	rightsSuper = nil
	return res
}

func (w *NodesWork) MTP_DivideSegs(ts *NodeSeg, rs **NodeSeg, ls **NodeSeg,
	bbox *NodeBounds, pseudoSuper *Superblock, rightsSuper, leftsSuper **Superblock,
	chosen *NodeSeg) {
	best := chosen

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

	w.DivideSegsActual(ts, rs, ls, bbox, best, c, pseudoSuper, rightsSuper, leftsSuper)
}

// This was supposed to preallocate a "cache line" of superblocks ready to use
// at the beginning of fast workers (MTPWorker_SpeedGenerateBSPTrees)
// It didn't contribute anything to performance
func zoneAllocSupers(sz int, pseudoSuper *Superblock) *Superblock {
	zone := make([]Superblock, sz)
	dummy := NodesWork{}
	dummySeg := &NodeSeg{}
	for i, _ := range zone {
		// without segs, will be mistaken for a pseudosuper and not become a
		// part of cache
		zone[i].segs = dummySeg
		// NOTE I actually put root superblocks' sector and secEquiv len in
		// pseudoSuper.x1, but this is not used and InitSectorsIfNeeded uses
		// default capacity. Makes the entire concept of preallocated pool
		// pretty worthless, as arrays for the first many superblocks will get
		// reallocated
		zone[i].InitSectorsIfNeeded(pseudoSuper)
		dummy.returnSuperblockToPool(&(zone[i]))
	}
	return dummy.qallocSupers
}

func MTP_OneTree(w *NodesWork, ts *NodeSeg, bbox *NodeBounds,
	super *Superblock, lineIdx uint16, rootFunc CreateRootNodeForPick) *NodeInProcess {
	cur := w.allSegs[0].Linedef
	idx := 0
	for ; idx < len(w.allSegs); idx++ {
		cur = w.allSegs[idx].Linedef
		if cur == lineIdx {
			break
		}
	}
	if cur != lineIdx {
		Log.Panic("Seg from linedef %d is not found\n", lineIdx)
	}
	return rootFunc(w, ts, bbox, super, idx)
}

func MTP_ZenRootEnumerate(w *NodesWork, bbox *NodeBounds) []int {

	w2, bbox2 := MultiTree_Clone(w, bbox)

	w2.sectorHits = make([]uint8, len(w2.sectors))
	w2.blocksHit = make([]BlocksHit, 0)
	substSuper := w2.doInitialSuperblocks(bbox2, true) // with sectors, always
	w2.multipart = true
	PickNode_ZennodeDepth(w2, w2.allSegs[0], bbox2, substSuper)
	res := make([]int, len(w2.parts))
	for i := range w2.parts {
		for j := range w2.allSegs {
			if w2.allSegs[j].Linedef == w2.parts[i].Linedef {
				res[i] = j
			}
		}
	}

	Log.Merge(w2.mlog, "")
	return res
}
