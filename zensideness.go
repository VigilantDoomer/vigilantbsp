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
	"time"
)

// zensideness module is about optimization of zenlike algorithm for multi-tree
// mode.
// This is only useful for multi-tree (although can be forced for single-tree
// via a debug parameter), because the time to build cache is significant,
// while superblocks technology eliminates quite a lot of possible queries to it

// Speeds up repeated access to WhichSide* values for Zennode-like partitioners
// (effective when multi-tree is used)
// Cache needs to be fully built before use as it offers no way to discern
// <no data> from actual values (except by not allowing aliases later than cache
// and blacklisting split or otherwise undesirable segs), and has to be always
// immutable during partitioning process.
type SidenessCache struct {
	maxKnownAlias int     // aliases greater than this CAN get generated but are not cached here
	readOnly      bool    // a flag to catch logical mistakes made by programmer when refactoring (expected value is always true)
	data          []uint8 // row - partition alias, col - check seg's linedef
	colCount      int
}

const (
	SIDENESS_COLLINEAR = 0x0
	SIDENESS_INTERSECT = 0x1
	SIDENESS_RIGHT     = 0x2
	SIDENESS_LEFT      = 0x3
)

// buildSidenessCache does all the operations required to bring sidenessCache
// to proper state. The cache in question is used to make Zennode-like
// partitioner fast(er)
// All segs must be reachable from rootSeg, and in super as well
// Memory efficiency - DONE (one cell stores values for 4 keys. One key is
// a compound <alias,linedef> - implementation-wise, linedef indices are
// substituted for a reordering of them that improves cache locality)
// TODO advanced memory efficiency - not store certain segs in cache (those
// which are excluded by superblocks 90% of the time). Need heuristic for this,
// and putting flag SEG_FLAG_NOCACHE on those segs and not assigning them
// sidenessIdx. To invent the heuristic, will need to gather some data on which
// cells tend to be never accessed (debug mode - add array to keep track of
// visits, perhaps even number of those?), make statistical analysis, brainstorm
// hypothesis etc.
func (w *NodesWork) buildSidenessCache(rootSeg *NodeSeg, super *Superblock) {
	start := time.Now()
	Log.Printf("[nodes] Building cache for sideness\n")
	lineCount := w.precomputeAliasesForCache(rootSeg, super)
	w.sidenessCache.maxKnownAlias = w.segAliasObj.maxAlias // more aliases can generated later, but cache will not have data for them
	w.sidenessCache.colCount = int(lineCount)
	w.sidenessCache.data = make([]uint8, // 2 bits per value
		(w.sidenessCache.maxKnownAlias*w.sidenessCache.colCount)>>2+1)
	setSidenessIdxForAll(super)
	w.computeAndLockSidenessCache(rootSeg, super)
	Log.Printf("[nodes] sideness cache took %s\n", time.Since(start))
}

// precomputeAliasesForCache assigns initial aliases to linedefs, as the only
// practical way to represent cache is as array. Cache will store results only
// for these aliases, and not any that are generated later.
// Zennode-like partitioner needs this to create cache for doLinesIntersect
// results for <alias (of partition candidate),linedef (of seg being checked),
// flip (seg relative to linedef)> key combination
// Returns number of indexable linedefs (since not every linedef might have
// a corresponding seg)
func (w *NodesWork) precomputeAliasesForCache(ts *NodeSeg, super *Superblock) int {
	var previousPart *NodeSeg // keep track of previous partition - test only one seg per partner pair
	w.segAliasObj.UnvisitAll()
	var c IntersectionContext
	lines := make(map[uint16]bool)
	for part := ts; part != nil; part = part.next { // Use each Seg as partition
		if part.partner != nil && part.partner == previousPart {
			// Partner segs are kept next to each other, they would result in
			// same nodeline - so skip second partner
			continue
		}
		lines[part.Linedef] = true
		if part.alias != 0 {
			if w.segAliasObj.MarkAndRecall(part.alias) {
				// More advanced way to skip all colinear segs (which would also
				// create the exact same nodeline). This check is more
				// expensive than partnership check (verified on big maps)
				continue
			}
		} else { // = 0 means alias was not assigned (...)
			// Generate and assign new alias
			// Note we don't assign anything to partner HERE, partners are skipped
			// as part of big loop but get covered by inner loop anyway
			part.alias = w.segAliasObj.Generate()
			// Aliases get copied in the inner loop: when a line we are checking
			// is colinear to partition, it "inherits" alias from partition
		}
		previousPart = part // used for check above
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

func (w *NodesWork) evalComputeAliasesWorker(block *Superblock, part *NodeSeg,
	c *IntersectionContext) {

forblock:
	// -AJA- this is the heart of my superblock idea, it tests the
	//       _whole_ block against the partition line to quickly handle
	//       all the segs within it at once.  Only when the partition
	//       line intercepts the box do we need to go deeper into it.
	num := BoxOnLineSide(block, part)
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
					// see evalPartitionWorker_ZennodeDepth for why to vet
					// this more
					if check.alias != part.alias && vetAliasTransfer(c) {
						check.alias = part.alias
					}
				}
			}
		}
	}

	// handle sub-blocks recursively

	if block.subs[0] != nil {
		w.evalComputeAliasesWorker(block.subs[0], part, c)
	}

	// tail call eliminated for second block
	if block.subs[1] != nil {
		block = block.subs[1]
		goto forblock
	}
}

// setSidenessIdxForAll tries to make the order in which cache is going to
// be accessed closer to sequential. The seg creation order, that corresponds
// to linedefs indices, is not the order of traversal because superblocks are
// used, so this works on assumption that the order of traversal in future
// superblocks would be somewhat similar to that in original superblock (never
// proven)
func setSidenessIdxForAll(super *Superblock) {
	remap := make(map[uint16]int)
	gen := 0
	evalSidenessIdx(super, remap, &gen)
}

func evalSidenessIdx(block *Superblock, remap map[uint16]int, gen *int) {
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

		evalSidenessIdx(block.subs[num], remap, gen)
	}
}

// For multi-tree, so that we have shared read-only sideness cache
func (w *NodesWork) computeAndLockSidenessCache(ts *NodeSeg, super *Superblock) {
	if w.sidenessCache.readOnly {
		Log.Panic("Can't compute sideness cache - already locked (read only) (programmer error)")
		return
	}
	if w.sidenessCache.maxKnownAlias > 0 {
		var previousPart *NodeSeg // keep track of previous partition - test only one seg per partner pair
		w.segAliasObj.UnvisitAll()
		var c IntersectionContext
		for part := ts; part != nil; part = part.next { // Use each Seg as partition
			if getGlobalFlip(part) {
				part.flags |= SEG_FLAG_GLOBAL_FLIP
			}
			if part.partner != nil && part.partner == previousPart {
				// Partner segs are kept next to each other, they would result in
				// same nodeline - so skip second partner
				continue
			}
			if part.alias != 0 {
				if w.segAliasObj.MarkAndRecall(part.alias) {
					// More advanced way to skip all colinear segs (which would also
					// create the exact same nodeline). This check is more
					// expensive than partnership check (verified on big maps)
					continue
				}
			} else { // = 0 means alias was not assigned (...)
				// Generate and assign new alias
				// Note we don't assign anything to partner HERE, partners are skipped
				// as part of big loop but get covered by inner loop anyway
				part.alias = w.segAliasObj.Generate()
				// Aliases get copied in the inner loop: when a line we are checking
				// is colinear to partition, it "inherits" alias from partition
			}
			previousPart = part // used for check above
			c.psx = part.psx
			c.psy = part.psy
			c.pex = part.pex
			c.pey = part.pey
			c.pdx = c.psx - c.pex
			c.pdy = c.psy - c.pey
			w.evalComputeSidenessCache(super, part, &c)
		}
		w.segAliasObj.UnvisitAll() // so that data to be purged is not copied for multi-tree
	}
	w.sidenessCache.readOnly = true
}

func (w *NodesWork) evalComputeSidenessCache(block *Superblock, part *NodeSeg,
	c *IntersectionContext) {

forblock:
	// -AJA- this is the heart of my superblock idea, it tests the
	//       _whole_ block against the partition line to quickly handle
	//       all the segs within it at once.  Only when the partition
	//       line intercepts the box do we need to go deeper into it.
	num := BoxOnLineSide(block, part)
	if num < 0 {
		// LEFT
		w.storeEntireBlockSideness(block, part, SIDENESS_LEFT)
		return
	} else if num > 0 {
		// RIGHT
		w.storeEntireBlockSideness(block, part, SIDENESS_RIGHT)
		return
	}

	for check := block.segs; check != nil; check = check.nextInSuper {
		w.computeAndCacheSideness(part, check, c)
	}

	// handle sub-blocks recursively

	if block.subs[0] != nil {
		w.evalComputeSidenessCache(block.subs[0], part, c)
	}

	// tail call eliminated for second block
	if block.subs[1] != nil {
		block = block.subs[1]
		goto forblock
	}
}

func (w *NodesWork) storeEntireBlockSideness(block *Superblock, part *NodeSeg,
	sideness uint8) {

forblock:
	for check := block.segs; check != nil; check = check.nextInSuper {
		w.storeSidenessDirectly(part, check, sideness)
	}

	// handle sub-blocks recursively

	if block.subs[0] != nil {
		w.storeEntireBlockSideness(block.subs[0], part, sideness)
	}

	// tail call eliminated for second block
	if block.subs[1] != nil {
		block = block.subs[1]
		goto forblock
	}
}

func (w *NodesWork) WhichSideCached(part, check *NodeSeg,
	c *IntersectionContext) uint8 {
	if part.alias == 0 || part.alias > w.sidenessCache.maxKnownAlias ||
		(check.flags&SEG_FLAG_NOCACHE != 0) {
		// not in cache
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

func flipVal(val uint8) uint8 {
	if val == SIDENESS_LEFT {
		return SIDENESS_RIGHT
	}
	if val == SIDENESS_RIGHT {
		return SIDENESS_LEFT
	}
	return val
}

// computeAndCacheSideness will calculate value for given partition alias and
// checked seg and store it. Clobbering existing values not allowed
func (w *NodesWork) computeAndCacheSideness(part, check *NodeSeg, c *IntersectionContext) {
	if part.alias == 0 || part.alias > w.sidenessCache.maxKnownAlias ||
		(check.flags&SEG_FLAG_NOCACHE != 0) {
		return
	}
	cell := (part.alias-1)*w.sidenessCache.colCount + check.sidenessIdx
	mov := (cell & 0x3) << 1
	cell >>= 2
	raw := w.sidenessCache.data[cell]
	subraw := raw >> mov & 0x3
	if subraw != 0 { // although strictly speaking, 0 is a value too
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

// storeSidenessDirectly tells cache to store specific value for partition alias
// and checked seg rather than compute it. Clobbering existing values still not
// allowed
func (w *NodesWork) storeSidenessDirectly(part, check *NodeSeg, sideness uint8) {
	if part.alias == 0 || part.alias > w.sidenessCache.maxKnownAlias ||
		(check.flags&SEG_FLAG_NOCACHE != 0) {
		return
	}
	cell := (part.alias-1)*w.sidenessCache.colCount + check.sidenessIdx
	mov := (cell & 0x3) << 1
	cell >>= 2
	raw := w.sidenessCache.data[cell]
	subraw := raw >> mov & 0x3
	if subraw != 0 { // although strictly speaking, 0 is a value too
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

func getGlobalFlip(part *NodeSeg) bool {
	return IntVertexPairCOrdering(part.StartVertex, part.EndVertex, false)
}

// vetAliasTransfer checks whether checked seg is still considered collinear
// to partition line when collinearity is tested with higher (extended
// nodes-grade) precision.
// One of the "tests" in nodegen_test.go gives case where vanilla intersection
// checker falsely reports collinearity. It seems that it is especially likely
// to occur if checked seg is short (<= 2 px of dx/dy)
// Partition algorithms that utilize cache for doLinesIntersect(or WhichSide)
// need to be cautious about transfering aliases, unlike algorithms that
// evaluate doLinesIntersect using specific lines. Wrong alias in partition
// algorithm without cache is merely one missed partition candidate, whereas
// wrong alias in cache can create avalance effect and introduce gross errors to
// many partition candidates' score, causing very suboptimal (in reality)
// candidates to be selected
// For this reason, it is also not recommended to use cache when partition
// actually occurs (DivideSegs) as opposed to merely evaluated
func vetAliasTransfer(c *IntersectionContext) bool {
	val := ZdoLinesIntersect_Proto(c)
	return (val&1 != 0) && (val&16 != 0)
}

// Similar, but for partitioners that use even less precise check than
// doLinesIntersect by default and so don't have intersection context ready
func vetAliasTransfer2(part, check *NodeSeg) bool {
	if check.len >= 4 {
		return true
	}
	c := &IntersectionContext{
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
	val := ZdoLinesIntersect_Proto(c)
	return (val&1 != 0) && (val&16 != 0)
}

func (w *NodesWork) WhichSideInternal(check *NodeSeg,
	c *IntersectionContext) uint8 {
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
	return 0 // never happens
}
