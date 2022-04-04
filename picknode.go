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
	"math"
	"sort"
)

// To be able to divide the nodes down, this routine must decide which is the
// best Seg to use as a nodeline. Credit to Raphael Quinet and DEU for the
// original implementation, Lee Killough for performance improvements and
// creating algorithm variation that reduces visplanes.
// Futher performance improvements on big levels thanks to AJ-BSP and Zennode
// (TODO: credit authors)

// VigilantDoomer: I use partner segs, superblocks (both ideas from AJ-BSP)
// and aliases (idea from Zennone) to drastically speed up PickNode* algos.

// Direct cost value attributed to a partition by a particular undesirable
// impact on something - default value, can be reconfigured
const PICKNODE_FACTOR = 17 // default factor of BSP v5.2.

// Cost multiplier for splitting a seg that should have remained unsplit
const PRECIOUS_MULTIPLY = 64

const INITIAL_BIG_COST = 2147483647 // 32-bit signed positive max for compatibility with 32-bit executable

// Direct cost value attributed to a partition because it is not axis-aligned
// - default value, can be reconfigured
const DIAGONAL_PENALTY = 34

// Multiplier for partitions that would have all segs on only one side of it
// according to the slower, but more correct, doLinesIntersect() function.
// (used by some - not all - PickNode implementations)
const ONESIDED_MULTIPLY = 20

// How much segs (>=) needs to be in a node for PickNode_maelstrom to attempt
// the shortcut rather than applying traditional algorithm of balancing seg
// count on both sides
const SEG_FAST_THRESHHOLD = 200

// TODO port "passing through a vertex of precious linedef" check from AJ-BSP

// PickNode_traditional is an implementation of PickNode that is classic (since
// DEU5beta source code (c) Raphael Quinet) way to pick a partition: partitions
// are chosen based on seg so that there is minimal amount of seg splits and the
// difference in the number of segs on both sides is minimal.
func PickNode_traditional(w *NodesWork, ts *NodeSeg, bbox *NodeBounds,
	super *Superblock) *NodeSeg {
	best := ts                        // make sure always got something to return
	bestcost := int(INITIAL_BIG_COST) //
	cnt := 0

	for part := ts; part != nil; part = part.next { // Count once and for all
		cnt++
	}
	var previousPart *NodeSeg // keep track of previous partition - test only one seg per partner pair

	w.segAliasObj.UnvisitAll() // remove marks from previous PickNode calls

	for part := ts; part != nil; part = part.next { // Use each Seg as partition
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
		} else { // = 0 means alias was not assigned (or was intentionally dropped when segs were split)
			// Generate and assign new alias
			// Note we don't assign anything to partner HERE, partners are skipped
			// as part of big loop but get covered by inner loop anyway
			part.alias = w.segAliasObj.Generate()
			// Aliases get copied in the inner loop: when a line we are checking
			// is colinear to partition, it "inherits" alias from partition
		}
		previousPart = part // used for check above
		cost := 0
		tot := 0
		diff := cnt

		// See PickNode_visplaneKillough for explanation. There it happens in
		// a different place - but here we don't count sectors, so this ends up
		// being applied always, if the cost was enabled -- VigilantDoomer
		if w.diagonalPenalty != 0 && part.pdx != 0 && part.pdy != 0 {
			cost += w.diagonalPenalty
		}

		//progress();           	        // Something for the user to look at.

		prune := w.evalPartitionWorker_Traditional(super, part, &tot, &diff,
			&cost, bestcost)
		if prune { // Early exit and skip past the tests below
			continue
		}

		// Take absolute value. diff is being used to obtain the
		// min/max values by way of: min(a,b)=(a+b-abs(a-b))/2

		diff -= tot
		if diff < 0 {
			diff = -diff
		}

		// Make sure at least one Seg is on each side of the partition
		if (tot + cnt) > diff {
			cost += diff
			if cost < bestcost {
				// We have a new better choice
				bestcost = cost
				best = part // Remember which Seg
			}
		}

	}
	return best // All finished, return best Seg
}

// If returns true, the partition &part must be skipped, because it produced
// many splits early so that cost exceed bestcost
func (w *NodesWork) evalPartitionWorker_Traditional(block *Superblock,
	part *NodeSeg, tot, diff, cost *int, bestcost int) bool {

	// -AJA- this is the heart of my superblock idea, it tests the
	//       _whole_ block against the partition line to quickly handle
	//       all the segs within it at once.  Only when the partition
	//       line intercepts the box do we need to go deeper into it.
	num := BoxOnLineSide(block, part)
	if num < 0 {
		// LEFT
		*diff -= 2 * block.realNum
		return false
	} else if num > 0 {
		// RIGHT
		return false
	}

	for check := block.segs; check != nil; check = check.nextInSuper { // Check partition against all Segs
		// get state of lines' relation to each other
		leftside := false
		a := part.pdy*check.psx - part.pdx*check.psy + part.perp
		b := part.pdy*check.pex - part.pdx*check.pey + part.perp
		if (a ^ b) < 0 {
			if (a != 0) && (b != 0) {
				// Line is split; a,b nonzero, opposite sign
				l := check.len
				// Distance from start of intersection
				// !!! 32-bit version of BSP v5.2 had overflow here on big
				// lines at a formidable distance. Must use 64-bit math here!
				// l*a may be too big for int32! -- VigilantDoomer
				d := int((int64(l) * int64(a)) / (int64(a) - int64(b)))
				if d >= 2 {
					// If the linedef associated with this seg has a sector tag >= 900,
					// treat it as precious; i.e. don't split it unless all other options
					// are exhausted. This is used to protect deep water and invisible
					// lifts/stairs from being messed up accidentally by splits. - Killough
					// VigilantDoomer: so is a tag on a linedef, or on a sector?
					// Currently put on the linedef, but damn, there are other special
					// tag values >= 900, not sure if I should give them this meaning too
					if w.lines.IsTaggedPrecious(check.Linedef) {
						// If this seg will have to be split anyway, prefer
						// it done by axis-aligned partition line for better
						// Hexen polyobj compatibility
						if w.diagonalPenalty != 0 && part.pdx != 0 && part.pdy != 0 {
							*cost += w.pickNodeFactor * PRECIOUS_MULTIPLY * 2
						} else {
							*cost += w.pickNodeFactor * PRECIOUS_MULTIPLY
						}
					}

					*cost += w.pickNodeFactor
					if *cost > bestcost {
						// This is the heart of my pruning idea
						// it catches bad segs early on. Killough
						return true
					}
					(*tot)++
				} else if checkPorn1(l, d, check.pdx, part.pdx, check.pdy, part.pdy, b) {
					leftside = true
				}
			} else {
				leftside = true
			}
		} else if a <= 0 {
			if a != 0 {
				leftside = true
			} else if b == 0 {
				// a=0 && b=0 => co-linear, must share alias
				// since partners are colinear, they also get aliases this
				// way, sooner or later (the latter may happen when a few
				// earlier colinear segs get pruned in "seg split by
				// partition" found branch before reaching here)
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
	// handle sub-blocks recursively

	for num := 0; num < 2; num++ {
		if block.subs[num] == nil {
			continue
		}

		if w.evalPartitionWorker_Traditional(block.subs[num], part, tot, diff,
			cost, bestcost) {
			return true
		}
	}

	// no "bad seg" was found
	return false
}

func checkPorn1(l, d, cpdx, ppdx, cpdy, ppdy, b int) bool {
	if (l - d) < 2 {
		return cpdx*ppdx+cpdy*ppdy < 0
	} else {
		return b < 0
	}
}

// PickNode_visplaneKillough is an implementation of PickNode suggested by
// Lee Killough which builds on Raphael Quinet idea by additionally balancing
// the number of sectors on both sides of would-be partition AND rejecting
// partitions that are incident with segs in less than half of partition line
// length (the latter can cut across large rooms in a way that prevents dynamic
// merging of visplanes implemented in Doom's engine)
// BEGIN QUOTE
// Lee Killough 06/1997:
//
// The chances of visplane overflows can be reduced by attemping to
// balance the number of distinct sector references (as opposed to
// SEGS) that are on each side of the node line, and by rejecting
// node lines that cut across wide open space, as measured by the
// proportion of the node line which is incident with segs, inside
// the bounding box.
//
// Node lines which are extensions of linedefs whose vertices are
// on the boundary of the bounding box, are therefore preferable,
// as long as the number of sectors referenced on either side is
// not too unbalanced.
//
// Contrary to what many say, visplane overflows are not simply
// caused by too many sidedefs, linedefs, light levels, etc. All
// of those factors are correlated with visplane overflows, but
// most importantly, so is how the node builder selects node
// lines. The number of visible changes in flats is the main
// cause of visplane overflows, with visible changes not being
// counted if only invisible regions separate the visible areas.
// END QUOTE
func PickNode_visplaneKillough(w *NodesWork, ts *NodeSeg, bbox *NodeBounds,
	super *Superblock) *NodeSeg {
	best := ts                        // make sure always got something to return
	bestcost := int(INITIAL_BIG_COST) //
	cnt := 0

	for part := ts; part != nil; part = part.next { // Count once and for all
		cnt++
	}

	var previousPart *NodeSeg // keep track of previous partition - test only one seg per partner pair

	w.segAliasObj.UnvisitAll() // remove marks from previous PickNode calls

	for part := ts; part != nil; part = part.next { // Use each Seg as partition
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
		} else { // = 0 means alias was not assigned (or was intentionally dropped when segs were split)
			// Generate and assign new alias
			// Note we don't assign anything to partner HERE, partners are skipped
			// as part of big loop but get covered by inner loop anyway
			part.alias = w.segAliasObj.Generate()
			// Aliases get copied in the inner loop: when a line we are checking
			// is colinear to partition, it "inherits" alias from partition
		}
		previousPart = part // used for check above
		cost := 0
		tot := 0
		slen := 0 // length of partition that is incidental with segs
		diff := cnt
		// Fill sectorHits array with zeros FAST (fewer bound checks)
		// Credit: gist github.com taylorza GO-Fillslice.md
		w.sectorHits[0] = 0
		sectorCount := len(w.sectorHits)
		for j := 1; j < sectorCount; j = j << 1 {
			copy(w.sectorHits[j:], w.sectorHits[:j])
		}

		//progress();           	        // Something for the user to look at.
		w.blocksHit = w.blocksHit[:0]
		prune := w.evalPartitionWorker_VisplaneKillough(super, part, &tot, &diff,
			&cost, bestcost, &slen)
		if prune { // Early exit and skip past the tests below
			continue
		}

		// Take absolute value. diff is being used to obtain the
		// min/max values by way of: min(a,b)=(a+b-abs(a-b))/2

		diff -= tot
		if diff < 0 {
			diff = -diff
		}

		// Make sure at least one Seg is on each side of the partition
		if tot+cnt <= diff {
			continue
		}

		// Apply missing sectorHits from evalPartitionWorker
		for _, hitRecord := range w.blocksHit {
			hitRecord.block.MarkSectorsHit(w.sectorHits, hitRecord.mask)
		}

		/* Compute difference in number of sector
		   references on each side of node line */
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

		// VigilantDoomer: this is new logic, not from BSP v5.2. If partitioning
		// something with multiple sectors (something that is non-convex single
		// sector), penalize diagonal lines, as they might make splitting lines
		// we don't want to split unavoidable. This is especially important for
		// maps that have polyobjects, but also tends to reduce visplanes at
		// the cost of creating more seg splits overall.
		// When partitioning non-convex single sector, on the other hand, this
		// can't affect visplanes nor should it fuck up polyobjects (more
		// research needed though, but I reckon polyobjects need to be in convex
		// sectors to begin with), this is better skipped to tame the final seg
		// splits count
		if flat > 1 && w.diagonalPenalty != 0 && (part.pdx != 0 && part.pdy != 0) {
			cost += w.diagonalPenalty
		}
		cost += diff
		if cost >= bestcost {
			continue
		}

		// Lee Killough's second row of visplane reduction logic:
		// "If the node line is incident with SEGS in less than 1/2th of its
		// length inside the bounding box, increase the cost since this is
		// _likely_ a node line cutting across a large room but only sharing
		// space with a tiny SEG in the middle -- this is another contributor
		// to visplane overflows."
		// NOTE If the part of node line that is not incidental with segs
		// is cutting the void space, it is a very good pick that gets rejected
		l := GetPartitionLength_LegacyWay(part, bbox)

		// Lee Killough did it this way. Instead of "1/2th of length inside
		// the bounding box", like in the description above, we are comparing
		// full length here. Probably expecting slen to be computed from
		// segs that have partners, as would segs coming from 2-sided lines do.
		//  --VigilantDoomer
		if slen < l {
			cost += w.pickNodeFactor
			if cost >= bestcost {
				continue
			}
		}

		// We have a new better choice
		bestcost = cost
		best = part // Remember which Seg
	}
	return best // All finished, return best Seg
}

// If returns true, the partition &part must be skipped, because it produced
// many splits early so that cost exceed bestcost
func (w *NodesWork) evalPartitionWorker_VisplaneKillough(block *Superblock,
	part *NodeSeg, tot, diff, cost *int, bestcost int, slen *int) bool {

	// -AJA- this is the heart of my superblock idea, it tests the
	//       _whole_ block against the partition line to quickly handle
	//       all the segs within it at once.  Only when the partition
	//       line intercepts the box do we need to go deeper into it.
	num := BoxOnLineSide(block, part)
	if num < 0 {
		// LEFT
		*diff -= 2 * block.realNum
		w.blocksHit = append(w.blocksHit, BlocksHit{
			block: block,
			mask:  uint8(1),
		})
		return false
	} else if num > 0 {
		// RIGHT
		w.blocksHit = append(w.blocksHit, BlocksHit{
			block: block,
			mask:  uint8(2),
		})
		return false
	}

	for check := block.segs; check != nil; check = check.nextInSuper { // Check partition against all Segs
		// get state of lines' relation to each other
		leftside := false
		a := part.pdy*check.psx - part.pdx*check.psy + part.perp
		b := part.pdy*check.pex - part.pdx*check.pey + part.perp
		mask := uint8(2)
		if (a ^ b) < 0 {
			if (a != 0) && (b != 0) {
				// Line is split; a,b nonzero, opposite sign
				l := check.len
				// Distance from start of intersection
				// !!! 32-bit version of BSP v5.2 had overflow here on big
				// lines at a formidable distance. Must use 64-bit math here!
				// l*a may be too big for int32! -- VigilantDoomer
				d := int((int64(l) * int64(a)) / (int64(a) - int64(b)))
				if d >= 2 {
					// If the linedef associated with this seg has a sector tag >= 900,
					// treat it as precious; i.e. don't split it unless all other options
					// are exhausted. This is used to protect deep water and invisible
					// lifts/stairs from being messed up accidentally by splits. - Killough
					if w.lines.IsTaggedPrecious(check.Linedef) {
						// If this seg will have to be split anyway, prefer
						// it done by axis-aligned partition line for better
						// Hexen polyobj compatibility
						if w.diagonalPenalty != 0 && part.pdx != 0 && part.pdy != 0 {
							*cost += w.pickNodeFactor * PRECIOUS_MULTIPLY * 2
						} else {
							*cost += w.pickNodeFactor * PRECIOUS_MULTIPLY
						}
					}

					*cost += w.pickNodeFactor
					if *cost > bestcost {
						// This is the heart of my pruning idea
						// it catches bad segs early on. Killough
						return true
					}
					(*tot)++
					mask = uint8(4)
				} else if checkPorn1(l, d, check.pdx, part.pdx, check.pdy, part.pdy, b) {
					leftside = true
				}
			} else {
				leftside = true
			}
		} else if a <= 0 {
			if a != 0 {
				leftside = true
			} else if b == 0 {
				// a=0 && b=0 => co-linear, must share alias then
				// partners are supposed to be co-linear, so they also get
				// covered here.
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
	// handle sub-blocks recursively

	for num := 0; num < 2; num++ {
		if block.subs[num] == nil {
			continue
		}

		if w.evalPartitionWorker_VisplaneKillough(block.subs[num], part, tot, diff,
			cost, bestcost, slen) {
			return true
		}
	}

	// no "bad seg" was found
	return false
}

// PickNode_visplaneVigilant is a refinement of PickNode_visplaneKillough,
// that corrects how certain checks are done, specifically:
// 1. Sector equivalences are taken into account when determining how many
// distinct sectors there are on each side. Partitions that have all equivalent
// sectors go on one side of it are preferred
// 2. Length "incident with segs" is computed more accurately, without
// overlapping segs contributing to length several times
// 3. Length of node line that goes through void is removed from its total
// length, as it doesn't matter
// 4. Since sidechecker in PickNode* is not consistent with DivideSegs logic
// of what goes left/right, there is a quick test (which is performed last) to
// detect if partition would actually be grossly unbalanced, and penalizing it
// if it is so. This usually rules out such partitions from being picked when
// there would be a dozen sectors on one side or so (at the time of the testing,
// it seemed that blocking them always is not a good idea - I may be wrong)
// 5. Additional options are supported, like penalizing sector splits. These
// are currently not exposed to user, though.
// NOTE I tried to use secondary scores like in Zennode to achieve lower depth
// or fewer seg splits (overall seg count). However, going for either tend to
// make some VPOs more easy to trigger on the test map I use. I need to come up
// with something way better than that.
func PickNode_visplaneVigilant(w *NodesWork, ts *NodeSeg, bbox *NodeBounds,
	super *Superblock) *NodeSeg {
	best := ts                        // make sure always got something to return
	bestcost := int(INITIAL_BIG_COST) //
	cnt := 0

	for part := ts; part != nil; part = part.next { // Count once and for all
		cnt++
	}

	var previousPart *NodeSeg // keep track of previous partition - test only one seg per partner pair

	w.segAliasObj.UnvisitAll() // remove marks from previous PickNode calls

	for part := ts; part != nil; part = part.next { // Use each Seg as partition
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
		} else { // = 0 means alias was not assigned (or was intentionally dropped when segs were split)
			// Generate and assign new alias
			// Note we don't assign anything to partner HERE, partners are skipped
			// as part of big loop but get covered by inner loop anyway
			part.alias = w.segAliasObj.Generate()
			// Aliases get copied in the inner loop: when a line we are checking
			// is colinear to partition, it "inherits" alias from partition
		}
		previousPart = part // used for check above
		cost := 0
		tot := 0
		slen := 0 // length of partition that is incidental with segs
		diff := cnt
		// Fill sectorHits array with zeros FAST (fewer bound checks)
		// Credit: gist github.com taylorza GO-Fillslice.md
		w.sectorHits[0] = 0
		hitArrayLen := len(w.sectorHits)
		for j := 1; j < hitArrayLen; j = j << 1 {
			copy(w.sectorHits[j:], w.sectorHits[:j])
		}
		// Empty incidental seg list ( array )
		w.incidental = w.incidental[:0]

		//progress();           	        // Something for the user to look at.

		w.blocksHit = w.blocksHit[:0]
		hasLeft := false
		prune := w.evalPartitionWorker_VisplaneVigilant(super, part, &tot, &diff,
			&cost, bestcost, &slen, &hasLeft)
		if prune { // Early exit and skip past the tests below
			continue
		}

		// Take absolute value. diff is being used to obtain the
		// min/max values by way of: min(a,b)=(a+b-abs(a-b))/2

		diff -= tot
		if diff < 0 {
			diff = -diff
		}

		// Make sure at least one Seg is on each side of the partition
		if tot+cnt <= diff {
			continue
		}

		// Apply missing sectorHits from evalPartitionWorker
		for _, hitRecord := range w.blocksHit {
			hitRecord.block.MarkSecEquivsHit(w.sectorHits, hitRecord.mask)
		}
		// Compute difference in number of _sector equivalencies_
		// references on each side of node line
		diff = 0
		tot = 0
		// And let's see how many sectors are actually hit, could it be we
		// have only one at all? --VigilantDoomer
		flat := 0
		// Sectors that will generate multiple visplanes as the result of
		// being split by partition line
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
			// Start penalizing only when more than 1 sector was split, because
			// if there is only 1 sector (or sector equivalent) being split
			// by partition line, it can be merged back into a single visplane
			// when drawn if I am correct
			unmerged--
		}

		cost += diff
		if config.PenalizeSectorSplits {
			// Another "vigilant visplanes" exclusive - penalize partition
			// candidates for splitting multiple distinct sectors
			cost += unmerged * w.pickNodeFactor
		}
		if cost >= bestcost {
			continue
		}

		// VigilantDoomer if all sectors that are left in this node are
		// visplane-compatible to the extent that we effectively same sector
		// on both sides, the bulk of further evaluation can be skipped. This
		// should help avoid generating unnecessary seg splits when there
		// is no adverse effect on visplanes
		if flat > 1 {
			// Diagonal lines need to be penalized only if we are partitioning
			// something with multiple sectors, so this check is performed here
			// rather where it is in other functions.
			if w.diagonalPenalty != 0 && (part.pdx != 0 && part.pdy != 0) {
				cost += w.diagonalPenalty
				if cost >= bestcost {
					continue
				}
			}

			// Lee Killough's second row of visplane reduction logic:
			// "If the node line is incident with SEGS in less than 1/2th of its
			// length inside the bounding box, increase the cost since this is
			// likely a node line cutting across a large room but only sharing
			// space with a tiny SEG in the middle -- this is another contributor
			// to visplane overflows."
			// This time the check is more thoroughly implemented to avoid losing
			// good partition candidates!
			// Part I - partition line length is computed WITHOUT segments that
			// go through void outside the map.
			l := w.GetPartitionLength_VigilantWay(part, bbox)
			if l == 0 {
				// Glaring error - got zero non-void length, but obviously
				// partition crosses this node and has non-zero length
				l = GetFullPartitionLength(part, bbox)
				Log.Verbose(2, "Recomputing partition length the old way, because I got zero length doing it the new way.\n")
			}

			// part II - Remove overlaps from slen
			collinearSegs := CollinearVertexPairCByCoord(w.incidental) // type cast
			// must sort before GetOverlapsLength() can work
			sort.Sort(collinearSegs)
			// and now overlap length finally goes poof
			slen = slen - collinearSegs.GetOverlapsLength()
			if slen < 0 {
				// Yet to see this happen. But perhaps it could be better (as in
				// faster) to compute length without overlaps instead of substracting
				// total overlap length from the total length. For alpha version the
				// logic will stay in place; for squeezing the last bit of performance,
				// consider a rewrite
				Log.Verbose(2, "Oops, got negative length after removing overlaps! Must have overflowed somewhere.\n")
				slen = 0
			}

			// Divide node line length by two. This was verified as important
			// indeed (worse results produced when not dividing) --VigilantDoomer
			l = l >> 1

			// part III (final):
			// Now check if the length of node line incidental with segs is less
			// than 1/2 of its length cutting through open (not void) space.
			if slen < l {
				cost += w.pickNodeFactor
				if cost >= bestcost {
					continue
				}
			}
		}

		// Check if DivideSegs would actually sort all segs to the right
		// Removes some cases when too many sectors go there. The remaining
		// cases usually have fewer sectors. This check is located late because
		// it is relatively expensive otherwise
		// So, this looks for the first seg to go left and if found, returns
		// early that partition is not bad, accounting for the fact that
		// partition defaults to the right in DivideSegs, and only moved
		// to the left when nothing is there
		// NOTE if one or more of the superblocks went entirely to the left,
		// there is no need to perform this check, superblock test can be relied
		// upon as accurate
		if !hasLeft && VigilantGuard_IsBadPartition(part, ts, cnt) {
			cost += w.pickNodeFactor * ONESIDED_MULTIPLY
			if cost >= bestcost {
				continue
			}
		}

		// We have a new better choice
		bestcost = cost
		best = part // Remember which Seg
	}
	w.incidental = w.incidental[:0]

	return best // All finished, return best Seg
}

// If returns true, the partition &part must be skipped, because it produced
// many splits early so that cost exceed bestcost
func (w *NodesWork) evalPartitionWorker_VisplaneVigilant(block *Superblock,
	part *NodeSeg, tot, diff, cost *int, bestcost int, slen *int,
	hasLeft *bool) bool {

	// -AJA- this is the heart of my superblock idea, it tests the
	//       _whole_ block against the partition line to quickly handle
	//       all the segs within it at once.  Only when the partition
	//       line intercepts the box do we need to go deeper into it.
	num := BoxOnLineSide(block, part)
	if num < 0 {
		// LEFT
		*hasLeft = true
		*diff -= 2 * block.realNum
		w.blocksHit = append(w.blocksHit, BlocksHit{
			block: block,
			mask:  uint8(1),
		})
		return false
	} else if num > 0 {
		// RIGHT
		w.blocksHit = append(w.blocksHit, BlocksHit{
			block: block,
			mask:  uint8(2),
		})
		return false
	}

	for check := block.segs; check != nil; check = check.nextInSuper { // Check partition against all Segs
		// get state of lines' relation to each other
		leftside := false
		mask := uint8(2)
		a := part.pdy*check.psx - part.pdx*check.psy + part.perp
		b := part.pdy*check.pex - part.pdx*check.pey + part.perp
		if (a ^ b) < 0 {
			if (a != 0) && (b != 0) {
				// Line is split; a,b nonzero, opposite sign
				l := check.len
				// Distance from start of intersection
				// !!! 32-bit version of BSP v5.2 had overflow here on big
				// lines at a formidable distance. Must use 64-bit math here!
				// l*a may be too big for int32! -- VigilantDoomer
				d := int((int64(l) * int64(a)) / (int64(a) - int64(b)))
				if d >= 2 {
					// If the linedef associated with this seg has a sector tag >= 900,
					// treat it as precious; i.e. don't split it unless all other options
					// are exhausted. This is used to protect deep water and invisible
					// lifts/stairs from being messed up accidentally by splits. - Killough
					if w.lines.IsTaggedPrecious(check.Linedef) {
						// If this seg will have to be split anyway, prefer
						// it done by axis-aligned partition line for better
						// Hexen polyobj compatibility
						if w.diagonalPenalty != 0 && part.pdx != 0 && part.pdy != 0 {
							*cost += w.pickNodeFactor * PRECIOUS_MULTIPLY * 2
						} else {
							*cost += w.pickNodeFactor * PRECIOUS_MULTIPLY
						}
					}

					*cost += w.pickNodeFactor
					if *cost > bestcost {
						// This is the heart of my pruning idea
						// it catches bad segs early on. Killough
						return true
					}
					(*tot)++
					mask = uint8(4)
				} else if checkPorn1(l, d, check.pdx, part.pdx, check.pdy, part.pdy, b) {
					leftside = true
				}
			} else {
				leftside = true
			}
		} else if a <= 0 {
			if a != 0 {
				leftside = true
			} else if b == 0 {
				// a=0 && b=0 => co-linear, must share alias then
				// partners are supposed to be co-linear, so they also get
				// covered here.
				check.alias = part.alias
				*slen += check.len
				// add to incidental list. Will be used to correct slen
				// contribution above
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
		// Visplanes generated from subsectors made from compatible
		// sectors may merge, thus we want to have compatible sectors
		// on same side to give the gaming engine a chance to do this magic.
		// -- VigilantDoomer
		w.sectorHits[check.secEquiv] |= mask
	}
	// handle sub-blocks recursively

	for num := 0; num < 2; num++ {
		if block.subs[num] == nil {
			continue
		}

		if w.evalPartitionWorker_VisplaneVigilant(block.subs[num], part, tot, diff,
			cost, bestcost, slen, hasLeft) {
			return true
		}
	}

	// no "bad seg" was found
	return false
}

// Being tired of these "No left side, moving partition into left side"
// messages with a lot of sectors being on the right (concrete values of sectors
// can be seen under verbose=3),
// I desired to make PickNode use the same logic as DivideSegs. Apart from
// slowing program down, it also seemed to produce subpar visplane results
// (needs to be rechecked), although there were fewer seg splits.
// So I reduced it to merely another "add cost if partition is not desirable",
// and the criterion for undesirability is the absence of segs that go left.
// Early exiting as soon as seg that goes left is found allows to have minimal
// impact on perfomance
func VigilantGuard_IsBadPartition(part, ts *NodeSeg, cnt int) bool {
	// NOTE some code in this function may be redundant, as we exit early as
	// soon as there is something that will go to the left side
	// Partition line coords
	c := &IntersectionContext{
		psx: part.StartVertex.X,
		psy: part.StartVertex.Y,
		pex: part.EndVertex.X,
		pey: part.EndVertex.Y,
	}
	c.pdx = c.psx - c.pex
	c.pdy = c.psy - c.pey
	tot := 0
	diff := cnt

	for check := ts; check != nil; check = check.next { // Check partition against all Segs
		// get state of lines' relation to each other
		leftside := false
		c.lsx = check.StartVertex.X
		c.lsy = check.StartVertex.Y
		c.lex = check.EndVertex.X
		c.ley = check.EndVertex.Y
		val := c.doLinesIntersect()
		if ((val&2 != 0) && (val&64 != 0)) || ((val&4 != 0) && (val&32 != 0)) {
			tot++
			return false // even splitting not so bad
		} else {
			if check == part || check == part.partner {
				leftside = check == part.partner
				if leftside {
					return false
				}
			} else {
				if val&34 != 0 {
					// to the left
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
	// Take absolute value. diff is being used to obtain the
	// min/max values by way of: min(a,b)=(a+b-abs(a-b))/2

	diff -= tot
	if diff < 0 {
		diff = -diff
	}

	// Make sure at least one Seg is on each side of the partition
	return tot+cnt <= diff
}

// Returns full partition line length within the bounds. Parts that go through
// void space NOT excluded, and the computation is itself not accurate, it's
// a hacky optimization to avoid taking square root (Lee Killough did it this
// way)
func GetPartitionLength_LegacyWay(part *NodeSeg, bbox *NodeBounds) int {
	var l int          // length of partition line
	if part.pdx == 0 { // vertical line
		l = bbox.Ymax - bbox.Ymin
	} else if part.pdy == 0 { // horizontal line
		l = bbox.Xmax - bbox.Xmin
	} else { // diagonal line
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
		l = int((t1 - t2) * float64(part.len))
	}
	return l
}

// Returns full partition line length within the bounds, not excluding void
// space, but using floating point math for accuracy
func GetFullPartitionLength(part *NodeSeg, bbox *NodeBounds) int {
	var l int          // length of partition line
	if part.pdx == 0 { // vertical line
		l = bbox.Ymax - bbox.Ymin
	} else if part.pdy == 0 { // horizontal line
		l = bbox.Xmax - bbox.Xmin
	} else { // diagonal line
		var c IntersectionContext
		partSegCoords := part.toVertexPairC()
		c.psx = partSegCoords.StartVertex.X
		c.psy = partSegCoords.StartVertex.Y
		c.pex = partSegCoords.EndVertex.X
		c.pey = partSegCoords.EndVertex.Y
		c.pdx = c.pex - c.psx
		c.pdy = c.pey - c.psy
		contextStart, contextEnd := PartitionInBoundary(part, &c, bbox.Xmax,
			bbox.Ymax, bbox.Xmin, bbox.Ymin, partSegCoords)
		if contextStart == nil {
			// Backup
			return GetPartitionLength_LegacyWay(part, bbox)
		}
		fullXDiff := float64(contextStart.v.X) - float64(contextEnd.v.X)
		fullYDiff := float64(contextEnd.v.Y) - float64(contextStart.v.Y)
		l = int(math.Round(math.Sqrt(fullXDiff*fullXDiff + fullYDiff*fullYDiff)))
	}
	return l
}

// Return partition line length within the bounds AND without any parts that
// go through void space. (Not exactly an easy thing to do)
func (w *NodesWork) GetPartitionLength_VigilantWay(part *NodeSeg, bbox *NodeBounds) int {
	// An idea suggested by Lee Killough, but one that he didn't implement:
	// discard segments of the partition line that go through void space when
	// computing the ratio of its incidence with segs.

	// We run computation once per alias, and store it in cache
	if part.alias == 0 {
		// Programmer error
		Log.Verbose(2, "What? Alias should not be zero here. Falling back to old way of computing partition length. (Programmer error)")
		return GetFullPartitionLength(part, bbox)
	}
	nonVoidStruc, ok := w.nonVoidCache[part.alias]
	if !ok {
		// Need to find all segments that aren't in a void, didn't do it for
		// this alias yet
		nonVoidStruc = w.ComputeNonVoid(part)
		w.nonVoidCache[part.alias] = nonVoidStruc
	}
	if !nonVoidStruc.success {
		return GetFullPartitionLength(part, bbox)
	}

	// Apply bbox boundaries now. Damn, this means messing with OrientedVertex's
	// again. Fuck.
	contextStart, contextEnd := PartitionInBoundary(part, &(nonVoidStruc.c), bbox.Xmax, bbox.Ymax, bbox.Xmin, bbox.Ymin,
		nonVoidStruc.partSegCoords)
	if contextStart == nil || contextEnd == nil {
		// No worry, this error shouldn't happen anymore
		Log.Verbose(2, "This cannot be! Got so far but now failing (got all the segments of line on the map to see when it goes through the void and when it does not, but failed to determine the edges of line touching the current node's bounding box)\n")
		return GetFullPartitionLength(part, bbox)
	}

	if contextStart.equalTo(contextEnd) {
		Log.Verbose(2, "Partition line seems to have zero length inside the node box (%d,%d)-(%d,%d) in (%d,%d,%d,%d) yielded (%d,%d)-(%d,%d).\n",
			part.StartVertex.X, part.StartVertex.Y, part.EndVertex.X, part.EndVertex.Y,
			bbox.Xmax, bbox.Ymax, bbox.Xmin, bbox.Ymin,
			contextStart.v.X, contextStart.v.Y, contextEnd.v.X, contextEnd.v.Y)
		return 0
	}

	// Array of segments of line going through entire map (the ones that belong
	// to the map and not the void)
	nonVoid := nonVoidStruc.data

	// Ok, among those we need to determine first and last segment which are
	// inside the bounding box
	hitStart := -1
	hitStop := -1

	for i := 0; i < len(nonVoid); i++ {
		// forward!
		if AreOverlapping(contextStart.v, contextEnd.v, nonVoid[i].StartVertex,
			nonVoid[i].EndVertex) {
			hitStart = i
			break
		}
	}
	for i := len(nonVoid) - 1; i >= 0; i-- {
		// backward
		if AreOverlapping(contextStart.v, contextEnd.v, nonVoid[i].StartVertex,
			nonVoid[i].EndVertex) {
			hitStop = i
			break
		}
	}
	if hitStart < 0 || hitStop < 0 {
		// Really, how can this be? We had this seg within our bounds, but my
		// deconstruction didn't see it there
		Log.Verbose(2, nonVoidStruc.original.toString())
		Log.Verbose(2, "More dropouts! %d %d %s [%s-%s]\n", hitStart, hitStop,
			CollinearVertexPairCByCoord(nonVoid).toString(), contextStart.toString(),
			contextEnd.toString())
		return GetFullPartitionLength(part, bbox)
	}

	// At least it is possible to get here
	L := 0
	for i := hitStart; i <= hitStop; i++ {
		thisStart := nonVoid[i].StartVertex
		thisEnd := nonVoid[i].EndVertex
		// Note it is possible for hitStart == hitStop. Both branches should
		// execute then
		precomputedInvalid := false
		if i == hitStart {
			if !VertexPairCOrdering(contextStart.v, nonVoid[i].StartVertex, false) {
				thisStart = contextStart.v
			}
			precomputedInvalid = true
		}
		if i == hitStop {
			if VertexPairCOrdering(contextEnd.v, nonVoid[i].EndVertex, false) {
				thisEnd = contextEnd.v
			}
			precomputedInvalid = true
		}
		if !precomputedInvalid { // whole of current interval goes in
			L = L + nonVoid[i].len
		} else { // only part of current interval goes in
			dx := float64(thisEnd.X - thisStart.X)
			dy := float64(thisEnd.Y - thisStart.Y)
			L = L + int(math.Sqrt(dx*dx+dy*dy))
		}
	}
	return L
}

// Finds axis-aligned partition closest to center of current node. Can return
// nil if none of those were good
func subPickNode_fast(w *NodesWork, ts *NodeSeg, bbox *NodeBounds,
	super *Superblock, cnt int) *NodeSeg {
	var previousPart *NodeSeg // keep track of previous partition - test only one seg per partner pair
	var bestH, bestV *NodeSeg
	oldDistH := -1
	oldDistV := -1

	midX := (bbox.Xmax + bbox.Xmin) >> 1
	midY := (bbox.Ymax + bbox.Ymin) >> 1

	w.segAliasObj.UnvisitAll() // remove marks from previous PickNode calls

	for part := ts; part != nil; part = part.next {
		if part.partner != nil && part.partner == previousPart {
			continue
		}
		if part.alias != 0 {
			if w.segAliasObj.MarkAndRecall(part.alias) {
				continue
			}
		} // don't generate aliases here, as they can't propagate
		previousPart = part // used for check above
		if part.pdy == 0 {
			if bestH == nil {
				bestH = part
				oldDistH = Abs(part.psy - midY)
			} else {
				newDistH := Abs(part.psy - midY)
				if newDistH < oldDistH {
					bestH = part
					oldDistH = newDistH
				}
			}
		} else if part.pdx == 0 {
			if bestV == nil {
				bestV = part
				oldDistV = Abs(part.psx - midX)
			} else {
				newDistV := Abs(part.psx - midX)
				if newDistV < oldDistV {
					bestV = part
					oldDistV = newDistV
				}
			}
		}
	}

	// Let's see if there is no partition candidates somehow
	if bestH == nil && bestV == nil {
		return nil
	}

	// We have one-two candidates, check if they are good enough, if two of
	// them good enough, pick best of the two
	var best *NodeSeg
	bestcost := int(INITIAL_BIG_COST)

	for i := 0; i <= 1; i++ {
		var part *NodeSeg
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
		if prune { // Early exit and skip past the tests below
			continue
		}

		// Take absolute value. diff is being used to obtain the
		// min/max values by way of: min(a,b)=(a+b-abs(a-b))/2

		diff -= tot
		if diff < 0 {
			diff = -diff
		}

		// Make sure at least one Seg is on each side of the partition
		if (tot + cnt) > diff {
			cost += diff
			if cost < bestcost {
				// We have a new better choice
				bestcost = cost
				best = part // Remember which Seg
			}
		}
	}
	return best
}

// Very fast (but low quality) way to pick a node: when the number of segs
// exceeds threshold, use a sloppy algo to choose one of the two center segs
func PickNode_maelstrom(w *NodesWork, ts *NodeSeg, bbox *NodeBounds,
	super *Superblock) *NodeSeg {
	cnt := 0
	for part := ts; part != nil; part = part.next { // Count once and for all
		cnt++
	}
	if cnt >= SEG_FAST_THRESHHOLD {
		best := subPickNode_fast(w, ts, bbox, super, cnt)
		if best != nil {
			return best
		}
	}

	return PickNode_traditional(w, ts, bbox, super)
}

// Modification of evalPartitionWorker_Traditional that disallows splitting
// precious linedefs altogether
func (w *NodesWork) evalPartitionWorker_Maelstrom(block *Superblock,
	part *NodeSeg, tot, diff, cost *int, bestcost int) bool {

	// -AJA- this is the heart of my superblock idea, it tests the
	//       _whole_ block against the partition line to quickly handle
	//       all the segs within it at once.  Only when the partition
	//       line intercepts the box do we need to go deeper into it.
	num := BoxOnLineSide(block, part)
	if num < 0 {
		// LEFT
		*diff -= 2 * block.realNum
		return false
	} else if num > 0 {
		// RIGHT
		return false
	}

	for check := block.segs; check != nil; check = check.nextInSuper { // Check partition against all Segs
		// get state of lines' relation to each other
		leftside := false
		a := part.pdy*check.psx - part.pdx*check.psy + part.perp
		b := part.pdy*check.pex - part.pdx*check.pey + part.perp
		if (a ^ b) < 0 {
			if (a != 0) && (b != 0) {
				// Line is split; a,b nonzero, opposite sign
				l := check.len
				// Distance from start of intersection
				// !!! 32-bit version of BSP v5.2 had overflow here on big
				// lines at a formidable distance. Must use 64-bit math here!
				// l*a may be too big for int32! -- VigilantDoomer
				d := int((int64(l) * int64(a)) / (int64(a) - int64(b)))
				if d >= 2 {
					// If the linedef associated with this seg has a sector tag >= 900,
					// treat it as precious; i.e. don't split it unless all other options
					// are exhausted. This is used to protect deep water and invisible
					// lifts/stairs from being messed up accidentally by splits. - Killough
					// VigilantDoomer: don't select such partitions in maelstrom
					// fast path - we have a traditional path to fall back to
					if w.lines.IsTaggedPrecious(check.Linedef) {
						return true
					}

					*cost += w.pickNodeFactor
					if *cost > bestcost {
						// This is the heart of my pruning idea
						// it catches bad segs early on. Killough
						return true
					}
					(*tot)++
				} else if checkPorn1(l, d, check.pdx, part.pdx, check.pdy, part.pdy, b) {
					leftside = true
				}
			} else {
				leftside = true
			}
		} else if a <= 0 {
			if a != 0 {
				leftside = true
			} else if b == 0 {
				// a=0 && b=0 => co-linear, must share alias
				// since partners are colinear, they also get aliases this
				// way, sooner or later (the latter may happen when a few
				// earlier colinear segs get pruned in "seg split by
				// partition" found branch before reaching here)
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
	// handle sub-blocks recursively

	for num := 0; num < 2; num++ {
		if block.subs[num] == nil {
			continue
		}

		if w.evalPartitionWorker_Traditional(block.subs[num], part, tot, diff,
			cost, bestcost) {
			return true
		}
	}

	// no "bad seg" was found
	return false
}

// Good Lord, Raphael Quinet, you were right: writing a node builder takes HEAPS of
// time! Big thanks for reaching there before the rest of us. -- VigilantDoomer

/*---------------------------------------------------------------------------*

	This message has been taken, complete, from OBJECTS.C in DEU5beta source.
	It outlines the method used here to pick the nodelines.

	IF YOU ARE WRITING A DOOM EDITOR, PLEASE READ THIS:

   I spent a lot of time writing the Nodes builder.  There are some bugs in
   it, but most of the code is OK.  If you steal any ideas from this program,
   put a prominent message in your own editor to make it CLEAR that some
   original ideas were taken from DEU.  Thanks.

   While everyone was talking about LineDefs, I had the idea of taking only
   the Segs into account, and creating the Segs directly from the SideDefs.
   Also, dividing the list of Segs in two after each call to CreateNodes makes
   the algorithm faster.  I use several other tricks, such as looking at the
   two ends of a Seg to see on which side of the nodeline it lies or if it
   should be split in two.  I took me a lot of time and efforts to do this.

   I give this algorithm to whoever wants to use it, but with this condition:
   if your program uses some of the ideas from DEU or the whole algorithm, you
   MUST tell it to the user.  And if you post a message with all or parts of
   this algorithm in it, please post this notice also.  I don't want to speak
   legalese; I hope that you understand me...  I kindly give the sources of my
   program to you: please be kind with me...

   If you need more information about this, here is my E-mail address:
   Raphael.Quinet@eed.ericsson.se (Rapha‰l Quinet).

   Short description of the algorithm:
     1 - Create one Seg for each SideDef: pick each LineDef in turn.  If it
	 has a "first" SideDef, then create a normal Seg.  If it has a
	 "second" SideDef, then create a flipped Seg.
     2 - Call CreateNodes with the current list of Segs.  The list of Segs is
	 the only argument to CreateNodes.
     3 - Save the Nodes, Segs and SSectors to disk.  Start with the leaves of
	 the Nodes tree and continue up to the root (last Node).

   CreateNodes does the following:
     1 - Pick a nodeline amongst the Segs (minimize the number of splits and
	 keep the tree as balanced as possible).
     2 - Move all Segs on the right of the nodeline in a list (segs1) and do
	 the same for all Segs on the left of the nodeline (in segs2).
     3 - If the first list (segs1) contains references to more than one
	 Sector or if the angle between two adjacent Segs is greater than
	 180ш, then call CreateNodes with this (smaller) list.  Else, create
	 a SubSector with all these Segs.
     4 - Do the same for the second list (segs2).
     5 - Return the new node (its two children are already OK).

   Each time CreateSSector is called, the Segs are put in a global list.
   When there is no more Seg in CreateNodes' list, then they are all in the
   global list and ready to be saved to disk.

*---------------------------------------------------------------------------*/
