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

// secondary metric to balance costs
type MinorCosts struct {
	PreciousSplit int // number of "do not split" seg split
	SegsSplit     int // number of segs split
	SectorsSplit  int // number of sectors split
	Unmerged      int // number of sectors split rather directly
}

type SegMinorBundle struct {
	seg   *NodeSeg
	minor MinorCosts
}

func minorIsBetter_Dummy(current, prev MinorCosts) bool {
	return false
}

func minorIsBetter_Segs(current, prev MinorCosts) bool {
	if current.PreciousSplit < prev.PreciousSplit {
		return true
	} else if current.PreciousSplit > prev.PreciousSplit {
		return false
	}

	if current.SegsSplit < prev.SegsSplit {
		return true
	} else if current.SegsSplit > prev.SegsSplit {
		return false
	}

	if current.SectorsSplit < prev.SectorsSplit {
		return true
	}

	return false
}

func minorIsBetter_Sectors(current, prev MinorCosts) bool {
	if current.PreciousSplit < prev.PreciousSplit {
		return true
	} else if current.PreciousSplit > prev.PreciousSplit {
		return false
	}

	if current.SectorsSplit < prev.SectorsSplit {
		return true
	} else if current.SectorsSplit > prev.SectorsSplit {
		return false
	}

	if current.SegsSplit < prev.SegsSplit {
		return true
	}

	return false
}

func minorIsBetter_Balanced(current, prev MinorCosts) bool {
	if current.PreciousSplit < prev.PreciousSplit {
		return true
	} else if current.PreciousSplit > prev.PreciousSplit {
		return false
	}

	// Balanced - consider both splits equally
	curMetric := current.SegsSplit + current.SectorsSplit
	prevMetric := prev.SegsSplit + prev.SectorsSplit

	if curMetric < prevMetric {
		return true
	} else if curMetric > prevMetric {
		return false
	}

	if current.SectorsSplit < prev.SectorsSplit {
		return true
	} else if current.SectorsSplit > prev.SectorsSplit {
		return false
	}

	if current.Unmerged < prev.Unmerged {
		return true
	} else if current.Unmerged > prev.Unmerged {
		return false
	}

	if current.SegsSplit < prev.SegsSplit {
		return true
	}

	return false
}

// Needed for depth mode to aggregate all partition candidates with the
// same PRIMARY metric (cost)
func minorIsBetter_Always(current, prev MinorCosts) bool {
	return true
}

// Fpr simple partition algorithms - avoid splitting precious segs
func minorIsBetter_Precious(current, prev MinorCosts) bool {
	if current.PreciousSplit < prev.PreciousSplit {
		return true
	}
	return false
}

// PickNode_traditional is an implementation of PickNode that is classic (since
// DEU5beta source code (c) Raphael Quinet) way to pick a partition: partitions
// are chosen based on seg so that there is minimal amount of seg splits and the
// difference in the number of segs on both sides is minimal.
func PickNode_traditional(w *NodesWork, ts *NodeSeg, bbox *NodeBounds,
	super *Superblock) *NodeSeg {
	best := ts                        // make sure always got something to return
	bestcost := int(INITIAL_BIG_COST) //
	bestMinors := MinorCosts{
		PreciousSplit: int(INITIAL_BIG_COST),
	}
	cnt := 0
	if w.parts != nil {
		w.parts = w.parts[:0]
	}

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
		minors := MinorCosts{
			PreciousSplit: 0,
		}

		// See PickNode_visplaneKillough for explanation. There it happens in
		// a different place - but here we don't count sectors, so this ends up
		// being applied always, if the cost was enabled -- VigilantDoomer
		if w.diagonalPenalty != 0 && part.pdx != 0 && part.pdy != 0 {
			cost += w.diagonalPenalty
		}

		//progress();           	        // Something for the user to look at.

		prune := w.evalPartitionWorker_Traditional(super, part, &tot, &diff,
			&cost, bestcost, &minors)
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
			// NOTE btw, block under this may fail to execute and first seg
			// will keep being "best", yes. Other partitioners have this quirk,
			// too
			cost += diff
			if w.parts == nil { // default path (for modes other than hard multi-tree)
				if cost < bestcost || (cost == bestcost &&
					minorIsBetter_Precious(minors, bestMinors)) {
					// We have a new better choice
					bestcost = cost
					best = part // Remember which Seg
					bestMinors = minors
				}
			} else { // hard multi-tree
				strictlyBetter := false
				if cost < bestcost {
					strictlyBetter = true
				} else if cost == bestcost {
					if minorIsBetter_Precious(minors, bestMinors) {
						strictlyBetter = true
					} else if !minorIsBetter_Precious(bestMinors, minors) {
						// it's a tie!
						w.parts = append(w.parts, part)
					}
				}

				if strictlyBetter {
					bestcost = cost
					best = part
					bestMinors = minors
					w.parts = w.parts[:0]
					w.parts = append(w.parts, part)
				}
			}
		}

	}
	// NOTE bestMinors and best might have never been updated after being set
	// their initial values. Happens often enough! Applies to other partitioners
	// as well. This means that every partition attempt resulted in all segs
	// being to one side of the partition
	return best // All finished, return best Seg
}

// If returns true, the partition &part must be skipped, because it produced
// many splits early so that cost exceed bestcost
func (w *NodesWork) evalPartitionWorker_Traditional(block *Superblock,
	part *NodeSeg, tot, diff, cost *int, bestcost int, minors *MinorCosts) bool {

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
		if DiffSign(a, b) {
			if (a != 0) && (b != 0) {
				if w.PassingTooClose(part, check, cost, minors) {
					return true
				}
				// Line is split; a,b nonzero, opposite sign
				l := check.len
				// Distance from start of intersection
				// !!! 32-bit version of BSP v5.2 had overflow here on big
				// lines at a formidable distance. Must use 64-bit math here!
				// l*a may be too big for int32! -- VigilantDoomer
				d := Number((WideNumber(l) * WideNumber(a)) / (WideNumber(a) - WideNumber(b)))
				if d >= 2 {
					// If the linedef associated with this seg has a sector tag >= 900,
					// treat it as precious; i.e. don't split it unless all other options
					// are exhausted. This is used to protect deep water and invisible
					// lifts/stairs from being messed up accidentally by splits. - Killough
					// VigilantDoomer: so is a tag on a linedef, or on a sector?
					// Currently put on the linedef, but damn, there are other special
					// tag values >= 900, not sure if I should give them this meaning too
					if w.lines.IsTaggedPrecious(check.Linedef) &&
						!w.lines.SectorIgnorePrecious(check.sector) {
						// If this seg will have to be split anyway, prefer
						// it done by axis-aligned partition line for better
						// Hexen polyobj compatibility
						if part.pdx != 0 && part.pdy != 0 {
							*cost += w.pickNodeFactor * PRECIOUS_MULTIPLY * 2
						} else {
							*cost += w.pickNodeFactor * PRECIOUS_MULTIPLY
						}
						minors.PreciousSplit++
					}

					*cost += w.pickNodeFactor
					if *cost > bestcost {
						// This is the heart of my pruning idea
						// it catches bad segs early on. Killough
						return true
					}
					(*tot)++
				} else {
					if checkPorn1(l, d, check.pdx, part.pdx, check.pdy, part.pdy, b) {
						leftside = true
					}
				}
			} else {
				// This fixes the problem with Hexen map05 (Guardian of Steel)
				// polyobject recipient sector 275 getting broken. AJ-BSP and
				// ZDBSP also use this solution
				if w.lines.IsTaggedPrecious(check.Linedef) &&
					!w.lines.SectorIgnorePrecious(check.sector) &&
					!w.PartIsPolyobjSide(part, check) {
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
			cost, bestcost, minors) {
			return true
		}
	}

	// no "bad seg" was found
	return false
}

func checkPorn1(l, d, cpdx, ppdx, cpdy, ppdy, b Number) bool {
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
	bestMinors := MinorCosts{
		PreciousSplit: int(INITIAL_BIG_COST),
	}
	cnt := 0
	if w.parts != nil {
		w.parts = w.parts[:0]
	}

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
		slen := Number(0) // length of partition that is incidental with segs
		diff := cnt
		minors := MinorCosts{
			PreciousSplit: 0,
		}
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
			&cost, bestcost, &slen, &minors)
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
		if cost > bestcost || (cost == bestcost &&
			w.parts == nil && !minorIsBetter_Precious(minors, bestMinors)) {
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
			if cost > bestcost || (cost == bestcost &&
				w.parts == nil && !minorIsBetter_Precious(minors, bestMinors)) {
				continue
			}
		}

		if w.parts == nil { // default path - taken except for hard multi-tree

			// We have a new better choice
			bestcost = cost
			best = part // Remember which Seg
			bestMinors = minors

		} else { // hard multi-tree says hello

			strictlyBetter := false
			if cost < bestcost {
				strictlyBetter = true
			} else { // should be cost == bestcost already
				if minorIsBetter_Precious(minors, bestMinors) {
					// new one has better secondaries, so there is no tie
					strictlyBetter = true
				} else if !minorIsBetter_Precious(bestMinors, minors) {
					// minors are equal quality - it's a tie
					w.parts = append(w.parts, part)
				} // else it was worse
			}
			if strictlyBetter {
				bestcost = cost
				best = part
				bestMinors = minors
				w.parts = w.parts[:0]
				w.parts = append(w.parts, part)
			}

		}
	}
	return best // All finished, return best Seg
}

// If returns true, the partition &part must be skipped, because it produced
// many splits early so that cost exceed bestcost
func (w *NodesWork) evalPartitionWorker_VisplaneKillough(block *Superblock,
	part *NodeSeg, tot, diff, cost *int, bestcost int, slen *Number,
	minors *MinorCosts) bool {

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
		if DiffSign(a, b) {
			if (a != 0) && (b != 0) {
				if w.PassingTooClose(part, check, cost, minors) {
					return true
				}
				// Line is split; a,b nonzero, opposite sign
				l := check.len
				// Distance from start of intersection
				// !!! 32-bit version of BSP v5.2 had overflow here on big
				// lines at a formidable distance. Must use 64-bit math here!
				// l*a may be too big for int32! -- VigilantDoomer
				d := Number((WideNumber(l) * WideNumber(a)) / (WideNumber(a) - WideNumber(b)))
				if d >= 2 {
					// If the linedef associated with this seg has a sector tag >= 900,
					// treat it as precious; i.e. don't split it unless all other options
					// are exhausted. This is used to protect deep water and invisible
					// lifts/stairs from being messed up accidentally by splits. - Killough
					if w.lines.IsTaggedPrecious(check.Linedef) &&
						!w.lines.SectorIgnorePrecious(check.sector) {
						// If this seg will have to be split anyway, prefer
						// it done by axis-aligned partition line for better
						// Hexen polyobj compatibility
						if part.pdx != 0 && part.pdy != 0 {
							*cost += w.pickNodeFactor * PRECIOUS_MULTIPLY * 2
						} else {
							*cost += w.pickNodeFactor * PRECIOUS_MULTIPLY
						}
						minors.PreciousSplit++
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
				// This fixes the problem with Hexen map05 (Guardian of Steel)
				// polyobject recipient sector 275 getting broken. AJ-BSP and
				// ZDBSP also use this solution
				if w.lines.IsTaggedPrecious(check.Linedef) &&
					!w.lines.SectorIgnorePrecious(check.sector) &&
					!w.PartIsPolyobjSide(part, check) {
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
			cost, bestcost, slen, minors) {
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
func PickNode_visplaneVigilant(w *NodesWork, ts *NodeSeg, bbox *NodeBounds,
	super *Superblock) *NodeSeg {
	best := ts // make sure always got something to return
	var bestFallback *NodeSeg
	executed := false
	bestcost := int(INITIAL_BIG_COST) //
	bestMinors := MinorCosts{
		PreciousSplit: int(INITIAL_BIG_COST),
		SegsSplit:     int(INITIAL_BIG_COST),
		SectorsSplit:  int(INITIAL_BIG_COST),
		Unmerged:      int(INITIAL_BIG_COST),
	}
	var parts []SegMinorBundle
	if w.multipart { // zenscore used for secondary - not to be confused for multi-tree
		parts = make([]SegMinorBundle, 0)
	}
	cnt := 0
	if w.parts != nil { // and this one is for HARD multi-tree, oh yes
		w.parts = w.parts[:0]
		// Of note, if there is only one appropriate candidate, w.parts is not
		// required to contain it - it will be automatically filled in
		// w.DivideSegs*. Thus exiting this function with empty non-nil w.parts
		// is not an error
	}

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
		minors := MinorCosts{
			PreciousSplit: 0,
			SegsSplit:     0,
			SectorsSplit:  0,
			Unmerged:      0,
		}
		tot := 0
		slen := Number(0) // length of partition that is incidental with segs
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
			&cost, bestcost, &slen, &hasLeft, &minors)
		if prune { // Early exit and skip past the tests below
			continue
		}

		if bestFallback == nil {
			bestFallback = part
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
			// Start penalizing only when more than 1 sector was split, because
			// if there is only 1 sector (or sector equivalent) being split
			// by partition line, it can be merged back into a single visplane
			// when drawn if I am correct
			unmerged--
		}

		cost += diff
		if config.PenalizeSectorSplits && !w.multipart {
			// Another "vigilant visplanes" exclusive - penalize partition
			// candidates for splitting multiple distinct sectors
			// This path is disabled for depth evaluation, however
			cost += unmerged * w.pickNodeFactor
		} else {
			// On most wads, NOT applying the above addition to primary cost,
			// but offloading it to secondary cost, seems to reduce depth
			// Except on the vanilla wad I've in development, which needs the
			// other approach ...
			minors.Unmerged = unmerged
		}
		if (cost > bestcost) || (cost == bestcost && w.parts == nil &&
			!w.minorIsBetter(minors, bestMinors)) {
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
				if (cost > bestcost) || (cost == bestcost && w.parts == nil &&
					!w.minorIsBetter(minors, bestMinors)) {
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
				l = GetFullPartitionLength(w, part, bbox)
				w.mlog.Verbose(2, "Recomputing partition length the old way, because I got zero length doing it the new way.\n")
			}

			// part II - Remove overlaps from slen
			collinearSegs := IntCollinearVertexPairCByCoord(w.incidental) // type cast
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
				w.mlog.Verbose(2, "Oops, got negative length after removing overlaps! Must have overflowed somewhere.\n")
				slen = 0
			}

			// Divide node line length by two. This was verified as important
			// indeed (worse results produced when not dividing) --VigilantDoomer
			l = l / 2

			// part III (final):
			// Now check if the length of node line incidental with segs is less
			// than 1/2 of its length cutting through open (not void) space.
			if slen < l {
				cost += w.pickNodeFactor
				if (cost > bestcost) || (cost == bestcost && w.parts == nil &&
					!w.minorIsBetter(minors, bestMinors)) {
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
		if !hasLeft && w.VigilantGuard_IsBadPartition(part, ts, cnt) {
			cost += w.pickNodeFactor * ONESIDED_MULTIPLY
			if (cost > bestcost) || (cost == bestcost && w.parts == nil &&
				!w.minorIsBetter(minors, bestMinors)) {
				continue
			}
		}

		if w.parts == nil || parts != nil { // default
			// w.parts pertains to HARD multi-tree, and parts pertains to
			// zenscore as secondary metric
			// So, since we are here, either it's not HARD multi-tree, or
			// zenscore is used as secondary, or both.
			// In the case of zenscore + HARD multi-tree, w.parts for HARD
			// multi-tree will be  filled outside of the loop, after going
			// through zenscore module.
			// Note that if parts != nil, w.minorIsBetter return true always
			// (it is assigned to minorIsBetter_Always callback), so if
			// w.parts also != nil (we skipped comparing the minors), we still
			// don't owe to compare minors here

			// We have a new better choice
			if parts != nil { // multipart == true
				if bestcost != cost {
					// Different (lesser cost) found
					// Forget all parts for previous bestcost
					parts = parts[:0]
				}
				parts = append(parts, SegMinorBundle{
					seg:   part,
					minor: minors,
				})
			}
			bestcost = cost
			bestMinors = minors
			best = part // Remember which Seg
			executed = true

		} else { // hard multi-tree salutes you (and zenscore is not used, apparently)
			strictlyBetter := false
			if cost < bestcost {
				strictlyBetter = true
			} else {
				if w.minorIsBetter(minors, bestMinors) {
					strictlyBetter = true
				} else if !w.minorIsBetter(bestMinors, minors) {
					// a tie
					if minors.SectorsSplit > 0 || minors.SegsSplit > 0 {
						w.parts = append(w.parts, part)
					} // not notable otherwise (hypothesis) - decreases amount of trees to try
				} // else it was worse
			}
			if strictlyBetter {
				bestcost = cost
				bestMinors = minors
				best = part // Remember which Seg
				executed = true
				w.parts = w.parts[:0]
				w.parts = append(w.parts, part)
			}
		}
	}
	w.incidental = w.incidental[:0]
	if len(parts) > 1 {
		// Use Zennode-like algorithm intended to choose partition so as to
		// decrease BSP depth somewhat. Functions are in zenscore.go
		depthScores := ZenSegMinorToDepthScores(parts)
		newSectorHits := make([]uint8, len(w.sectors))
		w.ZenComputeScores(super, depthScores, newSectorHits, w.depthArtifacts)
		ZenPickBestScore(depthScores)
		if depthScores[0].scoreSeg != VERY_BAD_SCORE {
			best = depthScores[0].seg
			if w.parts != nil {
				w.parts = append(w.parts, best)
			}
			if w.parts != nil || config.VerbosityLevel >= 4 { // reference to global: config
				// Either need to grab all same-scoring stuff for hard multi-tree,
				// or have high enough verbosity level to show ambiguity rank to
				// user
				tst1, tst2, tst3, tst4 := depthScores[0].preciousSplit,
					depthScores[0].scoreTotal,
					depthScores[0].equivSplit,
					depthScores[0].segSplit
				if tst3 > 0 || tst4 > 0 {
					//if tst3 > 1 || tst4 > 2 {
					// Yes, you got it right - I decided to disallow multi-tree
					// branch when there is no more than 1 equivSplit and no
					// more than 2 segSplit. It seems that when these values
					// are less, our candidates often form a convex loop on
					// the outer edge. Even with values greater, though, we often
					// have a symmetric setup nonetheless
					track := 1
					for i := 1; i < len(depthScores); i++ {
						if tst1 == depthScores[i].preciousSplit &&
							tst2 == depthScores[i].scoreTotal &&
							tst3 == depthScores[i].equivSplit &&
							tst4 == depthScores[i].segSplit {
							if w.parts != nil {
								w.parts = append(w.parts, depthScores[i].seg)
							}
							track++
						} else {
							break
						}
					}
					if track > 1 {
						w.mlog.Verbose(4, "ZEN Ambiguity equal rank for %d records \n", track)
						/*Log.Verbose(1, "track = %d cnt = %d precSplit=%d scoreTotal=%d equivSplit=%d segSplit=%d\n",
						track, cnt, tst1, tst2, tst3, tst4)*/
					}
				}
			}
		}
	}
	if !executed && bestFallback != nil {
		// if can only choose a partition that has everything to one side of it,
		// choose one that was not turned down by eval (by PassingTooClose,
		// for example)
		best = bestFallback
	}
	return best // All finished, return best Seg
}

// If returns true, the partition &part must be skipped, because it produced
// many splits early so that cost exceed bestcost
func (w *NodesWork) evalPartitionWorker_VisplaneVigilant(block *Superblock,
	part *NodeSeg, tot, diff, cost *int, bestcost int, slen *Number,
	hasLeft *bool, minors *MinorCosts) bool {

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
		if DiffSign(a, b) {
			if (a != 0) && (b != 0) {
				if w.PassingTooClose(part, check, cost, minors) {
					return true
				}
				// Line is split; a,b nonzero, opposite sign
				l := check.len
				// Distance from start of intersection
				// !!! 32-bit version of BSP v5.2 had overflow here on big
				// lines at a formidable distance. Must use 64-bit math here!
				// l*a may be too big for int32! -- VigilantDoomer
				d := Number((WideNumber(l) * WideNumber(a)) / (WideNumber(a) - WideNumber(b)))
				if d >= 2 {
					// If the linedef associated with this seg has a sector tag >= 900,
					// treat it as precious; i.e. don't split it unless all other options
					// are exhausted. This is used to protect deep water and invisible
					// lifts/stairs from being messed up accidentally by splits. - Killough
					if w.lines.IsTaggedPrecious(check.Linedef) &&
						!w.lines.SectorIgnorePrecious(check.sector) {
						// If this seg will have to be split anyway, prefer
						// it done by axis-aligned partition line for better
						// Hexen polyobj compatibility
						if part.pdx != 0 && part.pdy != 0 {
							*cost += w.pickNodeFactor * PRECIOUS_MULTIPLY * 2
						} else {
							*cost += w.pickNodeFactor * PRECIOUS_MULTIPLY
						}
						minors.PreciousSplit++
					}

					*cost += w.pickNodeFactor
					if *cost > bestcost {
						// This is the heart of my pruning idea
						// it catches bad segs early on. Killough
						return true
					}
					(*tot)++
					minors.SegsSplit++
					mask = uint8(4)
				} else {
					if checkPorn1(l, d, check.pdx, part.pdx, check.pdy, part.pdy, b) {
						leftside = true
					}
				}
			} else {
				// This fixes the problem with Hexen map05 (Guardian of Steel)
				// polyobject recipient sector 275 getting broken. AJ-BSP and
				// ZDBSP also use this solution
				if w.lines.IsTaggedPrecious(check.Linedef) &&
					!w.lines.SectorIgnorePrecious(check.sector) &&
					!w.PartIsPolyobjSide(part, check) {
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
				// a=0 && b=0 => co-linear, must share alias then
				// partners are supposed to be co-linear, so they also get
				// covered here.
				if check.alias != part.alias && vetAliasTransfer2(part, check) {
					check.alias = part.alias
				}
				*slen += check.len
				// add to incidental list. Will be used to correct slen
				// contribution above
				w.incidental = append(w.incidental, check.toIntVertexPairC())
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
			cost, bestcost, slen, hasLeft, minors) {
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
func (w *NodesWork) VigilantGuard_IsBadPartition(part, ts *NodeSeg, cnt int) bool {
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
		val := w.doLinesIntersect(c)
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
func GetPartitionLength_LegacyWay(part *NodeSeg, bbox *NodeBounds) Number {
	var l Number       // length of partition line
	if part.pdx == 0 { // vertical line
		l = Number(bbox.Ymax - bbox.Ymin)
	} else if part.pdy == 0 { // horizontal line
		l = Number(bbox.Xmax - bbox.Xmin)
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
		l = Number((t1 - t2) * float64(part.len))
	}
	return l
}

// Returns full partition line length within the bounds, not excluding void
// space, but using floating point math for accuracy
func GetFullPartitionLength(w *NodesWork, part *NodeSeg, bbox *NodeBounds) Number {
	var l Number       // length of partition line
	if part.pdx == 0 { // vertical line
		l = Number(bbox.Ymax - bbox.Ymin)
	} else if part.pdy == 0 { // horizontal line
		l = Number(bbox.Xmax - bbox.Xmin)
	} else { // diagonal line
		var c FloatIntersectionContext
		partSegCoords := part.toVertexPairC()
		c.psx = partSegCoords.StartVertex.X
		c.psy = partSegCoords.StartVertex.Y
		c.pex = partSegCoords.EndVertex.X
		c.pey = partSegCoords.EndVertex.Y
		c.pdx = c.pex - c.psx
		c.pdy = c.pey - c.psy
		contextStart, contextEnd, _ := PartitionInBoundary(w, part, &c, bbox.Xmax,
			bbox.Ymax, bbox.Xmin, bbox.Ymin, partSegCoords)
		if contextStart == nil {
			// Backup
			return GetPartitionLength_LegacyWay(part, bbox)
		}
		fullXDiff := contextStart.X - contextEnd.X
		fullYDiff := contextEnd.Y - contextStart.Y
		l = Number(math.Round(math.Sqrt(fullXDiff*fullXDiff + fullYDiff*fullYDiff)))
	}
	return l
}

// Return partition line length within the bounds AND without any parts that
// go through void space. (Not exactly an easy thing to do)
func (w *NodesWork) GetPartitionLength_VigilantWay(part *NodeSeg, bbox *NodeBounds) Number {
	// An idea suggested by Lee Killough, but one that he didn't implement:
	// discard segments of the partition line that go through void space when
	// computing the ratio of its incidence with segs.

	// We run computation once per alias, and store it in cache
	if part.alias == 0 {
		// Programmer error
		w.mlog.Verbose(2, "What? Alias should not be zero here. Falling back to old way of computing partition length. (Programmer error)")
		return GetFullPartitionLength(w, part, bbox)
	}
	nonVoidStruc, ok := w.nonVoidCache[part.alias]
	if !ok {
		// Need to find all segments that aren't in a void, didn't do it for
		// this alias yet
		nonVoidStruc = w.ComputeNonVoid(part)
		w.nonVoidCache[part.alias] = nonVoidStruc
	}
	if !nonVoidStruc.success {
		//w.mlog.Verbose(2, "Computing old value...\n")
		return GetFullPartitionLength(w, part, bbox)
	}

	//w.mlog.Verbose(2, "Computing NEW value...\n")
	// Apply bbox boundaries now. Damn, this means messing with OrientedVertex's
	// again. Fuck.
	contextStart, contextEnd, _ := PartitionInBoundary(w, part,
		&(nonVoidStruc.c), bbox.Xmax, bbox.Ymax, bbox.Xmin, bbox.Ymin,
		nonVoidStruc.partSegCoords)
	if contextStart == nil || contextEnd == nil {
		// No worry, this error shouldn't happen anymore
		w.mlog.Verbose(2, "This cannot be! Got so far but now failing (got all the segments of line on the map to see when it goes through the void and when it does not, but failed to determine the edges of line touching the current node's bounding box)\n")
		return GetFullPartitionLength(w, part, bbox)
	}

	if contextStart.equalToWithEpsilon(contextEnd) {
		w.mlog.Verbose(2, "Partition line seems to have zero length inside the node box (%v,%v)-(%v,%v) in (%v,%v,%v,%v) yielded (%v,%v)-(%v,%v).\n",
			part.StartVertex.X, part.StartVertex.Y, part.EndVertex.X, part.EndVertex.Y,
			bbox.Xmax, bbox.Ymax, bbox.Xmin, bbox.Ymin,
			contextStart.X, contextStart.Y, contextEnd.X, contextEnd.Y)
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
		if AreOverlapping(contextStart, contextEnd, nonVoid[i].StartVertex,
			nonVoid[i].EndVertex) {
			hitStart = i
			break
		}
	}
	for i := len(nonVoid) - 1; i >= 0; i-- {
		// backward
		if AreOverlapping(contextStart, contextEnd, nonVoid[i].StartVertex,
			nonVoid[i].EndVertex) {
			hitStop = i
			break
		}
	}
	if hitStart < 0 || hitStop < 0 {
		// Really, how can this be? We had this seg within our bounds, but my
		// deconstruction didn't see it there
		w.mlog.Verbose(2, "to below: %s", nonVoidStruc.original.toString())
		w.mlog.Verbose(2, "More dropouts! %v %v %s\n  ...... %d [%s-%s]\n",
			hitStart, hitStop,
			CollinearVertexPairCByCoord(nonVoid).toString(), part.Linedef,
			contextStart.toString(), contextEnd.toString())
		return GetFullPartitionLength(w, part, bbox)
	}

	/* // Doesn't actually happen. But length is yielded zero. How?
	if hitStart > hitStop {
		Log.Verbose(2, "Partition %d in  (%v,%v,%v,%v) hitStart > hitStop, really? : %d > %d\n",
			part.Linedef, bbox.Xmax, bbox.Ymax, bbox.Xmin, bbox.Ymin, hitStart,
			hitStop)
	}*/
	// At least it is possible to get here
	L := Number(0)
	for i := hitStart; i <= hitStop; i++ {
		thisStart := nonVoid[i].StartVertex
		thisEnd := nonVoid[i].EndVertex
		// Note it is possible for hitStart == hitStop. Both branches should
		// execute then
		precomputedInvalid := false
		if i == hitStart {
			if !VertexPairCOrdering(contextStart, nonVoid[i].StartVertex, false) {
				thisStart = contextStart
			}
			precomputedInvalid = true
		}
		if i == hitStop {
			if VertexPairCOrdering(contextEnd, nonVoid[i].EndVertex, false) {
				thisEnd = contextEnd
			}
			precomputedInvalid = true
		}
		if !precomputedInvalid { // whole of current interval goes in
			L = L + nonVoid[i].len
			if nonVoid[i].len == 0 {
				w.mlog.Verbose(2, "non-void interval computed with zero length (precomputed)\n")
			} else if nonVoid[i].len < 0 {
				w.mlog.Verbose(2, "what? non-void interval computed with NEGATIVE length (precomputed)\n")
			}
		} else { // only part of current interval goes in
			dx := thisEnd.X - thisStart.X
			dy := thisEnd.Y - thisStart.Y
			/*if part.Linedef == 1897 && hitStart == 3 && hitStop == 3 {
				w.mlog.Printf("debug %v %v %s-%s context:%s-%s\n", dx, dy, thisStart.toString(), thisEnd.toString(),
					contextStart.toString(), contextEnd.toString())
			}*/
			l0 := Number(math.Sqrt(dx*dx + dy*dy))
			L = L + l0
			if l0 == 0 {
				// ok, this is a normal occurence, actually. Range can cut the
				// interval right at the end point
				/*w.mlog.Verbose(2, "part of non-void interval REcomputed with zero length against current nodebox %s - %s (%d)\n ....... %s ............... range (%d,%d)-(%d,%d)\n",
				thisStart.toString(), thisEnd.toString(), part.Linedef, Future_CollinearVertexPairCByCoord(nonVoid).toString(),
				bbox.Xmin, bbox.Ymin, bbox.Xmax, bbox.Ymax)*/
			} else if l0 < 0 {
				w.mlog.Verbose(2, "what? sqrt yieled NEGATIVE value after cast?\n")
			}
		}
	}
	if L == 0 {
		w.mlog.Verbose(2, "returning 0 (sad)\n ........... %d %s ........ range (%d,%d) - (%d, %d) hit: %d,%d \n",
			part.Linedef, CollinearVertexPairCByCoord(nonVoid).toString(),
			bbox.Xmin, bbox.Ymin, bbox.Xmax, bbox.Ymax,
			hitStart, hitStop)
	}
	return L
}

// Finds axis-aligned partition closest to center of current node. Can return
// nil if none of those were good
func subPickNode_fast(w *NodesWork, ts *NodeSeg, bbox *NodeBounds,
	super *Superblock, cnt int) *NodeSeg {
	var previousPart *NodeSeg // keep track of previous partition - test only one seg per partner pair
	var bestH, bestV *NodeSeg
	oldDistH := Number(-1)
	oldDistV := Number(-1)
	if w.parts != nil {
		w.parts = w.parts[:0]
	}

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
				oldDistH = (part.psy - Number(midY)).Abs()
			} else {
				newDistH := (part.psy - Number(midY)).Abs()
				if newDistH < oldDistH {
					bestH = part
					oldDistH = newDistH
				}
			}
		} else if part.pdx == 0 {
			if bestV == nil {
				bestV = part
				oldDistV = (part.psx - Number(midX)).Abs()
			} else {
				newDistV := (part.psx - Number(midX)).Abs()
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
				if w.parts != nil {
					w.parts = w.parts[:0]
					w.parts = append(w.parts, part)
				}
			} else if cost == bestcost {
				if w.parts != nil {
					w.parts = append(w.parts, part)
				}
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
		if DiffSign(a, b) {
			if (a != 0) && (b != 0) {
				if w.PassingTooClose(part, check, cost, nil) {
					return true
				}
				// Line is split; a,b nonzero, opposite sign
				l := check.len
				// Distance from start of intersection
				// !!! 32-bit version of BSP v5.2 had overflow here on big
				// lines at a formidable distance. Must use 64-bit math here!
				// l*a may be too big for int32! -- VigilantDoomer
				d := Number((WideNumber(l) * WideNumber(a)) / (WideNumber(a) - WideNumber(b)))
				if d >= 2 {
					// If the linedef associated with this seg has a sector tag >= 900,
					// treat it as precious; i.e. don't split it unless all other options
					// are exhausted. This is used to protect deep water and invisible
					// lifts/stairs from being messed up accidentally by splits. - Killough
					// VigilantDoomer: don't select such partitions in maelstrom
					// fast path - we have a traditional path to fall back to
					if w.lines.IsTaggedPrecious(check.Linedef) &&
						!w.lines.SectorIgnorePrecious(check.sector) {
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
				// This fixes the problem with Hexen map05 (Guardian of Steel)
				// polyobject recipient sector 275 getting broken. AJ-BSP and
				// ZDBSP also use this solution
				if w.lines.IsTaggedPrecious(check.Linedef) &&
					!w.lines.SectorIgnorePrecious(check.sector) &&
					!w.PartIsPolyobjSide(part, check) {
					// VigilantDoomer: don't select such partitions in maelstrom
					// fast path - we have a traditional path to fall back to
					return true
				}
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

		if w.evalPartitionWorker_Maelstrom(block.subs[num], part, tot, diff,
			cost, bestcost) {
			return true
		}
	}

	// no "bad seg" was found
	return false
}

// PassingTooClose is a stub replaced by a real penalty-applying function
// for extended nodes. If it returns true, partition should not be chosen.
// It may modify cost and minors as well.
func (w *NodesWork) PassingTooClose(part, check *NodeSeg, cost *int,
	minors *MinorCosts) bool {
	return false
}

// PartIsPolyobjSide returns whether part is alongside any side of polyobject
// sector (which should be convex) that check comes from
func (w *NodesWork) PartIsPolyobjSide(part, check *NodeSeg) bool {
	lines := w.lines.GetAllPolyobjLines(check.Linedef)
	if lines == nil {
		return false
	}
	c := &IntersectionContext{
		psx: part.psx,
		psy: part.psy,
		pdx: part.pdx,
		pdy: part.pdy,
		pex: part.pex,
		pey: part.pey,
	}
	for _, line := range lines {
		x1, x2, y1, y2 := w.lines.GetAllXY(line)
		c.lsx = Number(x1)
		c.lsy = Number(x2)
		c.lex = Number(y1)
		c.ley = Number(y2)
		val := w.doLinesIntersect(c)
		if (val&1 != 0) && (val&16 != 0) {
			return true
		}
	}
	return false
}

// Zennode-like node picker. Split/sideness evaluation is not zennode's, but
// the scoring system is more or less faithful
func PickNode_ZennodeDepth(w *NodesWork, ts *NodeSeg, bbox *NodeBounds,
	super *Superblock) *NodeSeg {
	best := ts // make sure always got something to return
	if w.parts != nil {
		w.parts = w.parts[:0]
	}

	var previousPart *NodeSeg // keep track of previous partition - test only one seg per partner pair

	w.segAliasObj.UnvisitAll() // remove marks from previous PickNode calls
	w.zenScores = w.zenScores[:0]
	var c IntersectionContext

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
		cost := 0           // cost is most likely never used,
		tot := 0
		slen := Number(0) // length of partition that is incidental with segs

		w.zenScores = append(w.zenScores, DepthScoreBundle{})
		bundle := &(w.zenScores[len(w.zenScores)-1])
		minorsDummy := MinorCosts{}
		bundle.seg = part
		// Fill sectorHits array with zeros FAST (fewer bound checks)
		// Credit: gist github.com taylorza GO-Fillslice.md
		w.sectorHits[0] = 0
		sectorCount := len(w.sectorHits)
		for j := 1; j < sectorCount; j = j << 1 {
			copy(w.sectorHits[j:], w.sectorHits[:j])
		}

		c.psx = part.StartVertex.X
		c.psy = part.StartVertex.Y
		c.pex = part.EndVertex.X
		c.pey = part.EndVertex.Y
		c.pdx = c.psx - c.pex
		c.pdy = c.psy - c.pey

		//progress();           	        // Something for the user to look at.
		w.blocksHit = w.blocksHit[:0]
		inter := &ZenIntermediary{}
		prune := w.evalPartitionWorker_ZennodeDepth(super, part, &c, &cost,
			&slen, bundle, inter, &minorsDummy)
		prune = prune ||
			inter.segR == 0 ||
			(inter.segR == 1 && inter.segL == 0)
		if prune { // Early exit and skip past the tests below
			w.zenScores = w.zenScores[:len(w.zenScores)-1] // remove corresponding record
			continue
		}

		// Apply missing sectorHits from evalPartitionWorker
		for _, hitRecord := range w.blocksHit {
			hitRecord.block.MarkSectorsHit(w.sectorHits, hitRecord.mask)
		}

		// Count sectors on each side of partition line, and the split sectors
		flat := 0
		for tot = 0; tot < len(w.sectorHits); tot++ {
			switch w.sectorHits[tot] {
			case 0x0F:
				{
					inter.sectorL++
					flat++
				}
			case 0xF0:
				{
					inter.sectorR++
					flat++

				}
			case 0xFF:
				{
					inter.sectorS++
					flat++
				}
			}
		}

		if flat > 1 && w.diagonalPenalty != 0 && (part.pdx != 0 && part.pdy != 0) {
			bundle.diagonalFactor++
		}

		scoreIntermediate(bundle, inter, w.depthArtifacts)

		best = part // this should be overridden later, and only used as a fallback
	}
	if len(w.zenScores) > 0 {
		ZenPickBestScore(w.zenScores)
		best = w.zenScores[0].seg
		if w.parts != nil {
			w.parts = append(w.parts, best)
		}
		if w.parts != nil || config.VerbosityLevel >= 4 { // reference to global: config
			// Either need to grab all same-scoring stuff for hard multi-tree,
			// or have high enough verbosity level to show ambiguity rank to
			// user
			tst1, tst2, tst3, tst4 := w.zenScores[0].preciousSplit,
				w.zenScores[0].scoreTotal,
				w.zenScores[0].equivSplit,
				w.zenScores[0].segSplit
			track := 1
			for i := 1; i < len(w.zenScores); i++ {
				if tst1 == w.zenScores[i].preciousSplit &&
					tst2 == w.zenScores[i].scoreTotal &&
					tst3 == w.zenScores[i].equivSplit &&
					tst4 == w.zenScores[i].segSplit {
					if w.parts != nil {
						w.parts = append(w.parts, w.zenScores[i].seg)
					}
					track++
				} else {
					break
				}
			}
			if track > 1 {
				w.mlog.Verbose(4, "ZEN Ambiguity equal rank for %d records \n", track)
			}
		}
	}
	return best // All finished, return best Seg
}

// If returns true, the partition &part must be skipped, because it produced
// many splits early so that cost exceed bestcost
// Worker for Zennode prefers not to quit early, though
func (w *NodesWork) evalPartitionWorker_ZennodeDepth(block *Superblock,
	part *NodeSeg, c *IntersectionContext, cost *int, slen *Number,
	bundle *DepthScoreBundle, inter *ZenIntermediary, minors *MinorCosts) bool {

	// -AJA- this is the heart of my superblock idea, it tests the
	//       _whole_ block against the partition line to quickly handle
	//       all the segs within it at once.  Only when the partition
	//       line intercepts the box do we need to go deeper into it.
	num := BoxOnLineSide(block, part)
	if num < 0 {
		// LEFT
		inter.segL += block.realNum
		w.blocksHit = append(w.blocksHit, BlocksHit{
			block: block,
			mask:  uint8(0x0F),
		})
		return false
	} else if num > 0 {
		// RIGHT
		inter.segR += block.realNum
		w.blocksHit = append(w.blocksHit, BlocksHit{
			block: block,
			mask:  uint8(0xF0),
		})
		return false
	}

	for check := block.segs; check != nil; check = check.nextInSuper { // Check partition against all Segs
		// get state of lines' relation to each other
		leftside := false
		mask := uint8(0xF0)
		if check == part || check == part.partner {
			// Partition itself or its partner
			leftside = check == part.partner
			if leftside {
				inter.segL++
			} else {
				inter.segR++
			}
		} else {
			val := w.WhichSideCached(part, check, c)
			if val == SIDENESS_INTERSECT {
				if w.PassingTooClose(part, check, cost, nil) {
					return true
				}
				// Split line
				inter.segS++
				mask = uint8(0xFF)
				if w.lines.IsTaggedPrecious(check.Linedef) &&
					!w.lines.SectorIgnorePrecious(check.sector) {
					bundle.preciousSplit++
				}
			} else {
				// if to the left or to the right and not a collinear seg,
				// check for passing through vertex if this is precious
				checkPrecious := false
				if val == SIDENESS_LEFT {
					// to the left
					leftside = true
					inter.segL++
					checkPrecious = true
				}
				if val == SIDENESS_RIGHT {
					// to the right
					inter.segR++
					checkPrecious = true
				}
				if checkPrecious && w.lines.IsTaggedPrecious(check.Linedef) &&
					!w.lines.SectorIgnorePrecious(check.sector) &&
					passingThrough(part, check) &&
					!w.PartIsPolyobjSide(part, check) {
					// passing through vertex is bad for polyobject support (cue
					// taken from AJ_BSP)
					bundle.preciousSplit++
				}
				if val == SIDENESS_COLLINEAR {
					// Collinear seg... or so doLinesIntersect would tell us
					// This algorithm caches doLinesIntersect values rather
					// than computing them face to face so additional vetting
					// is needed
					if check.alias != part.alias && vetAliasTransfer(c) {
						check.alias = part.alias
					}
					*slen += check.len
					if check.pdx*part.pdx+check.pdy*part.pdy < 0 {
						leftside = true
						inter.segL++
					} else {
						inter.segR++
					}
				}
			}
		}
		if leftside {
			mask = uint8(0x0F)
		}

		w.sectorHits[check.sector] |= mask
	}
	// handle sub-blocks recursively

	for num := 0; num < 2; num++ {
		if block.subs[num] == nil {
			continue
		}

		if w.evalPartitionWorker_ZennodeDepth(block.subs[num], part, c,
			cost, slen, bundle, inter, minors) {
			return true
		}
	}

	// no "bad seg" was found
	return false
}

// passingThrough returns whether partition line part passes through either
// of the end vertices of check
func passingThrough(part, check *NodeSeg) bool {
	a := part.pdy*check.psx - part.pdx*check.psy + part.perp
	b := part.pdy*check.pex - part.pdx*check.pey + part.perp
	if DiffSign(a, b) {
		if a != 0 && b != 0 {

		} else {
			return true
		}
	}
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
