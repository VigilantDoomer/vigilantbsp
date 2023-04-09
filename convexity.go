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

// convexity
package main

// Special partitioning of non-convex nodes comprised of single sector
// Such "leaf" clusters usually are small, allowing to use more expensive
// metrics, while also cutting the clutter supposed to handle multiple sectors
// Another reason this special treatment is made avaiable is because any choice
// of partitioning from here on can't affect visplanes, thus it makes sense to
// do seg split minimization to mitigate possible extra splits made by advanced
// visplanes minimization algo (advanced seems to create more seg splits than
// Killough's vanilla method)
// Also, penalizing diagonal lines is no longer necessary (nor implemented) here
// TODO I attempted to:
// 1. Pick some partitions from arbitrary pair of vertices rather than segs
// 2. Evaluate whether either of sides will be convex and penalize the pick if
// neither are
// - maybe when both sides are convex, the pick should be selected, ignoring
// other parameters. Worth a revisit.
// Both approaches separate and combined failed to make a positive difference,
// while sometimes increasing running time and make the seg count worse not
// better.
// Thus this now stands by true and tried approach of increased seg split cost.
// There are plans for revisiting this, but it would only be justified by
// complete redesign of the picker, no less. It would probably need to do rounds
// of evaluation in depth (to evaluate several consecutive partition choices at
// once), cost function for a single partition pick is as good as it can be it
// seems

// CreateNodeForSingleSector processes a list of segs all formed from linedefs
// of one sector only, the list is NOT a convex a polygon else we would have a
// subsector already
// This also has a twin in stknode.go
func CreateNodeForSingleSector(w *NodesWork, ts *NodeSeg, bbox *NodeBounds,
	super *Superblock) *NodeInProcess {
	res := new(NodeInProcess)
	var rights *NodeSeg
	var lefts *NodeSeg
	var rightsSuper *Superblock
	var leftsSuper *Superblock
	// Divide node in two
	w.totals.numNodes++
	w.DivideSegsForSingleSector(ts, &rights, &lefts, bbox, super, &rightsSuper,
		&leftsSuper, nil)
	super = nil // NOTE after DivideSegs return, super may no longer be valid
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
	if w.isItConvex(lefts) == CONVEX_SUBSECTOR {
		res.nextL = nil
		res.LChild = w.CreateSSector(lefts) | SSECTOR_DEEP_MASK
		w.returnSuperblockToPool(leftsSuper)
	} else { // only NONCONVEX_ONESECTOR can be here
		res.nextL = CreateNodeForSingleSector(w, lefts, leftBox, leftsSuper)
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
		res.RChild = w.CreateSSector(rights) | SSECTOR_DEEP_MASK
		w.returnSuperblockToPool(rightsSuper)
	} else { // only NONCONVEX_ONESECTOR can be here
		res.nextR = CreateNodeForSingleSector(w, rights, rightBox, rightsSuper)
		res.RChild = 0
	}

	// CheckNodeBounds(bbox, leftBox, rightBox)

	return res
}

// DivideSegsForSingleSector is like DivideSegs, but nodepicker is different
func (w *NodesWork) DivideSegsForSingleSector(ts *NodeSeg, rs **NodeSeg,
	ls **NodeSeg, bbox *NodeBounds, super *Superblock, rightsSuper,
	leftsSuper **Superblock, partsegs *[]PartSeg) {
	// Pick best node to use
	best := PickNode_SingleSector(w, ts, bbox, super)

	if best == nil { // To programmers: write PickNode so it never happens
		panic("Couldn't pick nodeline!")
	}

	if partsegs != nil {
		w.GetPartSegs(ts, best, partsegs)
	}

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

	w.DivideSegsActual(ts, rs, ls, bbox, best, c, super, rightsSuper, leftsSuper)
}

// A modification of PickNode_Traditional, which uses more expensive (but more
// precise) doLinesIntersect for evaluating a partition + split cost is doubled,
// as we are specifically looking to eradicate seg splits, everything else be
// damned.
func PickNode_SingleSector(w *NodesWork, ts *NodeSeg, bbox *NodeBounds,
	super *Superblock) *NodeSeg {
	best := ts                        // make sure always got something to return
	bestcost := int(INITIAL_BIG_COST) //
	cnt := 0
	if w.parts != nil { // hard multi-tree support
		w.parts = w.parts[:0]
	}

	for part := ts; part != nil; part = part.next { // Count once and for all
		cnt++
	}

	var previousPart *NodeSeg // keep track of previous partition - test only one seg per partner pair

	w.segAliasObj.UnvisitAll()                      // remove marks from previous PickNode calls
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

		//progress();           	        // Something for the user to look at.

		c := &IntersectionContext{
			psx: part.StartVertex.X,
			psy: part.StartVertex.Y,
			pex: part.EndVertex.X,
			pey: part.EndVertex.Y,
		}
		c.pdx = c.psx - c.pex
		c.pdy = c.psy - c.pey
		leftcnt := 0
		rightcnt := 0
		prune := false

		for check := ts; check != nil; check = check.next { // Check partition against all Segs
			// get state of lines' relation to each other
			leftside := false
			c.lsx = check.StartVertex.X
			c.lsy = check.StartVertex.Y
			c.lex = check.EndVertex.X
			c.ley = check.EndVertex.Y
			val := w.doLinesIntersect(c)
			if ((val&2 != 0) && (val&64 != 0)) || ((val&4 != 0) && (val&32 != 0)) {
				// splits are now double as bad as before
				cost += PICKNODE_FACTOR << 1
				if cost >= bestcost {
					prune = true // Lee Killough's master speed-up
					break
				}
				tot++
				leftcnt++
				rightcnt++
			} else {
				if check == part || check == part.partner {
					leftside = check == part.partner
					if leftside {
						check.alias = part.alias
						leftcnt++
					} else {
						rightcnt++
					}
				} else {
					if val&34 != 0 {
						// to the left
						leftside = true
						leftcnt++
					}
					if val&68 != 0 {
						// to the right
						rightcnt++
					}
					if (val&1 != 0) && (val&16 != 0) {
						if check.alias != part.alias && vetAliasTransfer(c) {
							check.alias = part.alias
						}
						if check.pdx*part.pdx+check.pdy*part.pdy < 0 {
							leftside = true
							leftcnt++
						} else {
							rightcnt++
						}
					}
				}
			}
			if leftside {
				diff -= 2
			}
		}
		if prune {
			continue
		}

		if rightcnt == 0 || (rightcnt == 1 && leftcnt == 0) { // in this case nothing can be done to salvage the situation
			continue
		}

		if leftcnt == 0 {
			// penalize a little bit, but not as much as split
			cost += PICKNODE_FACTOR
		}

		// Take absolute value. diff is being used to obtain the
		// min/max values by way of: min(a,b)=(a+b-abs(a-b))/2

		diff -= tot
		if diff < 0 {
			diff = -diff
		}

		cost += diff
		if cost < bestcost {
			// We have a new better choice
			bestcost = cost
			best = part // Remember which Seg
			if w.parts != nil {
				w.parts = w.parts[:0]
				w.parts = append(w.parts, part)
			}
		} else if cost == bestcost && w.parts != nil {
			w.parts = append(w.parts, part)
		}
	}

	return best // All finished, return best Seg
}
