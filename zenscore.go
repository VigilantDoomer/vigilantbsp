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
	"sort"
)

const VERY_BAD_SCORE = -2147483648

// zenscore module - provides the partition score system from Zennode family
// of nodebuilders. It is designed to reduce depth.
// This code is basically ported from Zennode v1.2.1 (c) Mark Rousseau and
// ZokumBSP v 1.0.11 nodebuilders and
// contains logic that, as anecdotal evidence tends to suggest, can reduce
// BSP tree depth provided appropriate magic constants are used in its
// arcane formula

// metric = S? (L * R) / (X1 ? X1 * S / X2 : 1) - (X3 * S + X4) * S : (L * R)
// where ? : is a C ternary operator, L = number of segs to the left,
// R = number of segs to the right, S = number of split segs, and X1-4 are
// magic numbers defined in constants below. They were derived empirically
// by Mark Rousseau based on how they affected the wads he had at his disposal
// to try and find the best possible depth-reducing algorithm
// In all honesty, I (VigilantDoomer) have no idea how this formula works
// towards depth reduction - unlike BSP's visplane reduction algorithm, there
// is no rationale to be found behind either numbers or the formula

const ZEN_X1 = 20

const ZEN_X2 = 10

const ZEN_X3 = 1

const ZEN_X4 = 25

// similar metric, but for sectors instead of segs
// Note: Zennode and ZokumBSP defined the values found below, and even allowed
// to modify them with environment variables, but used X-constants in their
// place, while these remained unused

const ZEN_Y1 = 1

const ZEN_Y2 = 7

const ZEN_Y3 = 1

const ZEN_Y4 = 0

// So, important note about working on it: this metric is not cost, greater
// value WINS. Pay attention to how callbacks passed to qsort are written in
// Zennode / ZokumBSP
// Also, maybe this will have to redefine all secondary modes, not just depth
// one

// Use secondary score for partition selection:
// - Split minimization with no tree balancing
// - Minimize depth, favor no split segs
// - Minimize depth, favor fewer subsectors
// - Minimize depth, favor both of above equally

type DepthScoreBundle struct {
	seg           *NodeSeg
	preciousSplit int // lesser value wins
	scoreSeg      int // greater value wins
	scoreSector   int // greater value wins
	scoreTotal    int // greater value wins
}

type ZenIntermediary struct {
	segL    int
	segR    int
	segS    int
	sectorL int
	sectorR int
	sectorS int
}

type DepthScoresBySeg []DepthScoreBundle

func (x DepthScoresBySeg) Len() int { return len(x) }
func (x DepthScoresBySeg) Less(i, j int) bool {
	if x[i].scoreSeg < x[j].scoreSeg {
		return false
	}
	if x[i].scoreSeg > x[j].scoreSeg {
		return true
	}
	if x[i].scoreSector < x[j].scoreSector {
		return false
	}
	if x[i].scoreSector > x[j].scoreSeg {
		return true
	}
	return x[i].seg.Linedef < x[j].seg.Linedef
}
func (x DepthScoresBySeg) Swap(i, j int) { x[i], x[j] = x[j], x[i] }

type DepthScoresBySector []DepthScoreBundle

func (x DepthScoresBySector) Len() int { return len(x) }
func (x DepthScoresBySector) Less(i, j int) bool {
	if x[i].scoreSector < x[j].scoreSector {
		return false
	}
	if x[i].scoreSector > x[j].scoreSeg {
		return true
	}
	if x[i].scoreSeg < x[j].scoreSeg {
		return false
	}
	if x[i].scoreSeg > x[j].scoreSeg {
		return true
	}
	return x[i].seg.Linedef < x[j].seg.Linedef
}
func (x DepthScoresBySector) Swap(i, j int) { x[i], x[j] = x[j], x[i] }

type DepthScoresByTotal []DepthScoreBundle

func (x DepthScoresByTotal) Len() int { return len(x) }
func (x DepthScoresByTotal) Less(i, j int) bool {
	if x[i].preciousSplit < x[j].preciousSplit {
		return true
	} else if x[i].preciousSplit > x[j].preciousSplit {
		return false
	}

	if x[i].scoreTotal < x[j].scoreTotal {
		return false
	} else if x[i].scoreTotal > x[j].scoreTotal {
		return true
	}

	return x[i].seg.Linedef < x[j].seg.Linedef
}
func (x DepthScoresByTotal) Swap(i, j int) { x[i], x[j] = x[j], x[i] }

// Sorts the argument with respect to several metric evaluations so that
// the best record is the first (#0) in array
// The argument is mutated. Code is courtesy of Zennode (c) Mark Rousseau
func ZenPickBestScore(sc []DepthScoreBundle) {
	sort.Sort(DepthScoresBySeg(sc))
	rank := 0
	for i := 0; i < len(sc); i++ {
		sc[i].scoreSeg = rank
		if i < len(sc)-1 && sc[i].scoreSeg != sc[i+1].scoreSeg {
			rank++
		}
	}
	sort.Sort(DepthScoresBySector(sc))
	rank = 0
	for i := 0; i < len(sc); i++ {
		sc[i].scoreSector += rank
		if i < len(sc)-1 && sc[i].scoreSector != sc[i+1].scoreSector {
			rank++
		}
	}
	sort.Sort(DepthScoresByTotal(sc))
	// The #0 element now contains the best choice
}

// These builds seg and sector scores for the entire array. Other fields
// not touched
func ZenComputeScores(super *Superblock, sc []DepthScoreBundle,
	sectorHits []uint8, depthArtifacts bool) {
	for i, _ := range sc {
		inter := ZenIntermediary{
			segL:    0,
			segR:    0,
			segS:    0,
			sectorL: 0,
			sectorR: 0,
			sectorS: 0,
		}
		// Fill sectorHits array with zeros FAST (fewer bound checks)
		// Credit: gist github.com taylorza GO-Fillslice.md
		sectorHits[0] = 0
		hitArrayLen := len(sectorHits)
		for j := 1; j < hitArrayLen; j = j << 1 {
			copy(sectorHits[j:], sectorHits[:j])
		}
		// Obtain data for current partitition by evaluating all segs against
		// it again, as we avoided doing so earlier (performance reasons).
		evalPartitionWorker_Zen(super, &(sc[i]), &inter, sectorHits)
		for j := 0; j < len(sectorHits); j++ {
			switch sectorHits[j] {
			case 0x0F:
				{
					inter.sectorL++
				}
			case 0xF0:
				{
					inter.sectorR++
				}
			case 0xFF:
				{
					inter.sectorS++
				}
			}
		}

		// Compute both metrics now
		sc[i].scoreSeg = (inter.segL + inter.segS) * (inter.segR + inter.segS)
		sc[i].scoreSector = (inter.sectorL + inter.sectorS) * (inter.sectorR + inter.sectorS)

		if sc[i].scoreSeg == 0 {
			// one-sided (bad). Force it to rank lowest
			sc[i].scoreSeg = VERY_BAD_SCORE
			sc[i].scoreSector = VERY_BAD_SCORE
			continue
		}

		// TODO computations below feature both multiplication and division,
		// there might be condition when they overflow on 32-bit system

		// Finish computing seg metric
		if inter.segS > 0 {
			// Have seg splits, so
			tmp := ZEN_X1 * inter.segS
			if ZEN_X2 < tmp {
				sc[i].scoreSeg = ZEN_X2 * sc[i].scoreSeg / tmp
			}
			if depthArtifacts {
				// ZokumBSP calibrated formula differently
				sc[i].scoreSeg -= (ZEN_X3*inter.segS*(inter.segS/3) +
					ZEN_X4) * inter.segS
			} else {
				sc[i].scoreSeg -= (ZEN_X3*inter.segS + ZEN_X4) * inter.segS
			}
		} else { // Logic introduced in ZokumBSP, activated when no SEG splits are preferred
			// It also makes better balanced partitions with respect to SEG
			// count on both sides score better (higher)
			sc[i].scoreSeg = 0x7FFFFFFF - Abs(inter.segL-
				inter.segR)
		}

		// Finish computing sector metric
		if depthArtifacts {
			// Ok, both Zennode and ZokumBSP use ZEN_X* rather than ZEN_Y*
			// consts. That is not the only change here, however
			if inter.sectorS > 0 {
				tmp := ZEN_X1 * inter.sectorS
				if ZEN_X2 < tmp {
					sc[i].scoreSector = ZEN_X2 * sc[i].scoreSector / tmp
				}
				// Bye bye sanity - yes the last multiplicative is number
				// of split segs not sectors in Zennode and ZokumBSP
				// --VigilantDoomer
				sc[i].scoreSector -= (ZEN_X3*inter.sectorS + ZEN_X4) * inter.segS
			} else { // Logic introduced in ZokumBSP, activated when no SECTOR splits are preferred
				// It also makes better balanced partitions with respect to
				// SECTOR count on both sides score better (higher)
				sc[i].scoreSector = 0x7FFFFFFF - Abs(inter.sectorL-
					inter.sectorR)
			}
		} else {
			// What I though it would be
			if inter.sectorS > 0 {
				tmp := ZEN_Y1 * inter.sectorS
				if ZEN_Y2 < tmp {
					sc[i].scoreSector = ZEN_Y2 * sc[i].scoreSector / tmp
				}
				// Next formula is not what Zennode and ZokumBSP defacto
				// use
				sc[i].scoreSector -= (ZEN_Y3*inter.sectorS + ZEN_Y4) * inter.sectorS
			} else { // Logic introduced in ZokumBSP, activated when no SECTOR splits are preferred
				// It also makes better balanced partitions with respect to
				// SECTOR count on both sides score better (higher)
				sc[i].scoreSector = 0x7FFFFFFF - Abs(inter.sectorL-
					inter.sectorR)
			}
		}
	}
}

func evalPartitionWorker_Zen(block *Superblock, rec *DepthScoreBundle,
	intermediate *ZenIntermediary, sectorHits []uint8) {
	part := rec.seg
	num := BoxOnLineSide(block, part)
	if num < 0 {
		// LEFT
		intermediate.segL += block.realNum
		block.MarkSectorsHitNoCached(sectorHits, uint8(0x0F))
		return
	} else if num > 0 {
		// RIGHT
		intermediate.segR += block.realNum
		block.MarkSectorsHitNoCached(sectorHits, uint8(0xF0))
		return
	}

	for check := block.segs; check != nil; check = check.nextInSuper { // Check partition against all Segs
		// get state of lines' relation to each other
		leftside := false
		mask := uint8(0xF0)
		c := &IntersectionContext{
			psx: part.StartVertex.X,
			psy: part.StartVertex.Y,
			pex: part.EndVertex.X,
			pey: part.EndVertex.Y,
		}
		c.pdx = c.psx - c.pex
		c.pdy = c.psy - c.pey
		c.lsx = check.StartVertex.X
		c.lsy = check.StartVertex.Y
		c.lex = check.EndVertex.X
		c.ley = check.EndVertex.Y
		val := c.doLinesIntersect() // use more accurate side evaluation
		if ((val&2 != 0) && (val&64 != 0)) || ((val&4 != 0) && (val&32 != 0)) {
			// Split line
			intermediate.segS++
			mask = uint8(0xFF)
		} else {
			if check == part || check == part.partner {
				// Partition itself or its partner
				leftside = check == part.partner
				if leftside {
					intermediate.segL++
				} else {
					intermediate.segR++
				}
			} else {
				if val&34 != 0 {
					// to the left
					leftside = true
					intermediate.segL++
				}
				if val&64 != 0 {
					// to the right
					intermediate.segR++
				}
				if (val&1 != 0) && (val&16 != 0) {
					// Collinear seg
					if check.pdx*part.pdx+check.pdy*part.pdy < 0 {
						leftside = true
						intermediate.segL++
					} else {
						intermediate.segR++
					}
				}
			}
		}
		if leftside {
			mask = uint8(0x0F)
		}

		sectorHits[check.sector] |= mask
	}

	// handle sub-blocks recursively
	for num := 0; num < 2; num++ {
		if block.subs[num] == nil {
			continue
		}

		evalPartitionWorker_Zen(block.subs[num], rec, intermediate,
			sectorHits)
	}

}

func ZenSegMinorToDepthScores(input []SegMinorBundle) []DepthScoreBundle {
	res := make([]DepthScoreBundle, len(input))
	for i, entry := range input {
		res[i].seg = entry.seg
		res[i].preciousSplit = entry.minor.PreciousSplit
		res[i].scoreSeg = 0
		res[i].scoreSector = 0
		res[i].scoreTotal = 0
	}
	return res
}
