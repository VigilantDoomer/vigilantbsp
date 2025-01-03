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
	"sort"
)

// The grand nodebuilding speed-up technique from AJ-BSP by Andrew Apted:
// superblocks.

// smallest distance between two points before being considered equal
const DIST_EPSILON float64 = 1.0 / 128.0

const DIST_SHIFT = 7 // << DIST_SHIFT = 128 (1 / DIST_EPSILON)

const IFFY_LEN = 4.0

const MARGIN_LEN = 6 // MUST EQUAL int(IFFY_LEN * 1.5)

type Superblock struct {
	// parent of this block, or nil for a top-level block
	parent *Superblock
	// coordinates on map for this block, from lower-left corner to
	// upper-right corner.  Pseudo-inclusive, i.e (x,y) is inside block
	// if and only if x1 <= x < x2 and y1 <= y < y2.
	x1, y1 int
	x2, y2 int
	// sub-blocks. Nil when empty. [0] has the lower coordinates, and
	// [1] has the higher coordinates. Division of a square always
	// occurs horizontally (e.g. 512x512 -> 256x512 -> 256x256).
	subs [2]*Superblock
	// number of real segs contained by this block
	// (including all sub-blocks below it). If in future we have minisegs,
	// then we will have separate counter for them.
	realNum int
	// list of segs _directly_ contained by this block. Doesn't include those
	// contained in subblocks.
	segs *NodeSeg
	// For visplane reduction algorigthms, it helps performance to cache lists
	// of all sector within this and child superblocks as superblock is being
	// constructed
	sectors   []uint16
	secEquivs []uint16
	// secMap allows us to build those arrays without duplicates
	// NOTE didn't get improvements when substituting map for array, so kept map
	secMap map[uint16]struct{}
	nwlink *NodesWork
}

type BlocksHit struct {
	block *Superblock
	mask  uint8
}

// SuperIsLeaf() == true defines when superblock is no longer divisible into
// subblocks
func (s *Superblock) SuperIsLeaf() bool {
	return (s.x2-s.x1) <= 256 && (s.y2-s.y1) <= 256
}

func (s *Superblock) AddSegToSuper(seg *NodeSeg) {
	if seg == nil {
		return
	}
	block := s
	for {
		var p1, p2 bool
		var child int
		xMid := (block.x1 + block.x2) >> 1
		yMid := (block.y1 + block.y2) >> 1
		// update seg counts
		// TODO if GL nodes generation will be implemented, minisegs (seg not
		// from linedef) are not be added to realNum counter, but to their own
		// counter
		block.realNum++
		// update sector counts, if needed
		if block.sectors != nil {
			sec := seg.sector
			if !block.markAndRecall(block.sectors, sec) {
				block.sectors = append(block.sectors, sec)
			}
		} else if block.secEquivs != nil {
			sec := seg.secEquiv
			if !block.markAndRecall(block.secEquivs, sec) {
				block.secEquivs = append(block.secEquivs, sec)
			}
		}
		if block.SuperIsLeaf() {
			// block is not allowed to be subdivised any further
			seg.nextInSuper = block.segs
			seg.block = block
			block.segs = seg
			return
		}
		if block.x2-block.x1 >= block.y2-block.y1 {
			// block is wider than it is high, or square

			p1 = seg.StartVertex.X >= Number(xMid)
			p2 = seg.EndVertex.X >= Number(xMid)
		} else {
			// block is higher than it is wide

			p1 = seg.StartVertex.Y >= Number(yMid)
			p2 = seg.EndVertex.Y >= Number(yMid)
		}

		if p1 && p2 {
			child = 1
		} else if !p1 && !p2 {
			child = 0
		} else {
			// line crosses midpoint -- link it in and return
			seg.nextInSuper = block.segs
			seg.block = block
			block.segs = seg
			return
		}

		// OK, the seg lies in one half of this block.  Create the block
		// if it doesn't already exist, and loop back to add the seg.

		if block.subs[child] == nil {
			sub := s.nwlink.getNewSuperblock(s)
			block.subs[child] = sub
			sub.parent = block

			if block.x2-block.x1 >= block.y2-block.y1 {
				if child == 1 {
					sub.x1 = xMid
				} else {
					sub.x1 = block.x1
				}
				sub.y1 = block.y1

				if child == 1 {
					sub.x2 = block.x2
				} else {
					sub.x2 = xMid
				}
				sub.y2 = block.y2
			} else {
				sub.x1 = block.x1
				if child == 1 {
					sub.y1 = yMid
				} else {
					sub.y1 = block.y1
				}

				sub.x2 = block.x2
				if child == 1 {
					sub.y2 = block.y2
				} else {
					sub.y2 = yMid
				}
			}
		}
		block = block.subs[child]
	}
}

// rounds the value _up_ to the nearest power of two.
func RoundPOW2(x int) int {
	if x <= 2 {
		return x
	}

	x--

	for tmp := x >> 1; tmp != 0; tmp >>= 1 {
		x |= tmp
	}

	return x + 1
}

func (s *Superblock) InitSectorsIfNeeded(template *Superblock) {
	if template.sectors != nil {
		s.sectors = make([]uint16, 0)
	}
	if template.secEquivs != nil {
		s.secEquivs = make([]uint16, 0)
	}
}

func (s *Superblock) SetBounds(box *NodeBounds) {
	dx := (box.Xmax - box.Xmin + 127) >> BLOCK_BITS
	dy := (box.Ymax - box.Ymin + 127) >> BLOCK_BITS

	s.x1 = box.Xmin
	s.x2 = box.Xmin + (RoundPOW2(dx) << BLOCK_BITS)
	s.y1 = box.Ymin
	s.y2 = box.Ymin + (RoundPOW2(dy) << BLOCK_BITS)
}

// Register hits for all sectors present in current superblock in the sectorsHit
// aggregate. mask depends on whether whole superblock was left or right of
// partition being evaluated for which the sectorsHit is currently being updated
func (s *Superblock) MarkSectorsHit(sectorsHit []uint8, mask uint8) {
	for _, sector := range s.sectors {
		sectorsHit[sector] |= mask
	}
}

// Register hits for all _sector equivalencies_ present in current
// superblock in the sectorsHit aggregate. mask depends on whether whole
// superblock was left or right of partition being evaluated for which the
// sectorsHit is currently being updated
func (s *Superblock) MarkSecEquivsHit(sectorsHit []uint8, mask uint8) {
	for _, secEquiv := range s.secEquivs {
		sectorsHit[secEquiv] |= mask
	}
}

// Like MarkSectorsHit, but doesn't assume there is a built already cache
// for sectors belonging to superblock
func (s *Superblock) MarkSectorsHitNoCached(sectorsHit []uint8, mask uint8) {
	for seg := s.segs; seg != nil; seg = seg.nextInSuper {
		sectorsHit[seg.sector] |= mask
	}

	// Recurse!
	for num := 0; num < 2; num++ {
		if s.subs[num] == nil {
			continue
		}

		s.subs[num].MarkSectorsHitNoCached(sectorsHit, mask)
	}
}

// Multitree_plain hack. Copies only some superblock metadata but no
// children or segs. Intended to be called only on root superblock, the returned
// value is to be used as ephemism for initial root "superblock" for MTP trees
func (s *Superblock) DerivePseudo() *Superblock {
	res := &Superblock{
		parent: nil,
		//x1:     s.x1,
		//y1:     s.x1,
		//x2:     s.x2,
		//y2:     s.y2,
		subs: [2]*Superblock{nil, nil},
		segs: nil,
	}
	res.InitSectorsIfNeeded(s) // this is actually what the return value is used for
	// Store original length of secEquivs and sectors in x1 (zoneAlloc will read
	// this value later)
	// x1,y1,x2,y2 from pseudosuperblock are not actually used
	res.x1 = cap(s.secEquivs) + cap(s.sectors)
	return res
}

// func UtilPerpDist(part *NodeSeg, x, y float64) float64 {
// 		return (x*float64(part.pdy) - y*float64(part.pdx) +
// 			float64(part.perp)) / part.flen
// }

// Returns (x * part.pdy - y * part.pdx + part.perp) / part.len
// in fixed point scaled by (<< DIST_SHIFT). That is, 1 represents 1/128
func UtilPerpDist(part *NodeSeg, x, y int) int64 {
	return ((int64(x)*int64(part.pdy) - int64(y)*int64(part.pdx) +
		int64(part.perp)) << DIST_SHIFT) / int64(part.len)
}

// Doesn't distinguish between zero and positive, before the use case doesn't
// require it
func Int64AbsAndSign(x int64) (int64, bool) {
	if x >= 0 {
		return x, false
	}
	return -x, true
}

// Returns -1 for left, +1 for right, or 0 for intersect.
// Replaced with different implementation for Zdoom nodes
func PointOnLineSide(part *NodeSeg, x, y int) int {
	perp := UtilPerpDist(part, x, y)
	ab, sgn := Int64AbsAndSign(perp)
	if ab <= 1 { // remember that UtilPerpDist returns value scaled (multiplied by 128)
		return 0
	}
	if sgn {
		return -1
	}
	return +1
}

// Which side of partition line is the superblock?
// Returns -1 for left, +1 for right, or 0 for intersect.
func BoxOnLineSide(box *Superblock, part *NodeSeg) int {
	x1 := box.x1 - MARGIN_LEN
	y1 := box.y1 - MARGIN_LEN
	x2 := box.x2 + MARGIN_LEN
	y2 := box.y2 + MARGIN_LEN

	var p1, p2 int

	// handle simple cases (vertical & horizontal lines)
	if part.pdx == 0 {
		if Number(x1) > part.psx {
			p1 = +1
		} else {
			p1 = -1
		}
		if Number(x2) > part.psx {
			p2 = +1
		} else {
			p2 = -1
		}
		if part.pdy < 0 {
			p1 = -p1
			p2 = -p2
		}
	} else if part.pdy == 0 {
		if Number(y1) < part.psy {
			p1 = +1
		} else {
			p1 = -1
		}
		if Number(y2) < part.psy {
			p2 = +1
		} else {
			p2 = -1
		}

		if part.pdx < 0 {
			p1 = -p1
			p2 = -p2
		}
	} else if WideNumber(part.pdx)*WideNumber(part.pdy) > 0 { // now handle the cases of positive and negative slope
		p1 = PointOnLineSide(part, x1, y2)
		p2 = PointOnLineSide(part, x2, y1)
	} else { // NEGATIVE
		p1 = PointOnLineSide(part, x1, y1)
		p2 = PointOnLineSide(part, x2, y2)
	}

	if p1 == p2 {
		return p1
	}
	return 0
}

// markAndRecall is function for internal superblock maintenance. It is used
// for quick adding of sectors/secEquivs without duplicates, and will use an
// optimal strategy for checking if sec is already in arr
// If it returns true, sec shall not be added to arr as it is already present
// in it, if it is false, it must be added
func (block *Superblock) markAndRecall(arr []uint16, sec uint16) bool {
	if len(arr) <= 4 {
		// Small arrays are more efficiently checked directly - this actually
		// benefits performance and gives garbage collector less work.
		// Reason is, on many maps, especially those under vanilla limits,
		// over 50% of ever used superblocks never grow beyond having just 4
		// sectors/secEquivs! And hashmap is generally too much memory for
		// what it stores.
		for _, chk := range arr {
			if chk == sec {
				return true
			}
		}
		return false
	}
	// Bigger arrays are checked against secMap
	if block.secMap == nil {
		// initialize map and add all sectors so far to that
		block.secMap = make(map[uint16]struct{}, 64)
		for _, chk := range arr {
			block.secMap[chk] = struct{}{}
		}
	}
	if _, ex := block.secMap[sec]; !ex {
		block.secMap[sec] = struct{}{}
		return false
	}
	return true
}

func (w *NodesWork) getNewSuperblock(template *Superblock) *Superblock {
	if w.qallocSupers == nil {
		ret := &Superblock{}
		ret.InitSectorsIfNeeded(template)
		ret.nwlink = w
		return ret
	}
	ret := w.qallocSupers
	w.qallocSupers = w.qallocSupers.subs[0] // see returnSuperblockToPool
	ret.subs[0] = nil
	ret.nwlink = w
	if ret.sectors != nil { // shall give same result as template.sectors != nil
		ret.sectors = ret.sectors[:0]
	}
	if ret.secEquivs != nil { // shall give same result as template.secEquivs != nil
		ret.secEquivs = ret.secEquivs[:0]
	}
	return ret
}

func (w *NodesWork) returnSuperblockToPool(block *Superblock) {
	if block.segs == nil {
		// pseudoSuperblock - don't touch! shared between threadds
		return
	}
	for num := 0; num < 2; num++ {
		if block.subs[num] == nil {
			continue
		}
		w.returnSuperblockToPool(block.subs[num])
		block.subs[num] = nil
	}
	block.segs = nil
	block.secMap = nil
	block.realNum = 0
	block.parent = nil
	block.nwlink = nil
	// qallocSupers is pool of reusable (but limited to current thread)
	// superblocks chained via subs[0]
	block.subs[0] = w.qallocSupers
	w.qallocSupers = block
}

func (w *NodesWork) dismissChildrenToSuperblockPool(block *Superblock) {
	if block.segs == nil {
		// pseudoSuperblock - don't touch! shared between threadds
		return
	}
	for num := 0; num < 2; num++ {
		if block.subs[num] == nil {
			continue
		}
		w.returnSuperblockToPool(block.subs[num])
		block.subs[num] = nil
	}
}

func (w *NodesWork) newSuperblockNoProto() *Superblock {
	ret := &Superblock{}
	if w.pickNodeUser == PICKNODE_VISPLANE || w.pickNodeUser == PICKNODE_ZENLIKE {
		ret.sectors = make([]uint16, 0)
	} else if w.pickNodeUser == PICKNODE_VISPLANE_ADV {
		ret.secEquivs = make([]uint16, 0)
	}
	return ret
}

func (w *NodesWork) newSectorUsingSuperblock() *Superblock {
	ret := &Superblock{}
	ret.sectors = make([]uint16, 0)
	return ret
}

// OptimizeSectorAddresses is intended to be called on root superblock for node
// It helps visplane-reducing partitioners linearize their accesses to sectorsHit
// as well as determine minimum and maximum sector within the node, and thus
// clean/iterate over only its subset, saving time.
// It might or might not also sort segs by sector (or secEquiv) references.
// After call to OptimizeSectorAddresses(), sectors or secEquiv slice of the ROOT
// superblock (the one on which function is called) is guaranteed to be sorted in
// ascending order.
// Note that this does nothing relevant (from optimization perspective) for
// non-visplane partitioners such as traditional (seg-only) and maelstrom.
// (Zennode-like does count a visplane-reducing partitioner)
func (s *Superblock) OptimizeSectorAddresses() {
	// sectors and secEquiv arrays should contain unique elements
	if s.sectors != nil {
		sort.Sort(Uint16Slice(s.sectors))
	}
	if s.secEquivs != nil {
		sort.Sort(Uint16Slice(s.secEquivs))
	}
	// Ok, the above already yielded some improvements. But I want to squeeze further.

	// Will be sorting seg.nextInSuper links to align with sectors -- should linearize
	// accesses to sectorsHit in evalPartitionWorker.
	// UPDATE: And this other idea didn't work at all. The sort itself was fast,
	// but it did not improve access times in evalPartitionWorker on that cursed line:
	// w.sectorHits[check.secEquiv] |= mask
	// The reason being is that it restarted for each nested superblock. So hardly
	// linearizing anything. Not to say that it involves reading and writing to the
	// same address...
	/*
		fbigarr := make([]*NodeSeg, s.realNum)
		start := new(int)
		*start = 0
		s.sortSuperSegs(fbigarr, start)
		fbigarr = nil*/
}

func (s *Superblock) sortSuperSegs(fbigarr []*NodeSeg, start *int) {
	if s == nil {
		return
	}

	if s.segs != nil {
		// yeah, superblock can have s.segs == nil, in which case all segs fit into
		// child superblocks
		*start += s.sortSegsInCurrent(fbigarr[*start:])
	}

	s.subs[0].sortSuperSegs(fbigarr, start)
	s.subs[1].sortSuperSegs(fbigarr, start)
}

func (s *Superblock) sortSegsInCurrent(window []*NodeSeg) int {
	// s.realNum is total count including subblocks, where seg.nextInSuper links
	// track only directly embedded ones, without subblocks. So need to count them
	embedCount := 1
	seg := s.segs
	for ; seg.nextInSuper != nil; seg = seg.nextInSuper {
		window[embedCount-1] = seg
		embedCount++
	}
	window[embedCount-1] = seg

	window = window[:embedCount]

	if s.sectors != nil {
		// sort must be stable -- actually, review. Partner segs may reference
		// different sectors. But, partners should not be required to be together
		// through "nextInSuper" pointer, only through "next" pointer
		sort.Sort(SegInSuperBySector(window))
	} else if s.secEquivs != nil {
		sort.Sort(SegInSuperBySecEquiv(window))
	}

	// Now change those nextInSuper to reflect order in array
	for i := range window[:len(window)-1] {
		window[i].nextInSuper = window[i+1]
	}
	window[len(window)-1].nextInSuper = nil
	s.segs = window[0]
	return embedCount
}

type SegInSuperBySector []*NodeSeg

func (x SegInSuperBySector) Len() int           { return len(x) }
func (x SegInSuperBySector) Less(i, j int) bool { return x[i].sector < x[j].sector }
func (x SegInSuperBySector) Swap(i, j int)      { x[i], x[j] = x[j], x[i] }

type SegInSuperBySecEquiv []*NodeSeg

func (x SegInSuperBySecEquiv) Len() int           { return len(x) }
func (x SegInSuperBySecEquiv) Less(i, j int) bool { return x[i].secEquiv < x[j].secEquiv }
func (x SegInSuperBySecEquiv) Swap(i, j int)      { x[i], x[j] = x[j], x[i] }
