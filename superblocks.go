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
)

// The grand nodebuilding speed-up technique from AJ-BSP by Andrew Apted:
// superblocks.

// smallest distance between two points before being considered equal
const DIST_EPSILON float64 = 1.0 / 128.0

const IFFY_LEN = 4.0

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
	secMap map[uint16]bool
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
			if !block.secMap[sec] {
				block.sectors = append(block.sectors, sec)
				block.secMap[sec] = true
			}
		} else if block.secEquivs != nil {
			sec := seg.secEquiv
			if !block.secMap[sec] {
				block.secEquivs = append(block.secEquivs, sec)
				block.secMap[sec] = true
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

			p1 = seg.StartVertex.X >= xMid
			p2 = seg.EndVertex.X >= xMid
		} else {
			// block is higher than it is wide

			p1 = seg.StartVertex.Y >= yMid
			p2 = seg.EndVertex.Y >= yMid
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
			sub := NewSuperBlock()
			block.subs[child] = sub
			sub.parent = block
			sub.InitSectorsIfNeeded(block)

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

func NewSuperBlock() *Superblock {
	return &Superblock{}
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
	if template.secMap != nil {
		s.secMap = make(map[uint16]bool)
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

func UtilPerpDist(part *NodeSeg, x, y float64) float64 {
	return (x*float64(part.pdy) - y*float64(part.pdx) +
		float64(part.perp)) / part.flen
}

// Returns -1 for left, +1 for right, or 0 for intersect.
func PointOnLineSide(part *NodeSeg, x, y float64) int {
	perp := UtilPerpDist(part, x, y)
	if math.Abs(perp) <= DIST_EPSILON {
		return 0
	}
	if math.Signbit(perp) {
		return -1
	}
	return +1
}

// Which side of partition line is the superblock?
// Returns -1 for left, +1 for right, or 0 for intersect.
func BoxOnLineSide(box *Superblock, part *NodeSeg) int {
	x1 := float64(box.x1) - IFFY_LEN*1.5
	y1 := float64(box.y1) - IFFY_LEN*1.5
	x2 := float64(box.x2) + IFFY_LEN*1.5
	y2 := float64(box.y2) + IFFY_LEN*1.5

	var p1, p2 int

	// handle simple cases (vertical & horizontal lines)
	if part.pdx == 0 {
		if x1 > float64(part.psx) {
			p1 = +1
		} else {
			p1 = -1
		}
		if x2 > float64(part.psx) {
			p2 = +1
		} else {
			p2 = -1
		}
		if part.pdy < 0 {
			p1 = -p1
			p2 = -p2
		}
	} else if part.pdy == 0 {
		if y1 < float64(part.psy) {
			p1 = +1
		} else {
			p1 = -1
		}
		if y2 < float64(part.psy) {
			p2 = +1
		} else {
			p2 = -1
		}

		if part.pdx < 0 {
			p1 = -p1
			p2 = -p2
		}
	} else if int64(part.pdx)*int64(part.pdy) > 0 { // now handle the cases of positive and negative slope
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
