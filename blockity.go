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

// The name Blockity means "Block iterator blablabla"

const (
	BLOCK_FOLLOW_AXIS = iota
	BLOCK_INARRAY
)

// Note: in regards to replacing BlockityLines.cur field type with non-pointer
// uint16 (by using the fact that 0xFFFF linedef is unlikely to be used in
// proper maps):
// 1. The performance gain for reject, if it exists, is not noticeable with graphs.
// 2. The (possible?) performance gain for reject without graphs is negligible
// 3. 0xFFFF can still be used as valid linedef if not included in blockmap, I
// guess, so you can't really use it as a special value and remain within 0xFFFF
// So, in the end I ditched using blockity in reject code, which saved a lot of
// time indeed.

// BlockityLines makes it easy to get all lines that are in same block
// as the line specified by SetContext method. Important feature: no line
// is ever returned twice since the last call to SetContext
type BlockityLines struct {
	bm         *Blockmap
	xmin, ymin int
	xblocks    int
	// seenLines - don't return same line twice
	seenLines [65536 / 8]uint8 // bool array compressed as bit array (performs better than map)
	// cur stands for "cursor"
	cur              *uint16
	curBlock         int
	curLine          int
	x1, y1           int
	dx, dy           int
	bx, by, bx2, by2 int
	wbeg, wend       int
	bstep            int
	mode             int
	xchange, ychange int
	xadd, yadd       int
	ymove            int
	done             bool
	blocks           []int // for diagonal lines, we have to store blocks
	curInBlocks      int   // position of cursor within blocks array field above
}

func GetBlockityLines(bm *Blockmap) *BlockityLines {
	return &BlockityLines{
		bm:      bm,
		xmin:    int(bm.header.XMin),
		ymin:    int(bm.header.YMin),
		xblocks: int(bm.header.XBlocks),
		cur:     nil,
		done:    false,
	}
}

func (it *BlockityLines) ClearSeenLines() {
	for i, _ := range it.seenLines {
		it.seenLines[i] = 0
	}
}

// This is for rejectLOS subroutines. Expected to preceed a loop calling
// SetSubcontextFromRow
func (it *BlockityLines) SetContextForReject() {
	it.ClearSeenLines() // start with a clean map
	it.bstep = 1
	it.mode = BLOCK_FOLLOW_AXIS
}

// This is for rejectLOS subroutines. Caller must ensure colBeg < colEnd, we
// don't do checks here.
func (it *BlockityLines) SetSubcontextFromRow(row, colBeg, colEnd int) {
	// You are supposed to call SetContextForReject() first before series of
	// calls to SetSubcontextFromRow
	// Horizontal line
	it.wbeg = colBeg + row*it.xblocks
	it.wend = colEnd + row*it.xblocks
	it.done = false
	it.cur = nil
}

// SetContext initializes iterator to follow blocks intersected by specific line
// It does the computing which blocks the line intersects and thus which
// blocks all the line that it could intersect belong. The algorithm is basically
// the same one Marisa Heit used for computing blockmap
func (it *BlockityLines) SetContext(x1, y1, x2, y2 int) {
	it.ClearSeenLines() // start with a clean map
	if it.blocks == nil {
		it.blocks = make([]int, 0)
	} else {
		it.blocks = it.blocks[:0]
	}
	it.x1 = x1
	it.y1 = y1
	it.dx = x2 - x1
	it.dy = y2 - y1
	it.bx = (x1 - it.xmin) >> BLOCK_BITS
	it.by = (y1 - it.ymin) >> BLOCK_BITS
	it.bx2 = (x2 - it.xmin) >> BLOCK_BITS
	it.by2 = (y2 - it.ymin) >> BLOCK_BITS
	it.done = false
	it.cur = nil
	// pointers to blocklist of the blocks that host starting and
	// ending vertices
	it.wbeg = it.bx + it.by*it.xblocks
	it.wend = it.bx2 + it.by2*it.xblocks

	if it.wbeg == it.wend { // Single block
		it.bstep = 1
		it.mode = BLOCK_FOLLOW_AXIS
	} else if it.by == it.by2 { // Horizontal line
		it.bstep = 1
		it.mode = BLOCK_FOLLOW_AXIS
		if it.bx > it.bx2 {
			// swap beginning and end
			it.wbeg, it.wend = it.wend, it.wbeg
		}
	} else if it.bx == it.bx2 { // Vertical line
		it.bstep = it.xblocks
		it.mode = BLOCK_FOLLOW_AXIS
		if it.by > it.by2 {
			// swap beginning and end
			it.wbeg, it.wend = it.wend, it.wbeg
		}
	} else { // Diagonal line
		// Toughest case, yeah
		it.xchange = Sign(it.dx)
		it.ychange = Sign(it.dy)
		it.ymove = it.ychange * it.xblocks
		adx := Abs(it.dx)
		ady := Abs(it.dy)
		if adx == ady { // 45 degrees
			xb := (x1 - it.xmin) & (BLOCK_WIDTH - 1)
			yb := (y1 - it.ymin) & (BLOCK_WIDTH - 1)
			if it.dx < 0 {
				xb = BLOCK_WIDTH - xb
			}
			if it.dy < 0 {
				yb = BLOCK_WIDTH - yb
			}
			if xb < yb {
				adx--
			}
		}
		if adx >= ady { // X major
			it.mode = BLOCK_INARRAY
			var yadd int
			if it.dy < 0 {
				yadd = -1
			} else {
				yadd = BLOCK_WIDTH
			}
			it.yadd = yadd
			// Now there would be a loop in bblocks.go!CreateBlockmap... ah,
			// fuck trying implementing Python generators in Go, now we simply
			// construct a slice with all the block numbers
			it.curInBlocks = 0
			it.GetBlocksMajorX(&it.blocks)
		} else { // Y major
			it.mode = BLOCK_INARRAY
			var xadd int
			if it.dx < 0 {
				xadd = -1
			} else {
				xadd = BLOCK_WIDTH
			}
			it.xadd = xadd
			// Now there would be a loop in bblocks.go!CreateBlockmap... ah,
			// fuck trying implementing Python generators in Go, now we simply
			// construct a slice with all the block numbers
			it.curInBlocks = 0
			it.GetBlocksMajorY(&it.blocks)
		}
	}
}

func (it *BlockityLines) GetLine() uint16 {
	if it.cur == nil {
		Log.Printf("Incorrect use of BlockityLines - you should have called NextLine() first.")
		return 0
	}
	return *it.cur
}

// nextBlock() is for internal use within the BlockityLines class
func (it *BlockityLines) nextBlock() bool {
	switch it.mode {
	case BLOCK_FOLLOW_AXIS:
		{
			if it.cur != nil {
				it.wbeg += it.bstep
			}
			// This skips zero length blocks _IF_ we hit one prior to this
			for it.wbeg <= it.wend && len(it.bm.blocklist[it.wbeg]) == 0 {
				it.wbeg += it.bstep
			}
			if it.wbeg > it.wend {
				return false
			}
			it.curBlock = it.wbeg
			return len(it.bm.blocklist[it.wbeg]) != 0
		}
	case BLOCK_INARRAY:
		{
			if it.cur == nil {
				if len(it.blocks) == 0 { // no blocks at all
					return false
				}
			} else {
				it.curInBlocks++
			}
			// This skips zero length blocks _IF_ we hit one prior to this
			for it.curInBlocks < len(it.blocks) && len(it.bm.blocklist[it.blocks[it.curInBlocks]]) == 0 {
				it.curInBlocks++
			}
			if it.curInBlocks >= len(it.blocks) {
				return false
			}
			it.wbeg = it.blocks[it.curInBlocks]
			it.curBlock = it.wbeg
		}
	}
	return len(it.bm.blocklist[it.wbeg]) != 0
}

// NextLine returns false when there is nothing left to iterate. After this
// happens, there is no use for iterator anymore until next call to SetContext()
// Note: it automatically skips lines that were already iterated, even if
// from different block, since the last call to SetContext()
func (it *BlockityLines) NextLine() bool {
	if it.done { // Not reusable. Iterator works one direction only.
		return false
	}

	repeat := true
	for repeat {
		if it.cur == nil {
			b := it.nextBlock()
			if b {
				it.curLine = 0
				it.cur = &(it.bm.blocklist[it.curBlock][it.curLine])
			} // else it.cur remains nil, we are done
		} else {
			it.curLine++
			if it.curLine >= len(it.bm.blocklist[it.curBlock]) {
				b := it.nextBlock()
				if b {
					it.curLine = 0
					it.cur = &(it.bm.blocklist[it.curBlock][it.curLine])
				} else {
					it.cur = nil
				}
			} else {
				it.cur = &(it.bm.blocklist[it.curBlock][it.curLine])
			}
		}

		// Skip all lines the user has seen already - repeat this loop until
		// an unseen one found or ran out of lines
		repeat = it.cur != nil && it.markAndRecall(*it.cur)
	}

	it.done = it.cur == nil // done?
	return !it.done
}

// markAndRecall - if line #cur was not marked as seen yet, mark it
// Return true if was already marked (we already gave user this line some time
// before)
func (it *BlockityLines) markAndRecall(cur uint16) bool {
	bte := cur >> 3              // #byte = cur / 8
	bit := uint8(1 << (cur % 8)) // #bit
	retA := it.seenLines[bte] & bit
	if retA == bit {
		return true
	} else {
		it.seenLines[bte] = it.seenLines[bte] | bit
		return false
	}
}

func (it *BlockityLines) GetBlocksMajorX(cum *[]int) {
	for {
		stop := (Scale(it.by<<BLOCK_BITS+it.yadd-(it.y1-it.ymin), it.dx, it.dy) + (it.x1 - it.xmin)) >> BLOCK_BITS
		for it.bx != stop {
			*cum = append(*cum, it.wbeg)
			it.wbeg += it.xchange
			it.bx += it.xchange
		}
		*cum = append(*cum, it.wbeg)
		it.wbeg += it.ymove
		it.by += it.ychange
		if it.by == it.by2 {
			break
		}
	}
	for it.wbeg != it.wend {
		*cum = append(*cum, it.wbeg)
		it.wbeg += it.xchange
	}
	*cum = append(*cum, it.wbeg)
}

func (it *BlockityLines) GetBlocksMajorY(cum *[]int) {
	for {
		stop := (Scale(it.bx<<BLOCK_BITS+it.xadd-(it.x1-it.xmin), it.dy, it.dx) + (it.y1 - it.ymin)) >> BLOCK_BITS
		for it.by != stop {
			*cum = append(*cum, it.wbeg)
			it.wbeg += it.ymove
			it.by += it.ychange
		}
		*cum = append(*cum, it.wbeg)
		it.wbeg += it.xchange
		it.bx += it.xchange
		if it.bx == it.bx2 {
			break
		}
	}
	for it.wbeg != it.wend {
		*cum = append(*cum, it.wbeg)
		it.wbeg += it.ymove
	}
	*cum = append(*cum, it.wbeg)
}
