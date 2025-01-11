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

// rejectLOS
package main

import (
	"fmt"
)

// reject.go grew too big for my liking. All calls descending from reject.go
// (*RejectWork).CheckLos are implemented here + prepareBlockmapForLOS, as it
// setups things used exclusively by code here

type PolyLine struct {
	numPoints int
	lastPoint int
	points    []*IntVertex
}

type LineSet struct {
	lines   []*SolidLine
	loIndex int
	hiIndex int
}

type WorldInfo struct {
	src       *TransLine
	tgt       *TransLine
	solidSet  LineSet
	upperPoly PolyLine
	lowerPoly PolyLine
}

type BlockMapBounds struct {
	lo int
	hi int
}

func (r *RejectWork) initializeWorld(world *WorldInfo, src, tgt *TransLine) {
	world.src = src
	world.tgt = tgt

	world.solidSet.lines = r.testLines
	world.solidSet.loIndex = 0
	world.solidSet.hiIndex = -1

	r.p1 = *(src.start)
	r.p2 = *(src.end)
	r.p3 = *(tgt.start)
	r.p4 = *(tgt.end)

	src.loPoint = &(r.p1)
	src.lo = 0.0
	src.hiPoint = &(r.p2)
	src.hi = 1.0
	tgt.loPoint = &(r.p3)
	tgt.lo = 0.0
	tgt.hiPoint = &(r.p4)
	tgt.hi = 1.0

	lowerPoly := &(world.lowerPoly)
	lowerPoly.points = r.polyPoints[:len(r.solidLines)+2]
	lowerPoly.numPoints = 2
	lowerPoly.lastPoint = -1
	lowerPoly.points[0] = src.hiPoint
	lowerPoly.points[1] = tgt.loPoint

	upperPoly := &(world.upperPoly)
	upperPoly.points = r.polyPoints[len(r.solidLines)+2:]
	upperPoly.numPoints = 2
	upperPoly.lastPoint = -1
	upperPoly.points[0] = tgt.hiPoint
	upperPoly.points[1] = src.loPoint
}

func (p *IntVertex) String() string {
	return fmt.Sprintf("&IntVertex{%d,%d}", p.X, p.Y)
}

func (r *RejectWork) markBlockMap(world *WorldInfo) {
	r.loRow = int(r.blockmap.header.YBlocks)
	r.hiRow = -1

	// Determine boundaries for the BLOCKMAP search
	// VigilantDoomer: bet this can be redone more efficiently eventually
	// UPD April 2023: Actually really hard to do anything about it. The prime
	// culprit is the REPEATED division operation (inside loop) that is
	// unavoidable when a line spawns multiple blocks diagonally.
	r.drawBlockMapLine(world.src.start, world.src.end)
	r.drawBlockMapLine(world.tgt.start, world.tgt.end)
	r.drawBlockMapLine(world.src.start, world.tgt.end)
	r.drawBlockMapLine(world.tgt.start, world.src.end)

}

func (r *RejectWork) prepareBlockmapForLOS() {
	rowCount := int(r.blockmap.header.YBlocks)
	colCount := int(r.blockmap.header.XBlocks)
	r.blockMapBounds = make([]BlockMapBounds, rowCount)
	for row := 0; row < rowCount; row++ {
		r.blockMapBounds[row].lo = colCount
		r.blockMapBounds[row].hi = -1
	}

	// Now I would like to convert the blockmap - a 2D array with linedef
	// indices - to a solidMap, a contiguous array of solidLines indices. This
	// improves performance of future findInterveningLines calls by being
	// friendlier to cache (less trips to RAM)
	r.solidList = make([]SolidmapSlice, len(r.blockmap.blocklist))
	totalEntries := 0
	for i, list := range r.blockmap.blocklist {
		r.solidList[i].offset = totalEntries
		r.solidList[i].length = len(list)
		totalEntries += r.solidList[i].length
	}
	r.solidMap = make([]uint16, totalEntries)
	i := 0
	for _, list := range r.blockmap.blocklist {
		for _, v := range list {
			r.solidMap[i] = r.indexToSolid[v]
			i++
		}
	}
}

/*
// Pre-optimization original
func (r *RejectWork) drawBlockMapLine(p1, p2 *IntVertex) {
	x0 := p1.X - int(r.blockmap.header.XMin)
	y0 := p1.Y - int(r.blockmap.header.YMin)
	x1 := p2.X - int(r.blockmap.header.XMin)
	y1 := p2.Y - int(r.blockmap.header.YMin)

	startX := x0 >> BLOCK_BITS
	startY := y0 >> BLOCK_BITS
	endX := x1 >> BLOCK_BITS
	endY := y1 >> BLOCK_BITS

	if startY < r.loRow {
		r.loRow = startY
	}
	if startY > r.hiRow {
		r.hiRow = startY
	}

	if endY < r.loRow {
		r.loRow = endY
	}
	if endY > r.hiRow {
		r.hiRow = endY
	}

	r.updateRow(startX, startY)

	if startX == endX {

		if startY != endY { // vertical line
			var dy int
			if endY > startY {
				dy = 1
			} else {
				dy = -1
			}
			b := true
			for b {
				startY += dy
				r.updateRow(startX, startY)
				b = startY != endY
			}
		}

	} else {

		if startY != endY { // diagonal line

			var dy int
			if endY > startY {
				dy = 1
			} else {
				dy = -1
			}

			// Calculate the pre-scaled values to be used in the for loop
			deltaX := ((x1 - x0) << BLOCK_BITS) * dy
			deltaY := (y1 - y0) << BLOCK_BITS
			nextX := x0 * (y1 - y0)

			// Figure out where the 1st row ends
			switch dy {
			case -1:
				{
					nextX += (startY<<BLOCK_BITS - y0) * (x1 - x0)
				}
			case 1:
				{
					nextX += (startY<<BLOCK_BITS + BLOCK_WIDTH - y0) * (x1 - x0)
				}
			}

			lastX := nextX / deltaY
			r.updateRow(lastX, startY)

			// Now do the rest using integer math - each row is a delta Y of 128
			// bound := &(r.blockMapBounds[startY]) // doesn't seem to be used now that I've converted away from pointer arithmetic
			curY := startY
			if x0 < x1 {
				for true {
					// Do the next row
					curY = curY + dy
					bound := &(r.blockMapBounds[curY])
					if lastX < bound.lo {
						bound.lo = lastX
					}
					// Stop before we overshoot endX
					if curY == endY {
						break
					}
					nextX += deltaX
					lastX = nextX / deltaY
					if lastX > bound.hi {
						bound.hi = lastX
					}
				}
			} else {
				for true {
					// Do the next row
					curY = curY + dy
					bound := &(r.blockMapBounds[curY])
					if lastX > bound.hi {
						bound.hi = lastX
					}
					// Stop before we overshoot endX
					if curY == endY {
						break
					}
					nextX += deltaX
					lastX = nextX / deltaY
					if lastX < bound.lo {
						bound.lo = lastX
					}
				}
			}
		}

		r.updateRow(endX, endY)
	}
}*/

func (r *RejectWork) drawBlockMapLine(p1, p2 *IntVertex) {
	x0 := p1.X - int(r.blockmap.header.XMin)
	y0 := p1.Y - int(r.blockmap.header.YMin)
	x1 := p2.X - int(r.blockmap.header.XMin)
	y1 := p2.Y - int(r.blockmap.header.YMin)

	startX := x0 >> BLOCK_BITS
	startY := y0 >> BLOCK_BITS
	endX := x1 >> BLOCK_BITS
	endY := y1 >> BLOCK_BITS

	if startY < r.loRow {
		r.loRow = startY
	}
	if startY > r.hiRow {
		r.hiRow = startY
	}

	if endY < r.loRow {
		r.loRow = endY
	}
	if endY > r.hiRow {
		r.hiRow = endY
	}

	r.updateRow(startX, startY)

	if startX == endX {

		if startY != endY { // vertical line

			// unrolling to get rid of dy makes this function significantly
			// longer, and it is copied over by codegen because it's a
			// RejectWork method
			// --VigilantDoomer
			var dy int
			if endY > startY {
				dy = 1
			} else {
				dy = -1
			}
			b := true
			for b {
				startY += dy
				r.updateRow(startX, startY)
				b = startY != endY
			}

		}

	} else {

		if startY != endY { // diagonal line

			// Calculate the pre-scaled values to be used in the for loop
			var deltaX int
			deltaY := (y1 - y0) << BLOCK_BITS
			nextX := x0 * (y1 - y0)

			var dy int
			if endY > startY {
				// Figure out where the 1st row ends
				nextX += (startY<<BLOCK_BITS + BLOCK_WIDTH - y0) * (x1 - x0)
				deltaX = ((x1 - x0) << BLOCK_BITS)
				dy = 1
			} else {
				// Figure out where the 1st row ends
				nextX += (startY<<BLOCK_BITS - y0) * (x1 - x0)
				deltaX = ((x0 - x1) << BLOCK_BITS)
				dy = -1
			}

			lastX := nextX / deltaY
			r.updateRow(lastX, startY)

			// Now do the rest using integer math - each row is a delta Y of 128
			// bound := &(r.blockMapBounds[startY]) // doesn't seem to be used now that I've converted away from pointer arithmetic
			curY := startY
			if x0 < x1 {
				for true {
					// Do the next row
					curY = curY + dy
					bound := &(r.blockMapBounds[curY])
					if lastX < bound.lo {
						bound.lo = lastX
					}
					// Stop before we overshoot endX
					if curY == endY {
						break
					}
					nextX += deltaX
					lastX = nextX / deltaY
					if lastX > bound.hi {
						bound.hi = lastX
					}
				}
			} else {
				for true {
					// Do the next row
					curY = curY + dy
					bound := &(r.blockMapBounds[curY])
					if lastX > bound.hi {
						bound.hi = lastX
					}
					// Stop before we overshoot endX
					if curY == endY {
						break
					}
					nextX += deltaX
					lastX = nextX / deltaY
					if lastX < bound.lo {
						bound.lo = lastX
					}
				}
			}
		}

		r.updateRow(endX, endY)
	}
}

func (r *RejectWork) updateRow(col, row int) {
	bound := &(r.blockMapBounds[row])
	if col < bound.lo {
		bound.lo = col
	}
	if col > bound.hi {
		bound.hi = col
	}
}

func (r *RejectWork) findInterveningLines(set *LineSet) bool {
	// Collect all lines that have been bounded (in markBlockMap) into set
	lineCount := 0

	// Remove "seen" status from lines
	for i, _ := range r.seenLines {
		r.seenLines[i] = 0
	}

	// March 2023: now using a solidMap, which stores indices in solidLines
	// array, not the linedef indices, and is also one contiguous array instead
	// of array of arrays. The speedgain is awesome.

	xblocks := int(r.blockmap.header.XBlocks)
	for row := r.loRow; row <= r.hiRow; row++ {
		bound := &(r.blockMapBounds[row])
		blocklistIdx := row*xblocks + bound.lo
		for col := bound.lo; col <= bound.hi; col++ {
			obeg := r.solidList[blocklistIdx].offset
			oend := obeg + r.solidList[blocklistIdx].length
			for _, line := range r.solidMap[obeg:oend] {
				if !r.markAndRecall(line) {
					set.lines[lineCount] = &(r.solidLines[line])
					lineCount++
				}
			}
			blocklistIdx++ // per col
		}
		bound.lo = int(r.blockmap.header.XBlocks)
		bound.hi = -1
	}

	set.loIndex = 0
	set.hiIndex = lineCount - 1
	set.lines[lineCount] = nil

	if lineCount > 0 {
		return true
	}

	return false
}

// Mark line #cur as seen and return true if it was seen already
func (r *RejectWork) markAndRecall(cur uint16) bool {
	bte := cur >> 3              // #byte = cur / 8
	bit := uint8(1 << (cur % 8)) // #bit
	retA := r.seenLines[bte] & bit
	if retA == bit {
		return true
	} else {
		r.seenLines[bte] = r.seenLines[bte] | bit
		return false
	}
}

func TrimSetBounds(set *LineSet) bool {

	if set.loIndex >= set.hiIndex {
		return false
	}

	for set.lines[set.loIndex].ignore {
		set.loIndex++
		if set.loIndex >= set.hiIndex {
			return false
		}
	}

	for set.lines[set.hiIndex].ignore {
		set.hiIndex--
	}

	return true
}

type RotatePointData struct {
	X  int
	Y  int
	DX int
	DY int
}

func (r *RotatePointData) RotatePoint(p *IntVertex, x, y int) {

	p.X = r.DX*(x-r.X) + r.DY*(y-r.Y)
	p.Y = r.DX*(y-r.Y) - r.DY*(x-r.X)
}

//go:nosplit
func TrimLines(src, tgt *TransLine, set *LineSet) int {
	var loY, hiY, loX, hiX int

	// LOSGetBounds inlined manually (because compiler wouldn't inline this)
	// (gcc didn't inline this in ZokumBSP source code either, so Go is not
	// behind in any way)
	// Might uninline back. Go switched the strategy from branching to
	// conditional moves after manual inlining, and while gcc also uses
	// conditional moves, on my computer branch conditionals version did better
	// The removal of call overhead cancels it out, of course, and different
	// machines may exhibit different performance, but generally I think this
	// stuff is somehow indeed predictable, may have something to do with
	// src and tgt going in opposite direction, also the iteration enumerating
	// src against different tgts, preserving much of conditions between all
	// work on the same src?
	// --------------- LOSGetBounds START
	ss, se, ts, te := src.start, src.end, tgt.start, tgt.end
	if ss.Y < se.Y {
		if ts.Y < te.Y {
			if ss.Y < ts.Y {
				loY = ss.Y
			} else {
				loY = ts.Y
			}
			if se.Y > te.Y {
				hiY = se.Y
			} else {
				hiY = te.Y
			}
		} else {
			if ss.Y < te.Y {
				loY = ss.Y
			} else {
				loY = te.Y
			}
			if se.Y > ts.Y {
				hiY = se.Y
			} else {
				hiY = ts.Y
			}
		}
	} else {
		if ts.Y < te.Y {
			if se.Y < ts.Y {
				loY = se.Y
			} else {
				loY = ts.Y
			}
			if ss.Y > te.Y {
				hiY = ss.Y
			} else {
				hiY = te.Y
			}
		} else {
			if se.Y < te.Y {
				loY = se.Y
			} else {
				loY = te.Y
			}
			if ss.Y > ts.Y {
				hiY = ss.Y
			} else {
				hiY = ts.Y
			}
		}
	}
	// Now all the same with x coords
	if ss.X < se.X {
		if ts.X < te.X {
			if ss.X < ts.X {
				loX = ss.X
			} else {
				loX = ts.X
			}
			if se.X > te.X {
				hiX = se.X
			} else {
				hiX = te.X
			}
		} else {
			if ss.X < te.X {
				loX = ss.X
			} else {
				loX = te.X
			}
			if se.X > ts.X {
				hiX = se.X
			} else {
				hiX = ts.X
			}
		}
	} else {
		if ts.X < te.X {
			if se.X < ts.X {
				loX = se.X
			} else {
				loX = ts.X
			}
			if ss.X > te.X {
				hiX = ss.X
			} else {
				hiX = te.X
			}
		} else {
			if se.X < te.X {
				loX = se.X
			} else {
				loX = te.X
			}
			if ss.X > ts.X {
				hiX = ss.X
			} else {
				hiX = ts.X
			}
		}
	}
	// --------------- LOSGetBounds END

	// Set up data used by RotatePoint
	rp := RotatePointData{
		X:  src.start.X,
		Y:  src.start.Y,
		DX: tgt.end.X - src.start.X,
		DY: tgt.end.Y - src.start.Y,
	}

	// Variables for a rotated bounding box
	var p1, p2, p3 IntVertex
	rp.RotatePoint(&p1, src.end.X, src.end.Y)
	rp.RotatePoint(&p2, tgt.start.X, tgt.start.Y)
	rp.RotatePoint(&p3, tgt.end.X, tgt.end.Y)

	var minX, maxX, minY int
	if p1.X < 0 {
		minX = 0
	} else {
		minX = p1.X
	}
	if p2.X < p3.X { // TODO investigate
		maxX = p2.X // seriously! this is how zennode did it (see zenreject.cpp TrimLines). wonder what the hell is going on...
	} else {
		maxX = p3.X // no, really. It might be legit, but then it's probably worth documenting what those are maxes of
	}
	if p1.Y < p2.Y {
		minY = p1.Y
	} else {
		minY = p2.Y
	}

	linesLeft := 0

	checkBlock := ((minX <= maxX) && ((rp.DX != 0) || (rp.DY != 0)))

	for _, line := range set.lines[set.loIndex : set.hiIndex+1] {

		line.ignore = true

		// Eliminate any lines completely outside the axis aligned bounding box
		if line.start.Y <= loY {
			if line.end.Y <= loY {
				continue
			}
		} else if line.start.Y >= hiY {
			if line.end.Y >= hiY {
				continue
			}
		}
		if line.start.X >= hiX {
			if line.end.X >= hiX {

				continue
			}
		} else if line.start.X <= loX {
			if line.end.X <= loX {
				continue
			}
		}

		// Stop if we find a single line that obstructs the view completely
		if checkBlock {
			var start, end IntVertex
			start.Y = rp.DX*(line.start.Y-rp.Y) - rp.DY*(line.start.X-rp.X)
			if (start.Y >= 0) || (start.Y <= minY) {
				end.Y = rp.DX*(line.end.Y-rp.Y) - rp.DY*(line.end.X-rp.X)
				if ((end.Y <= minY) && (start.Y >= 0)) || ((end.Y >= 0) && (start.Y <= minY)) {
					start.X = rp.DX*(line.start.X-rp.X) + rp.DY*(line.start.Y-rp.Y)
					if (start.X >= minX) && (start.X <= maxX) {
						end.X = rp.DX*(line.end.X-rp.X) + rp.DY*(line.end.Y-rp.Y)
						if (end.X >= minX) && (end.X <= maxX) {
							return -1
						}
					}
					// Use the new information and see if line is outside the bounding box
				} else if ((end.Y >= 0) && (start.Y >= 0)) || ((end.Y <= minY) && (start.Y <= minY)) {
					continue
				}
			}
		}

		line.ignore = false
		linesLeft++

	}

	if linesLeft == 0 {
		return 0
	}

	if ((src.DX != 0) && (src.DY != 0)) || ((tgt.DX != 0) && (tgt.DY != 0)) {

		// Eliminate lines that touch the src/tgt lines but are not in view
		for _, line := range set.lines[set.loIndex : set.hiIndex+1] {
			if line.ignore {
				continue
			}
			y := 1
			// April 2023: solid lines' vertices are stored in SolidLine
			// structure directly, not as pointers (friendlier to CPU cache)
			if (line.start == *src.start) || (line.start == *src.end) {
				y = src.DX*(line.end.Y-src.start.Y) - src.DY*(line.end.X-src.start.X)
			} else if (line.end == *src.start) || (line.end == *src.end) {
				y = src.DX*(line.start.Y-src.start.Y) - src.DY*(line.start.X-src.start.X)
			} else if (line.start == *tgt.start) || (line.start == *tgt.end) {
				y = tgt.DX*(line.end.Y-tgt.start.Y) - tgt.DY*(line.end.X-tgt.start.X)
			} else if (line.end == *tgt.start) || (line.end == *tgt.end) {
				y = tgt.DX*(line.start.Y-tgt.start.Y) - tgt.DY*(line.start.X-tgt.start.X)
			}
			if y <= 0 {
				line.ignore = true
				linesLeft--
			}
		}
	}

	TrimSetBounds(set)

	return linesLeft
}

func FindObstacles(world *WorldInfo) bool {
	// VigilantDoomer: looks like zennode's author didn't finish this - it
	// always returns false
	// 2025 -- so eliminate dead code

	/*
		if world.solidSet.hiIndex < world.solidSet.loIndex {
			return false
		}

		// If we have an unbroken line between src & tgt there is a direct LOS
		if world.upperPoly.numPoints == 2 {
			return false
		}
		if world.lowerPoly.numPoints == 2 {
			return false
		}
	*/
	// To be absolutely correct, we should create a list of obstacles
	// (ie: connected lineDefs completely enclosed by upperPoly & lowerPoly)
	// and see if any of them completely block the LOS

	return false
}

// Find out which side of the poly-line the line is on
//
//	Return Values:
//	    1 - above (not completely below) the poly-line
//	    0 - intersects the poly-line
//	   -1 - below the poly-line (one or both end-points may touch the poly-line)
//	   -2 - can't tell start this segment
func Intersects(p1, p2, t1, t2 *IntVertex) int {
	var DX, DY, y1, y2 int

	// Rotate & translate using p1->p2 as the +X-axis
	DX = p2.X - p1.X
	DY = p2.Y - p1.Y

	y1 = DX*(t1.Y-p1.Y) - DY*(t1.X-p1.X)
	y2 = DX*(t2.Y-p1.Y) - DY*(t2.X-p1.X)

	// Eliminate the 2 easy cases (t1 & t2 both above or below the x-axis)
	if (y1 > 0) && (y2 > 0) {
		return 1
	}
	if (y1 <= 0) && (y2 <= 0) {
		return -1
	}
	// t1->t2 crosses poly-Line segment (or one point touches it and the other is above it)

	// Rotate & translate using t1->t2 as the +X-axis
	DX = t2.X - t1.X
	DY = t2.Y - t1.Y

	y1 = DX*(p1.Y-t1.Y) - DY*(p1.X-t1.X)
	y2 = DX*(p2.Y-t1.Y) - DY*(p2.X-t1.X)

	// Eliminate the 2 easy cases (p1 & p2 both above or below the x-axis)
	if (y1 > 0) && (y2 > 0) {
		return -2
	}
	if (y1 < 0) && (y2 < 0) {
		return -2
	}

	return 0
}

func FindSide(line *SolidLine, poly *PolyLine) int {

	completelyBelow := true
	for i := 0; i < poly.numPoints-1; i++ {
		p1 := poly.points[i]
		p2 := poly.points[i+1]
		switch Intersects(p1, p2, &line.start, &line.end) {
		case -1:
			{
			}
		case 0:
			{
				return 0
			}
		case -2:
			{
				completelyBelow = false
			}
		case 1:
			{
				completelyBelow = false
			}
		}
	}
	if completelyBelow {
		return -1
	}
	return 1
}

func AddToPolyLine(poly *PolyLine, line *SolidLine) {

	var DX, DY, y1, y2 int

	y1 = 0

	// Find new index start from the 'left'
	var i int
	for i = 0; i < poly.numPoints-1; i++ {
		p1 := poly.points[i]
		p2 := poly.points[i+1]
		DX = p2.X - p1.X
		DY = p2.Y - p1.Y

		y1 = DX*(line.start.Y-p1.Y) - DY*(line.start.X-p1.X)
		y2 = DX*(line.end.Y-p1.Y) - DY*(line.end.X-p1.X)
		if (y1 > 0) != (y2 > 0) {
			break
		}
	}
	i += 1

	// Find new index start from the 'right'
	var j int
	for j = poly.numPoints - 1; j > i; j-- {
		p1 := poly.points[j-1]
		p2 := poly.points[j]
		DX = p2.X - p1.X
		DY = p2.Y - p1.Y

		y1 = DX*(line.start.Y-p1.Y) - DY*(line.start.X-p1.X)
		y2 = DX*(line.end.Y-p1.Y) - DY*(line.end.X-p1.X)
		if (y1 > 0) != (y2 > 0) {
			break
		}
	}

	ptsRemoved := j - i
	toCopy := poly.numPoints - j
	if toCopy > 0 {
		copy(poly.points[i+1:], poly.points[j:j+toCopy])
	}
	poly.numPoints += 1 - ptsRemoved

	if y1 > 0 {
		poly.points[i] = &line.start
	} else {
		poly.points[i] = &line.end
	}
	poly.lastPoint = i
}

func PolyLinesCross(upper, lower *PolyLine) bool {
	foundAbove := false
	ambiguous := false
	last := 0
	max := upper.numPoints - 1
	if upper.lastPoint > 0 { // Zennode never used bound checks to see that:
		// upper.lastPoint == 0 happens often enough after CorrectForNewStart call
		// if upper.lastPoint != -1 { // this is how the condition was defined in zennode (WRONG!!!)
		max = 2
		last = upper.lastPoint - 1
	}
	for i := 0; i < max; i++ {
		// now we would have out of bounds access (which is what happens in
		// zennode, actually) when upper.lastPoint had been 0 prior to this
		// function call (would now became -1 if I didn't modify check on it
		// above). Go used to crash here (like everyone should) when code was
		// just ported from C++ zennode source code
		p1 := upper.points[last+i]
		p2 := upper.points[last+i+1]
		for j := 0; j < lower.numPoints-1; j++ {
			p3 := lower.points[j]
			p4 := lower.points[j+1]
			switch Intersects(p1, p2, p3, p4) {
			case 1:
				{
					foundAbove = true
				}
			case 0:
				{
					return true
				}
			case -2:
				{
					ambiguous = true
				}
			}
		}
	}

	if foundAbove {
		return false
	}

	if ambiguous {
		p1 := upper.points[0]
		p2 := upper.points[upper.numPoints-1]
		DX := p2.X - p1.X
		DY := p2.Y - p1.Y
		for i := 1; i < lower.numPoints-1; i++ {
			testPoint := lower.points[i]
			if DX*(testPoint.Y-p1.Y)-DY*(testPoint.X-p1.X) < 0 {
				return true
			}
		}
	}

	return false
}

func CorrectForNewStart(poly *PolyLine) bool {
	p0 := poly.points[0]
	for i := poly.numPoints - 1; i > 1; i-- {
		p1 := poly.points[i]
		p2 := poly.points[i-1]
		dx := p1.X - p0.X
		dy := p1.Y - p0.Y
		y := dx*(p2.Y-p0.Y) - dy*(p2.X-p0.X)
		if y < 0 {
			poly.points[i-1] = p0
			poly.points = poly.points[i-1:]
			poly.numPoints -= i - 1
			poly.lastPoint -= i - 1
			// yes this does make poly.lastPoint == 0 more often than it doesn't
			return true
		}
	}
	return false
}

func CorrectForNewEnd(poly *PolyLine) bool {
	p0 := poly.points[poly.numPoints-1]
	for i := 0; i < poly.numPoints-2; i++ {
		p1 := poly.points[i]
		p2 := poly.points[i+1]
		dx := p0.X - p1.X
		dy := p0.Y - p1.Y
		y := dx*(p2.Y-p1.Y) - dy*(p2.X-p1.X)
		if y < 0 {
			poly.points[i+1] = p0
			poly.numPoints -= poly.numPoints - i - 2
			return true
		}
	}
	return false
}

func AdjustEndPoints(left, right *TransLine, upper, lower *PolyLine) bool {

	if upper.lastPoint == -1 {
		return true
	}
	test := upper.points[upper.lastPoint]

	var dx, dy, y int
	changed := false

	dx = test.X - left.hiPoint.X
	dy = test.Y - left.hiPoint.Y
	y = dx*(right.hiPoint.Y-left.hiPoint.Y) -
		dy*(right.hiPoint.X-left.hiPoint.X)
	if y > 0 {
		num := (right.start.Y-left.hiPoint.Y)*dx -
			(right.start.X-left.hiPoint.X)*dy
		det := right.DX*dy - right.DY*dx
		t := float64(num) / float64(det)
		if t <= right.lo {
			return false
		}
		if t < right.hi {
			right.hi = t
			right.hiPoint.X = right.start.X + int(t*float64(right.DX))
			right.hiPoint.Y = right.start.Y + int(t*float64(right.DY))
			changed = changed || CorrectForNewStart(upper)
		}
	}

	dx = test.X - right.loPoint.X
	dy = test.Y - right.loPoint.Y
	y = dx*(left.loPoint.Y-right.loPoint.Y) -
		dy*(left.loPoint.X-right.loPoint.X)
	if y < 0 {
		num := (left.start.Y-right.loPoint.Y)*dx -
			(left.start.X-right.loPoint.X)*dy
		det := left.DX*dy - left.DY*dx
		t := float64(num) / float64(det)
		if t >= left.hi {
			return false
		}
		if t > left.lo {
			left.lo = t
			left.loPoint.X = left.start.X + int(t*float64(left.DX))
			left.loPoint.Y = left.start.Y + int(t*float64(left.DY))
			changed = changed || CorrectForNewEnd(upper)
		}
	}

	if changed && PolyLinesCross(upper, lower) {
		return false
	}

	return true
}

func FindPolyLines(world *WorldInfo) bool {
	upperPoly := &world.upperPoly
	lowerPoly := &world.lowerPoly

	set := &world.solidSet

	for true {

		done := true
		stray := false

		for _, line := range set.lines[set.loIndex : set.hiIndex+1] {

			if line.ignore {
				continue
			}

			switch FindSide(line, lowerPoly) {

			case 1: // Completely above the lower/right poly-Line
				switch FindSide(line, upperPoly) {

				case 1: // Line is between the two poly-lines
					stray = true

				case 0: // Intersects the upper/left poly-Line
					if stray {
						done = false
					}
					AddToPolyLine(upperPoly, line)
					if (lowerPoly.numPoints > 2) && PolyLinesCross(upperPoly, lowerPoly) {
						return false
					}
					if !AdjustEndPoints(world.src, world.tgt, upperPoly, lowerPoly) {
						return false
					}
					line.ignore = true
				case -1: // Completely above the upper/left poly-line
					line.ignore = true

				}

			case 0: // Intersects the lower/right poly-Line
				if stray {
					done = false
				}
				AddToPolyLine(lowerPoly, line)
				if PolyLinesCross(lowerPoly, upperPoly) {
					return false
				}
				if !AdjustEndPoints(world.tgt, world.src, lowerPoly, upperPoly) {
					return false
				}
				line.ignore = true
			case -1: // Completely below the lower/right poly-Line
				line.ignore = true

			}
		}

		if done {

			break
		}

		TrimSetBounds(set)
	}

	return true
}
