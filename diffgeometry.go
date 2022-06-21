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

// diffgeometry
package main

import (
	"fmt"
	"math"
	"sort"
)

// picknode.go grew very big after I implemented partition length evaluation
// without void segments, so I had to split all the gory differential geometry
// (and the like) from there into this file.

func (w *NodesWork) ComputeNonVoid(part *NodeSeg) NonVoidPerAlias {
	// Chapter I
	// To be able to tell void space from sector space, we need to prolong the
	// partition line both ways until it hits level boundaries. Had we prolonged
	// it only to the node boundaries, we wouldn't know if the leftmost(uppermost)
	// vertice of the line starts in void space or sector space

	blXMin := int(w.solidMap.header.XMin)
	blXMax := w.solidMap.XMax
	blYMin := int(w.solidMap.header.YMin)
	blYMax := w.solidMap.YMax

	// Let's see if we can use simple cases for orthogonal partition
	partSegCoords := part.toVertexPairC()
	var c IntersectionContext
	c.psx = partSegCoords.StartVertex.X
	c.psy = partSegCoords.StartVertex.Y
	c.pex = partSegCoords.EndVertex.X
	c.pey = partSegCoords.EndVertex.Y
	c.pdx = c.pex - c.psx
	c.pdy = c.pey - c.psy

	partStart, partEnd := PartitionInBoundary(part, &c, blXMax, blYMax, blXMin,
		blYMin, partSegCoords)
	if partStart == nil || partEnd == nil {
		// Damnation!
		return NonVoidPerAlias{
			success: false,
		}
	}

	partStart.left = true // as if was a solid line looking left (to the right is void; beyond map boundaries)
	partEnd.left = false  // as if was a solid line looking right (to the left is void; beyond map boundaries)

	// --------------
	// Test case: #1
	// linedef to test on: 2861
	// Comments: seems I didn't even get line correctly
	// Reason: wrong computation of solid space (we got VOID intervals instead of SOLID intervals!!!)
	// Error output:
	// More dropouts! -1 -1 ; [(4416;5392)-(4424;5395)]; [(4472;5416)-(4488;5424)]; [(4992;5648)-(5126;5707)]; [(6306;6232)-(6312;6234)]; [(6544;6337)-(6704;6408)]; [(7064;6568)-(7340;6691)]; [(7696;6849)-(7800;6896)] [RIGHT(3616,5036)-RIGHT(4128,5264)]
	// --------------
	// Test case: #2
	// linedef to test on: 98
	// Comments: axis-aligned line (horizontal), I've got every vertice value correctly
	// Reason: fluger wrong
	// Error output:
	// Sanity check failed! Evaluated partition line (5112,6704)-(5008,6704) doesn't consistently go in/out of the void when crossing solid lines. We may have missed some solid line or the level contains problems. [,LEFT(32,6704),LEFT(3832,6704),RIGHT(3888,6704),LEFT(4000,6704),RIGHT(4376,6704),LEFT(4388,6704),RIGHT(4388,6704),LEFT(4400,6704),RIGHT(4496,6704),LEFT(4528,6704),RIGHT(4624,6704),LEFT(4656,6704),RIGHT(4656,6704),LEFT(4784,6704),RIGHT(4888,6704),LEFT(4904,6704),RIGHT(5008,6704),LEFT(5112,6704),RIGHT(5112,6704),LEFT(5152,6704),RIGHT(5152,6704),LEFT(5600,6704),RIGHT(5600,6704),LEFT(5640,6704),RIGHT(5640,6704),LEFT(5664,6704),RIGHT(5664,6704),LEFT(5792,6704),RIGHT(5792,6704),LEFT(5832,6704),RIGHT(5832,6704),LEFT(5836,6704),RIGHT(5876,6704),LEFT(5876,6704),RIGHT(5880,6704),LEFT(5880,6704),LEFT(5920,6704),LEFT(6064,6704),RIGHT(6544,6704),LEFT(6576,6704),RIGHT(6576,6704),LEFT(6672,6704),RIGHT(6672,6704),LEFT(6688,6704),RIGHT(7056,6704),LEFT(7328,6704),RIGHT(7456,6704),LEFT(7456,6704),RIGHT(7496,6704),LEFT(7496,6704),LEFT(7500,6704),RIGHT(7540,6704),LEFT(7540,6704),RIGHT(7544,6704),LEFT(7544,6704),LEFT(7584,6704),RIGHT(9512,6704)]
	// --------------
	// Test case: #3
	// linedef to test on: 82
	// Comments: diagonal line, got every vertice value correctly, but wrong direction of vertices
	// Comments: intersects line 716 ~ (48,11856)-(48,12368) must yield RIGHT (not LEFT)
	// Reason: fluger wrong
	// Error output:
	// Sanity check failed! Evaluated partition line (6496,5252)-(6504,5244) doesn't consistently go in/out of the void when crossing solid lines. We may have missed some solid line or the level contains problems. [,LEFT(32,11716),LEFT(48,11700),LEFT(116,11632),RIGHT(3572,8176),RIGHT(3656,8092),RIGHT(3668,8080),RIGHT(3672,8076),LEFT(4144,7604),RIGHT(4368,7380),LEFT(4400,7348),RIGHT(4624,7124),LEFT(4656,7092),LEFT(5044,6704),RIGHT(5207,6540),RIGHT(5904,5844),RIGHT(6228,5520),LEFT(6512,5236),RIGHT(6522,5226),LEFT(9292,2456),RIGHT(9444,2304)]
	// --------------

	// Chapter II
	// We use a blockmap to reduce the number of solid lines we need to check for
	// intersecting our partition
	pts := CollinearOrientedVertices(make([]OrientedVertex, 1))
	pts[0] = *partStart
	if w.blockity == nil {
		w.blockity = GetBlockityLines(w.solidMap)
	}
	iter := w.blockity
	// Note: the coords start at bounding box edges - we need to follow the line
	// through entire map to know for sure when it is in the void and when it
	// is not
	iter.SetContext(int(partStart.v.X), int(partStart.v.Y), int(partEnd.v.X), int(partEnd.v.Y))
	for iter.NextLine() {
		aline := iter.GetLine()
		lsx, lsy, lex, ley := w.lines.GetAllXY(aline)
		c.lsx = Number(lsx)
		c.lsy = Number(lsy)
		c.lex = Number(lex)
		c.ley = Number(ley)
		if c.lsx == c.lex && c.lsy == c.ley { // skip zero-length lines
			continue
		}
		pt1, pt2 := c.getIntersectionOrIndicence()
		if pt1 != nil {
			if pt2 != nil {
				// Incidence - need to add two points looking at each other,
				// between them we have non-void 1-D space
				var ov1, ov2 OrientedVertex
				if VertexPairCOrdering(pt1, pt2, false) {
					ov1.v = pt1
					ov1.left = false
					ov2.v = pt2
					ov2.left = true
				} else {
					ov1.v = pt2
					ov1.left = false
					ov2.v = pt1
					ov2.left = true
				}
				pts = append(pts, ov1)
				pts = append(pts, ov2)
			} else { // pt2 == nil
				// Intersection
				ov := OrientedVertex{
					v: pt1,
					left: IsClockwiseTriangle(&NodeVertex{X: c.lsx, Y: c.lsy},
						&NodeVertex{X: c.lex, Y: c.ley}, partStart.v),
				}
				pts = append(pts, ov)
			}
		}
	}

	pts = append(pts, *partEnd)

	// Chapter III
	// Sort all these things, so the vertexes indicate where partition line
	// candidate enters the map and where it goes to the void outside
	sort.Stable(pts)
	if pts[0] != *partStart || pts[len(pts)-1] != *partEnd {
		Log.Verbose(2, "Failed to produce a solid hits array %t %t (%d, %d) != (%d, %d).\n",
			pts[0] != *partStart, pts[len(pts)-1] != *partEnd,
			pts[len(pts)-1].v.X, pts[len(pts)-1].v.Y,
			partEnd.v.X, partEnd.v.Y)
		// Bail out, damn it
		return NonVoidPerAlias{
			success: false,
		}
	}

	// Now we deal with having multiple directions (from different lines)
	// specified for a single map vertex. This is still not doing its best,
	// but I will have to release it in this state, as I wasted enough days
	// (weeks?!) trying to figure out the infallible approach
	ptsOld := append(CollinearOrientedVertices([]OrientedVertex{}), pts...)
	if !pts.Coalesce() {
		// Doesn't happen anymore
		Log.Verbose(2, "Coalesce fail at %s\n", ptsOld.toString())
		return NonVoidPerAlias{
			success: false,
		}
	}

	// Sanity check: every vertex flips the direction, with the exception
	// of bordering vertexes, they might be next to vertexes of the same
	// direction, because solid level usually starts some distance away (that is,
	// we have void near borders)
	fluger := true // pts[0].left
	for i := 2; i < len(pts)-2; i++ {
		fluger = fluger && (pts[i].left != pts[i-1].left)
	}
	if !fluger {
		// Output warning, but persist nonetheless (instead of failing), unless
		// PersistThroughInsanity was set to false
		// Log.Verbose(2, "Sanity check failed! Evaluated partition line (%d,%d)-(%d,%d) doesn't consistently go in/out of the void when crossing solid lines. %s\nOld content: %s",
		// part.psx, part.psy, part.pex, part.pey, pts.toString(), ptsOld.toString())
		Log.Verbose(2, "Sanity check failed! Evaluated partition line (%d,%d)-(%d,%d) doesn't consistently go in/out of the void when crossing solid lines. %s\n",
			part.psx, part.psy, part.pex, part.pey, pts.toString())
		if !config.PersistThroughInsanity { // reference to global: config
			return NonVoidPerAlias{
				success: false,
			}
		}
	}

	// Either vertice sequence alternated directions as it should have or we
	// chose to "persist through insanity", but we are going to compile all
	// non-void segments now!

	// Non-void segments are formed by vertices that point towards each other,
	// with regards to sorting, that is:
	// (startVertex; endVertex) = (right-oriented vertex; left-oriented vertex)
	// So let's build an array of all non-void segments, which is why we were
	// called in first place!
	nonVoid := make([]VertexPairC, 0)
	for i := 1; i < len(pts); i++ {
		if pts[i-1].left || !pts[i].left {
			continue
		}
		dx := float64(pts[i].v.X - pts[i-1].v.X)
		dy := float64(pts[i].v.Y - pts[i-1].v.Y)
		nonVoid = append(nonVoid, VertexPairC{
			StartVertex: pts[i-1].v,
			EndVertex:   pts[i].v,
			len:         Number(math.Sqrt(dx*dx + dy*dy)),
		})
	}

	return NonVoidPerAlias{
		data: nonVoid,
		// line should never be completely in the void; anyway, if we failed
		// to produce an array, we let cache know our inability to produce it
		// for this input so they don't call us on it again
		success:       len(nonVoid) > 0,
		c:             c,
		partSegCoords: partSegCoords,
		original:      ptsOld,
	}
}

func PartitionInBoundary(part *NodeSeg, c *IntersectionContext,
	blXMax, blYMax, blXMin, blYMin int, partSegCoords VertexPairC) (*OrientedVertex, *OrientedVertex) {

	var partStart, partEnd OrientedVertex
	partStart.v = new(NodeVertex)
	partEnd.v = new(NodeVertex)
	if part.pdx == 0 { // vertical line
		// Y coords: starts at the top, ends at the bottom of blockmap box
		// X coord is taken from partition line
		partStart.v.Y = Number(blYMax)
		partStart.v.X = partSegCoords.StartVertex.X
		partEnd.v.Y = Number(blYMin)
		partEnd.v.X = partStart.v.X // should be == part.psx
	} else if part.pdy == 0 { // horizontal line
		// X coords: starts at the left, ends at the right of blockmap box
		// Y coord is taken from partition line
		partStart.v.X = Number(blXMin)
		partStart.v.Y = partSegCoords.StartVertex.Y
		partEnd.v.X = Number(blXMax)
		partEnd.v.Y = partStart.v.Y // should be == part.psy
	} else { // diagonal line (the worst)
		// Ok, I suck at this geometry thingy, but have to do it nonetheless
		linesTried := 0
		intersectPoints := make([]OrientedVertex, 0)
		for len(intersectPoints) < 2 && linesTried < 4 {
			switch linesTried {
			case 0:
				{
					// Left vertical blockmap boundary
					c.lsx = Number(blXMin)
					c.lsy = Number(blYMax)
					c.lex = c.lsx
					c.ley = Number(blYMin)
					v := c.getIntersectionPoint_InfiniteLines()
					if v != nil && tskCheckBounds(v, blXMax, blYMax, blXMin,
						blYMin) {
						intersectPoints = appendNoDuplicates(intersectPoints,
							OrientedVertex{
								v: v,
							})
					}
				}
			case 1:
				{
					// Right vertical blockmap boundary
					c.lsx = Number(blXMax)
					c.lsy = Number(blYMax)
					c.lex = c.lsx
					c.ley = Number(blYMin)
					v := c.getIntersectionPoint_InfiniteLines()
					if v != nil && tskCheckBounds(v, blXMax, blYMax, blXMin,
						blYMin) {
						intersectPoints = appendNoDuplicates(intersectPoints,
							OrientedVertex{
								v: v,
							})
					}
				}
			case 2:
				{
					// Top horizontal blockmap boundary
					c.lsx = Number(blXMin)
					c.lsy = Number(blYMax)
					c.lex = Number(blXMax)
					c.ley = c.lsy
					v := c.getIntersectionPoint_InfiniteLines()
					if v != nil && tskCheckBounds(v, blXMax, blYMax, blXMin,
						blYMin) {
						intersectPoints = appendNoDuplicates(intersectPoints,
							OrientedVertex{
								v: v,
							})
					}
				}
			case 3:
				{
					// Bottom horizontal blockmap boundary
					c.lsx = Number(blXMin)
					c.lsy = Number(blYMin)
					c.lex = Number(blXMax)
					c.ley = c.lsy
					v := c.getIntersectionPoint_InfiniteLines()
					if v != nil && tskCheckBounds(v, blXMax, blYMax, blXMin,
						blYMin) {
						intersectPoints = appendNoDuplicates(intersectPoints,
							OrientedVertex{
								v: v,
							})
					}
				}
			}
			linesTried++
		}
		if len(intersectPoints) < 2 {
			// BSP v5.2 used to compute only 0-1 intersection points in some
			// cases. After I rolled my own implementation, it initially had
			// the same problem (much less frequenty, though), until I figured
			// out I should be rounding float result according to math rules
			// before casting to int. Now, I don't seem to get this anymore on
			// my big test map - good progress I made
			// Hid behind verbose lvl2 (reason: message lack of clarity level / technical speak)
			Log.Verbose(2, "Couldn't determine point of intersection between partition line and solid internal blockmap bounding box (%d, %d). Falling back to legacy way of measuring length.\n",
				len(intersectPoints), linesTried)
			Log.Verbose(2, "part from linedef %d!%d+%d: (%d %d) - (%d %d) bbox: (%d %d) - (%d %d)\n",
				part.Linedef, part.Flip, part.Offset, c.psx, c.psy, c.pex,
				c.pey, blXMin, blYMax, blXMax, blYMin)
			for i := 0; i < len(intersectPoints); i++ {
				Log.Verbose(2, "Intersection#%d: %s",
					i, intersectPoints[i].toString())
			}
			// Bail out
			return nil, nil
		}
		if VertexPairCOrdering(intersectPoints[0].v, intersectPoints[1].v,
			false) {
			partStart, partEnd = intersectPoints[0], intersectPoints[1]
		} else {
			partStart, partEnd = intersectPoints[1], intersectPoints[0]
		}
	}
	return &partStart, &partEnd
}

// Checks only first element, cause it is a lazy wrapper over append
// in PartitionInBoundary specifically. So that lines that go through a node box
// corner don't get the same intersection point reported twice instead of
// finding two distinct intersection points
func appendNoDuplicates(x []OrientedVertex, v OrientedVertex) []OrientedVertex {
	if len(x) == 0 || !x[0].equalTo(&v) {
		return append(x, v)
	}
	return x
}

// VigilantDoomer: my experience with doLinesIntersect/computeIntersection was
// that they often failed to find two intersection points between a line
// and a box, for a line definitely lying within the box. Thus I had to roll
// out my own implementation, which has a caveat - it doesn't intersect partition
// line against a line segment, but it intersects two infinite lines against
// each other. Thus another function: tskCheckBounds, is found below
func (c *IntersectionContext) getIntersectionPoint_InfiniteLines() *NodeVertex {
	ldx := float64(c.lex - c.lsx)
	ldy := float64(c.ley - c.lsy)
	d := float64(c.pdy)*ldx - ldy*float64(c.pdx)
	if d == 0.0 {
		return nil
	}
	c1 := float64(c.pdy)*float64(c.psx) - float64(c.pdx)*float64(c.psy)
	c2 := ldy*float64(c.lsx) - ldx*float64(c.lsy)
	id := 1 / d
	return &NodeVertex{
		X: Number(int(math.Round((ldx*c1 - float64(c.pdx)*c2) * id))),
		Y: Number(int(math.Round((ldy*c1 - float64(c.pdy)*c2) * id))),
	}
}

// This checks whether a vertex is within the box
func tskCheckBounds(v *NodeVertex, blXMax, blYMax, blXMin, blYMin int) bool {
	return v.X <= Number(blXMax) && v.X >= Number(blXMin) &&
		v.Y <= Number(blYMax) && v.Y >= Number(blYMin)
}

// This finds the intersection between a partition line and a line segment,
// also handling the case of line segment being coincident with partition line.
func (c *IntersectionContext) getIntersectionOrIndicence() (*NodeVertex, *NodeVertex) {
	ldx := float64(c.lex - c.lsx)
	ldy := float64(c.ley - c.lsy)
	d := float64(c.pdy)*ldx - ldy*float64(c.pdx)
	if d == 0.0 { // parallel
		ptmp := c.pdx*c.psy - c.psx*c.pdy
		a := c.pdy*c.lsx - c.pdx*c.lsy + ptmp
		b := c.pdy*c.lex - c.pdx*c.ley + ptmp
		if a == 0 && b == 0 {
			// incidence
			return &NodeVertex{
					X: c.lsx,
					Y: c.lsy,
				}, &NodeVertex{
					X: c.lex,
					Y: c.ley,
				}
		} else {
			// parallel with no incidence
			return nil, nil
		}
	}
	c1 := float64(c.pdy)*float64(c.psx) - float64(c.pdx)*float64(c.psy)
	c2 := ldy*float64(c.lsx) - ldx*float64(c.lsy)
	id := 1 / d
	v := &NodeVertex{
		X: Number(int(math.Round((ldx*c1 - float64(c.pdx)*c2) * id))),
		Y: Number(int(math.Round((ldy*c1 - float64(c.pdy)*c2) * id))),
	}

	// See if intersection lies within the line segment of the other
	// (non-partition) line - cause we were supposed to be intersecting a
	// segment and not the entirety of that line
	if v != nil {
		var blXmax, blXmin, blYmax, blYmin int
		if c.lsx < c.lex {
			blXmax = int(c.lex)
			blXmin = int(c.lsx)
		} else {
			blXmax = int(c.lsx)
			blXmin = int(c.lex)
		}
		if c.lsy < c.ley {
			blYmax = int(c.ley)
			blYmin = int(c.lsy)
		} else {
			blYmax = int(c.lsy)
			blYmin = int(c.ley)
		}
		if tskCheckBounds(v, blXmax, blYmax, blXmin, blYmin) {
			return v, nil
		}
	}
	// Nothing of those two options
	return nil, nil
}

// Prefab for consistent ordering of collinear lines. "C" doesn't stand for
// anything in particular
type VertexPairC struct {
	StartVertex *NodeVertex
	EndVertex   *NodeVertex
	len         Number
}

func (s *NodeSeg) toVertexPairC() VertexPairC {
	// orders StartVertex -> EndVertex so that
	// 1. dx != 0 then dx > 0
	// 2. dx = 0 then dy < 0
	// minx -> maxx (dx > 0) (horizontal projection, high priority)
	// maxy -> miny (dy < 0) (vertical projection, lower priority)
	if VertexPairCOrdering(s.StartVertex, s.EndVertex, false) {
		return VertexPairC{
			StartVertex: s.StartVertex,
			EndVertex:   s.EndVertex,
			len:         s.len,
		}
	} else { // swap
		return VertexPairC{
			StartVertex: s.EndVertex,
			EndVertex:   s.StartVertex,
			len:         s.len,
		}
	}
}

// Establishes consistent way of sorting a set of vertices all lying on the same
// line (differential geometry: 1-dimensional plane lol)
// Returns true if a and b vertices are ordered in this consistent way,
// false if should be swapped.
// If strictLess is false, returns true for equal vertices (they don't need to
// be swapped)
// If strictLess is true, establishes strict ordering (returns true if a < b
// instead of a <= b). This latter option is used when implementing Less() for
// sort.Sort() or sort.Stable()
func VertexPairCOrdering(a, b *NodeVertex, strictLess bool) bool {
	// a -> b is in correct "COrder" if either of the following is true
	// 1. dx != 0 then dx > 0
	// 2. dx = 0 then dy <= 0
	// minx -> maxx (dx > 0) (horizontal projection, high priority)
	// maxy -> miny (dy < 0) (vertical projection, lower priority)
	if a.X == b.X {
		if a.Y < b.Y {
			return false
		} else {
			return !strictLess || a.Y > b.Y
		}
	} else if a.X < b.X {
		return true
	} else { // a.X > b.X
		return false
	}
}

func ProjectsLeftToVPCOrdering(a, b *NodeVertex) bool {
	if a.X == b.X {
		if a.Y < b.Y {
			return false
		} else {
			return true
		}
	} else if a.X < b.X {
		return false
	} else {
		return true
	}
}

type CollinearVertexPairCByCoord []VertexPairC

func (x CollinearVertexPairCByCoord) Len() int { return len(x) }
func (x CollinearVertexPairCByCoord) Less(i, j int) bool {
	// So we know all segs are collinear
	// Now put their starting vertices in the same uniform order we put
	// each vertice pair
	return VertexPairCOrdering(x[i].StartVertex, x[j].StartVertex, true)
}
func (x CollinearVertexPairCByCoord) Swap(i, j int) { x[i], x[j] = x[j], x[i] }

// Returns the sum of all overlaps length
// Return value only valid if sorted beforehand
// I am not sure if it is better to remove overlaps or just count a new length
// without all the overlaps - I will figure out the way to optimize later xD
func (x CollinearVertexPairCByCoord) GetOverlapsLength() Number {
	// With the ordering set to VertexPairCOrdering and lines known to be colinear,
	// the two lines overlap if and only if: current line start vertex is
	// between previous line start vertex (inclusive) and end vertex (not inclusive).
	// That's all
	overlapLength := Number(0)
	for i := 1; i < len(x); i++ {
		prevStart := x[i-1].StartVertex
		prevEnd := x[i-1].EndVertex
		curStart := x[i].StartVertex
		if VertexPairCOrdering(curStart, prevEnd, true) &&
			VertexPairCOrdering(prevStart, curStart, false) {
			// Calculate dx and dy of overlap
			dx := prevEnd.X - curStart.X
			dy := prevEnd.Y - curStart.Y
			overlapLength += Number(math.Sqrt(float64(dx*dx) + float64(dy*dy)))
		}
	}
	return overlapLength
}

// And this function, unlike the GetOverlapsLength(), checks for a generic
// case of whether two line segments are overlapping or not, without any
// pre-existing knowledge
func AreOverlapping(seg1Start, seg1End, seg2Start, seg2End *NodeVertex) bool {
	return (VertexPairCOrdering(seg1Start, seg2Start, false) &&
		VertexPairCOrdering(seg2Start, seg1End, false)) ||
		(VertexPairCOrdering(seg1Start, seg2End, false) &&
			VertexPairCOrdering(seg2End, seg1End, false)) ||
		(VertexPairCOrdering(seg2Start, seg1Start, false) &&
			VertexPairCOrdering(seg1Start, seg2End, false) &&
			VertexPairCOrdering(seg2Start, seg1End, false) &&
			VertexPairCOrdering(seg1End, seg2End, false))
}

func (x CollinearVertexPairCByCoord) toString() string {
	if len(x) == 0 {
		return "{EMPTY!}"
	}
	s := ""
	for i := 0; i < len(x); i++ {
		s += fmt.Sprintf("; [(%d;%d)-(%d;%d)]", x[i].StartVertex.X, x[i].StartVertex.Y,
			x[i].EndVertex.X, x[i].EndVertex.Y)
	}
	return s
}

// Vertex plus a sign. Sign indicates the direction of vector aligned with the
// partition line which vector is the result of projection of solid line ort
// vector (which describes which direction solid line faces) onto the partition
// line.
// The vertex is the intersection between solid line and partition line.
// An array of such vertexes, ordered so they are encountered sequentially on
// partition line when going from one end to another, can be used to tell
// segments of partition line that go through a void from segments of partition
// line that are within the level's sector space. Specifically, segments formed
// from two neighbor vertexes facing away from each other goes through the
// void, whereas if two neighbor vertexes face towards each other the segment
// is inside the level.
type OrientedVertex struct {
	v    *NodeVertex
	left bool // true == points towards -dx (non-vertical partition line) or +dy (vertical partition line) (towards "lesser" vertex on a VertexPairCOrdering)
}

func (ov1 *OrientedVertex) equalTo(ov2 *OrientedVertex) bool {
	if ov1 == nil || ov2 == nil {
		return false
	}
	if ov1.v == nil || ov2.v == nil {
		return false
	}
	return ov1.v.X == ov2.v.X && ov1.v.Y == ov2.v.Y && ov1.left == ov2.left
}

// An array of OrientedVertex instances
type CollinearOrientedVertices []OrientedVertex

func (x CollinearOrientedVertices) Len() int { return len(x) }
func (x CollinearOrientedVertices) Less(i, j int) bool {
	// All vertices in this set lie on the same line, which allows us to perform
	// an ordering
	if x[i].v.X == x[j].v.X && x[i].v.Y == x[j].v.Y {
		if i == len(x)-1 {
			// don't put the last one before anything, unless we had something
			// with different coords coming later
			return false
		}
		return x[i].left && !x[j].left
	}
	return VertexPairCOrdering(x[i].v, x[j].v, true)
}
func (x CollinearOrientedVertices) Swap(i, j int) { x[i], x[j] = x[j], x[i] }

func (x CollinearOrientedVertices) toString() string {
	s := "["
	for i := 0; i < len(x); i++ {
		s = s + fmt.Sprintf(",%s", x[i].toString())
	}
	return s + "]"
}

// When some vertex occurs multiple times with different (or maybe same)
// directions, this recognises which of these vertex occurencies should remain
// so that it is possible to infer when line states changes from going through
// void to going through level, or doesn't change at all
func (x *CollinearOrientedVertices) Coalesce() bool {
	allOk := true
	l := len(*x)
	for i := 1; i < l; i++ {
		// If values are equal, or roughly equal (due to rounding problems,
		// have sometimes values that are very near on one axis, but are clearly
		// meant to be same point)
		if (*x)[i].valuesEqualWithEpsilon(&((*x)[i-1])) {
			p, hasDominant, dominantDir := x.seekPastEqualsAndNoteSign(&((*x)[i-1]), i)
			dropLen := p - i + 1
			dropStart := i
			// If dominant direction concides with the direction BEFORE this
			// sequence of equal vertices, either of the two happened:
			// 1. We have an error
			// 2. State of line didn't change at the current coords just as if
			// there was no dominant direction
			hasDominant = hasDominant && !x.matchToPrev(i, p, dominantDir)
			if hasDominant {
				// State of line changed, as defined by the new dominant
				// direction. Only dominant direction needs to be kept, and thus
				// only one vertex. The start of the sequence inherits dominant
				// direction, and survives
				(*x)[i-1].left = dominantDir
			} else {
				// The state of line doesn't change at these coords - if it was
				// going through the void, it continues through the void, if it
				// was going through the level, it continues through the level.
				// Thus omit in entirety. Except the #0 and #last vertices of
				// entire array are not to be removed even in this case,
				// although their direction can change.
				if (i > 1) && (p < l-1) {
					dropLen++
					dropStart--
				}
			}
			l = l - dropLen
			for j := dropStart; j < l; j++ {
				(*x)[j] = (*x)[j+dropLen]
			}
			i = dropStart - 1 // so that next iteration starts on i = dropStart of current
			*x = (*x)[:l]
		}
	}
	a := (*x)[:l]
	*x = a
	return allOk
}

// When dominant direction of sequence of equal vertices matches the direction
// of distinct vertex preceeding that sequence, but non-dominant direction
// existed, we don't have an error but the state of line didn't really change
// here
// Returns true if this condition was detected, false otherwise
func (x *CollinearOrientedVertices) matchToPrev(i, p int, dominantDir bool) bool {
	// Does preceeding vertex exist?
	if i-2 > 0 {
		prevDir := (*x)[i-2].left
		if prevDir == dominantDir {
			// To make sure the condition is not result of an error, we check
			// if there were both directions present
			for j := i - 1; j <= p; j++ {
				if (*x)[j].left != dominantDir {
					return true // yes, one of vertices in the equal sequence is in the opposite direction
					break
				}
			}
		}
	}
	return false
}

func (ov1 *OrientedVertex) valuesEqualWithEpsilon(ov2 *OrientedVertex) bool {
	if ov1 == nil || ov2 == nil {
		return false
	}
	if ov1.v == nil || ov2.v == nil {
		return false
	}
	XequalWithEpsilon := Abs(int(ov1.v.X-ov2.v.X)) <= 1
	YequalWithEpsilon := Abs(int(ov1.v.Y-ov2.v.Y)) <= 1
	return XequalWithEpsilon && YequalWithEpsilon
}

// Returns last position where x[i] == ov since i>startPos, and also whether
// there is a dominant direction and which one it is (true means left, false
// means right)
func (x *CollinearOrientedVertices) seekPastEqualsAndNoteSign(ov *OrientedVertex, startPos int) (int, bool, bool) {
	l := len(*x)
	a := startPos
	leftCnt := 0
	rightCnt := 0
	if ov.left {
		leftCnt++
	} else {
		rightCnt++
	}
	for i := startPos; i < l; i++ {
		if !((*x)[i].valuesEqualWithEpsilon(ov)) {
			break
		}
		if (*x)[i].left {
			leftCnt++
		} else {
			rightCnt++
		}
	}
	hasDominant := leftCnt != rightCnt
	dominantDir := hasDominant && (leftCnt > rightCnt) // true means left, right otherwise
	return a, hasDominant, dominantDir
}

func (x *OrientedVertex) toString() string {
	if x == nil {
		return "nil"
	}
	str := "RIGHT"
	if x.left {
		str = "LEFT"
	}
	return fmt.Sprintf("%s(%d,%d)", str, x.v.X, x.v.Y)
}

// A cached record of useful things related to quickly finding the segments of
// partition line that are inside the level (which allow to quickly compute the
// length of partition line made solely of such segments), stored for alias of
// partition line
type NonVoidPerAlias struct {
	data          []VertexPairC
	success       bool
	c             IntersectionContext
	partSegCoords VertexPairC
	original      CollinearOrientedVertices
}

func IsClockwiseTriangle(p1, p2, p3 *NodeVertex) bool {
	sgn := (p2.X-p1.X)*(p3.Y-p1.Y) - (p2.Y-p1.Y)*(p3.X-p1.X)
	return sgn < 0
}
