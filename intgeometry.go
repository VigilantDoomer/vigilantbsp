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

// intgeometry
package main

import (
	"fmt"
	"math"
)

// some functions from old version of diffgeometry.go had to be retained,
// because porting all instances of their use (for example, in reject builder
// "identifying outer sector for self-referencing sector" code) to new API built
// around float rather than integers would be time consuming - much testing
// would be needed.

func IntPartitionInBoundary(part *NodeSeg, c *IntersectionContext,
	blXMax, blYMax, blXMin, blYMin int, partSegCoords IntVertexPairC) (*IntOrientedVertex, *IntOrientedVertex) {

	var partStart, partEnd IntOrientedVertex
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
		intersectPoints := make([]IntOrientedVertex, 0)
		for len(intersectPoints) < 2 && linesTried < 4 {
			switch linesTried {
			case 0:
				{
					// Left vertical blockmap boundary
					c.lsx = Number(blXMin)
					c.lsy = Number(blYMax)
					c.lex = c.lsx
					c.ley = Number(blYMin)
					v := c.intGetIntersectionPoint_InfiniteLines()
					if v != nil && intTskCheckBounds(v, blXMax, blYMax, blXMin,
						blYMin) {
						intersectPoints = intAppendNoDuplicates(intersectPoints,
							IntOrientedVertex{
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
					v := c.intGetIntersectionPoint_InfiniteLines()
					if v != nil && intTskCheckBounds(v, blXMax, blYMax, blXMin,
						blYMin) {
						intersectPoints = intAppendNoDuplicates(intersectPoints,
							IntOrientedVertex{
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
					v := c.intGetIntersectionPoint_InfiniteLines()
					if v != nil && intTskCheckBounds(v, blXMax, blYMax, blXMin,
						blYMin) {
						intersectPoints = intAppendNoDuplicates(intersectPoints,
							IntOrientedVertex{
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
					v := c.intGetIntersectionPoint_InfiniteLines()
					if v != nil && intTskCheckBounds(v, blXMax, blYMax, blXMin,
						blYMin) {
						intersectPoints = intAppendNoDuplicates(intersectPoints,
							IntOrientedVertex{
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
			Log.Verbose(2, "part from linedef %d!%d+%d: (%v %v) - (%v %v) bbox: (%v %v) - (%v %v)\n",
				part.Linedef, part.getFlip(), part.Offset, c.psx, c.psy, c.pex,
				c.pey, blXMin, blYMax, blXMax, blYMin)
			for i := 0; i < len(intersectPoints); i++ {
				Log.Verbose(2, "Intersection#%d: %s",
					i, intersectPoints[i].toString())
			}
			// Bail out
			return nil, nil
		}
		if IntVertexPairCOrdering(intersectPoints[0].v, intersectPoints[1].v,
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
func intAppendNoDuplicates(x []IntOrientedVertex, v IntOrientedVertex) []IntOrientedVertex {
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
func (c *IntersectionContext) intGetIntersectionPoint_InfiniteLines() *NodeVertex {
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
func intTskCheckBounds(v *NodeVertex, blXMax, blYMax, blXMin, blYMin int) bool {
	return v.X <= Number(blXMax) && v.X >= Number(blXMin) &&
		v.Y <= Number(blYMax) && v.Y >= Number(blYMin)
}

// This finds the intersection between a partition line and a line segment,
// also handling the case of line segment being coincident with partition line.
func (c *IntersectionContext) intGetIntersectionOrIndicence() (*NodeVertex, *NodeVertex) {
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
		if intTskCheckBounds(v, blXmax, blYmax, blXmin, blYmin) {
			return v, nil
		}
	}
	// Nothing of those two options
	return nil, nil
}

// Prefab for consistent ordering of collinear lines. "C" doesn't stand for
// anything in particular
type IntVertexPairC struct {
	StartVertex *NodeVertex
	EndVertex   *NodeVertex
	len         Number
}

func (s *NodeSeg) toIntVertexPairC() IntVertexPairC {
	// orders StartVertex -> EndVertex so that
	// 1. dx != 0 then dx > 0
	// 2. dx = 0 then dy < 0
	// minx -> maxx (dx > 0) (horizontal projection, high priority)
	// maxy -> miny (dy < 0) (vertical projection, lower priority)
	if IntVertexPairCOrdering(s.StartVertex, s.EndVertex, false) {
		return IntVertexPairC{
			StartVertex: s.StartVertex,
			EndVertex:   s.EndVertex,
			len:         s.len,
		}
	} else { // swap
		return IntVertexPairC{
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
func IntVertexPairCOrdering(a, b *NodeVertex, strictLess bool) bool {
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

type IntCollinearVertexPairCByCoord []IntVertexPairC

func (x IntCollinearVertexPairCByCoord) Len() int { return len(x) }
func (x IntCollinearVertexPairCByCoord) Less(i, j int) bool {
	// So we know all segs are collinear
	// Now put their starting vertices in the same uniform order we put
	// each vertice pair
	return IntVertexPairCOrdering(x[i].StartVertex, x[j].StartVertex, true)
}
func (x IntCollinearVertexPairCByCoord) Swap(i, j int) { x[i], x[j] = x[j], x[i] }

// Returns the sum of all overlaps length
// Return value only valid if sorted beforehand
// I am not sure if it is better to remove overlaps or just count a new length
// without all the overlaps - I will figure out the way to optimize later xD
func (x IntCollinearVertexPairCByCoord) GetOverlapsLength() Number {
	// With the ordering set to VertexPairCOrdering and lines known to be colinear,
	// the two lines overlap if and only if: current line start vertex is
	// between previous line start vertex (inclusive) and end vertex (not inclusive).
	// That's all
	overlapLength := Number(0)
	for i := 1; i < len(x); i++ {
		prevStart := x[i-1].StartVertex
		prevEnd := x[i-1].EndVertex
		curStart := x[i].StartVertex
		if IntVertexPairCOrdering(curStart, prevEnd, true) &&
			IntVertexPairCOrdering(prevStart, curStart, false) {
			// Calculate dx and dy of overlap
			dx := prevEnd.X - curStart.X
			dy := prevEnd.Y - curStart.Y
			overlapLength += Number(math.Sqrt(float64(dx*dx) + float64(dy*dy)))
		}
	}
	return overlapLength
}

func (x IntCollinearVertexPairCByCoord) toString() string {
	if len(x) == 0 {
		return "{EMPTY!}"
	}
	s := ""
	for i := 0; i < len(x); i++ {
		s += fmt.Sprintf("; [(%v;%v)-(%v;%v)]", x[i].StartVertex.X, x[i].StartVertex.Y,
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
type IntOrientedVertex struct {
	v    *NodeVertex
	left bool // true == points towards -dx (non-vertical partition line) or +dy (vertical partition line) (towards "lesser" vertex on a VertexPairCOrdering)
}

func (ov1 *IntOrientedVertex) equalTo(ov2 *IntOrientedVertex) bool {
	if ov1 == nil || ov2 == nil {
		return false
	}
	if ov1.v == nil || ov2.v == nil {
		return false
	}
	return ov1.v.X == ov2.v.X && ov1.v.Y == ov2.v.Y && ov1.left == ov2.left
}

// An array of OrientedVertex instances
type IntCollinearOrientedVertices []IntOrientedVertex

func (x IntCollinearOrientedVertices) Len() int { return len(x) }
func (x IntCollinearOrientedVertices) Less(i, j int) bool {
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
	return IntVertexPairCOrdering(x[i].v, x[j].v, true)
}
func (x IntCollinearOrientedVertices) Swap(i, j int) { x[i], x[j] = x[j], x[i] }

func (x IntCollinearOrientedVertices) toString() string {
	s := "["
	for i := 0; i < len(x); i++ {
		s = s + fmt.Sprintf(",%s", x[i].toString())
	}
	if len(s) > 1 {
		s = "[" + s[2:] // remove "," immediately after "["
	}
	return s + "]"
}

func (x *IntOrientedVertex) toString() string {
	if x == nil {
		return "nil"
	}
	str := "RIGHT"
	if x.left {
		str = "LEFT"
	}
	return fmt.Sprintf("%s(%v,%v)", str, x.v.X, x.v.Y)
}

func IntIsClockwiseTriangle(p1, p2, p3 *NodeVertex) bool {
	sgn := (p2.X-p1.X)*(p3.Y-p1.Y) - (p2.Y-p1.Y)*(p3.X-p1.X)
	return sgn < 0
}
