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

// diffgeometry.go
package main

import (
	"fmt"
	"math"
	"sort"
	"strings"
)

// picknode.go grew very big after I implemented partition length evaluation
// without void segments, so I had to split all the gory differential geometry
// (and the like) from there into this file.

// Major rewrite: now uses floating point even for vanilla nodes generator
// Some legacy stuff retained in intgeometry.go

const (
	SIDE_LEFT_VERTICAL     = 0
	SIDE_RIGHT_VERTICAL    = 1
	SIDE_TOP_HORIZONTAL    = 2
	SIDE_BOTTOM_HORIZONTAL = 3
)

const MOVE_SAFE_MARGIN = 10

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
	partSegCoords := w.SegOrLineToVertexPairC(part)
	var c FloatIntersectionContext
	c.psx = partSegCoords.StartVertex.X
	c.psy = partSegCoords.StartVertex.Y
	c.pex = partSegCoords.EndVertex.X
	c.pey = partSegCoords.EndVertex.Y
	c.pdx = c.pex - c.psx
	c.pdy = c.pey - c.psy

	partStart, partEnd, bside := PartitionInBoundary(part, &c, blXMax,
		blYMax, blXMin, blYMin, partSegCoords)
	if partStart == nil || partEnd == nil {
		// Damnation!
		return NonVoidPerAlias{
			success: false,
		}
	}

	w.dgVertexMap.RestoreOrBeginSnapshot()
	ipsx := int(math.Round(partStart.X))
	ipsy := int(math.Round(partStart.Y))
	ipex := int(math.Round(partEnd.X))
	ipey := int(math.Round(partEnd.Y))
	// Some intersection points may be close to these. In such cases, they
	// should get identical values, otherwise (and this did happen when I
	// tested) they may be seen as being outside the interval and the projected
	// direction may be computed wrong.
	// Since vertex map will check against earliest "close enough" match,
	// I need to allow these points to be altered to such a match if it exists
	partStart = w.dgVertexMap.SelectVertexClose(partStart.X, partStart.Y)
	partEnd = w.dgVertexMap.SelectVertexClose(partEnd.X, partEnd.Y)

	// I have to use a vertex that WOULD occur even earlier alongside the
	// line (according to the direction identified by partStart -> partEnd)
	// than partStart vertex to be able to perform IsClockwiseTriangle
	// when the intersection point (p1 or p2) is EXACTLY on partStart.
	setContextFromBlockSide(&c, bside, blXMax, blYMax, blXMin, blYMin,
		MOVE_SAFE_MARGIN)
	partOrient := c.getIntersectionPoint_InfiniteLines()
	if partOrient == nil {
		Log.Verbose(2, "Failed to compute a special vertex for clockwise-triangle checking.\n")
	}

	// Chapter II
	// We use a blockmap to reduce the number of solid lines we need to check for
	// intersecting our partition
	// We also use vertex map to help identify identical vertices among products
	// of intersection (or incidence)
	pts := CollinearOrientedVertices(make([]OrientedVertex, 0))
	if w.blockity == nil {
		w.blockity = GetBlockityLines(w.solidMap)
	}
	iter := w.blockity
	// Note: the coords start at bounding box edges - we need to follow the line
	// through entire map to know for sure when it is in the void and when it
	// is not
	iter.SetContext(ipsx, ipsy, ipex, ipey)
	cntIncident := 0
	for iter.NextLine() {
		aline := iter.GetLine()
		lsx, lsy, lex, ley := w.lines.GetAllXY(aline)
		if lsx == lex && lsy == ley { // skip zero-length lines
			continue
		}
		c.lsx = float64(lsx)
		c.lsy = float64(lsy)
		c.lex = float64(lex)
		c.ley = float64(ley)
		pt1, pt2 := c.getIntersectionOrIndicence()
		if pt1 != nil {
			RoundToFixed1616(pt1)
			pt1 = w.dgVertexMap.SelectVertexClose(pt1.X, pt1.Y)
			if pt2 != nil {
				// Incidence - need to add two points looking at each other,
				// between them we have non-void 1-D space
				RoundToFixed1616(pt2)
				pt2 = w.dgVertexMap.SelectVertexClose(pt2.X, pt2.Y)
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
				cntIncident++
			} else { // pt2 == nil
				// Intersection
				ov := OrientedVertex{
					v: pt1,
					left: IsClockwiseTriangle(&FloatVertex{X: c.lsx, Y: c.lsy},
						&FloatVertex{X: c.lex, Y: c.ley}, partOrient),
				}
				pts = append(pts, ov)
			}
		}
		// Was a case of the same linedef but differing segs in EXTENDED mode
		// not getting an intersection with the line PASSING through the same
		// vertex as the linedef! Solved by using linedef coords instead of
		// seg coords in extended mode
		/*if aline == 2819 && part.Linedef == 2820 {
			Log.Printf("part %d aline %d part_alias %d part_coords %s-%s pt1 = %s, pt2 = %s\n .......... ic (%f,%f)-(%f,%f)",
				part.Linedef, aline, part.alias, partStart.toString(), partEnd.toString(), pt1.toString(), pt2.toString(),
				c.psx, c.psy, c.pex, c.pey)
		}*/
	}

	// Chapter III
	// Sort all these things, so the vertexes indicate where partition line
	// candidate enters the map and where it goes to the void outside
	if len(pts) < 2 {
		// Need at least two points: one where map is entered, another one
		// where it is exited
		Log.Verbose(2, "Failed to produce a solid hits array: %d items (need at least two)\n",
			len(pts))
		return NonVoidPerAlias{
			success: false,
		}
	}
	sort.Sort(pts)
	startBound := VertexPairCOrdering(partStart, pts[0].v, false)
	endBound := VertexPairCOrdering(pts[len(pts)-1].v, partEnd, false)
	if !startBound || !endBound {
		tailStr := ""
		if !startBound {
			tailStr = fmt.Sprintf("(%f, %f) != (%f, %f)",
				pts[0].v.X, pts[0].v.Y,
				partStart.X, partStart.Y)
		}
		if !endBound {
			if tailStr != "" {
				tailStr = "s " + tailStr + " e "
			}
			tailStr += fmt.Sprintf("(%f, %f) != (%f, %f)",
				pts[len(pts)-1].v.X, pts[len(pts)-1].v.Y,
				partEnd.X, partEnd.Y)
		}
		Log.Verbose(2, "Failed to produce a solid hits array for partition line %d: %t %t %s.\n",
			part.Linedef, startBound, endBound, tailStr)
		// Bail out, damn it
		return NonVoidPerAlias{
			success: false,
		}
	}
	// Not rewriting Coalesce now, and that damn thing relies on borders being
	// included in this cursed array
	ovStart := OrientedVertex{
		v:    partStart,
		left: true,
	}
	ovEnd := OrientedVertex{
		v:    partEnd,
		left: false,
	}
	ptsOld := CollinearOrientedVertices(make([]OrientedVertex, 0))
	for _, it := range pts {
		ptsOld = append(ptsOld, OrientedVertex{
			v:    it.v,
			left: it.left,
		})
	}

	pts_fix := CollinearOrientedVertices(make([]OrientedVertex, 0))
	pts_fix = append(pts_fix, ovStart)
	for _, it := range pts {
		pts_fix = append(pts_fix, OrientedVertex{
			v:    it.v,
			left: it.left,
		})
	}
	pts_fix = append(pts_fix, ovEnd)
	pts = pts_fix
	pts.Coalesce() // Resolve differing directions at identical points

	// Sanity check: every vertex flips the direction, with the exception
	// of bordering vertexes, they might be next to vertexes of the same
	// direction, because solid level usually starts some distance away (that is,
	// we have void near borders)
	fluger := true
	for i := 2; i < len(pts)-2; i++ {
		fluger = fluger && (pts[i].left != pts[i-1].left)
	}
	if !fluger {
		// Output warning, but persist nonetheless (instead of failing), unless
		// PersistThroughInsanity was set to false
		// Log.Verbose(2, "Sanity check failed! Evaluated partition line (%d,%d)-(%d,%d) doesn't consistently go in/out of the void when crossing solid lines. %s\nOld content: %s",
		// part.psx, part.psy, part.pex, part.pey, pts.toString(), ptsOld.toString())
		Log.Verbose(2, "... for the next line - old content: %s",
			ptsOld.toString())
		Log.Verbose(2, "Sanity check failed! Evaluated partition line %d (%v,%v)-(%v,%v) doesn't consistently go in/out of the void when crossing solid lines (incidence count: %d). %s\n",
			part.Linedef, part.psx, part.psy, part.pex, part.pey, cntIncident, pts.toString()) //pts[1:len(pts)-1].toString())
		/*if part.Linedef == 13993 && len(pts) >= 6 {
			Log.Printf("%s=%s: %t %t %t (%3.10f,%3.10f)-(%3.10f,%3.10f)\n", pts[3].toString(), pts[4].toString(), pts[3].v == pts[4].v,
				pts[3].v.X == pts[4].v.X, pts[3].v.Y == pts[4].v.Y,
				pts[3].v.X, pts[3].v.Y, pts[4].v.X, pts[4].v.Y)
		}*/ // see parameters for building Water Spirit in valuesEqual

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
		dx := pts[i].v.X - pts[i-1].v.X
		dy := pts[i].v.Y - pts[i-1].v.Y
		nonVoid = append(nonVoid, VertexPairC{
			StartVertex: pts[i-1].v,
			EndVertex:   pts[i].v,
			len:         Number(math.Sqrt(dx*dx + dy*dy)),
		})
	}

	if len(nonVoid) == 0 {
		Log.Verbose(2, "part %d trace produced ZERO non-void intervals\n",
			part.Linedef)
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

func RoundToFixed1616(v *FloatVertex) {
	// see valuesEqual - somehow VertexMap can return different pointers with
	// equal-valued vertices, but how? this reduces such occurences, but one
	// last one in Water Spirit required to implement value check instead of
	// reference check in valuesEqual
	v.X = float64(int(v.X*FIXED16DOT16_MULTIPLIER)) / FIXED16DOT16_MULTIPLIER
	v.Y = float64(int(v.Y*FIXED16DOT16_MULTIPLIER)) / FIXED16DOT16_MULTIPLIER
}

// PartitionInBoundary returns two points of intersection between partition
// line and a bounding box represented by blXMax, blYMax, blXMin, blYMin.
// Argument part is only used for log output (linedef index etc.), the values
// are computed using c (IntersectionContext) and partSegCoords instead
func PartitionInBoundary(part *NodeSeg, c *FloatIntersectionContext,
	blXMax, blYMax, blXMin, blYMin int, partSegCoords VertexPairC) (*FloatVertex, *FloatVertex, int) {

	partStart := new(FloatVertex)
	partEnd := new(FloatVertex)
	var side [2]int
	if part.pdx == 0 { // vertical line
		// Y coords: starts at the top, ends at the bottom of blockmap box
		// X coord is taken from partition line
		partStart.Y = float64(blYMax)
		partStart.X = partSegCoords.StartVertex.X
		partEnd.Y = float64(blYMin)
		partEnd.X = partStart.X // should be == part.psx
		side[0] = SIDE_TOP_HORIZONTAL
		side[1] = SIDE_BOTTOM_HORIZONTAL
	} else if part.pdy == 0 { // horizontal line
		// X coords: starts at the left, ends at the right of blockmap box
		// Y coord is taken from partition line
		partStart.X = float64(blXMin)
		partStart.Y = partSegCoords.StartVertex.Y
		partEnd.X = float64(blXMax)
		partEnd.Y = partStart.Y // should be == part.psy
		side[0] = SIDE_LEFT_VERTICAL
		side[1] = SIDE_RIGHT_VERTICAL
	} else { // diagonal line (the worst)
		// Ok, I suck at this geometry thingy, but have to do it nonetheless
		linesTried := 0
		intersectPoints := make([]*FloatVertex, 0)
		for len(intersectPoints) < 2 && linesTried < 4 {
			added := false
			setContextFromBlockSide(c, linesTried, blXMax, blYMax, blXMin,
				blYMin, 0)
			v := c.getIntersectionPoint_InfiniteLines()
			if v != nil && CheckBoundsForIntersectPoint(v, blXMax, blYMax, blXMin,
				blYMin) {
				intersectPoints, added = appendNoDuplicates(intersectPoints,
					v)
			}
			if added {
				sideIdx := len(intersectPoints) - 1
				if sideIdx < 2 {
					side[sideIdx] = linesTried
				} else {
					Log.Verbose(2, "More than 2 intersection points between line and a box - error.\n")
					return nil, nil, 0
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
				Log.Verbose(2, "Intersection#%d: (%v, %v)",
					i, intersectPoints[i].X, intersectPoints[i].Y)
			}
			// Bail out
			return nil, nil, 0
		}
		if VertexPairCOrdering(intersectPoints[0], intersectPoints[1],
			false) {
			partStart, partEnd = intersectPoints[0], intersectPoints[1]
		} else {
			partStart, partEnd = intersectPoints[1], intersectPoints[0]
			side[0], side[1] = side[1], side[0]
		}
	}
	return partStart, partEnd, side[0]
}

// appendNoDuplicates adds v to x if it is not already present, and
// returns new value of x and true if that happened. If x was not changed,
// the second return value is false
// Caveat: checks only first element, cause it is a lazy wrapper over append
// in PartitionInBoundary specifically. So that lines that go through a node box
// corner don't get the same intersection point reported twice instead of
// finding two distinct intersection points
func appendNoDuplicates(x []*FloatVertex, v *FloatVertex) ([]*FloatVertex, bool) {
	if len(x) == 0 || !x[0].equalToWithEpsilon(v) {
		return append(x, v), true
	}
	return x, false
}

// VigilantDoomer: my experience with doLinesIntersect/computeIntersection was
// that they often failed to find two intersection points between a line
// and a box, for a line definitely lying within the box. Thus I had to roll
// out my own implementation, which has a caveat - it doesn't intersect partition
// line against a line segment, but it intersects two infinite lines against
// each other. Thus another function: tskCheckBounds, is found below
func (c *FloatIntersectionContext) getIntersectionPoint_InfiniteLines() *FloatVertex {
	ldx := c.lex - c.lsx
	ldy := c.ley - c.lsy
	d := c.pdy*ldx - ldy*c.pdx
	if d == 0.0 {
		return nil
	}
	c1 := c.pdy*c.psx - c.pdx*c.psy
	c2 := ldy*c.lsx - ldx*c.lsy
	id := 1 / d
	return &FloatVertex{
		X: (ldx*c1 - c.pdx*c2) * id,
		Y: (ldy*c1 - c.pdy*c2) * id,
	}
}

// tskCheckBounds checks whether a vertex is within the box
// Only used in conjunction with getIntersectionPoint_InfiniteLines()
func CheckBoundsForIntersectPoint(v *FloatVertex, blXMax, blYMax, blXMin, blYMin int) bool {
	x := int(math.Round(v.X))
	y := int(math.Round(v.Y))
	return x <= blXMax && x >= blXMin &&
		y <= blYMax && y >= blYMin
}

// setContextFromBlockSide assigns c.lsx, c.lsy, c.lex, c.ley with coords of
// the line that represents a specified side of the box blXMax, blYMax, blXmin,
// blYMin. If margin is non-zero, this side is also moved parallel to its
// original coords by margin px alongside respective axis, in a manner that
// would make the box bigger
func setContextFromBlockSide(c *FloatIntersectionContext, side int,
	blXMax, blYMax, blXMin, blYMin int, margin int) {
	switch side {
	case SIDE_LEFT_VERTICAL:
		// Left vertical blockmap boundary
		c.lsx = float64(blXMin - margin)
		c.lsy = float64(blYMax)
		c.lex = c.lsx
		c.ley = float64(blYMin)
	case SIDE_RIGHT_VERTICAL:
		// Right vertical blockmap boundary
		c.lsx = float64(blXMax + margin)
		c.lsy = float64(blYMax)
		c.lex = c.lsx
		c.ley = float64(blYMin)
	case SIDE_TOP_HORIZONTAL:
		// Top horizontal blockmap boundary
		c.lsx = float64(blXMin)
		c.lsy = float64(blYMax + margin)
		c.lex = float64(blXMax)
		c.ley = c.lsy
	case SIDE_BOTTOM_HORIZONTAL:
		// Bottom horizontal blockmap boundary
		c.lsx = float64(blXMin)
		c.lsy = float64(blYMin - margin)
		c.lex = float64(blXMax)
		c.ley = c.lsy
	default:
		Log.Panic("Unknown side\n")
	}
}

// getIntersectionOrIndicence finds the intersection between a partition line
// and a line segment, also handling the case of line segment being coincident
// with partition line. In case of intersection, returned values should be
// monotonous alongside partition line
func (c *FloatIntersectionContext) getIntersectionOrIndicence() (*FloatVertex, *FloatVertex) {
	ldx := c.lex - c.lsx
	ldy := c.ley - c.lsy
	d := c.pdy*ldx - ldy*c.pdx
	if d == 0.0 { // parallel
		ptmp := c.pdx*c.psy - c.psx*c.pdy
		a := c.pdy*c.lsx - c.pdx*c.lsy + ptmp
		b := c.pdy*c.lex - c.pdx*c.ley + ptmp
		if a == 0 && b == 0 {
			// incidence
			return &FloatVertex{
					X: c.lsx,
					Y: c.lsy,
				}, &FloatVertex{
					X: c.lex,
					Y: c.ley,
				}
		} else {
			// parallel with no incidence
			return nil, nil
		}
	}
	// TODO compare performance now and before
	cx := c.psx - c.lsx
	cy := c.lsy - c.psy
	num := cx*c.pdy + cy*c.pdx
	frac := num / d
	if frac > 1 || frac < 0 {
		// intersection point was outside of segment defined by c.l**
		// (valid intersection between infinite lines, but not between a line
		// and a line segment)
		return nil, nil
	}

	// I need return values to be monotonous alongside partition line
	num2 := cx*ldy + cy*ldx
	// no multiplication of num2 by (-1) is necessary,
	// cause the d in the following formula also would have been (-d)
	// i.e. -num2 / -d = num2 / d
	frac2 := num2 / d

	x := c.psx
	y := c.psy
	x = math.FMA(frac2, c.pex-x, x)
	y = math.FMA(frac2, c.pey-y, y)
	v := &FloatVertex{
		X: x,
		Y: y,
	}
	return v, nil
}

type FloatIntersectionContext struct {
	psx, psy, pex, pey float64 // start, end of partition coordinates
	pdx, pdy           float64 // used in intersection calculations
	lex, lsx, ley, lsy float64 // - same for checking line
}

// A cached record of useful things related to quickly finding the segments of
// partition line that are inside the level (which allow to quickly compute the
// length of partition line made solely of such segments), stored for alias of
// partition line
type NonVoidPerAlias struct {
	data          []VertexPairC
	success       bool
	c             FloatIntersectionContext
	partSegCoords VertexPairC
	original      CollinearOrientedVertices
}

func IsClockwiseTriangle(p1, p2, p3 *FloatVertex) bool {
	sgn := (p2.X-p1.X)*(p3.Y-p1.Y) - (p2.Y-p1.Y)*(p3.X-p1.X)
	return sgn < 0
}

func (v *NodeVertex) toFloatVertex() *FloatVertex {
	return &FloatVertex{
		Id: int(v.idx),
		X:  float64(v.X),
		Y:  float64(v.Y),
	}
}

func (v1 *FloatVertex) equalTo(v2 *FloatVertex) bool {
	if v1 == nil || v2 == nil {
		return false
	}
	return v1.X == v2.X && v1.Y == v2.Y
}

func (v1 *FloatVertex) equalToWithEpsilon(v2 *FloatVertex) bool {
	if v1 == nil || v2 == nil {
		return false
	}
	// same precision as VertexMap
	return math.Abs(v1.X-v2.X) < VERTEX_EPSILON &&
		math.Abs(v1.Y-v2.Y) < VERTEX_EPSILON
}

// Prefab for consistent ordering of collinear lines. "C" doesn't stand for
// anything in particular
type VertexPairC struct {
	StartVertex *FloatVertex
	EndVertex   *FloatVertex
	len         Number
}

func (s *NodeSeg) toVertexPairC() VertexPairC {
	// orders StartVertex -> EndVertex so that
	// 1. dx != 0 then dx > 0
	// 2. dx = 0 then dy < 0
	// minx -> maxx (dx > 0) (horizontal projection, high priority)
	// maxy -> miny (dy < 0) (vertical projection, lower priority)
	sv := s.StartVertex.toFloatVertex()
	ev := s.EndVertex.toFloatVertex()
	if VertexPairCOrdering(sv, ev, false) {
		return VertexPairC{
			StartVertex: sv,
			EndVertex:   ev,
			len:         s.len,
		}
	} else { // swap
		return VertexPairC{
			StartVertex: ev,
			EndVertex:   sv,
			len:         s.len,
		}
	}
}

// SegOrLineToFutureVertexPairC is replaced for extended nodes, see zdefs.go
func (w *NodesWork) SegOrLineToVertexPairC(part *NodeSeg) VertexPairC {
	// in nodes without extra precision, current coordinates is all one can
	// rely on
	return part.toVertexPairC()
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
func VertexPairCOrdering(a, b *FloatVertex, strictLess bool) bool {
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
	v    *FloatVertex
	left bool // true == points towards -dx (non-vertical partition line) or +dy (vertical partition line) (towards "lesser" vertex on a VertexPairCOrdering)
}

// An array of OrientedVertex instances
type CollinearOrientedVertices []OrientedVertex

func (x CollinearOrientedVertices) Len() int { return len(x) }
func (x CollinearOrientedVertices) Less(i, j int) bool {
	// All vertices in this set lie on the same line, which allows us to perform
	// an ordering
	// Additionally all vertices MUST have been filtered through
	// VertexMap.SelectVertexClose, meaning vertices with identical coords have
	// identical pointers
	if x[i].v == x[j].v {
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
	if len(s) > 1 {
		s = "[" + s[2:] // remove "," immediately after "["
	}
	return s + "]"
}

func (x *OrientedVertex) toString() string {
	if x == nil {
		return "nil"
	}
	str := "RIGHT"
	if x.left {
		str = "LEFT"
	}
	return fmt.Sprintf("%s%s", str, x.v.toString())
}

// When some vertex occurs multiple times with different (or maybe same)
// directions, this recognises which of these vertex occurencies should remain
// so that it is possible to infer when line states changes from going through
// void to going through level, or doesn't change at all
func (x *CollinearOrientedVertices) Coalesce() bool {
	allOk := true
	l := len(*x)
	for i := 2; i < l-1; i++ {
		// If values are equal, or roughly equal (due to rounding problems,
		// have sometimes values that are very near on one axis, but are clearly
		// meant to be same point)
		if (*x)[i].valuesEqual(&((*x)[i-1])) {
			p, hasDominant, dominantDir := x.seekPastEqualsAndNoteSign(&((*x)[i-1]), i)
			dropLen := p - i + 1
			dropStart := i
			if hasDominant {
				// State of line should have changed. Only dominant direction
				// needs to be kept, and thus only one vertex. The start of the
				// sequence inherits dominant direction, and survives
				(*x)[i-1].left = dominantDir
			} else {
				// The state of line didn't change at these coords - if it was
				// going through the void, it continues through the void, if it
				// was going through the level, it continues through the level.
				// Thus omit in entirety. Except the #0 and #last vertices of
				// entire array are not to be removed even in this case
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
			// Log.Printf("iter: %s\n", x.toString())
		}
	}
	a := (*x)[:l]
	*x = a
	return allOk
}

// valuesEqual is only valid if both vertices where returned through
// VertexMap.SelectVertexClose and so it is enough to compare references
func (ov1 *OrientedVertex) valuesEqual(ov2 *OrientedVertex) bool {
	if ov1 == nil || ov2 == nil {
		return false
	}
	if ov1.v == nil || ov2.v == nil {
		return false
	}
	// FIXME must be able to compare by reference only
	// somehow VertexMap can return distinct pointers to same vertices in
	// SelectVertexClose, what the hell
	// check vigilantbsp -na=2p=1i=2c=z -b- -r- -d watrsp.wad -o watrsp_test.wad -vvv > watrsplog.txt
	// 1-4 Sanity checks due to fluger stumbling on runs of equal-valued vertices
	// with same direction
	return ov1.v.X == ov2.v.X && ov1.v.Y == ov2.v.Y // NO!!!
	//return ov1.v == ov2.v // this is what it should be instead: pointer test
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
	for i := startPos; i < l-1; i++ {
		if !((*x)[i].valuesEqual(ov)) {
			break
		}
		if (*x)[i].left {
			leftCnt++
		} else {
			rightCnt++
		}
	}
	a += leftCnt + rightCnt - 2
	hasDominant := leftCnt != rightCnt
	dominantDir := hasDominant && (leftCnt > rightCnt) // true means left, right otherwise
	// Log.Printf("v = %s, len = %d, l: %d, r: %d, d? %t, d=%t\n", ov.toString(), a-startPos+1, leftCnt, rightCnt, hasDominant, dominantDir)
	return a, hasDominant, dominantDir
}

// And this function, unlike the GetOverlapsLength(), checks for a generic
// case of whether two line segments are overlapping or not, without any
// pre-existing knowledge
func AreOverlapping(seg1Start, seg1End, seg2Start, seg2End *FloatVertex) bool {
	return (VertexPairCOrdering(seg1Start, seg2Start, false) &&
		VertexPairCOrdering(seg2Start, seg1End, false)) ||
		(VertexPairCOrdering(seg1Start, seg2End, false) &&
			VertexPairCOrdering(seg2End, seg1End, false)) ||
		(VertexPairCOrdering(seg2Start, seg1Start, false) &&
			VertexPairCOrdering(seg1Start, seg2End, false) &&
			VertexPairCOrdering(seg2Start, seg1End, false) &&
			VertexPairCOrdering(seg1End, seg2End, false))
}

type CollinearVertexPairCByCoord []VertexPairC

func (x CollinearVertexPairCByCoord) toString() string {
	if len(x) == 0 {
		return "{EMPTY!}"
	}
	s := ""
	for i := 0; i < len(x); i++ {
		s += fmt.Sprintf("; [%s-%s]", x[i].StartVertex.toString(), x[i].EndVertex.toString())
	}
	return s
}

func (x *FloatVertex) toString() string {
	if x == nil {
		return "nil"
	}
	return replaceAfterDotZeros(fmt.Sprintf("(%f,%f)", x.X, x.Y))
}

func replaceAfterDotZeros(s string) string {
	return strings.ReplaceAll(s, ".000000", ".")
}
