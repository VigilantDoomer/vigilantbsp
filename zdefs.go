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

// zdefs.go
package main

import (
	"encoding/binary"
	"math"
)

// -----------------------------------------------------------------------------
// Block of pragma directives
//
// The pragmas aren't part of Go languages and are not parsed by Go compiler or
// the go build command. Instead, go generate will be used to call a special
// program I wrote that will parse source code into ASTs, apply modifications to
// it, and then produce a new file (see gen/codegen.go in VigilantBSP source
// tree)
// The idea is to avoid having separate SOURCE code for vanilla/Deep nodes and
// Zdoom extended nodes (where all updates and bugfixes would have to be
// duplicated by human between two), but not make vanilla node generation any
// slower and not worsen its quality as well YET, because of the latter, one has
// to acknowledge that this necessiates different running algorithms for vanilla
// case and Zdoom extended nodes case.
// So, the SHARED code will be written for vanilla case, while Zdoom extended
// case will be generated from it + incorporate some minor stuff that is unique
// to it
// -----------------------------------------------------------------------------

// All entities (types, functions) generated automatically will have the
// specified prefix
//
// #pragma setprefix "ZExt_"

// Type replacements will also recursively replace all structs using them with
// automatically generated structs using new types for fields that were using
// the replaced types
//
// #pragma replace Number with ZNumber
// #pragma replace WideNumber with ZWideNumber

// Some functions calls shall be replaced with pre-defined functions, instead
// of functions generated from replaced ones
//
// #pragma replace Number.Trunc with ZNumber.Trunc
// #pragma replace RoundToPrecision with ZRoundToPrecision
// #pragma replace DiffSign with ZDiffSign
// #pragma replace Number.Abs with ZNumber.Abs
// #pragma replace Number.Ceil with ZNumber.Ceil
// #pragma replace Number.Floor with ZNumber.Floor

// And some functions need to be generated, but from a different function than
// the one it replaces
//
// #pragma replace_prototype *NodesWork.AddVertex with *NodesWork.ZAddVertex_Proto
// #pragma replace_prototype *NodesWork.CreateSSector with *NodesWork.ZCreateSSector_Proto
// #pragma replace_prototype PointOnLineSide with ZPointOnLineSide_Proto
// #pragma replace_prototype *NodesWork.PassingTooClose with *NodesWork.ZPassingTooClose_Proto
// #pragma replace_prototype *IntersectionContext.computeIntersection with *IntersectionContext.ZcomputeIntersection_Proto
// #pragma replace_prototype doLinesIntersectStandard with ZdoLinesIntersect_Proto
// #pragma replace_prototype *NodesWork.SetNodeCoords with *NodesWork.ZSetNodeCoords_Proto
// #pragma replace_prototype *NodesWork.updateSegLenBetter with *NodesWork.ZupdateSegLenBetter_Proto
// #pragma replace_prototype *NodesWork.SegOrLineToVertexPairC with *NodesWork.ZSegOrLineToVertexPairC_Proto
// #pragma replace_prototype *NodesWork.upgradeToDeep with *NodesWork.ZupgradeToDeep_Proto
// #pragma replace_prototype vetAliasTransfer with ZVetAliasTransfer_Proto
// #pragma replace_prototype vetAliasTransfer2 with ZVetAliasTransfer2_Proto
// #pragma replace_prototype *NodesWork.tooManySegsCantFix with *NodesWork.ZtooManySegsCantFix_Proto
// #pragma replace_prototype *NodesWork.getZdoomNodesBytes with *NodesWork.ZgetZdoomNodesBytes_Proto
// #pragma replace_prototype *NodesWork.reverseNodes with *NodesWork.ZreverseNodes_Proto
// #pragma replace_prototype *NodesWork.convertNodesStraight with *NodesWork.ZconvertNodesStraight_Proto

// Finally, we need to assign the core function - which will include all other
// generated stuff in its calltree - to a predefined callback. The generated
// code will perform the assignment in init() function
//
// #pragma init ZNodesGenerator with morphed NodesGenerator

//// debugging - these are local variables that should not be found: #pragma init sectorEquiv with morphed solidMap

// -----------------------------------------------------------------------------
// End block of pragma directives
// -----------------------------------------------------------------------------

type ZNumber float64

type ZWideNumber float64

type NodesGeneratorWorker = func(input *NodesInput)

// ZDBSP: "vertices within this distance of each other are considered to be
// the same vertex".
const VERTEX_EPSILON = 6.0 / 65536.0

const UNIT_FRACTION = 1.0 / 65536.0

// ZDBSP: "points within this distance of a line will be considered on the line"
// Used in doLinesIntersect-like side checker for extended nodes
// Note: ZDBSP value was 6.5536 of type double, but the constant was used in
// context where fixed-point values were directly cast to double (and thus
// values were scaled by 65536 exactly). The value has to be descaled or you
// will see bugs caused by your subsectors not really being convex
const SIDE_EPSILON = float64(0.0001)

// FAR_ENOUGH is Zdoom const and likely needs to be modified, as the original
// was most likely used together with scaled values
const FAR_ENOUGH = float64(17179869184)

// This callback must be overriden in init section of a go source file that is
// automatically generated
var ZNodesGenerator NodesGeneratorWorker = nil

// On ZNumber, this returns x unchanged. The value of n is ignored.
func (n ZNumber) Trunc(x float64) float64 {
	return x
}

func ZRoundToPrecision(n float64) ZNumber {
	return ZNumber(n)
}

func ZDiffSign(a, b ZNumber) bool {
	// I am assuming I don't have to deal with negative zero
	return math.Signbit(float64(a)) != math.Signbit(float64(b))
}

func (n ZNumber) Abs() ZNumber {
	return ZNumber(math.Abs(float64(n)))
}

func (n ZNumber) Ceil() int {
	return int(math.Ceil(float64(n)))
}

func (n ZNumber) Floor() int {
	return int(math.Floor(float64(n)))
}

// Adds a new vertex at specified position (or might found an existing one and
// return it).
func (w *NodesWork) ZAddVertex_Proto(x, y Number) *NodeVertex {
	v := w.vertexMap.SelectVertexClose(float64(x), float64(y))
	if v.Id != -1 { // already exists
		w.vertexExists++
		if w.vertexSink != nil {
			w.vertexSink = append(w.vertexSink, v.Id)
		}
		return &(w.vertices[v.Id])
	}

	// note that v.X and v.Y need not match the original x, y !
	idx := len(w.vertices)
	v.Id = idx // our responsibility to update!
	// Here, we are not calling w.lines.AddVertex, as ZDoom nodes don't add
	// additional vertices to vertices lump but to it's own lump instead
	w.vertices = append(w.vertices, NodeVertex{
		X:   Number(v.X),
		Y:   Number(v.Y),
		idx: uint32(idx),
	})
	if w.vertexSink != nil {
		w.vertexSink = append(w.vertexSink, idx)
	}
	return &(w.vertices[idx])
}

// Adds a subsector. Version strictly for Zdoom nodes
func (w *NodesWork) ZCreateSSector_Proto(tmps *NodeSeg) uint32 {
	subsectorIdx := uint32(len(w.zdoomSubsectors))
	oldNumSegs := uint32(len(w.zdoomSegs))

	w.totals.numSSectors++
	var currentCount uint32
	for ; tmps != nil; tmps = tmps.next {
		tmps.block = nil
		tmps.nextInSuper = nil
		w.zdoomSegs = append(w.zdoomSegs, ZdoomNode_Seg{
			StartVertex: uint32(tmps.StartVertex.idx),
			EndVertex:   uint32(tmps.EndVertex.idx),
			Linedef:     tmps.Linedef,
			Flip:        byte(tmps.getFlip()),
		})
	}
	currentCount = uint32(len(w.zdoomSegs)) - oldNumSegs
	if currentCount > w.totals.maxSegCountInSubsector {
		w.totals.maxSegCountInSubsector = currentCount
	}
	w.totals.numSegs += currentCount
	w.zdoomSubsectors = append(w.zdoomSubsectors, currentCount)
	return subsectorIdx
}

// Returns -1 for left, +1 for right, or 0 for intersect.
func ZPointOnLineSide_Proto(part *NodeSeg, x, y int) int {
	perp := UtilPerpDist_Float64(part, float64(x), float64(y))
	ab, sgn := Float64AbsAndSign(perp)
	if ab <= DIST_EPSILON {
		return 0
	}
	if sgn {
		return -1
	}
	return +1
}

func UtilPerpDist_Float64(part *NodeSeg, x, y float64) float64 {
	return (x*float64(part.pdy) - y*float64(part.pdx) +
		float64(part.perp)) / float64(part.len)
}

func Float64AbsAndSign(perp float64) (float64, bool) {
	return math.Abs(perp), math.Signbit(perp)
}

// ZPassingTooClose_Proto checks whether partition crosses check seg too close
// near its end points to mean trouble. Based on algorithm in ZDBSP
// Caller must guarantee that part and check DO cross and are not in some
// other state related to each other.
// If it returns true, partition should not be chosen. This occurs when the
// crossing point would equal actual point after rounding or "close vertex"
// lookup on MapVertex
// It may modify cost and minors as well. This occurs when the crossing point
// produces short-length seg that is nonetheless a valid occurence (gray zone)
// P.S. This correction is NOT yet proved essential. It has nothing to do with
// fixing bug in Water Spirit map02 that v0.75a released with
func (w *NodesWork) ZPassingTooClose_Proto(part, check *NodeSeg, cost *int,
	minors *MinorCosts) bool {
	frac := InterceptVector(part, check)
	if frac < 0.001 || frac > 0.999 {
		x := float64(check.psx)
		y := float64(check.psy)
		x = math.FMA(frac, float64(check.pex)-x, x)
		y = math.FMA(frac, float64(check.pey)-y, y)
		if math.Abs(x-float64(check.psx)) <= VERTEX_EPSILON &&
			math.Abs(y-float64(check.psy)) <= VERTEX_EPSILON {
			return true
		}
		if math.Abs(x-float64(check.pex)) <= VERTEX_EPSILON &&
			math.Abs(y-float64(check.pey)) <= VERTEX_EPSILON {
			return true
		}
		// TODO increase cost or minors?
	}
	return false
}

func InterceptVector(part, check *NodeSeg) float64 {
	return InterceptVectorCoord(float64(part.psx), float64(part.psy),
		float64(part.pdx), float64(part.pdy),
		float64(check.psx), float64(check.psy), float64(check.pdx),
		float64(check.pdy))
}

func InterceptVectorCoord(partPSX, partPSY, partPDX, partPDY, checkPSX, checkPSY,
	checkPDX, checkPDY float64) float64 {
	v2x := checkPSX
	v2y := checkPSY
	v2dx := checkPDX
	v2dy := checkPDY
	v1dx := partPDX
	v1dy := partPDY
	// den := part.pdy * check.pdx - part.pdx * check.pdy
	den := v1dy*v2dx - v1dx*v2dy
	if den == 0.0 {
		return 0.0
	}
	v1x := partPSX
	v1y := partPSY
	num := (v1x-v2x)*v1dy + (v2y-v1y)*v1dx
	return num / den
}

// ZcomputeIntersectionProto calculates the point of intersection of two lines
// for extended nodes format. It is actually rewritten based on ZDBSP code,
// ps?->pe? & ls?->le?
// returns xcoord float64, ycoord float64
func (c *IntersectionContext) ZcomputeIntersection_Proto() (ZNumber, ZNumber) {
	frac := InterceptVectorCoord(float64(c.psx), float64(c.psy), float64(c.pdx),
		float64(c.pdy), float64(c.lsx), float64(c.lsy), float64(c.lex-c.lsx),
		float64(c.ley-c.lsy))

	newx := float64(c.lsx)
	newy := float64(c.lsy)
	newx = math.FMA(frac, float64(c.lex)-newx, newx)
	newy = math.FMA(frac, float64(c.ley)-newy, newy)

	return ZNumber(newx), ZNumber(newy)
}

// ZdoLinesIntersect_Proto is distinct from original Lee Killough's function,
// modified with insight from ZDBSP. The updates are crucial to avoid
// significant errors when generating extended nodes for certain maps, as some
// of Lee Killough's assumptions (such as intersection point being closer than
// 2 pixels in distance to either end of the seg means point is on that end)
// are no longer valid
// TODO ZDBSP also checks for a and b having big values. Is it only because
// it uses scaled values (casting coords represented via 16.16 fixed point
// to float)? Investigate. See ClassifyLine2 in "nodebuild_classify_nosse2.cpp"
// in ZDBSP source
func ZdoLinesIntersect_Proto(c *IntersectionContext) uint8 {
	dx2 := c.psx - c.lsx // Checking line -> partition
	dy2 := c.psy - c.lsy
	dx3 := c.psx - c.lex
	dy3 := c.psy - c.ley

	a := c.pdy*dx2 - c.pdx*dy2
	b := c.pdy*dx3 - c.pdx*dy3
	if DiffSign(a, b) && (a != 0) && (b != 0) { // Line is split, just check that
		x, y := c.computeIntersection()
		dx2 = c.lsx - x // Find distance from line start
		dy2 = c.lsy - y // to split point
		if dx2 == 0 && dy2 == 0 {
			a = 0
		} else {
			// This is ZDBSP way to determine when point is too close to
			// end of line. Replaces old Killough's assumption (and fixes
			// bug in building map02 in Water Spirit)
			cmp := float64(a) * float64(a) / float64(c.pdx*c.pdx+c.pdy*c.pdy)
			if cmp < SIDE_EPSILON*SIDE_EPSILON {
				a = 0
			}
		}
		dx3 = c.lex - x // Find distance from line end
		dy3 = c.ley - y // to split point
		if dx3 == 0 && dy3 == 0 {
			b = 0
		} else {
			// This is ZDBSP way...
			cmp := float64(b) * float64(b) / float64(c.pdx*c.pdx+c.pdy*c.pdy)
			if cmp < SIDE_EPSILON*SIDE_EPSILON { // same as start of line
				b = 0
			}
		}

	}

	var val uint8

	if a == 0 {
		val = val | 16 // start is on middle
	} else if a < 0 {
		val = val | 32 // start is on left side
	} else {
		val = val | 64 // start is on right side
	}

	if b == 0 {
		val = val | 1 // end is on middle
	} else if b < 0 {
		val = val | 2 // end is on left side
	} else {
		val = val | 4 // end is on right side
	}

	return val
}

func (w *NodesWork) ZSetNodeCoords_Proto(part *NodeSeg, bbox *NodeBounds,
	c *IntersectionContext) {
	// Node structure in extended nodes still uses 16-bit integer values, not
	// 16.16 fixed-point
	// To avoid messing with imprecise coords, let's try source linedef coords
	// first? Both AJ-BSP and ZDBSP use them, doesn't seem to be important if
	// linedef is within bounds or not
	x1, y1, x2, y2 := w.lines.GetAllXY(part.Linedef)
	if part.getFlip() != 0 {
		w.nodeX = x2
		w.nodeY = y2
		w.nodeDx = x1 - x2
		w.nodeDy = y1 - y2
	} else {
		w.nodeX = x1
		w.nodeY = y1
		w.nodeDx = x2 - x1
		w.nodeDy = y2 - y1
	}
	if w.nodeDx <= 32767 && w.nodeDy <= 32767 && w.nodeDx >= -32768 &&
		w.nodeDy >= -32768 {
		return
	}
	oldDx := w.nodeDx
	oldDy := w.nodeDy
	oldX := w.nodeX
	oldY := w.nodeY
	psx, psy := part.StartVertex.X, part.StartVertex.Y
	pex, pey := part.EndVertex.X, part.EndVertex.Y
	pexc, peyc := pex.Ceil(), pey.Ceil()
	w.nodeX = int(psx)
	w.nodeY = int(psy)
	w.nodeDx = pexc - w.nodeX
	w.nodeDy = peyc - w.nodeY
	// Is precise coords?
	if float64(w.nodeX) == float64(psx) && float64(w.nodeY) == float64(psy) &&
		float64(pexc) == float64(pex) && float64(peyc) == float64(pey) {
		// Yes, but what about overflow?
		if w.nodeDx <= 32767 && w.nodeDy <= 32767 && w.nodeDx >= -32768 &&
			w.nodeDy >= -32768 {
			// Avoided - good
			Log.Verbose(1, "I avoided overflow in node values by chosing seg coords instead of linedef coords (line too long). Source linedef: %d\n",
				part.Linedef)
			return
		}
	}

	// Restore values from partition line since the segment-derived ones were
	// bad
	w.nodeDx = oldDx
	w.nodeDy = oldDy
	w.nodeX = oldX
	w.nodeY = oldY
	XEnd := w.nodeX + w.nodeDx
	YEnd := w.nodeY + w.nodeDy

	// Either coords were not precise, or segment was also long
	// It looks like the only option is to scale down this partition line

	// Scale down via integer division, if can do _with zero remainders_
	dd := GCD(Abs(w.nodeDx), Abs(w.nodeDy))
	if dd != 1 {
		w.nodeDx = w.nodeDx / dd
		w.nodeDy = w.nodeDy / dd
		if dd > 2 {
			// Distance from partition line to other segs - if it is large - can
			// cause numerical overflows and visual glitches in run-time even in
			// advanced ports (PrBoom-plus etc). Let's try to move it closer
			// to the center of node's bounding box
			pCenter := ProjectBoxCenterOntoLine(bbox, w.nodeX, w.nodeY, XEnd,
				YEnd)
			dcx := int(pCenter.X) - w.nodeX
			dcy := int(pCenter.Y) - w.nodeY
			if w.nodeDx == 0 && w.nodeDy == 0 {
				// just make sure there is no crash (I don't expect this to ever
				// happen)
				Log.Verbose(1, "Really strange happenings - partition line is a point\n")
				return
			}
			if (Sign(dcx) != Sign(w.nodeDx)) ||
				(Sign(dcy) != Sign(w.nodeDy)) {
				Log.Verbose(2, "Signs don't match (move after scale op cancelled): dcx,dcy=(%d,%d) dx,dy=(%d,%d)\n",
					dcx, dcy, w.nodeDx, w.nodeDy)
				return
			}
			var d2 int
			if Abs(w.nodeDx) > Abs(w.nodeDy) {
				d2 = dcx / w.nodeDx
			} else {
				d2 = dcy / w.nodeDy
			}
			w.nodeX += d2 * w.nodeDx
			w.nodeY += d2 * w.nodeDy
		}
	}

	if w.nodeDx <= 32767 && w.nodeDy <= 32767 && w.nodeDx >= -32768 &&
		w.nodeDy >= -32768 {
		Log.Verbose(1, "Prevented partition line coords overflow (from segment of linedef %d).\n",
			part.Linedef)
		return
	}

	// Could do nothing
	// TODO could be acceptable enough if I scale down with rounding? Not sure -
	// the angle would be different and that should be probably considered when
	// sorting/splitting segs to each side?

	Log.Verbose(1, "Overflow: partition line DX=%d, DY=%d (from linedef %d) can not be represented correctly due to (-32768,32767) signed int16 range limit in format. Parts of map will not be rendered correctly in any port.\n",
		w.nodeDx, w.nodeDy, part.Linedef)
}

// ProjectBoxCenterOntoLine returns a point that is perpendicular projection of
// center point inside bbox onto line segment defined by x/y coords
func ProjectBoxCenterOntoLine(bbox *NodeBounds, x1, y1, x2, y2 int) *FloatVertex {
	centerX := float64(bbox.Xmin+bbox.Xmax) / 2
	centerY := float64(bbox.Ymin+bbox.Ymax) / 2
	return ProjectPointOntoLine(centerX, centerY, float64(x1), float64(y1),
		float64(x2), float64(y2))
}

// ProjectPointOntoLine returns a point that is perpendicular projection of
// point onto line defined by x/y coords.
// Implementation sourced from
// www.sunshine2k.de/coding/java/PointOnLine/PointOnLine.html (2022/10/27) under
// terms of MIT license copyright Bastian Molkenthin
// (www.sunshine2k.de/license.html)
func ProjectPointOntoLine(px, py float64, x1, y1, x2, y2 float64) *FloatVertex {
	e1x := x2 - x1
	e1y := y2 - y1
	e2x := px - x1
	e2y := py - y1
	dp := e1x*e2x + e1y*e2y   // dot product of e1 * e2
	len2 := e1x*e1x + e1y*e1y // length squared
	return &FloatVertex{
		X: x1 + (dp*e1x)/len2,
		Y: y1 + (dp*e1y)/len2,
	}
}

// Override for updateSegLenBetter, which updates seg len field for each seg
// creates as a result of the split.
func (w *NodesWork) ZupdateSegLenBetter_Proto(s *NodeSeg) {
	dy := float64(s.pey - s.psy)
	dx := float64(s.pex - s.psx)
	s.len = Number(math.Sqrt(dx*dx + dy*dy))
}

// Override for SegOrLineToFutureVertexPairC, it so happens that slight changes
// to partition seg coords occuring during line splitting in extended nodes can
// break line tracing (even though it is a rather rare occurence). Nonetheless,
// the precision of extended nodes was intended to keep lines angle almost
// unchanged, so let's use original linedef coords for tracing line through void
// and non-void
func (w *NodesWork) ZSegOrLineToVertexPairC_Proto(part *NodeSeg) VertexPairC {
	x1, y1, x2, y2 := w.lines.GetAllXY(part.Linedef)
	sv := &FloatVertex{
		X: float64(x1),
		Y: float64(y1),
	}
	ev := &FloatVertex{
		X: float64(x2),
		Y: float64(y2),
	}
	dx := float64(x2 - x1)
	dy := float64(y2 - y1)
	l := Number(math.Sqrt(dx*dx + dy*dy))
	if VertexPairCOrdering(sv, ev, false) {
		return VertexPairC{
			StartVertex: sv,
			EndVertex:   ev,
			len:         l,
		}
	} else { // swap
		return VertexPairC{
			StartVertex: ev,
			EndVertex:   sv,
			len:         l,
		}
	}
}

// In extended nodes mode, the used intersection evaluator is accurate enough
func ZVetAliasTransfer_Proto(c *IntersectionContext) bool {
	return true
}

func ZVetAliasTransfer2_Proto(part, check *NodeSeg) bool {
	if check.len >= 4 {
		return true
	}
	c := &IntersectionContext{
		psx: part.psx,
		psy: part.psy,
		pex: part.pex,
		pey: part.pey,
	}
	c.pdx = c.psx - c.pex
	c.pdy = c.psy - c.pey
	c.lsx = check.psx
	c.lsy = check.psy
	c.lex = check.pex
	c.ley = check.pey
	val := doLinesIntersectStandard(c) // will be intercepted of course
	return (val&1 != 0) && (val&16 != 0)
}

func (w *NodesWork) ZupgradeToDeep_Proto() {
	// Unused; deleted to shrink executable
}

func (w *NodesWork) ZtooManySegsCantFix_Proto(dryRun bool) bool {
	return false
}

func (w *NodesWork) ZgetZdoomNodesBytes_Proto() []byte {
	w.zdoomVertexHeader.NumExtendedVertices = uint32(len(w.vertices) -
		int(w.zdoomVertexHeader.ReusedOriginalVertices))
	w.zdoomVertices = make([]ZdoomNode_Vertex,
		w.zdoomVertexHeader.NumExtendedVertices)
	for i, srcv := range w.vertices[w.zdoomVertexHeader.ReusedOriginalVertices:] {
		w.zdoomVertices[i].X = srcv.X.ToFixed16Dot16()
		w.zdoomVertices[i].Y = srcv.Y.ToFixed16Dot16()
	}
	var writ *ZStream
	if w.nodeType == NODETYPE_ZDOOM_COMPRESSED {
		writ = CreateZStream(ZNODES_COMPRESSED_SIG[:], true)
	} else {
		writ = CreateZStream(ZNODES_PLAIN_SIG[:], false)
	}
	// NOTE always LittleEndian per Zdoom specs
	vertexHeader := *(w.zdoomVertexHeader)
	binary.Write(writ, binary.LittleEndian, vertexHeader)
	binary.Write(writ, binary.LittleEndian, w.zdoomVertices)
	binary.Write(writ, binary.LittleEndian, uint32(len(w.zdoomSubsectors)))
	binary.Write(writ, binary.LittleEndian, w.zdoomSubsectors)
	binary.Write(writ, binary.LittleEndian, uint32(len(w.zdoomSegs)))
	binary.Write(writ, binary.LittleEndian, w.zdoomSegs)
	binary.Write(writ, binary.LittleEndian, uint32(len(w.deepNodes)))
	binary.Write(writ, binary.LittleEndian, w.deepNodes)
	ret, err := writ.FinalizeAndGetBytes()
	if err != nil {
		Log.Panic("IO error at writing Zdoom nodes stream: %s\n", err.Error())
	}
	return ret
}

func (w *NodesWork) ZreverseNodes_Proto(node *NodeInProcess) uint32 {
	// unreachable function dummied out to reduce executable size
	return uint32(0)
}

func (w *NodesWork) ZconvertNodesStraight_Proto(node *NodeInProcess, idx uint32) uint32 {
	// unreachable function dummied out to reduce executable size
	return uint32(0)
}
