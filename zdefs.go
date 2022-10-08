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

// zdefs.go
package main

import (
	"fmt"
	"math"
)

// -----------------------------------------------------------------------------
// Block of pragma directives
//
// The pragmas aren't part of Go languages and are not parsed by Go compiler or
// the go build command. Instead, go generate will be used to call a special
// program I wrote that will parse source code into ASTs, apply modifications to
// it, and then produce a new file
// TODO the generator is NOT written yet - it is in the PLANS. The idea is to
// avoid having separate SOURCE code for vanilla/Deep nodes and Zdoom extended
// nodes, but not make vanilla node generation any slower and not worsen its
// quality as well YET one has to acknowledge that this necessiates different
// running algorithms for vanilla case and Zdoom extended nodes case.
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

// And some functions need to be generated, but from a different function than
// the one it replaces
//
// #pragma replace_prototype *NodesWork.AddVertex with *NodesWork.ZAddVertex_Proto
// #pragma replace_prototype *NodesWork.CreateSSector with *NodesWork.ZCreateSSector_Proto
// #pragma replace_prototype PointOnLineSide with ZPointOnLineSide_Proto
// #pragma replace_prototype *MyLogger.DumpSegs with *MyLogger.ZDumpSegs_Proto

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
	return (a > 0 && b < 0) || (a < 0 && b > 0)
}

func (n ZNumber) Abs() ZNumber {
	if n < ZNumber(0.0) {
		return -n
	} else if n > ZNumber(0.0) {
		return n
	} else {
		return 0
	}
}

func (n ZNumber) Ceil() int {
	iN := int(n)
	if ZNumber(iN) == n {
		return iN
	}
	return iN + 1
}

// Adds a new vertex at specified position (or might found an existing one and
// return it).
func (w *NodesWork) ZAddVertex_Proto(x, y Number) *NodeVertex {
	idx := len(w.vertices)
	// Here, we are not calling w.lines.AddVertex, as ZDoom nodes don't add
	// additional vertices to vertices lump but to it's own lump instead
	w.vertices = append(w.vertices, NodeVertex{
		X:   x,
		Y:   y,
		idx: uint32(idx),
	})
	return &(w.vertices[idx])
}

// Adds a subsector. Version strictly for Zdoom nodes
func (w *NodesWork) ZCreateSSector_Proto(tmps *NodeSeg) uint32 {
	subsectorIdx := uint32(len(w.zdoomSubsectors))
	oldNumSegs := uint32(len(w.zdoomSegs))

	w.totals.numSSectors++
	var currentCount uint32
	for ; tmps != nil; tmps = tmps.next {
		w.zdoomSegs = append(w.zdoomSegs, ZdoomNode_Seg{
			StartVertex: uint32(tmps.StartVertex.idx),
			EndVertex:   uint32(tmps.EndVertex.idx),
			Linedef:     tmps.Linedef,
			Flip:        byte(tmps.Flip),
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

func (log *MyLogger) ZDumpSegs_Proto(ts *NodeSeg) {
	if !config.DumpSegsFlag || ts == nil { // reference to global: config
		return
	}
	// Assume all come from same sector
	allSector := ts.sector
	log.segs.WriteString(fmt.Sprintf("Sector #%d:\n", allSector))
	for tmps := ts; tmps != nil; tmps = tmps.next {
		log.segs.WriteString(fmt.Sprintf(
			"  Linedef: %d Flip: %d (%f,%f) - (%f, %f)",
			tmps.Linedef, tmps.Flip, tmps.StartVertex.X, tmps.StartVertex.Y,
			tmps.EndVertex.X, tmps.EndVertex.Y))
		if tmps.sector != allSector {
			// Is not supposed to write stuff from multiple sectors. You'll have
			// to rewrite code in this function to adjust it to your use case
			log.segs.WriteString(fmt.Sprintf(" BAD! Sector = %d\n", tmps.sector))
		} else {
			log.segs.WriteString("\n")
		}
	}
}
