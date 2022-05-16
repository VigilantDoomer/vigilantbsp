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

// diffgeometry_test.go
package main

import (
	"fmt"
	"testing"
)

func TestPartitionInBoundary(t *testing.T) {
	/* TODO Make sure all these cases always yield 2 intersection points now.
	They already should, but this needs to be in tests
		Couldn't determine point of intersection between partition line and solid internal blockmap bounding box (1, 4). Falling back to legacy way of measuring length.
	part from linedef 4720!0+0: (2781 7702) - (2800 7721) bbox: (1679 8676) - (2904 7328)
	Intersection#0: RIGHT(2904,7825)
	This cannot be! Got so far but now failing (got all the segments of line on the map to see when it goes through the void and when it does not, but failed to determine the edges of line touching the current node's bounding box)
	Couldn't determine point of intersection between partition line and solid internal blockmap bounding box (1, 4). Falling back to legacy way of measuring length.
	part from linedef 6563!0+0: (2614 7506) - (2645 7525) bbox: (1679 8676) - (2904 7328)
	Intersection#0: RIGHT(2904,7683)
	This cannot be! Got so far but now failing (got all the segments of line on the map to see when it goes through the void and when it does not, but failed to determine the edges of line touching the current node's bounding box)
	Couldn't determine point of intersection between partition line and solid internal blockmap bounding box (1, 4). Falling back to legacy way of measuring length.
	part from linedef 2129!0+0: (2455 7446) - (2510 7459) bbox: (1679 8676) - (2904 7328)
	Intersection#0: RIGHT(2904,7552)
	This cannot be! Got so far but now failing (got all the segments of line on the map to see when it goes through the void and when it does not, but failed to determine the edges of line touching the current node's bounding box)
	Couldn't determine point of intersection between partition line and solid internal blockmap bounding box (1, 4). Falling back to legacy way of measuring length.
	part from linedef 4720!0+0: (2781 7702) - (2800 7721) bbox: (1679 8676) - (2904 7328)
	Intersection#0: RIGHT(2904,7825)
	This cannot be! Got so far but now failing (got all the segments of line on the map to see when it goes through the void and when it does not, but failed to determine the edges of line touching the current node's bounding box)
	Couldn't determine point of intersection between partition line and solid internal blockmap bounding box (1, 4). Falling back to legacy way of measuring length.
	part from linedef 6563!0+0: (2614 7506) - (2645 7525) bbox: (1679 8676) - (2904 7328)
	Intersection#0: RIGHT(2904,7683)
	*/

	//
	//blXMin := 1696
	/*blXMin := 1679
	blXMax := 2904
	blYMin := 7328
	blYMax := 8676
	part := &NodeSeg{
		StartVertex: &NodeVertex{
			X: 2781,
			Y: 7702,
		},
		EndVertex: &NodeVertex{
			X: 2800,
			Y: 7721,
		},
	}*/

	blXMin := 5936
	blXMax := 6544
	blYMin := 7476
	blYMax := 7952
	part := &NodeSeg{
		StartVertex: &NodeVertex{
			X: 6388,
			Y: 7488,
		},
		EndVertex: &NodeVertex{
			X: 6384,
			Y: 7476,
		},
	}
	s := part
	s.partner = nil
	s.pex = s.EndVertex.X
	s.psx = s.StartVertex.X
	s.pdx = s.pex - s.psx
	s.pey = s.EndVertex.Y
	s.psy = s.StartVertex.Y
	s.pdy = s.pey - s.psy
	s.perp = s.pdx*s.psy - s.psx*s.pdy
	partSegCoords := part.toVertexPairC()
	var c IntersectionContext
	c.psx = s.psx
	c.psy = s.psy
	c.pex = s.pex
	c.pey = s.pey
	c.pdx = c.pex - c.psx
	c.pdy = c.pey - c.psy
	ov1, ov2 := PartitionInBoundary(part, &c, blXMax, blYMax, blXMin, blYMin, partSegCoords)
	if ov1 == nil || ov2 == nil {
		t.Errorf("ov1 = %s, ov2 = %s wanted non-nil both", ov1.toString(), ov2.toString())
	} else {
		if !tskCheckBounds(ov1.v, blXMax, blYMax, blXMin, blYMin) ||
			!tskCheckBounds(ov2.v, blXMax, blYMax, blXMin, blYMin) {
			t.Errorf("ov1 or ov2 are out of bounds")
			fmt.Printf("Info: ov1 = %s, ov2 = %s\n", ov1.toString(), ov2.toString())
		} else {
			fmt.Printf("Good ov1 = %s, ov2 = %s\n", ov1.toString(), ov2.toString())
		}
	}
}

func TestRemoveDuplicatesInCOV(t *testing.T) {
	// Fuck's sake, this was a bitch
	pts := CollinearOrientedVertices(make([]OrientedVertex, 5))
	pts[0] = OrientedVertex{
		v: &NodeVertex{
			X: 1168,
			Y: 12368,
		},
		left: true,
	}
	pts[1] = OrientedVertex{
		v: &NodeVertex{
			X: 1168,
			Y: 12368,
		},
		left: false,
	}
	pts[2] = OrientedVertex{
		v: &NodeVertex{
			X: 1168,
			Y: 12368,
		},
		left: false,
	}
	pts[3] = OrientedVertex{
		v: &NodeVertex{
			X: 1168,
			Y: 11856,
		},
		left: true,
	}
	pts[4] = OrientedVertex{
		v: &NodeVertex{
			X: 1168,
			Y: 2304,
		},
		left: false,
	}
	pts.Coalesce()
	if len(pts) != 3 {
		t.Errorf("Coalesce failed to remove duplicates, remains: %s", pts.toString())
	}
}

func TestGetIntersectionOrIndicence(t *testing.T) {
	// Partition (4128,4880)-(4272,5024) against segment (3872,4912)-(4128,4912)
	// => no intersection
	c := &IntersectionContext{
		psx: 4128,
		psy: 4880,
		pex: 4272,
		pey: 5024,
		lsx: 3872,
		lsy: 4912,
		lex: 4128,
		ley: 4912,
	}
	c.pdx = c.pex - c.psx
	c.pdy = c.pey - c.psy
	v1, v2 := c.getIntersectionOrIndicence()
	if v1 != nil || v2 != nil {
		t.Errorf("Got %s, expected v1: nil; v2: nil\n", printV1V2(v1, v2))
	}

	// Partition (5796,6836)-(5792,6832) against segment (5808,6816)-(6048,7056)
	// => no intersection
	c = &IntersectionContext{
		psx: 5796,
		psy: 6836,
		pex: 5792,
		pey: 6832,
		lsx: 5808,
		lsy: 6816,
		lex: 6048,
		ley: 7056,
	}
	c.pdx = c.pex - c.psx
	c.pdy = c.pey - c.psy
	v1, v2 = c.getIntersectionOrIndicence()
	if v1 != nil || v2 != nil {
		t.Errorf("Got %s, expected v1: nil; v2: nil\n", printV1V2(v1, v2))
	}

	// Partition (5796,6836)-(5792,6832) against segment (5808,6808)-(5808,6816)
	// => no intersection
	c = &IntersectionContext{
		psx: 5796,
		psy: 6836,
		pex: 5792,
		pey: 6832,
		lsx: 5808,
		lsy: 6808,
		lex: 5808,
		ley: 6816,
	}
	c.pdx = c.pex - c.psx
	c.pdy = c.pey - c.psy
	v1, v2 = c.getIntersectionOrIndicence()
	if v1 != nil || v2 != nil {
		t.Errorf("Got %s, expected v1: nil; v2: nil\n", printV1V2(v1, v2))
	}
}

func TestOverlapping(t *testing.T) {
	contextStart := &OrientedVertex{
		v: &NodeVertex{
			X: 2540,
			Y: 8301,
		},
		left: false,
	}
	contextEnd := &OrientedVertex{
		v: &NodeVertex{
			X: 2601,
			Y: 7852,
		},
		left: false,
	}

	nonVoid := []VertexPairC{
		VertexPairC{StartVertex: &NodeVertex{
			X: 2493,
			Y: 8646,
		},
			EndVertex: &NodeVertex{
				X: 2667,
				Y: 7370,
			},
		},
	}

	good := false
	if AreOverlapping(contextStart.v, contextEnd.v, nonVoid[0].StartVertex,
		nonVoid[0].EndVertex) {
		good = true
	}
	if !good {
		t.Errorf("Not good.\n")
	}
}

func printV1V2(v1, v2 *NodeVertex) string {
	strV1 := "v1: nil; "
	strV2 := "v2: nil"
	if v1 != nil {
		strV1 = fmt.Sprintf("v1: (%d,%d); ", v1.X, v1.Y)
	}
	if v2 != nil {
		strV2 = fmt.Sprintf("v2: (%d,%d)", v2.X, v2.Y)
	}
	return strV1 + strV2
}

// Common error in version up to and including v0.72 seems to be dropping first
// interval
// Currenty this test fails. It seems that diffgeometry.go needs quite a quality
// rewrite to deal with errors. Coalesce/fluger needs probably to go, the
// thing is how to construct intervals from the set directly
func TestCoalesce1(t *testing.T) {
	// [,LEFT(-768,-3497),RIGHT(-768,-3497),LEFT(64,-3648),LEFT(64,-3648),RIGHT(2105,-4019),LEFT(2406,-4074),RIGHT(2704,-4128),LEFT(2849,-4154),RIGHT(2880,-4160),LEFT(3328,-4241),RIGHT(3808,-4329)]
	// More dropouts! -1 -1 ; [(2105;-4019)-(2406;-4074)]; [(2704;-4128)-(2849;-4154)]; [(2880;-4160)-(3328;-4241)] [RIGHT(-640,-3520)-RIGHT(64,-3648)]
	pts := CollinearOrientedVertices(make([]OrientedVertex, 0))
	pts = append(pts, OrientedVertex{
		v: &NodeVertex{
			X: -768,
			Y: -3497,
		},
		left: true,
	},
		OrientedVertex{
			v: &NodeVertex{
				X: -768,
				Y: -3497,
			},
			left: false,
		},
		OrientedVertex{
			v: &NodeVertex{
				X: 64,
				Y: -3648,
			},
			left: true,
		},
		OrientedVertex{
			v: &NodeVertex{
				X: 64,
				Y: -3648,
			},
			left: true,
		},
		OrientedVertex{
			v: &NodeVertex{
				X: 2105,
				Y: -4019,
			},
			left: false,
		},
		OrientedVertex{
			v: &NodeVertex{
				X: 2406,
				Y: -4704,
			},
			left: true,
		},
		OrientedVertex{
			v: &NodeVertex{
				X: 2704,
				Y: -4128,
			},
			left: false,
		},
		OrientedVertex{
			v: &NodeVertex{
				X: 2849,
				Y: -4154,
			},
			left: true,
		},
		OrientedVertex{
			v: &NodeVertex{
				X: 2880,
				Y: -4160,
			},
			left: false,
		},
		OrientedVertex{
			v: &NodeVertex{
				X: 3328,
				Y: -4241,
			},
			left: true,
		},
		OrientedVertex{
			v: &NodeVertex{
				X: 3808,
				Y: -4329,
			},
			left: false,
		})
	pts.Coalesce()
	fmt.Println(pts.toString())
	if pts[0].left != false || pts[0].v.X != -768 || pts[0].v.Y != -3497 {
		t.Errorf("Wrong #0 point\n")
	}
}
