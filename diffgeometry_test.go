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
	partSegCoords := part.toIntVertexPairC()
	var c IntersectionContext
	c.psx = s.psx
	c.psy = s.psy
	c.pex = s.pex
	c.pey = s.pey
	c.pdx = c.pex - c.psx
	c.pdy = c.pey - c.psy
	ov1, ov2 := IntPartitionInBoundary(part, &c, blXMax, blYMax, blXMin, blYMin, partSegCoords)
	if ov1 == nil || ov2 == nil {
		t.Errorf("ov1 = %s, ov2 = %s wanted non-nil both", ov1.toString(), ov2.toString())
	} else {
		if !intTskCheckBounds(ov1.v, blXMax, blYMax, blXMin, blYMin) ||
			!intTskCheckBounds(ov2.v, blXMax, blYMax, blXMin, blYMin) {
			t.Errorf("ov1 or ov2 are out of bounds")
			fmt.Printf("Info: ov1 = %s, ov2 = %s\n", ov1.toString(), ov2.toString())
		} else {
			fmt.Printf("Good ov1 = %s, ov2 = %s\n", ov1.toString(), ov2.toString())
		}
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
	v1, v2 := c.intGetIntersectionOrIndicence()
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
	v1, v2 = c.intGetIntersectionOrIndicence()
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
	v1, v2 = c.intGetIntersectionOrIndicence()
	if v1 != nil || v2 != nil {
		t.Errorf("Got %s, expected v1: nil; v2: nil\n", printV1V2(v1, v2))
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

func TestCoalesce1(t *testing.T) {
	// [,LEFT(-768,-3497),RIGHT(-768,-3497),LEFT(64,-3648),LEFT(64,-3648),RIGHT(2105,-4019),LEFT(2406,-4074),RIGHT(2704,-4128),LEFT(2849,-4154),RIGHT(2880,-4160),LEFT(3328,-4241),RIGHT(3808,-4329)]
	// More dropouts! -1 -1 ; [(2105;-4019)-(2406;-4074)]; [(2704;-4128)-(2849;-4154)]; [(2880;-4160)-(3328;-4241)] [RIGHT(-640,-3520)-RIGHT(64,-3648)]
	pts := CollinearOrientedVertices(make([]OrientedVertex, 0))
	pts = append(pts, OrientedVertex{
		v: &FloatVertex{
			X: -768,
			Y: -3497,
		},
		left: true,
	},
		OrientedVertex{
			v: &FloatVertex{
				X: -768,
				Y: -3497,
			},
			left: false,
		},
		OrientedVertex{
			v: &FloatVertex{
				X: 64,
				Y: -3648,
			},
			left: true,
		},
		OrientedVertex{
			v: &FloatVertex{
				X: 64,
				Y: -3648,
			},
			left: true,
		},
		OrientedVertex{
			v: &FloatVertex{
				X: 2105,
				Y: -4019,
			},
			left: false,
		},
		OrientedVertex{
			v: &FloatVertex{
				X: 2406,
				Y: -4704,
			},
			left: true,
		},
		OrientedVertex{
			v: &FloatVertex{
				X: 2704,
				Y: -4128,
			},
			left: false,
		},
		OrientedVertex{
			v: &FloatVertex{
				X: 2849,
				Y: -4154,
			},
			left: true,
		},
		OrientedVertex{
			v: &FloatVertex{
				X: 2880,
				Y: -4160,
			},
			left: false,
		},
		OrientedVertex{
			v: &FloatVertex{
				X: 3328,
				Y: -4241,
			},
			left: true,
		},
		OrientedVertex{
			v: &FloatVertex{
				X: 3808,
				Y: -4329,
			},
			left: false,
		})
	pts.Coalesce()
	fmt.Println(pts.toString())
	if pts[1].left != false || pts[0].v.X != -768 || pts[0].v.Y != -3497 {
		t.Errorf("Wrong #1 point\n")
	}
}

func TestCoalesce2(t *testing.T) {
	// [RIGHT(-1536.,5120.),LEFT(-1360.,5120.),RIGHT(-1344.,5120.),LEFT(-1168.,5120.),RIGHT(-1168.,5120.),LEFT(-1152.,5120.),RIGHT(-1152.,5120.),LEFT(-784.,5120.),LEFT(-784.,5120.),RIGHT(-784.,5120.),RIGHT(-784.,5120.),LEFT(-736.,5120.),RIGHT(-736.,5120.),LEFT(-528.,5120.),LEFT(-528.,5120.),RIGHT(-384.,5120.),RIGHT(-384.,5120.),LEFT(-128.,5120.),LEFT(-128.,5120.),RIGHT(0.,5120.),RIGHT(0.,5120.),LEFT(256.,5120.),LEFT(256.,5120.),RIGHT(384.,5120.),RIGHT(384.,5120.),LEFT(650.666667,5120.),RIGHT(756.444444,5120.),LEFT(1024.,5120.),RIGHT(1207.157895,5120.),LEFT(1416.421053,5120.),RIGHT(1488.,5120.),RIGHT(1488.,5120.),LEFT(2609.560976,5120.)]
	// More dropouts! -1 -1 ; [LEFT(-1536.,5120.),LEFT(-1360.,5120.),RIGHT(-1344.,5120.),RIGHT(-784.,5120.),LEFT(-528.,5120.),RIGHT(-384.,5120.),LEFT(-128.,5120.),RIGHT(0.,5120.),LEFT(256.,5120.),RIGHT(384.,5120.),LEFT(650.666667,5120.),RIGHT(756.444444,5120.),LEFT(1024.,5120.),RIGHT(1207.157895,5120.),LEFT(1416.421053,5120.),RIGHT(1488.,5120.),LEFT(2609.560976,5120.),RIGHT(4800.,5120.)]
	dgVertexMap := CreateVertexMap(&NodesWork{}, -1536, -688, 4800, 6272)
	pts := CollinearOrientedVertices(make([]OrientedVertex, 0))
	pts = append(pts, OrientedVertex{
		v:    dgVertexMap.SelectVertexClose(-1536, 5120),
		left: false,
	},
		OrientedVertex{
			v:    dgVertexMap.SelectVertexClose(-1360, 5120),
			left: true,
		},
		OrientedVertex{
			v:    dgVertexMap.SelectVertexClose(-1344, 5120),
			left: false,
		},
		OrientedVertex{
			v:    dgVertexMap.SelectVertexClose(-1168, 5120),
			left: true,
		},
		OrientedVertex{
			v:    dgVertexMap.SelectVertexClose(-1168, 5120),
			left: false,
		},
		OrientedVertex{
			v:    dgVertexMap.SelectVertexClose(-1152, 5120),
			left: true,
		},
		OrientedVertex{
			v: dgVertexMap.SelectVertexClose(-1152, 5120),

			left: false,
		},
		OrientedVertex{
			v:    dgVertexMap.SelectVertexClose(-784, 5120),
			left: true,
		},
		OrientedVertex{
			v:    dgVertexMap.SelectVertexClose(-784, 5120),
			left: true,
		},
		OrientedVertex{
			v:    dgVertexMap.SelectVertexClose(-784, 5120),
			left: false,
		},
		OrientedVertex{
			v:    dgVertexMap.SelectVertexClose(-784, 5120),
			left: false,
		},
		OrientedVertex{
			v:    dgVertexMap.SelectVertexClose(-736, 5120),
			left: true,
		},
		OrientedVertex{
			v:    dgVertexMap.SelectVertexClose(-736, 5120),
			left: false,
		},
		OrientedVertex{
			v:    dgVertexMap.SelectVertexClose(-528, 5120),
			left: true,
		},
		OrientedVertex{
			v:    dgVertexMap.SelectVertexClose(-528, 5120),
			left: true,
		},
		OrientedVertex{
			v:    dgVertexMap.SelectVertexClose(-384, 5120),
			left: false,
		},
		OrientedVertex{
			v:    dgVertexMap.SelectVertexClose(-384, 5120),
			left: false,
		},
		OrientedVertex{
			v:    dgVertexMap.SelectVertexClose(-128, 5120),
			left: true,
		},
		OrientedVertex{
			v:    dgVertexMap.SelectVertexClose(-128, 5120),
			left: true,
		},
		OrientedVertex{
			v:    dgVertexMap.SelectVertexClose(0, 5120),
			left: false,
		},
		OrientedVertex{
			v:    dgVertexMap.SelectVertexClose(0, 5120),
			left: false,
		}, // TODO ...
		OrientedVertex{
			v:    dgVertexMap.SelectVertexClose(2609.560976, 5120),
			left: false,
		})

	pts.Coalesce()
	fmt.Println(pts.toString())
	if pts[3].v.X == -784 {
		t.Errorf("Wrong #3 point\n")
	}
}

func TestCoalesce3(t *testing.T) {
	// left and right vertex missing in "old" sample
	// [RIGHT(909.391304,-3776.),LEFT(2423.226891,-110.924370),RIGHT(2427.446945,-100.707395),LEFT(2816.,840.),RIGHT(2820.956522,852.),LEFT(3262.086957,1920.)]
	// More dropouts! -1 -1 ; [(2427.446945,-100.707395)-(2816.,840.)]; [(2820.956522,852.)-(3262.086957,1920.)]
	// (3712,-6848) to (-3776,3840)
	dgVertexMap := CreateVertexMap(&NodesWork{}, -6848, -3776, 3840, 3712)
	pts := CollinearOrientedVertices(make([]OrientedVertex, 0))
	pts = append(pts, OrientedVertex{
		v:    dgVertexMap.SelectVertexClose(909.391304, -3776),
		left: false,
	},
		OrientedVertex{
			v:    dgVertexMap.SelectVertexClose(909.391304, -3776),
			left: true,
		},
		OrientedVertex{
			v:    dgVertexMap.SelectVertexClose(2423.22689, -110.924370),
			left: false,
		},
		OrientedVertex{
			v:    dgVertexMap.SelectVertexClose(2427.446945, -100.707395),
			left: true,
		},
		OrientedVertex{
			v:    dgVertexMap.SelectVertexClose(2816., 840.),
			left: false,
		},
		OrientedVertex{
			v:    dgVertexMap.SelectVertexClose(2820.956522, 852.),
			left: true,
		},
		OrientedVertex{
			v: dgVertexMap.SelectVertexClose(3262.086957, 1920.),

			left: false,
		},
	// missing last vertex
	)

	pts.Coalesce()
	fmt.Println(pts.toString())
	if pts[1].left != true || pts[1].v.X != 909.391304 ||
		pts[1].v.Y != -3776 {
		t.Errorf("Wrong #1 point\n")
	}
}

func TestCoalesce4(t *testing.T) {
	// left and right vertex missing in "old" sample
	// [RIGHT(3456.,4352.),LEFT(3456.,-1408.),RIGHT(3456.,-1536.),RIGHT(3456.,-1536.),LEFT(3456.,-1600.),RIGHT(3456.,-1600.),LEFT(3456.,-1664.),LEFT(3456.,-1664.)]
	// Sanity check failed! Evaluated partition line 13993 (3456,3308)-(3456,3318.4) doesn't consistently go in/out of the void when crossing solid lines (incidence count: 2). [LEFT(3456.,5256.),RIGHT(3456.,4352.),LEFT(3456.,-1408.),RIGHT(3456.,-1536.),RIGHT(3456.,-1536.),LEFT(3456.,-1664.),RIGHT(3456.,-2048.)]
	// (5256,-3392) to (-2048,9664)
	dgVertexMap := CreateVertexMap(&NodesWork{}, -2048, -3392, 9664, 5256)
	pts := CollinearOrientedVertices(make([]OrientedVertex, 0))
	pts = append(pts, OrientedVertex{
		v:    dgVertexMap.SelectVertexClose(3456, 5256),
		left: false,
	},
		OrientedVertex{
			v:    dgVertexMap.SelectVertexClose(3456, 4352),
			left: true,
		},
		OrientedVertex{
			v:    dgVertexMap.SelectVertexClose(3456, -1408),
			left: false,
		},
		OrientedVertex{
			v:    dgVertexMap.SelectVertexClose(3456, -1536),
			left: true,
		},
		OrientedVertex{
			v:    dgVertexMap.SelectVertexClose(3456, -1536),
			left: true,
		},
		OrientedVertex{
			v:    dgVertexMap.SelectVertexClose(3456, -1600.),
			left: false,
		},
		OrientedVertex{
			v: dgVertexMap.SelectVertexClose(3456, -1600.),

			left: true,
		},
		OrientedVertex{
			v: dgVertexMap.SelectVertexClose(3456, -1664),

			left: false,
		},
		OrientedVertex{
			v: dgVertexMap.SelectVertexClose(3456, -1664),

			left: false,
		},
		OrientedVertex{
			v: dgVertexMap.SelectVertexClose(3456, -2048),

			left: true,
		},
	// missing last vertex
	)

	pts.Coalesce()
	fmt.Println(pts.toString())
	if len(pts) != 6 || pts[3].v == pts[4].v {
		t.Errorf("Test failed %t %t\n", len(pts) == 6, pts[3].v != pts[4].v)
	}
}
