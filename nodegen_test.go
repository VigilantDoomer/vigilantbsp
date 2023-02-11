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

// diffgeometry_test.go
package main

import (
	//"fmt"
	"testing"
)

// Some functions declared here aren't proper tests, they never fail

// No problem here - they aren't collinear indeed
func TestForAliasCorrectness(t *testing.T) {
	// part linedef number 6396 watrsp map 03
	// check linedef number 12908 watrsp map 03
	c := &IntersectionContext{
		psx: 3520,
		psy: 768,
		pex: 3520,
		pey: 832,
		lsx: 6274,
		lsy: -821,
		lex: 6218,
		ley: -790,
	}
	c.pdx = c.pex - c.psx
	c.pdy = c.pey - c.psy
	val := doLinesIntersectStandard(c)
	t.Logf("val = %d\n", val)
	val = doLinesIntersectDetail(c)
	t.Logf("val = %d\n", val)
	val = ZdoLinesIntersect_Proto(c)
	t.Logf("val = %d\n", val)

	dx2 := c.psx - c.lsx // Checking line -> partition
	dy2 := c.psy - c.lsy
	dx3 := c.psx - c.lex
	dy3 := c.psy - c.ley

	a := c.pdy*dx2 - c.pdx*dy2
	b := c.pdy*dx3 - c.pdx*dy3
	/*a := WideNumber(c.pdy)*WideNumber(dx2) - WideNumber(c.pdx)*WideNumber(dy2)
	b := WideNumber(c.pdy)*WideNumber(dx3) - WideNumber(c.pdx)*WideNumber(dy3)*/
	t.Logf("dx2 = %d dy2 = %d dx3 = %d dy3 = %d\n", dx2, dy2, dx3, dy3)
	t.Logf("a = %d b = %d\n", a, b)

	x, y := c.computeIntersection()
	dx2 = c.lsx - x // Find distance from line start
	dy2 = c.lsy - y // to split point
	l := WideNumber(dx2)*WideNumber(dx2) + WideNumber(dy2)*WideNumber(dy2)
	t.Logf("dx2 = %d dy2 = %d l = %d\n", dx2, dy2, l)
}

// This can be remade into a proper test - it contains the failing example
// for vanilla doLinesIntersect and variations
func TestForCulpritAliasCorrectness(t *testing.T) {
	// part linedef number 12908 watrsp map 03
	// check linedef number 447 watrsp map 03
	c := &IntersectionContext{
		psx: 6274,
		psy: -821,
		pex: 6218,
		pey: -790,
		lsx: 3520,
		lsy: 704,
		lex: 3520,
		ley: 703,
	}

	/*
		// part linedef number 447 watrsp map 03
		// check linedef number 12908 watrsp map 03
		c := &IntersectionContext{
			psx: 3520,
			psy: 704,
			pex: 3520,
			pey: 703,
			lsx: 6274,
			lsy: -821,
			lex: 6218,
			ley: -790,
		}*/

	c.pdx = c.pex - c.psx
	c.pdy = c.pey - c.psy
	val := doLinesIntersectStandard(c) // gives wrong answer
	t.Logf("val = %d\n", val)
	val = doLinesIntersectDetail(c) // gives wrong answer
	t.Logf("val = %d\n", val)
	val = ZdoLinesIntersect_Proto(c) // gives correct answer
	t.Logf("val = %d\n", val)

	dx2 := c.psx - c.lsx // Checking line -> partition
	dy2 := c.psy - c.lsy
	dx3 := c.psx - c.lex
	dy3 := c.psy - c.ley

	a := c.pdy*dx2 - c.pdx*dy2
	b := c.pdy*dx3 - c.pdx*dy3
	/*a := WideNumber(c.pdy)*WideNumber(dx2) - WideNumber(c.pdx)*WideNumber(dy2)
	b := WideNumber(c.pdy)*WideNumber(dx3) - WideNumber(c.pdx)*WideNumber(dy3)*/
	t.Logf("dx2 = %d dy2 = %d dx3 = %d dy3 = %d\n", dx2, dy2, dx3, dy3)
	t.Logf("a = %d b = %d\n", a, b)

	x, y := c.computeIntersection()
	dx2 = c.lsx - x // Find distance from line start
	dy2 = c.lsy - y // to split point
	l := WideNumber(dx2)*WideNumber(dx2) + WideNumber(dy2)*WideNumber(dy2)
	t.Logf("dx2 = %d dy2 = %d l = %d\n", dx2, dy2, l)

	cmp := float64(b) * float64(b) / float64(c.pdx*c.pdx+c.pdy*c.pdy)
	t.Logf("cmp = %v\n", cmp)
}

func TestFlipVal(t *testing.T) {
	answers := make([][2]uint8, 0)
	answers = append(answers,
		[2]uint8{SIDENESS_COLLINEAR, SIDENESS_COLLINEAR},
		[2]uint8{SIDENESS_INTERSECT, SIDENESS_INTERSECT},
		[2]uint8{SIDENESS_LEFT, SIDENESS_RIGHT},
		[2]uint8{SIDENESS_RIGHT, SIDENESS_LEFT},
	)
	for i, answer := range answers {
		val := flipVal(answer[0])
		if val != answer[1] {
			t.Errorf("flip val id=%d given %d expected %d received %d\n",
				i, answer[0], answer[1], val)
		}
	}
}
