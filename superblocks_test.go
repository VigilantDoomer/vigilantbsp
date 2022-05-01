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

// superblocks_test.go
package main

import (
	"testing"
)

func TestSuperblocksConsts(t *testing.T) {
	marginLen := float64(IFFY_LEN * 1.5)
	if marginLen != float64(MARGIN_LEN) {
		t.Errorf("Const MARGIN_LEN must be equal to IFFY_LEN * 1.5n \n")
	}
	distMultiply := float64(1 << DIST_SHIFT)
	if 1.0/distMultiply != DIST_EPSILON {
		t.Errorf("Const DIST_SHIFT out of sync with DIST_EPSILON\n")
	}
}
