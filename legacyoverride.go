//go:build go1.16
// +build go1.16

// NOTE build annotation above MUST be followed by space line
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
package main

// This file compiles only on go 1.16 or newer.

import (
	"os"
)

func init() {
	// Make legacyport.go CreateTemp point to function already provided in
	// Go system library
	CreateTemp = os.CreateTemp
}
