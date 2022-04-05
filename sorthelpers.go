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

// sorthelpers
package main

// Implementations of sort.Interface for _stock_ Go types go here.
// Note: don't add implementations of sort.Interface of types invented for
// the project, keep those in the file where those are declared

type UInt32Slice []uint32

func (x UInt32Slice) Len() int           { return len(x) }
func (x UInt32Slice) Less(i, j int) bool { return x[i] < x[j] }
func (x UInt32Slice) Swap(i, j int)      { x[i], x[j] = x[j], x[i] }

type Uint16Slice []uint16

func (x Uint16Slice) Len() int           { return len(x) }
func (x Uint16Slice) Less(i, j int) bool { return x[i] < x[j] }
func (x Uint16Slice) Swap(i, j int)      { x[i], x[j] = x[j], x[i] }
