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

// trollgen
package main

import (
	"sort"
)

// Troll, Or Lump Starting Offset Dealiasing Broker
//
// Idea: WAD format allows sharing data between lumps, so do it
// Multiple zero-filled reject lumps can be effortlessly compressed this way
// Problem: SLADE3 drops all lumps that start on the same offset when loading
// or saving wads (cause some "troll wads" used that)
// Solution: make start offset different for each lump (but still share data).
// Not too difficult and saves waiting for SLADE3 to remove the limitation

type OffsetForSize struct {
	offset uint32
	next   *OffsetForSize
}

type OffsetContainer struct {
	first *OffsetForSize
}

// So, whether some lumps have the same size, or a different one for each lump,
// each of them needs UNIQUE offset in either case. Call AddSize for each
// ZERO-filled lump that'll be generated, not once per unique size, but once per lump
// (got 3 lumps of the same size - you call AddSize thrice with this size as parameter)
//
// And yeah. Several lumps may be of the same size but each of them needs their
// own UNIQUE offset. Be sure to call AddSize for each lump - if 3 lumps have same
// size you call AddSize for each of them (with the same argument, yes).
//
// Beware: all methods are mutating object
type Troll struct {
	keyToIds map[uint32]int     // size(key) to id(value), id = index in offsets slice
	allKeys  []uint32           // tracks all sizes as well as their repetitions
	offsets  []*OffsetContainer // index(id) is stored as value in a map keyToIds
	maxSize  uint32             // the maximum size ever seen as a key
	compiled bool
}

func CreateTroll() *Troll {
	na := Troll{
		keyToIds: make(map[uint32]int),
		allKeys:  make([]uint32, 0, 99),
		offsets:  make([]*OffsetContainer, 0, 99),
		maxSize:  0,
		compiled: false,
	}
	return &na
}

// Registers that a zero-byte filled lump of this size will be generated
// If you have several lumps of the same size, you call this several times!
func (t *Troll) AddSize(rejlump_size uint32) {
	t.allKeys = append(t.allKeys, rejlump_size)
	candidateId := len(t.offsets)
	if _, ok := t.keyToIds[rejlump_size]; !ok {
		t.keyToIds[rejlump_size] = candidateId
		dumb := new(OffsetContainer)
		dumb.first = nil
		t.offsets = append(t.offsets, dumb)
	}
	if t.maxSize < rejlump_size {
		t.maxSize = rejlump_size
	}
}

// Returns how much zero bytes will be needed for "lump data" region that
// will be reused for ALL of your lumps you registered with AddSize
func (t *Troll) Compile() uint32 {
	if t.compiled {
		panic("The offset generator was finalized already.")
	}
	zeroCount := t.maxSize
	sort.Sort(UInt32Slice(t.allKeys))
	newOffset := uint32(0)
	for i := len(t.allKeys) - 1; i >= 0; i-- {
		sz := t.allKeys[i]
		id, ok := t.keyToIds[sz]
		if !ok {
			// Must never happen
			Log.Panic("Inconsistent data (key not in map)")
		}
		p := t.offsets[id]
		rec := new(OffsetForSize)
		rec.next = p.first
		rec.offset = newOffset
		if newOffset+sz > zeroCount {
			zeroCount = newOffset + sz
		}
		newOffset++
		p.first = rec
	}
	t.compiled = true
	return zeroCount
}

// Call once for each lump (and not once for unique size) like you did AddSize.
// Do not reuse the return value between several lumps of the same size!
// You need unique offset for each lump, and if you called AddSize for each lump,
// you'll get it. Order of PopOffset calls doesn't matter, but every call should
// have been matched by a prior AddSize call with the same parameter
func (t *Troll) PopOffset(rejlump_size uint32) uint32 {
	id, ok := t.keyToIds[rejlump_size]
	if !ok {
		// Must never occur
		Log.Panic("Invalid request to lump offset generator: programmer never called AddSize with %d size as an argument",
			rejlump_size)
	}
	p := t.offsets[id]
	if p.first == nil {
		// Must never occur
		Log.Panic("Couldn't fetch _next_ offset for %d size - programmer may have forgot to register (yet) another instance of it via AddSize",
			rejlump_size)
	}
	res := p.first.offset
	p.first = p.first.next
	return res
}
