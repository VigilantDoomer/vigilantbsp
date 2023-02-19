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

// segalias
package main

const ALIAS_ROW_SIZE = 65536 / 8 // so can store 65536 aliases before need to grow

// Cheap integer aliases for colinear segs. Only a tiny bit of Zennode's original
// alias idea. Test one seg as a partition per all the colinear ones and receive
// a discount on execution time xD
type SegAliasHolder struct {
	visited  []uint8 // bit array, bit set to 1 means <visited> = true
	maxAlias int     // max known alias so far. Incremented by Generate
}

// Init must be called before SegAliasHolder can be used for the first time,
// and is intended to be called only once
func (s *SegAliasHolder) Init() {
	s.visited = make([]uint8, ALIAS_ROW_SIZE, ALIAS_ROW_SIZE) // can grow later
	s.maxAlias = 0
}

// Generate returns a new available alias that was not in use AND marks
// it as visited. Minimal return value is 1, so that you can use 0 to mean
// "no alias was assigned"
func (s *SegAliasHolder) Generate() int {
	s.maxAlias++
	idx := s.maxAlias - 1
	bte := idx >> 3
	bit := idx & 0x07
	if bte > len(s.visited)-1 {
		s.grow()
	}
	s.visited[bte] = s.visited[bte] | (1 << bit)
	return s.maxAlias
}

func (s *SegAliasHolder) grow() {
	L := len(s.visited)
	tmp := make([]uint8, L+ALIAS_ROW_SIZE, L+ALIAS_ROW_SIZE)
	copy(tmp, s.visited)
	s.visited = tmp
}

// MarkAndRecall marks alias as visited but returns whether it was visited already
func (s *SegAliasHolder) MarkAndRecall(alias int) bool {
	idx := alias - 1
	bte := idx >> 3
	bit := idx & 0x07
	b := s.visited[bte]&(1<<bit) != 0 // remember whether it was visited before
	if !b {                           // if not
		s.visited[bte] = s.visited[bte] | (1 << bit) // mark as visited now
	}
	return b // and return the previous value
}

// UnvisitAll marks all aliases as not yet visited - used in beginning of
// PickNode* before loop on partitions, so that values from previous PickNode
// calls are not retained
func (s *SegAliasHolder) UnvisitAll() {
	// sorry, nothing more efficient exists
	cutLen := s.maxAlias>>3 + 1
	if cutLen > len(s.visited) {
		cutLen = len(s.visited)
	}
	view := s.visited[:cutLen]
	for i, _ := range view {
		view[i] = 0
	}
}

func (s *SegAliasHolder) Clone() *SegAliasHolder {
	ret := new(SegAliasHolder)
	ret.maxAlias = s.maxAlias
	L := len(s.visited)
	ret.visited = make([]uint8, L, L)
	cutLen := s.maxAlias>>3 + 1
	if cutLen > len(s.visited) {
		cutLen = len(s.visited)
	}
	view := s.visited[:cutLen]
	for k, v := range view {
		ret.visited[k] = v
	}
	return ret
}
