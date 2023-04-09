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
package main

import (
	"math"
)

// VertexMap utils go here. Also contains some stuff for VertexCache

const VMAP_BLOCK_SHIFT = 8 + FRACBITS

const VMAP_BLOCK_SIZE = 1 << VMAP_BLOCK_SHIFT

const VMAP_SAFE_MARGIN = 2.0 // Floating point garbage

// VertexMap is a ZDBSP thingy. Allows to lookup close-enough vertices among
// existing ones to the one created as the result of intersection. I am not sure
// as to what the exact consequences of choosing a very slightly different
// vertex instead of exact match are. More investigation needed
// VertexMap is used in VigilantBSP when:
// 1) nodes are built for Zdoom extended/compressed format
// 2) in code for advanced visplane reduction that traces line through void and
// non-void
type VertexMap struct {
	w          *NodesWork
	Grid       [][]*FloatVertex
	Snapshot   []int
	BlocksWide int
	BlocksTall int
	MinX, MinY float64
	MaxX, MaxY float64
}

func CreateVertexMap(w *NodesWork, minx, miny, maxx, maxy int) *VertexMap {
	// Mitigation for a possible crash when producing line traces against solid
	// lines (void and non-void differentiation) in advanced visplane reduction
	// for, apparently, bent segs (previously split segs whose angle is
	// different from angle of the original linedef - in node format that lacks
	// precision) can produce intersection vertice slightly outside the bounds
	minx = minx - VMAP_SAFE_MARGIN
	miny = miny - VMAP_SAFE_MARGIN
	maxx = maxx + VMAP_SAFE_MARGIN
	maxy = maxy + VMAP_SAFE_MARGIN

	vm := &VertexMap{
		w:    w,
		MinX: float64(minx),
		MinY: float64(miny),
		BlocksWide: int((float64(maxx-minx+1)*
			FIXED16DOT16_MULTIPLIER + float64(VMAP_BLOCK_SIZE-1)) /
			float64(VMAP_BLOCK_SIZE)),
		BlocksTall: int((float64(maxy-miny+1)*
			FIXED16DOT16_MULTIPLIER + float64(VMAP_BLOCK_SIZE-1)) /
			float64(VMAP_BLOCK_SIZE)),
	}
	vm.MaxX = vm.MinX + float64(vm.BlocksWide*VMAP_BLOCK_SIZE-1)/FIXED16DOT16_MULTIPLIER
	vm.MaxY = vm.MinY + float64(vm.BlocksTall*VMAP_BLOCK_SIZE-1)/FIXED16DOT16_MULTIPLIER
	vm.Grid = make([][]*FloatVertex, vm.BlocksWide*vm.BlocksTall)
	return vm
}

func (vm *VertexMap) Clone() *VertexMap {
	if vm == nil {
		return nil
	}
	newVm := &VertexMap{}
	*newVm = *vm
	newVm.w = nil
	newVm.Grid = make([][]*FloatVertex, 0, len(vm.Grid))
	for _, it := range vm.Grid {
		cpit := make([]*FloatVertex, 0, len(it))
		for _, it2 := range it {
			nv := &FloatVertex{}
			*nv = *it2
			cpit = append(cpit, nv)
		}
		newVm.Grid = append(newVm.Grid, cpit)
	}
	if vm.Snapshot != nil {
		newVm.Snapshot = make([]int, len(vm.Grid))
		for i, it := range vm.Snapshot {
			newVm.Snapshot[i] = it
		}
	}
	return newVm
}

func (vm *VertexMap) GetBlock(x, y float64) int {
	// assert x >= MinX
	// assert y >= MinY
	// assert x <= MaxX
	// assert y <= MaxY
	// The above constraints are actually violated sometimes by some epsilon
	// because floating point is used and not fixed point like in ZDBSP. Such
	// cases don't produce out of bounds index, though, because of being really
	// close to the border values. For some runaway cases, see a different
	// mitigation below
	ret := int(uint((x-vm.MinX)*FIXED16DOT16_MULTIPLIER)>>VMAP_BLOCK_SHIFT +
		(uint((y-vm.MinY)*FIXED16DOT16_MULTIPLIER)>>VMAP_BLOCK_SHIFT)*
			uint(vm.BlocksWide))
	if ret < 0 || ret >= len(vm.Grid) {
		vm.w.mlog.Verbose(1, "Vertex map index out of range, source values: x=%f, y=%f xmin,ymin=(%f,%f) xmax,ymax=(%f,%f)\n",
			x, y, vm.MinX, vm.MinY, vm.MaxX, vm.MaxY)
		// Allow vertex map to function without panic in such cases, should they
		// happen
		// Accumulating such errors (if wrong borders are specified) can cause
		// slowdown, but not crash, and should not result in malfunction
		return 0
	}
	return ret
}

func (vm *VertexMap) SelectVertexExact(x, y float64, id int) *FloatVertex {
	block := &(vm.Grid[vm.GetBlock(x, y)])
	for _, it := range *block {
		if it.X == x && it.Y == y {
			return it
		}
	}
	return vm.insertVertex(x, y, id)
}

func (vm *VertexMap) SelectVertexClose(x, y float64) *FloatVertex {
	block := &(vm.Grid[vm.GetBlock(x, y)])
	for _, it := range *block {
		if math.Abs(it.X-x) < VERTEX_EPSILON &&
			math.Abs(it.Y-y) < VERTEX_EPSILON {
			return it
		}
	}
	return vm.insertVertex(x, y, -1)
}

func (vm *VertexMap) insertVertex(x, y float64, id int) *FloatVertex {
	// If a vertex is near a block boundary, then it will be inserted on
	// both sides of the boundary so that SelectVertexClose can find
	// it by checking in only one block.
	ret := &FloatVertex{
		X:  x,
		Y:  y,
		Id: id,
	}
	minx := vm.MinX
	if minx < (x - VERTEX_EPSILON) {
		minx = x - VERTEX_EPSILON
	}
	maxx := vm.MaxX
	if maxx > (x + VERTEX_EPSILON) {
		maxx = x + VERTEX_EPSILON
	}
	miny := vm.MinY
	if miny < (y - VERTEX_EPSILON) {
		miny = y - VERTEX_EPSILON
	}
	maxy := vm.MaxY
	if maxy > (y + VERTEX_EPSILON) {
		maxy = y + VERTEX_EPSILON
	}
	blk := [4]int{vm.GetBlock(minx, miny),
		vm.GetBlock(maxx, miny),
		vm.GetBlock(minx, maxy),
		vm.GetBlock(maxx, maxy)}
	blcount := [4]int{
		len(vm.Grid[blk[0]]),
		len(vm.Grid[blk[1]]),
		len(vm.Grid[blk[2]]),
		len(vm.Grid[blk[3]])}
	for i := 0; i < 4; i++ {
		if len(vm.Grid[blk[i]]) == blcount[i] {
			vm.Grid[blk[i]] = append(vm.Grid[blk[i]], ret)
		}
	}
	return ret
}

// RestoreOrBeginSnapshot() removes all vertices from map that were added
// since previous RestoreOrBeginSnapshot() call. Snapshots are useful to create
// distinct vertex spaces for line traces in diffgeometry operations (volatile
// vertices computed there might not end up being vertices actually placed on
// the map, and have additional restriction of being sortable alongside the
// line)
// As with the rest of VertexMap methods, snapshots do NOT provide for
// concurrent access
func (vm *VertexMap) RestoreOrBeginSnapshot() {
	if vm.Snapshot == nil {
		// begin
		vm.Snapshot = make([]int, len(vm.Grid))
		for i, it := range vm.Grid {
			vm.Snapshot[i] = len(it)
		}
	} else {
		// restore
		for i, it := range vm.Grid {
			vm.Grid[i] = it[:vm.Snapshot[i]]
		}
	}
}

func PopulateVertexMap(vm *VertexMap, allSegs []*NodeSeg) {
	for _, seg := range allSegs {
		vm.SelectVertexExact(float64(seg.psx), float64(seg.psy),
			int(seg.StartVertex.idx))
		vm.SelectVertexExact(float64(seg.pex), float64(seg.pey),
			int(seg.EndVertex.idx))
	}
}

func PopulateVertexMapFromLines(vm *VertexMap, lines AbstractLines) {
	l := int(lines.Len())
	for i := 0; i < l; i++ {
		x1, x2, y1, y2 := lines.GetAllXY(uint16(i))
		vm.SelectVertexExact(float64(x1), float64(y1), i)
		vm.SelectVertexExact(float64(x2), float64(y2), i)
	}
}

func PopulateVertexCache(cache map[SimpleVertex]int, allSegs []*NodeSeg) {
	for _, it := range allSegs {
		rec := SimpleVertex{int(it.StartVertex.X), int(it.StartVertex.Y)}
		cache[rec] = int(it.StartVertex.idx)
		rec = SimpleVertex{int(it.EndVertex.X), int(it.EndVertex.Y)}
		cache[rec] = int(it.EndVertex.idx)
	}
}
