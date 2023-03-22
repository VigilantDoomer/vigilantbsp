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

// rejectDFS
package main

// All the jazz related to speeding up reject builder with graphs goes into
// this file.

func (w *RejectWork) DFS(graph *RejGraph, sector *RejSector) int {
	// Initialize the sector
	sector.graph = graph
	sector.indexDFS = graph.numSectors
	sector.loDFS = graph.numSectors
	sector.isArticulation = false

	// Add this sector to the graph
	graph.sectors[graph.numSectors] = sector
	graph.numSectors++

	numChildren := 0

	for _, child := range w.DFSGetNeighborsAndGroupsiblings(sector) {
		if child == nil {
			break
		}
		if child.graph != graph {
			numChildren++
			child.graphParent = sector
			w.DFS(graph, child)
			if child.loDFS < sector.loDFS {
				sector.loDFS = child.loDFS
			}
			if child.loDFS >= sector.indexDFS {
				sector.isArticulation = true
			}
		} else if child != sector.graphParent {
			if child.indexDFS < sector.loDFS {
				sector.loDFS = child.indexDFS
			}
		}
	}

	sector.hiDFS = graph.numSectors - 1

	return numChildren
}

func (w *RejectWork) CreateGraph(root *RejSector) *RejGraph {
	w.graphTable.numGraphs++
	graph := &(w.graphTable.graphs[w.graphTable.numGraphs-1])

	graph.sectors = w.graphTable.sectorStart
	graph.numSectors = 0

	root.graphParent = nil
	if w.DFS(graph, root) > 1 {
		root.isArticulation = true
	} else {
		root.isArticulation = false
	}

	w.graphTable.sectorStart = w.graphTable.sectorStart[graph.numSectors:]

	return graph
}

func (w *RejectWork) HideComponents(oldGraph, newGraph *RejGraph) {
	for i := 0; i < oldGraph.numSectors; i++ {
		sec1 := oldGraph.sectors[i]
		if sec1.graph == oldGraph {
			for j := 0; j < newGraph.numSectors; j++ {
				sec2 := newGraph.sectors[j]
				w.markVisibility(sec1.index, sec2.index, VIS_HIDDEN)
			}
		}
	}
}

func (w *RejectWork) SplitGraph(oldGraph *RejGraph) {
	remainingSectors := oldGraph.numSectors - 1

	for i := 0; i < oldGraph.numSectors; i++ {
		sec := oldGraph.sectors[i]
		if sec.graph == oldGraph {
			newGraph := w.CreateGraph(sec)
			if newGraph.numSectors < remainingSectors {
				w.HideComponents(oldGraph, newGraph)
			}
			remainingSectors -= newGraph.numSectors - 1
		}
	}
}

func (w *RejectWork) InitializeGraphs(numSectors int) {
	Log.Verbose(1, "Creating sector graphs...\n")

	w.graphTable.numGraphs = 0
	w.graphTable.graphs = make([]RejGraph, w.numSectors*2)
	w.graphTable.sectorPool = make([]*RejSector, w.numSectors*4)
	w.graphTable.sectorStart = w.graphTable.sectorPool

	for _, v := range w.graphTable.graphs {
		v.numSectors = 0
		v.sectors = nil
	}

	for i, _ := range w.graphTable.sectorPool {
		w.graphTable.sectorPool[i] = nil
	}

	// Create the initial graph
	graph := &(w.graphTable.graphs[0])
	graph.numSectors = w.numSectors
	graph.sectors = w.graphTable.sectorStart
	w.graphTable.sectorStart = w.graphTable.sectorStart[w.numSectors:]
	w.graphTable.numGraphs++

	// Put all sectors in the initial graph
	for i := 0; i < w.numSectors; i++ {
		w.sectors[i].graph = graph
		graph.sectors[i] = &(w.sectors[i])
	}

	// Separate the individual graphs
	w.SplitGraph(graph)

	// Keep a permanent copy of the initial graph
	for i := 0; i < w.numSectors; i++ {
		w.sectors[i].baseGraph = w.sectors[i].graph
	}

	// Calculate the sector metrics
	for i := 1; i < w.graphTable.numGraphs; i++ {
		graph := &(w.graphTable.graphs[i])
		for j := 0; j < graph.numSectors; j++ {
			sec := graph.sectors[j]
			sum := 0
			left := graph.numSectors - 1
			for _, child := range w.DFSGetNeighborsAndGroupsiblings(sec) {
				if child == nil {
					break
				}

				if child.graphParent != sec {
					continue
				}
				if child.loDFS >= sec.indexDFS {
					num := child.hiDFS - child.indexDFS + 1
					left -= num
					sum += num * left
				}
			}
			sec.metric = sum
		}
	}
	// Hmm. It seems some maps have a lot of graphs. Could I utilize this to
	// spread reject building over multiple threads somehow? Reject building is
	// really slow operation
	Log.Verbose(1, "Reject: created %d graphs.\n", w.graphTable.numGraphs)
}

func (w *RejectWork) HideSectorFromComponents(key, root, sec *RejSector) {
	graph := sec.graph
	// Hide sec from all other sectors in its graph that are in different
	// bi-connected components
	for i := 0; i < root.indexDFS; i++ {
		w.markVisibility(sec.index, graph.sectors[i].index, VIS_HIDDEN)
	}
	for i := root.hiDFS + 1; i < graph.numSectors; i++ {
		w.markVisibility(sec.index, graph.sectors[i].index, VIS_HIDDEN)
	}
}

func (w *RejectWork) AddGraph(graph *RejGraph, sector *RejSector) {
	// Initialize the sector
	sector.graph = graph
	sector.indexDFS = graph.numSectors
	sector.loDFS = graph.numSectors

	// Add this sector to the graph
	graph.sectors[graph.numSectors] = sector
	graph.numSectors++

	// Add all this nodes children that aren't already in the graph
	for _, child := range w.DFSGetNeighborsAndGroupsiblings(sector) {
		if child == nil {
			break
		}
		if child.graph == sector.baseGraph {
			child.graphParent = sector
			w.AddGraph(graph, child)
			if child.loDFS < sector.loDFS {
				sector.loDFS = child.loDFS
			}
		} else if child != sector.graphParent {
			if child.indexDFS < sector.loDFS {
				sector.loDFS = child.indexDFS
			}
		}
	}

	sector.hiDFS = graph.numSectors - 1
}

func (w *RejectWork) QuickGraph(root *RejSector) *RejGraph {
	oldGraph := root.baseGraph
	for i := 0; i < oldGraph.numSectors; i++ {
		oldGraph.sectors[i].graph = oldGraph
	}

	graph := &(w.graphTable.graphs[w.graphTable.numGraphs])

	graph.sectors = w.graphTable.sectorStart
	graph.numSectors = 0

	root.graphParent = nil

	w.AddGraph(graph, root)

	return graph
}

func ShouldTest(src *TransLine, key uint16, tgt *TransLine, sector uint16) bool {
	y1 := src.DX*(tgt.start.Y-src.start.Y) - src.DY*(tgt.start.X-src.start.X)
	y2 := src.DX*(tgt.end.Y-src.start.Y) - src.DY*(tgt.end.X-src.start.X)

	if src.frontSector == key {
		if (y1 <= 0) && (y2 <= 0) {
			return false
		}
	} else if (y1 >= 0) && (y2 >= 0) {
		return false
	}

	x1 := tgt.DX*(src.start.Y-tgt.start.Y) - tgt.DY*(src.start.X-tgt.start.X)
	x2 := tgt.DX*(src.end.Y-tgt.start.Y) - tgt.DY*(src.end.X-tgt.start.X)

	if tgt.frontSector == sector {
		if (x1 <= 0) && (x2 <= 0) {
			return false
		}
	} else if (x1 >= 0) && (x2 >= 0) {
		return false
	}

	return true
}

func (w *RejectWork) ProcessSectorLines(key, root, sector *RejSector,
	lines []*TransLine) {

	isVisible := *(w.rejectTableIJ(key.index, sector.index)) == VIS_VISIBLE
	isUnknown := *(w.rejectTableIJ(key.index, sector.index)) == VIS_UNKNOWN

	done := false

	if isUnknown {
		ptr := lines
		for ptr[0] != nil {
			srcLine := ptr[0]
			ptr = ptr[1:]
			for i := 0; i < sector.numNeighbors; i++ {
				child := sector.neighbors[i]
				// Test each line that may lead back to the key sector (can reach higher up in the graph)
				if child.loDFS <= sector.indexDFS {
					for j := 0; j < sector.numLines; j++ {
						tgtLine := sector.lines[j]
						if (tgtLine.backSector == uint16(child.index)) || (tgtLine.frontSector == uint16(child.index)) {
							if ShouldTest(srcLine, uint16(key.index), tgtLine, uint16(sector.index)) {
								if w.testLinePair(srcLine, tgtLine) {
									w.markPairVisible(srcLine, tgtLine)
									done = true // the fuck with people still using goto in C++
									break
								}
							}
						}
					}
					if done {
						break
					}
				}
			}
			if done {
				break
			}
		}
	}

	// Another f-rant about use of goto was here, since deleted
	if !done && !isVisible { // if it was not made visible before, we hide it

		graph := sector.graph

		// See if we're in a loop
		if sector.loDFS == sector.indexDFS {

			// Nope. Hide ourself and all our children from the other components
			for i := sector.indexDFS; i <= sector.hiDFS; i++ {
				w.HideSectorFromComponents(key, root, graph.sectors[i])
			}

		} else {

			// Yep. Hide ourself
			w.HideSectorFromComponents(key, root, sector)

			for i := 0; i < sector.numNeighbors; i++ {
				child := sector.neighbors[i]
				if child.graphParent == sector {
					// Hide any child components that aren't in the loop
					if child.loDFS >= sector.indexDFS {
						for j := child.indexDFS; j <= child.hiDFS; j++ {
							w.HideSectorFromComponents(key, root, graph.sectors[j])
						}
					} else {
						w.ProcessSectorLines(key, root, child, lines)
					}
				}
			}
		}

	} else {
		done = true
	}

	if done {
		// Continue checking all of our children
		for i := 0; i < sector.numNeighbors; i++ {
			child := sector.neighbors[i]
			if child.graphParent == sector {
				w.ProcessSectorLines(key, root, child, lines)
			}
		}
	}
}

func (w *RejectWork) ProcessSector(sector *RejSector) {
	if sector.isArticulation {

		// For now, make sure this sector is at the top of the graph (keeps things simple)
		w.QuickGraph(sector)

		lines := make([]*TransLine, sector.numLines+1)

		for i := 0; i < sector.numNeighbors; i++ {

			child := sector.neighbors[i]

			// Find each child that is the start of a component of this sector
			if child.graphParent == sector {

				// Make a list of lines that connect this component
				index := 0
				for j := 0; j < sector.numLines; j++ {
					line := sector.lines[j]
					if (line.backSector == uint16(child.index)) || (line.frontSector == uint16(child.index)) {
						lines[index] = line
						index++
					}
				}

				// If this child is part of a loop, add lines from all the other children in the loop too
				if child.loDFS < child.indexDFS {
					for j := i + 1; j < sector.numNeighbors; j++ {
						child2 := sector.neighbors[j]
						if child2.indexDFS <= child.hiDFS {
							for k := 0; k < sector.numLines; k++ {
								line := sector.lines[k]
								if (line.backSector == uint16(child2.index)) || (line.frontSector == uint16(child2.index)) {
									lines[index] = line
									index++
								}
							}
						}
					}
				}

				lines[index] = nil
				w.ProcessSectorLines(sector, child, child, lines)
			}

		}

	} else {

		graph := sector.baseGraph
		for i := 0; i < graph.numSectors; i++ {
			tgtSector := graph.sectors[i]
			if *(w.rejectTableIJ(sector.index, tgtSector.index)) == VIS_UNKNOWN {
				next := false
				for j := 0; j < sector.numLines; j++ {
					srcLine := sector.lines[j]
					for k := 0; k < tgtSector.numLines; k++ {
						tgtLine := tgtSector.lines[k]
						if w.testLinePair(srcLine, tgtLine) {
							w.markPairVisible(srcLine, tgtLine)
							next = true
							break
						}
					}
					if next {
						break
					}
				}
				if next {
					continue
				}
				w.markVisibility(sector.index, tgtSector.index, VIS_HIDDEN)
			}
		}
	}
}

func reSectorsCompare_WithGraphs(x reSectors_SorterWithGraphs, i, j int) int {
	sec1 := x[i]
	sec2 := x[j]

	// Favor the sector with the best metric (higher is better)
	if sec1.metric != sec2.metric {
		return sec2.metric - sec1.metric
	}

	// Favor the sector that is not part of a loop
	var sec1Loop, sec2Loop int
	if sec1.loDFS < sec1.indexDFS {
		sec1Loop = 1
	} else {
		sec1Loop = 0
	}
	if sec2.loDFS < sec2.indexDFS {
		sec2Loop = 1
	} else {
		sec2Loop = 0
	}

	if sec1Loop != sec2Loop {
		return sec1Loop - sec2Loop
	}

	// Favor the sector with the most neighbors
	if sec1.numNeighbors != sec2.numNeighbors {
		return sec2.numNeighbors - sec1.numNeighbors
	}

	// Favor the sector with the most visible lines
	if sec1.numLines != sec2.numLines {
		return sec2.numLines - sec1.numLines
	}

	// It's a tie - use the sector index - lower index favored
	return sec1.index - sec2.index
}

type reSectors_SorterWithGraphs []*RejSector

func (x reSectors_SorterWithGraphs) Len() int { return len(x) }
func (x reSectors_SorterWithGraphs) Less(i, j int) bool {
	return reSectorsCompare_WithGraphs(x, i, j) < 0
}
func (x reSectors_SorterWithGraphs) Swap(i, j int) { x[i], x[j] = x[j], x[i] }

func (w *RejectWork) setupMixer() {
	w.extra.mixer = make([]byte, len(w.rejectTable))
	copy(w.extra.mixer, w.rejectTable)
}

func (w *RejectWork) mergeAndDestroyMixer() {
	for i, v := range w.rejectTable {
		if v == VIS_UNKNOWN && w.extra.mixer[i] != VIS_UNKNOWN {
			w.rejectTable[i] = w.extra.mixer[i]
		}
	}
	w.extra.mixer = nil
}

// Returns not just sector's neighbors, but also sectors in the same group as
// this one, IF in reject builder configuration groupShareVis == true.
// Doesn't return their neighbors, though
func (r *RejectWork) DFSGetNeighborsAndGroupsiblings(s *RejSector) []*RejSector {
	if !r.groupShareVis {
		return s.neighbors
	}
	group := r.groups[r.groups[s.index].parent]
	if len(group.sectors) == 1 {
		return s.neighbors
	}
	ret := make([]*RejSector, 0, len(s.neighbors)+len(group.sectors))
	filt := make(map[int]bool)
	for _, sec := range s.neighbors {
		// No test for duplicates here - there already must be none
		if sec == nil {
			// weird stuff like this exists because some questionable things
			// were ported over from Zennode's C++ code
			break
		}
		filt[sec.index] = true
		ret = append(ret, sec)
	}
	for _, si := range group.sectors {
		// but group siblings are likely to coincide with neighbors, as per
		// original intent of old RMB program. Also don't include self in list,
		// of course
		if s.index != si && !filt[si] {
			sec := &(r.sectors[si])
			ret = append(ret, sec)
			filt[si] = true
		}
	}
	return ret
}
