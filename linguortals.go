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
// linguortals.go
package main

// LINGUORTAL_OPEN is a writeable "constant" denoting linedef action number for
// linguortal viewing point. This number is not yet stable, hence a variable
// instead of true constant
// It seems I might need to support two distinct types of linguortal:
// 1) non-translating one, simply replace node with another one
// 2) translating one, coordinates of segs in linguortal are adjusted
// But first, we need to do the normal one (the first one) till the end, right?
var LINGUORTAL_SIMPLE_OPEN = uint16(1097)

type Linguortal struct {
	// lidx is an index of TWO-sided linedef with its back side diverted to
	// provide the view into linguortal instead of what is actually
	// geometrically behind it
	lidx uint16
	tag  uint16
	// didx is an index of linedef belonging to linguortal destination - it
	// helps locate the island (which is to be contained within a node or, in
	// rare cases it's possible, subsector) which will be displayed at entirely
	// different location, replacing the original node/subsector on back side of
	// lidx linedef
	didx   uint16
	dset   bool         // was destination (didx) set? since linedef #0 is legitimate index
	dgraph *CullerGraph // destination graph (set of lines)
	bbox   *NodeBounds
}

type LinguortalBundle struct {
	linguortals []Linguortal
	cooperative bool
	freeBoxes   []*NodeBounds
	bigFree     *NodeBounds
	bigLing     *NodeBounds
}

// returns whether mapper is cooperative (stub)
func DetectLinguortals(lines WriteableLines) *LinguortalBundle {
	linguortals := make([]Linguortal, 0)
	cntLines := lines.Len()
	for i := uint16(0); i < cntLines; i++ {
		if IsLinguortalAction(lines, i) {
			if lines.GetTag(i) == 0 {
				Log.Printf("Ignoring linguortal-opening linedef %d because it has zero tag\n",
					i)
				continue
			} else if uint16(lines.GetFlags(i))&LF_TWOSIDED == 0 {
				// TODO	check needs to be more robust. What if one side is
				// instructed to be removed from rendering? This shouldn't be
				// allowed, either!
				Log.Printf("Ignoring linguortal-opening linedef %d because it is not two-sided\n",
					i)
				continue
			}
			linguortals = append(linguortals, Linguortal{
				lidx: uint16(i),
				tag:  lines.GetTag(i),
			})
		}
	}
	if len(linguortals) == 0 { // no linguortals - good!
		return nil
	}
	// Now find linedefs that denote target islands into which linguortals
	// are opened (identifying islands from linedefs is a subject for later)
	// Several linguortal-opening linedefs may target the same island, this is
	// ok, but having multiple linedefs as targets is NOT ok
	for i := uint16(0); i < cntLines; i++ {
		if lines.GetTag(i) == 0 {
			continue
		}
		for j, item := range linguortals {
			if item.tag == lines.GetTag(i) && !IsLinguortalAction(lines, i) {
				if item.dset {
					Log.Panic("Duplicate linguortal destination for linedef %d found: first %d, second %d\n",
						item.lidx, item.didx, i)
				}
				linguortals[j].dset = true
				linguortals[j].didx = uint16(i)
			}
		}
	}
	// All linguortals have destination linedef?
	l := len(linguortals)
	for i := 0; i < l; i++ {
		item := linguortals[i]
		if !item.dset {
			Log.Printf("Linguortal-opening linedef %d is missing destination marked by linedef with the same tag but different (or zero) action\n",
				item.lidx)
			// delete item
			if i < l-1 {
				for j := i + 1; j < l; j++ {
					linguortals[j-1] = linguortals[j]
				}
			}
			linguortals = linguortals[:l-1]
			l--
		}
	}
	if len(linguortals) == 0 {
		Log.Printf("None initially discovered linguortal definitions were complete - exiting.\n")
		return nil
	}

	// The task of identifying islands is complex. I approach it gradually:
	// first, level linedefs and vertices are represented as a graph, then
	// the connected components (subgraphs) are identified, then islands are
	// built from merging some of components with each other if they satisfy
	// certain conditions
	// Note that we can't use sectors adjacency for identifying islands, because
	// sectors can consist of remotely joined parts, residing in different
	// islands.
	lidxs := make([]uint16, cntLines)
	for i := uint16(0); i < cntLines; i++ {
		lidxs[i] = i
	}
	levelGraph := convertLinesToGraph(lines, lidxs, false)
	components, ok := levelGraph.split()
	if !ok {
		Log.Panic("DetectLinguortals: failed to split a level into connected components\n")
	}

	// Components were built, now need to do islands
	// So, check if certain graphs are completely "inside" others from geometric
	// perspective (coordinates) and so must be merged into them
	// This is really difficult to properly define
	// Starting approximation relies on the fact that if graph1 is nested inside
	// graph2, it's bounding box is nested inside bounding box of graph2, but
	// the reverse, unfortunately, is not true, and thus it needs to be
	// corrected.
	// FIXME Correction step is not implemented yet
	islands := identifyIslands(components)

	// Now we must check that each island contains at most one linguortal
	// destination (this limitation may be too restrictive?), and linguortal
	// destinations are never in the same graph as linguortal sources (this
	// as well, actually, imagine if we have linguortals inside linguortals...
	// but the algorithmic complexity for supporting such setup is sure high)
	freeIslands := make([]*CullerGraph, 0)
	targetIslands := make([]*CullerGraph, 0)
	targeted := make([]bool, len(islands))

	for i, island := range islands {
		for _, line := range island.lines {
			for k, ling := range linguortals {
				if ling.didx == line.id {
					linguortals[k].dgraph = islands[i]
					targeted[i] = true
				}
			}
		}
	}

	for _, ling := range linguortals {
		if ling.dgraph != nil {
			add := true
			for _, check := range targetIslands {
				if check == ling.dgraph {
					add = false
				}
			}
			if add {
				targetIslands = append(targetIslands, ling.dgraph)
				for _, line := range (*ling.dgraph).lines {
					for _, ling2 := range linguortals {
						if ling2.lidx == line.id {
							Log.Panic("Linguortal-opening lines are not allowed inside linguortal destinations.\n")
						}
					}
				}
			}
		} else {
			Log.Panic("Failed to identify linguortal destination as a set of lines.\n")
		}
	}

	for i, v := range targeted {
		if !v {
			freeIslands = append(freeIslands, islands[i])
		}
	}

	if len(freeIslands) == 0 {
		// Impossible because we prohibited linguortal-opening lines inside
		// linguortal destinations, so at least one free island should have
		// existed by time we reach here
		Log.Panic("Impossible condition: all islands appear to be linguortal destinations.\n")
	}

	// Then we need to determine the smallest bounding box for each linguortal
	// destination
	for i, _ := range linguortals {
		linguortals[i].bbox = getGraphBounds(linguortals[i].dgraph)
	}

	freeBoxes := make([]*NodeBounds, len(freeIslands))
	for i, _ := range freeIslands {
		freeBoxes[i] = getGraphBounds(freeIslands[i])
	}

	// Now check if there is overlap between freeBoxes and linguortals
	// In fact, might first check if there is overlap between a common
	// bounding box of all freeBoxes and all linguortals.
	// Also, linguortals should not overlap with each other either

	for _, boxF := range freeBoxes {
		for _, ling := range linguortals {
			boxL := ling.bbox
			if intersectBbox(boxF, boxL) {
				Log.Panic("Rectangle surrounding part of normal map is NOT allowed to intersect rectangle surrounding linguortal destination.\n Coords (%d,%d,%d,%d) & (%d,%d,%d,%d), the latter is linguortal identified by linedef %d intended to be viewed from linedef %d\n",
					boxF.Xmin, boxF.Ymin, boxF.Xmax, boxF.Ymax,
					boxL.Xmin, boxL.Ymin, boxL.Xmax, boxL.Ymax,
					ling.didx, ling.lidx)
			}
		}
	}

	for i, ling1 := range linguortals {
		for j, ling2 := range linguortals {
			if i == j {
				continue
			}
			box1 := ling1.bbox
			box2 := ling2.bbox
			if intersectBbox(box1, box2) {
				Log.Panic("Rectangle surrounding one linguortal is NOT allowed to intersect rectangle surrounding another linguortal.\n Coords (%d,%d,%d,%d) & (%d,%d,%d,%d), first linguortal identified by linedef %d intended to be viewed from linedef %d, second linguortal identified by linedef %d intended to be viewed from linedef %d\n",
					box1.Xmin, box1.Ymin, box1.Xmax, box1.Ymax,
					box2.Xmin, box2.Ymin, box2.Xmax, box2.Ymax,
					ling1.didx, ling1.lidx, ling2.didx, ling2.lidx)
			}
		}
	}

	// let's see if mapper is cooperative one
	bigFree := freeBoxes[0]
	bigLing := linguortals[0].bbox

	for i := 1; i < len(freeBoxes); i++ {
		bigFree = mergeBbox(bigFree, freeBoxes[i])
	}

	for i := 1; i < len(linguortals); i++ {
		bigLing = mergeBbox(bigLing, linguortals[i].bbox)
	}

	cooperative := !intersectBbox(bigFree, bigLing)
	if cooperative {
		Log.Verbose(1, "DetectLinguortals: map setup is cooperative - linguortal islands are isolated from the rest of map islands as one\n")
		Log.Verbose(1, "Free: (%d,%d,%d,%d) Linguortal(s): (%d,%d,%d,%d)\n",
			bigFree.Xmin, bigFree.Ymin, bigFree.Xmax, bigFree.Ymax,
			bigLing.Xmin, bigLing.Ymin, bigLing.Xmax, bigLing.Ymax)
	} else {
		// Additional limitation to check - dunno if/when will remove:
		// bigFree is not allowed to intersect ANY of linguortals
		for i, _ := range linguortals {
			if intersectBbox(bigFree, linguortals[i].bbox) {
				Log.Panic("Rectange surrounding all islands that are not linguortal destinations AT ONCE must intersect NO rectangle around any island that is linguortal destination\n"+
					"Free: (%d,%d,%d,%d) Linguortal %d: (%d,%d,%d,%d)\n",
					bigFree.Xmin, bigFree.Ymin, bigFree.Xmax, bigFree.Ymax,
					i, linguortals[i].bbox.Xmin, linguortals[i].bbox.Ymin,
					linguortals[i].bbox.Xmax, linguortals[i].bbox.Ymax)
			}
		}
		Log.Verbose(1, "DetectLinguortals: map setup is UNcooperative -  linguortal islands are NOT isolated from the rest of map islands as one\n")
		Log.Verbose(1, "Free: (%d,%d,%d,%d) Linguortal(s): (%d,%d,%d,%d)\n",
			bigFree.Xmin, bigFree.Ymin, bigFree.Xmax, bigFree.Ymax,
			bigLing.Xmin, bigLing.Ymin, bigLing.Xmax, bigLing.Ymax)
		// Then we have trouble. We have a rather difficult sequence of isolating things from each other
	}
	lb := LinguortalBundle{
		linguortals: linguortals,
		cooperative: cooperative,
		freeBoxes:   freeBoxes,
		bigFree:     bigFree,
		bigLing:     bigLing,
	}
	return &lb
}

// there might be multiple types of linguortal-opening linedefs (as both
// translating and non-translating linguortal effects have their benefits),
// and they also may be disableable via parameter
func IsLinguortalAction(lines WriteableLines, i uint16) bool {
	a := lines.GetAction(i)
	return LINGUORTAL_SIMPLE_OPEN != 0 && a == LINGUORTAL_SIMPLE_OPEN
}

func getGraphBounds(graph *CullerGraph) *NodeBounds {
	var r NodeBounds
	// using 32-bit signed min/max values as initial, even though most engines
	// don't have coords this big
	r.Xmax = -2147483648
	r.Ymax = -2147483648
	r.Xmin = 2147483647
	r.Ymin = 2147483647
	for _, v := range graph.vertices {

		if v.X < Number(r.Xmin) {
			r.Xmin = int(v.X)
		}
		if v.X > Number(r.Xmax) {
			r.Xmax = v.X.Ceil()
		}
		if v.Y < Number(r.Ymin) {
			r.Ymin = int(v.Y)
		}
		if v.Y > Number(r.Ymax) {
			r.Ymax = v.Y.Ceil()
		}

	}
	return &r
}

func intersectBbox(bbox1, bbox2 *NodeBounds) bool {
	return asymmetricIntersectBoxes(bbox1, bbox2) ||
		asymmetricIntersectBoxes(bbox2, bbox1)
}

func asymmetricIntersectBoxes(bbox1, bbox2 *NodeBounds) bool {
	return (pieceBound(bbox1.Xmin, bbox1.Xmax, bbox2.Xmin) &&
		(pieceBound(bbox1.Ymin, bbox1.Ymax, bbox2.Ymin) ||
			pieceBound(bbox1.Ymin, bbox1.Ymax, bbox2.Ymax))) ||
		(pieceBound(bbox1.Xmin, bbox1.Xmax, bbox2.Xmax) &&
			(pieceBound(bbox1.Ymin, bbox1.Ymax, bbox2.Ymin) ||
				pieceBound(bbox1.Ymin, bbox1.Ymax, bbox2.Ymax)))
}

// "C" is for candidate (candidate for outer, candidate for inner)
func isNestedBbox(outerC, innerC *NodeBounds) bool {
	return innerC.Xmin >= outerC.Xmin && innerC.Xmax <= outerC.Xmax &&
		innerC.Ymin >= outerC.Ymin && innerC.Ymax <= outerC.Ymax
}

func pieceBound(loCoord, hiCoord, testCoord int) bool {
	return loCoord <= testCoord && hiCoord >= testCoord
}

func mergeBbox(bbox1, bbox2 *NodeBounds) *NodeBounds {
	var r NodeBounds
	r.Xmax = bbox1.Xmax
	r.Ymax = bbox1.Ymax
	r.Xmin = bbox1.Xmin
	r.Ymin = bbox1.Ymin

	if r.Xmax < bbox2.Xmax {
		r.Xmax = bbox2.Xmax
	}

	if r.Xmin > bbox2.Xmin {
		r.Xmin = bbox2.Xmin
	}

	if r.Ymax < bbox2.Ymax {
		r.Ymax = bbox2.Ymax
	}

	if r.Ymin > bbox2.Ymin {
		r.Ymin = bbox2.Ymin
	}
	return &r
}

func mergeGraphs(g1, g2 *CullerGraph) *CullerGraph {
	gR := &CullerGraph{
		lines:    append(make([]CullerLine, 0), g1.lines...),
		vertices: append(make([]NodeVertex, 0), g1.vertices...),
	}

	lv := len(gR.vertices)
	ll := len(gR.lines)
	gR.lines = append(gR.lines, g2.lines...)
	gR.vertices = append(gR.vertices, g2.vertices...)
	for i := ll; i < len(gR.lines); i++ {
		gR.lines[i].startVertex += lv
		gR.lines[i].endVertex += lv
	}
	// Not sure if gR.vertices[*].idx needs to be updated

	return gR
}

// each island is a collection of graphs one of which encompasses the rest -
// lingourtals can be composed of multiple sectors, and their borders not
// necessary make up a single connected component of entire map graph
func identifyIslands(src []CullerGraph) []*CullerGraph {
	boxes := make([]*NodeBounds, len(src))
	dst := make([]*CullerGraph, len(src))
	for i, _ := range src {
		boxes[i] = getGraphBounds(&src[i])
		dst[i] = &(src[i])
	}
	eliminate := make([]bool, len(src))

	for i, _ := range src {
		if eliminate[i] {
			continue
		}
		item := dst[i]
		for j, _ := range src {
			if i == j || eliminate[j] {
				continue
			}
			item2 := dst[j]
			if isNestedBbox(boxes[i], boxes[j]) {
				item = mergeGraphs(item, item2)
				eliminate[j] = true
			}
		}
		dst[i] = item
	}

	res := make([]*CullerGraph, 0)
	for i, _ := range src {
		if !eliminate[i] {
			res = append(res, dst[i])
		}
	}
	Log.Verbose(2, "DetectLinguortals: identified %d islands (from %d connected components).\n", len(res), len(src))
	if config.VerbosityLevel >= 3 { // reference to global: config
		for i := 0; i < len(res); i++ {
			box := getGraphBounds(res[i])
			Log.Printf("DetectLinguortals: island %d: (%d,%d,%d,%d)\n",
				i, box.Xmin, box.Ymin, box.Xmax, box.Ymax)
		}
	}
	return res
}
