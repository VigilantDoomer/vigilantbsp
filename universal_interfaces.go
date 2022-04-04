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

// Game-agnostic interfaces and their game-dependent implementations
package main

import (
	"math"
)

const POLY_BOX_SZ = 10

// Represents read-only ordered collection of lines for use in game-agnostic
// computations
// Lines can represent Linedefs or Segs of any game: Doom, Hexen, etc.
// Objects implementing this interface MUST implement methods declared here in
// such a way that they are stateless, allowing the interface to be accessed
// concurrently
type AbstractLines interface {
	// returns coordinates of both #idx line's vertices, in the order X1,Y1,X2,Y2
	GetAllXY(idx uint16) (int, int, int, int)
	// returns one of the line's sidedef
	GetSidedefIndex(lidx uint16, front bool) uint16
	// returns line's flags as 32-bit integer. Note: most engines store it as
	// 16-bit integer, except for Doom 64, hence I made flags 32-bit in advance
	GetFlags(idx uint16) uint32
	// returns total count of lines
	Len() uint16
	// this line should be ignored because its properties known to implementation
	// render it contextually irrelevant for building a particular kind of blockmap
	BlockmapSkipThis(idx uint16) bool
	// Returns interface that represents the same lines, but fit to construct
	// _internal purpose_ blockmap that includes all (not just solid) lines. For
	// use in reject builder, when trying to rewire self-referencing effect
	// lines to indicate the true sectors
	GetAuxVersion() AbstractLines
}

// WriteableLines subclasses AbstractLines interface, AND adds methods that DO
// mutate the internal state of whatever structure implements it, as well as
// ability to retrieve some of the structures originally used (as they may be
// modified through this interface)
type WriteableLines interface {
	AbstractLines
	// MUTATES and must be called before any operation on vertices, such as
	// PruneUnusedVertices or AddVertex. Might copy other structures too (linedefs?)
	UnshareData()
	// MUTATES remove vertices that are not referenced by any linedef
	PruneUnusedVertices()
	// Zokumbsp's tag 998 for lines that should not be rendered
	IsDoNotRender(lidx uint16) bool
	// BSP v5.2 horizon effect for tag 999 on one-sided lines
	IsHorizonEffect(lidx uint16) bool
	// Return numeric code for linedef's action. Use other methods when possible!
	GetAction(lidx uint16) uint16
	// Return numeric code for linedef's tag. Use other methods when possible!
	GetTag(lidx uint16) uint16
	// Returns what kind of treatment needs to be applied to seg's computed
	// angle to produce the final seg's value
	GetBAMEffect(lidx uint16) BAMEffect
	// returns indices for linedef's StartVertex and EndVertex
	GetLinedefVertices(lidx uint16) (int, int)
	// returns whether the linedef was marked as a precious, which means nodes
	// builder should avoid splitting it at all costs
	IsTaggedPrecious(lidx uint16) bool
	// returns number of vertices
	GetVerticesCount() int
	// MUTATES linedefs of sectors containing polyobjects are "tagged" precious
	DetectPolyobjects()
	// MUTATES adds a new vertex and returns its index
	AddVertex(X, Y int) int
	// returns underlying slice used for representing vertices
	GetVertices() interface{}
	// returns underlying slice used for representing linedefs
	GetLinedefs() interface{}
}

// This is used to:
// 1. build regular blockmap, for BLOCKMAP lump. Only methods from AbstractLines
// interface are used in that instance
// BlockmapSkipThis(idx) == true for lines that shouldn't be included in it, such
// as explicitly excluded by user
// 2. build nodes. This implements WriteableLines interface, and yes methods
// that mutate state are absolutely used for this
type DoomLinedefs struct {
	linedefs []Linedef
	vertices []Vertex
}

// This is used to build blockmap consisting only of solid lines, which is used
// for internal purposes
// BlockmapSkipThis(idx) == true for lines with two-sided flag set
type DoomSolidLinedefs struct {
	linedefs []Linedef
	vertices []Vertex
}

// This is used to build blockmap that contain all of lines, for internal
// purposes, with no intention to omit non-collidable and other stuff.
type DoomAuxLinedefs struct {
	linedefs []Linedef
	vertices []Vertex
}

// Like DoomLinedefs, but unfortunately there is no support for zokum tags in
// Hexen format YET,
// because Arg1 (which seems to perform function similar to tags) is uint8 not
// uint16, and thus tags > 900 are definitely out of range. Actions are also
// uint8 instead of uint16. This means zokum special effects and horizon effect
// from BSP v5.2 are not supported in Hexen
// Precious linedefs are automatically discovered: linedefs of sectors that
// make up polyobjects are the ones considered precious
type HexenLinedefs struct {
	linedefs         []HexenLinedef
	vertices         []Vertex
	things           []HexenThing
	sidedefs         []Sidedef
	sectorHasPolyobj map[uint16]bool
	preciousLinedefs []bool // true for linedefs that are part of sector that has a polyobj
}

// Hexen analogue of DoomSolidLinedefs
type HexenSolidLinedefs struct {
	linedefs []HexenLinedef
	vertices []Vertex
}

// Hexen analogue of DoomAuxLinedefs
type HexenAuxLinedefs struct {
	linedefs []HexenLinedef
	vertices []Vertex
}

func (o *DoomLinedefs) GetAllXY(idx uint16) (int, int, int, int) {
	line := o.linedefs[idx]
	x1 := int(o.vertices[line.StartVertex].XPos)
	y1 := int(o.vertices[line.StartVertex].YPos)
	x2 := int(o.vertices[line.EndVertex].XPos)
	y2 := int(o.vertices[line.EndVertex].YPos)
	return x1, y1, x2, y2
}

func (o *DoomLinedefs) Len() uint16 {
	return uint16(len(o.linedefs))
}

func (o *DoomLinedefs) BlockmapSkipThis(idx uint16) bool {
	line := o.linedefs[idx]
	if line.Tag == 999 { // Zokumbsp: explicit "Remove this linedef from blockmap" for level designer (Tag 999)
		// By the way, real good that 999 is a Tag not a linedef special, so
		// that avoiding intercepts overflow by removing linedefs from blockmap
		// with this tag won't contribute to spechits
		return true
	}
	return false
}

func (o *DoomLinedefs) GetSidedefIndex(lidx uint16, front bool) uint16 {
	if front {
		return o.linedefs[lidx].FrontSdef
	} else {
		return o.linedefs[lidx].BackSdef
	}
}

func (o *DoomLinedefs) GetFlags(idx uint16) uint32 {
	return uint32(o.linedefs[idx].Flags)
}

func (o *DoomLinedefs) UnshareData() {
	// MUST make copy of original, as original is passed as read-only to other
	// threads. Remember that slices are passed by reference
	newVertices := make([]Vertex, len(o.vertices))
	copy(newVertices, o.vertices)
	o.vertices = newVertices
	newLinedefs := make([]Linedef, len(o.linedefs))
	copy(newLinedefs, o.linedefs)
	o.linedefs = newLinedefs
}

func (o *DoomLinedefs) PruneUnusedVertices() {
	// Uses bsp v5.2 algo by Lee Killough, that will remove all unused trash
	// regardless of where it is, even if it is not at the end of vertices list.
	// The reason: there is a limit on a number of vertices, and for vanilla it
	// is signed int16, thus its better to do a thorough search
	numVerts := len(o.vertices)

	translate := make([]int, numVerts)
	for i, _ := range translate { // Unmark all vertices
		translate[i] = -1
	}
	for i := 0; i < len(o.linedefs); i++ {
		s := int(o.linedefs[i].StartVertex)
		e := int(o.linedefs[i].EndVertex)
		if s >= numVerts || e >= numVerts {
			// TODO this should go to stderr
			Log.Printf("Linedef %d has vertex out of range\n", i)
		}
		translate[s] = 0
		translate[e] = 0
	}
	usedVerts := 0
	for i := 0; i < numVerts; i++ { // Sift up all unused vertices
		if translate[i] == 0 {
			translate[i] = usedVerts
			o.vertices[usedVerts] = o.vertices[i]
			usedVerts++
		}
	}

	for i := 0; i < len(o.linedefs); i++ { // Renumber vertices
		s := translate[o.linedefs[i].StartVertex]
		e := translate[o.linedefs[i].EndVertex]
		if s >= usedVerts || e >= usedVerts {
			// TODO Internal error
			Log.Printf("Trouble in PruneUnusedVertices: Renumbering\n")
		}
		o.linedefs[i].StartVertex = uint16(s)
		o.linedefs[i].EndVertex = uint16(e)
	}

	if numVerts > usedVerts {
		Log.Verbose(1, "Loaded %d vertices, but %d were unused\n(this is normal if the nodes were built before).\n",
			numVerts, numVerts-usedVerts)
	} else {
		Log.Verbose(1, "Loaded %d vertices.\n", numVerts)
	}
	numVerts = usedVerts
	if numVerts == 0 {
		// TODO stderr
		Log.Printf("Couldn't find any used Vertices\n")
	}
	// Cut the array to the number of vertices used
	o.vertices = o.vertices[:numVerts]
	if numVerts > 65535 {
		// TODO stderr, cause nodes builder can only add more, and this is already too much
		Log.Printf("The number of vertices remains too big for any port despite the pruning. %d > %d", numVerts, 65535)
	} else if numVerts > 32767 {
		Log.Printf("The number of vertices remains too big for vanilla despite the pruning. %d > %d", numVerts, 32767)
	}
}

func (o *DoomLinedefs) DetectPolyobjects() {
	// No-op - no polyobjects in Doom format
}

func (o *DoomLinedefs) GetVertices() interface{} {
	return o.vertices
}

func (o *DoomLinedefs) GetLinedefs() interface{} {
	return o.linedefs
}

func (o *DoomLinedefs) GetLinedefVertices(lidx uint16) (int, int) {
	return int(o.linedefs[lidx].StartVertex), int(o.linedefs[lidx].EndVertex)
}

func (o *DoomLinedefs) GetVerticesCount() int {
	return len(o.vertices)
}

func (o *DoomLinedefs) IsHorizonEffect(lidx uint16) bool {
	// One-sided linedefs tagged 999 are to be considered horizon effect
	// and have special angle treatment based on offset
	// Note: BSP v5.2 didn't take one-sidedness to account, while zokumbsp
	// made tag 999 mean "remove from blockmap" which although nice for horizon,
	// means that some way to instruct to remove lines from blockmap is needed.
	// So this is the way: horizon effect is only useful and so only used
	// for one-sided lines, on two-sided lines we'll remove the line from blockmap
	// without applying horizon effect (see BlockmapSkipThis method)
	return (o.linedefs[lidx].Tag == 999) && !(o.linedefs[lidx].Flags&LF_TWOSIDED == LF_TWOSIDED)
}

func (o *DoomLinedefs) IsDoNotRender(lidx uint16) bool {
	return o.linedefs[lidx].Tag == 998 || o.linedefs[lidx].Action == 1086
}

func (o *DoomLinedefs) IsTaggedPrecious(lidx uint16) bool {
	tag := o.linedefs[lidx].Tag
	// Like BSP nodebuilder as per Lee Killough ideas, but don't consider things
	// removed from blockmap as precious, ditto with no render for which seg
	// ain't created
	return (tag >= 900) && (tag != 999) && (tag != 998)
}

func (o *DoomLinedefs) GetAction(lidx uint16) uint16 {
	return o.linedefs[lidx].Action
}

func (o *DoomLinedefs) GetTag(lidx uint16) uint16 {
	return o.linedefs[lidx].Tag
}

func (o *DoomLinedefs) GetBAMEffect(lidx uint16) BAMEffect {
	action := o.linedefs[lidx].Action
	bamEffect := BAMEffect{
		Action: BAM_NOSPECIAL,
		Value:  0,
	}
	if action == 1081 || action == 1083 {
		bamEffect.Action = BAM_REPLACE
	} else if action == 1080 || action == 1082 {
		bamEffect.Action = BAM_ADDITIVE
	}
	if action == 1080 || action == 1081 {
		bamEffect.Value = int16(float64(o.linedefs[lidx].Tag) * 65536.0 / 360.0)
	} else if action == 1082 || action == 1083 {
		bamEffect.Value = int16(o.linedefs[lidx].Tag)
	}
	return bamEffect
}

func (o *DoomLinedefs) AddVertex(X, Y int) int {
	o.vertices = append(o.vertices, Vertex{
		XPos: int16(X),
		YPos: int16(Y),
	})
	return len(o.vertices) - 1
}

func (o *DoomLinedefs) GetAuxVersion() AbstractLines {
	return &DoomAuxLinedefs{
		linedefs: o.linedefs,
		vertices: o.vertices,
	}
}

func (o *HexenLinedefs) GetAllXY(idx uint16) (int, int, int, int) {
	line := o.linedefs[idx]
	x1 := int(o.vertices[line.StartVertex].XPos)
	y1 := int(o.vertices[line.StartVertex].YPos)
	x2 := int(o.vertices[line.EndVertex].XPos)
	y2 := int(o.vertices[line.EndVertex].YPos)
	return x1, y1, x2, y2
}

func (o *HexenLinedefs) Len() uint16 {
	return uint16(len(o.linedefs))
}

func (o *HexenLinedefs) BlockmapSkipThis(idx uint16) bool {
	return false
}

func (o *HexenLinedefs) GetSidedefIndex(lidx uint16, front bool) uint16 {
	if front {
		return o.linedefs[lidx].FrontSdef
	} else {
		return o.linedefs[lidx].BackSdef
	}
}

func (o *HexenLinedefs) GetFlags(idx uint16) uint32 {
	return uint32(o.linedefs[idx].Flags)
}

func (o *HexenLinedefs) UnshareData() {
	// MUST make copy of original, as original is passed as read-only to other
	// threads. Remember that slices are passed by reference
	newVertices := make([]Vertex, len(o.vertices))
	copy(newVertices, o.vertices)
	o.vertices = newVertices
	newLinedefs := make([]HexenLinedef, len(o.linedefs))
	copy(newLinedefs, o.linedefs)
	o.linedefs = newLinedefs
}

func (o *HexenLinedefs) PruneUnusedVertices() {
	// Uses bsp v5.2 algo by Lee Killough, that will remove all unused trash
	// regardless of where it is, even if it is not at the end of vertices list.
	// The reason: there is a limit on a number of vertices, and for vanilla it
	// is signed int16, thus its better to do a thorough search
	numVerts := len(o.vertices)

	translate := make([]int, numVerts)
	for i, _ := range translate { // Unmark all vertices
		translate[i] = -1
	}
	for i := 0; i < len(o.linedefs); i++ {
		s := int(o.linedefs[i].StartVertex)
		e := int(o.linedefs[i].EndVertex)
		if s >= numVerts || e >= numVerts {
			Log.Error("Linedef %d has vertex out of range\n", i)
		}
		translate[s] = 0
		translate[e] = 0
	}
	usedVerts := 0
	for i := 0; i < numVerts; i++ { // Sift up all unused vertices
		if translate[i] == 0 {
			translate[i] = usedVerts
			o.vertices[usedVerts] = o.vertices[i]
			usedVerts++
		}
	}

	for i := 0; i < len(o.linedefs); i++ { // Renumber vertices
		s := translate[o.linedefs[i].StartVertex]
		e := translate[o.linedefs[i].EndVertex]
		if s >= usedVerts || e >= usedVerts {
			Log.Error("Trouble in PruneUnusedVertices: Renumbering\n")
		}
		o.linedefs[i].StartVertex = uint16(s)
		o.linedefs[i].EndVertex = uint16(e)
	}

	if numVerts > usedVerts {
		Log.Verbose(1, "Loaded %d vertices, but %d were unused\n(this is normal if the nodes were built before).\n",
			numVerts, numVerts-usedVerts)
	} else {
		Log.Verbose(1, "Loaded %d vertices.\n", numVerts)
	}
	numVerts = usedVerts
	if numVerts == 0 {
		Log.Error("Couldn't find any used Vertices\n")
	}
	// Cut the array to the number of vertices used
	o.vertices = o.vertices[:numVerts]
	if numVerts > 65535 {
		Log.Error("The number of vertices remains too big for any port despite the pruning. %d > %d", numVerts, 65535)
	} else if numVerts > 32767 {
		Log.Printf("The number of vertices remains too big for vanilla despite the pruning. %d > %d", numVerts, 32767)
	}
}

// Based on code courtesy of Janis Legzdinsh.
// Possibly needs optimizations for maps heavy on polyobjects -- VigilantDoomer
func (o *HexenLinedefs) DetectPolyobjects() {
	o.preciousLinedefs = make([]bool, len(o.linedefs))

	// -JL- There's a conflict between Hexen polyobj thing types and Doom thing
	//      types. In Doom type 3001 is for Imp and 3002 for Demon. To solve
	//      this problem, first we are going through all lines to see if the
	//      level has any polyobjs. If found, we also must detect what polyobj
	//      thing types are used - Hexen ones or ZDoom ones. That's why we
	//      are going through all things searching for ZDoom polyobj thing
	//      types. If any found, we assume that ZDoom polyobj thing types are
	//      used, otherwise Hexen polyobj thing types are used.

	// -JL- First go through all lines to see if level contains any polyobjs
	hasRelevantActions := false
	for _, line := range o.linedefs {
		if line.Action == HEXEN_ACTION_POLY_START ||
			line.Action == HEXEN_ACTION_POLY_EXPLICIT {
			hasRelevantActions = true
			break
		}
	}
	if !hasRelevantActions {
		Log.Verbose(1, "No lines acting on polyobjects => no polyobjects.\n")
		// No lines acting on polyobjects in this level
		return
	}
	o.sectorHasPolyobj = make(map[uint16]bool)
	hexenStyle := true
	for _, thing := range o.things {
		if thing.Type == ZDOOM_PO_SPAWN_TYPE ||
			thing.Type == ZDOOM_PO_SPAWNCRUSH_TYPE {
			hexenStyle = false
			break
		}
	}
	if hexenStyle {
		Log.Verbose(1, "Using Hexen-style polyobj things.\n")
	} else {
		Log.Verbose(1, "Using Zdoom-style polyobj things.\n")
	}

	if hexenStyle {
		for i, thing := range o.things {
			if thing.Type != PO_SPAWN_TYPE && thing.Type != PO_SPAWNCRUSH_TYPE {
				continue
			}
			Log.Verbose(3, "Thing %d at (%d,%d) is a polyobj spawner\n",
				i, thing.XPos, thing.YPos)
			o.markPolyobjPoint(i, thing.XPos, thing.YPos)
		}
	} else {
		for i, thing := range o.things {
			if thing.Type != ZDOOM_PO_SPAWN_TYPE && thing.Type != ZDOOM_PO_SPAWNCRUSH_TYPE {
				continue
			}
			Log.Verbose(3, "Thing %d at (%d,%d) is a polyobj spawner\n",
				i, thing.XPos, thing.YPos)
			o.markPolyobjPoint(i, thing.XPos, thing.YPos)
		}
	}
	o.sectorHasPolyobj = nil // ran its course
}

// Basically port of AJ-BSP code, with comments also copied. -- VigilantDoomer
func (o *HexenLinedefs) markPolyobjPoint(thingNum int, x, y int16) {

	// -AJA- First we handle the "awkward" cases where the polyobj sits
	//       directly on a linedef or even a vertex.  We check all lines
	//       that intersect a small box around the spawn point.
	bminx := int(x) - POLY_BOX_SZ
	bminy := int(y) - POLY_BOX_SZ
	bmaxx := int(x) + POLY_BOX_SZ
	bmaxy := int(y) + POLY_BOX_SZ
	insideCount := 0
	for i, line := range o.linedefs {
		x1, y1, x2, y2 := o.GetAllXY(uint16(i))
		if checkLinedefInsideBox(bminx, bminy, bmaxx, bmaxy, x1, y1, x2, y2) {
			if line.FrontSdef != SIDEDEF_NONE {
				o.markPolyobjSector(o.sidedefs[line.FrontSdef].Sector)
			}
			if line.BackSdef != SIDEDEF_NONE {
				o.markPolyobjSector(o.sidedefs[line.BackSdef].Sector)
			}
			insideCount++
		}
	}
	if insideCount > 0 {
		return
	}

	// -AJA- Algorithm is just like in DEU: we cast a line horizontally
	//       from the given (x,y) position and find all linedefs that
	//       intersect it, choosing the one with the closest distance.
	//       If the point is sitting directly on a (two-sided) line,
	//       then we mark the sectors on both sides.
	yInt := int(y)
	bestDist := float64(99999999.0) // > maximum possible distance (max = 65536 * sqrt(2))
	bestMatch := -1
	for i, _ := range o.linedefs {
		x1, y1, x2, y2 := o.GetAllXY(uint16(i))
		if y1 == y2 {
			continue
		}
		if (yInt > y1 && yInt > y2) || (yInt < y1 && yInt < y2) {
			continue
		}
		xCut := float64(x1) + float64(x2-x1)*float64(yInt-y1)/
			float64(y2-y1) - float64(x)
		if math.Abs(xCut) < math.Abs(bestDist) {
			bestMatch = i
			bestDist = xCut
		}
	}

	if bestMatch < 0 {
		Log.Error("Bad polyobj thing index %d at (%d, %d) - I failed to trace its enclosing sector.\n",
			thingNum, x, y)
		return
	}
	_, y1, _, y2 := o.GetAllXY(uint16(bestMatch))
	Log.Verbose(2, "Closest line (polyobj thing = %d) was %d y1..y2=%d..%d (dist=%f)\n",
		thingNum, bestMatch, y1, y2, bestDist)
	sector := -1
	line := o.linedefs[bestMatch]
	if (y1 > y2) == (bestDist > 0.0) {
		if line.FrontSdef != SIDEDEF_NONE {
			sector = int(o.sidedefs[line.FrontSdef].Sector)
		}
	} else {
		if line.BackSdef != SIDEDEF_NONE {
			sector = int(o.sidedefs[line.BackSdef].Sector)
		}
	}
	if sector < 0 {
		Log.Error("Bad polyobj thing index %d at (%d, %d) - traced line %d but it didn't have sidedef on a relevant side.\n",
			thingNum, x, y, bestMatch)
		return
	}
	o.markPolyobjSector(uint16(sector))
}

func (o *HexenLinedefs) markPolyobjSector(sector uint16) {
	if o.sectorHasPolyobj[sector] {
		return
	}
	o.sectorHasPolyobj[sector] = true
	// mark all lines of this sector as precious, to prevent the sector
	// from being split.
	for i, line := range o.linedefs {
		// FIXME what if sidedef at this index doesn't exist (malformed input)?
		// Need safe lookup...
		if (line.FrontSdef != SIDEDEF_NONE &&
			o.sidedefs[line.FrontSdef].Sector == sector) ||
			(line.BackSdef != SIDEDEF_NONE &&
				o.sidedefs[line.BackSdef].Sector == sector) {
			o.preciousLinedefs[i] = true
		}
	}
}

func checkLinedefInsideBox(xmin, ymin, xmax, ymax, x1, y1, x2, y2 int) bool {
	count := 2
	for {
		if y1 > ymax {
			if y2 > ymax {
				return false
			}
			x1 = x1 + int(float64(x2-x1)*float64(ymax-y1)/float64(y2-y1))
			y1 = ymax
			count = 2
			continue
		}

		if y1 < ymin {
			if y2 < ymin {
				return false
			}
			x1 = x1 + int(float64(x2-x1)*float64(ymin-y1)/float64(y2-y1))
			y1 = ymin
			count = 2
			continue
		}

		if x1 > xmax {
			if x2 > xmax {
				return false
			}
			y1 = y1 + int(float64(y2-y1)*float64(xmax-x1)/float64(x2-x1))
			x1 = xmax
			count = 2
			continue
		}

		if x1 < xmin {
			if x2 < xmin {
				return false
			}
			y1 = y1 + int(float64(y2-y1)*float64(xmin-x1)/float64(x2-x1))
			x1 = xmin
			count = 2
			continue
		}

		count--
		if count == 0 {
			break
		}

		// swap end points
		x1, x2 = x2, x1
		y1, y2 = y2, y1
	}
	// linedef touches block
	return true
}

func (o *HexenLinedefs) GetVertices() interface{} {
	return o.vertices
}

func (o *HexenLinedefs) GetLinedefs() interface{} {
	return o.linedefs
}

func (o *HexenLinedefs) GetLinedefVertices(lidx uint16) (int, int) {
	return int(o.linedefs[lidx].StartVertex), int(o.linedefs[lidx].EndVertex)
}

func (o *HexenLinedefs) GetVerticesCount() int {
	return len(o.vertices)
}

func (o *HexenLinedefs) IsHorizonEffect(lidx uint16) bool {
	return false
}

func (o *HexenLinedefs) IsDoNotRender(lidx uint16) bool {
	return false
}

func (o *HexenLinedefs) GetAction(lidx uint16) uint16 {
	return uint16(o.linedefs[lidx].Action)
}

func (o *HexenLinedefs) GetTag(lidx uint16) uint16 {
	return uint16(o.linedefs[lidx].Arg1)
}

func (o *HexenLinedefs) IsTaggedPrecious(lidx uint16) bool {
	return o.preciousLinedefs[lidx]
}

func (o *HexenLinedefs) GetBAMEffect(lidx uint16) BAMEffect {
	// can't support zokum's numbers for bam effect specials because
	// of action being only 8 bits in Hexen format
	return BAMEffect{
		Action: BAM_NOSPECIAL,
		Value:  0,
	}
}

func (o *HexenLinedefs) AddVertex(X, Y int) int {
	o.vertices = append(o.vertices, Vertex{
		XPos: int16(X),
		YPos: int16(Y),
	})
	return len(o.vertices) - 1
}

func (o *HexenLinedefs) GetAuxVersion() AbstractLines {
	return &HexenAuxLinedefs{
		linedefs: o.linedefs,
		vertices: o.vertices,
	}
}

func (o *DoomSolidLinedefs) GetAllXY(idx uint16) (int, int, int, int) {
	line := o.linedefs[idx]
	x1 := int(o.vertices[line.StartVertex].XPos)
	y1 := int(o.vertices[line.StartVertex].YPos)
	x2 := int(o.vertices[line.EndVertex].XPos)
	y2 := int(o.vertices[line.EndVertex].YPos)
	return x1, y1, x2, y2
}

func (o *DoomSolidLinedefs) Len() uint16 {
	return uint16(len(o.linedefs))
}

func (o *DoomSolidLinedefs) BlockmapSkipThis(idx uint16) bool {
	line := o.linedefs[idx]
	if (line.Flags & LF_TWOSIDED) == LF_TWOSIDED {
		return true
	}
	return false
}

func (o *DoomSolidLinedefs) GetSidedefIndex(lidx uint16, front bool) uint16 {
	if front {
		return o.linedefs[lidx].FrontSdef
	} else {
		return o.linedefs[lidx].BackSdef
	}
}

func (o *DoomSolidLinedefs) GetFlags(idx uint16) uint32 {
	return uint32(o.linedefs[idx].Flags)
}

func (o *DoomSolidLinedefs) GetAuxVersion() AbstractLines {
	return &DoomAuxLinedefs{
		linedefs: o.linedefs,
		vertices: o.vertices,
	}
}

func (o *HexenSolidLinedefs) GetAllXY(idx uint16) (int, int, int, int) {
	line := o.linedefs[idx]
	x1 := int(o.vertices[line.StartVertex].XPos)
	y1 := int(o.vertices[line.StartVertex].YPos)
	x2 := int(o.vertices[line.EndVertex].XPos)
	y2 := int(o.vertices[line.EndVertex].YPos)
	return x1, y1, x2, y2
}

func (o *HexenSolidLinedefs) Len() uint16 {
	return uint16(len(o.linedefs))
}

func (o *HexenSolidLinedefs) BlockmapSkipThis(idx uint16) bool {
	line := o.linedefs[idx]
	if (line.Flags & LF_TWOSIDED) == LF_TWOSIDED {
		return true
	}
	return false
}

func (o *HexenSolidLinedefs) GetSidedefIndex(lidx uint16, front bool) uint16 {
	if front {
		return o.linedefs[lidx].FrontSdef
	} else {
		return o.linedefs[lidx].BackSdef
	}
}

func (o *HexenSolidLinedefs) GetFlags(idx uint16) uint32 {
	return uint32(o.linedefs[idx].Flags)
}

func (o *HexenSolidLinedefs) GetAuxVersion() AbstractLines {
	return &HexenAuxLinedefs{
		linedefs: o.linedefs,
		vertices: o.vertices,
	}
}

func (o *DoomAuxLinedefs) GetAllXY(idx uint16) (int, int, int, int) {
	line := o.linedefs[idx]
	x1 := int(o.vertices[line.StartVertex].XPos)
	y1 := int(o.vertices[line.StartVertex].YPos)
	x2 := int(o.vertices[line.EndVertex].XPos)
	y2 := int(o.vertices[line.EndVertex].YPos)
	return x1, y1, x2, y2
}

func (o *DoomAuxLinedefs) Len() uint16 {
	return uint16(len(o.linedefs))
}

func (o *DoomAuxLinedefs) BlockmapSkipThis(idx uint16) bool {
	return false
}

func (o *DoomAuxLinedefs) GetSidedefIndex(lidx uint16, front bool) uint16 {
	if front {
		return o.linedefs[lidx].FrontSdef
	} else {
		return o.linedefs[lidx].BackSdef
	}
}

func (o *DoomAuxLinedefs) GetFlags(idx uint16) uint32 {
	return uint32(o.linedefs[idx].Flags)
}

func (o *DoomAuxLinedefs) GetAuxVersion() AbstractLines {
	return o
}

func (o *HexenAuxLinedefs) GetAllXY(idx uint16) (int, int, int, int) {
	line := o.linedefs[idx]
	x1 := int(o.vertices[line.StartVertex].XPos)
	y1 := int(o.vertices[line.StartVertex].YPos)
	x2 := int(o.vertices[line.EndVertex].XPos)
	y2 := int(o.vertices[line.EndVertex].YPos)
	return x1, y1, x2, y2
}

func (o *HexenAuxLinedefs) Len() uint16 {
	return uint16(len(o.linedefs))
}

func (o *HexenAuxLinedefs) BlockmapSkipThis(idx uint16) bool {
	return false
}

func (o *HexenAuxLinedefs) GetSidedefIndex(lidx uint16, front bool) uint16 {
	if front {
		return o.linedefs[lidx].FrontSdef
	} else {
		return o.linedefs[lidx].BackSdef
	}
}

func (o *HexenAuxLinedefs) GetFlags(idx uint16) uint32 {
	return uint32(o.linedefs[idx].Flags)
}

func (o *HexenAuxLinedefs) GetAuxVersion() AbstractLines {
	return o
}
