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

// selfref.go
package main

import (
	"fmt"
	"math"
	"sort"
)

const ( // culler mode
	CREATE_SEGS = iota
	CREATE_SEGS_SLOPPY
	CREATE_REJECT
)

// Culler is responsible for informing the caller when to skip lines, and which
// lines, although initially skipped, may actually need to be treated with
// utmost care because they implement "self-referencing sector" effect (deep
// water/invisible floors).
// Which lines are skipped is dependent on mode, for reject it is simply all
// two-sided lines with same sector on both sides, for seg creation it is more
// involved.
// Skipping is determined by a call to AddLine, which should process all lines
// preferably (except for zero-length lines). After a series of call to AddLine,
// a call to Analyze identifies those lines that were skipped in error, and
// recovery is done by performing loop over SpewBack and consequent call to
// GetLine within the loop body.
// NOTE pedantic mode uses primarly GetPerimeters rather than loop over SpewBack,
// the latter still gets used as a fallback when something got wrong with the
// perimeters
// Culler is defined in selfref.go
type Culler struct {
	mode      int
	absLines  AbstractLines
	writLines WriteableLines
	sidedefs  []Sidedef
	// Definitions that are used exclusively in CREATE_SEG_SLOPPY mode
	invisibleLines  map[uint16][]uint16 // sector index -> array of linedef indices
	visibleLines    map[uint16][]uint16 // sector index -> array of linedef indices
	allLines        map[uint16][]uint16 // sector index -> array of linedef indices
	checkThisSector map[uint16]bool     // sector index -> has invisibleLines that weren't already omitted from rendering
	// Spewing out machinery
	spewOut       []uint16
	spewIdx       int
	perimeterSink map[uint16][][]uint16 // sector index -> array of array of linedef indices (array of perimeters)
}

type CullerLine struct {
	id          uint16
	startVertex int
	endVertex   int
}

type BareVertex struct {
	X int
	Y int
}

type CullerGraph struct {
	lines    []CullerLine
	vertices []NodeVertex // NodeVertex.idx will store graph num instead
}

// The mode determines everything: the criteria used to determine which lines
// are to be culled (when AddLine returns true), and how some of these lines
// can be "reanimated", or spewen back via "for SpewBack() { ... = GetLine()}"
// But, besides SetMode(), you are to call SetWriteableLines as well for
// CREATE_SEGS and CREATE_SEGS_SLOPPY modes, SetAbstractLines for CREATE_REJECT
// mode
func (c *Culler) SetMode(cullerMode int, sidedefs []Sidedef) {
	if c == nil {
		return
	}
	c.mode = cullerMode
	c.sidedefs = sidedefs
	switch c.mode {
	case CREATE_SEGS:
		{
			c.invisibleLines = make(map[uint16][]uint16) // sector index -> array of linedef indices
			c.allLines = make(map[uint16][]uint16)       // sector index -> array of linedef indices
			c.checkThisSector = make(map[uint16]bool)
			c.spewOut = make([]uint16, 0)
		}

	case CREATE_SEGS_SLOPPY:
		{
			c.invisibleLines = make(map[uint16][]uint16) // sector index -> array of linedef indices
			c.visibleLines = make(map[uint16][]uint16)   // sector index -> array of linedef indices
			c.checkThisSector = make(map[uint16]bool)
			c.spewOut = make([]uint16, 0)
		}
	case CREATE_REJECT:
		{
			c.invisibleLines = make(map[uint16][]uint16) // sector index -> array of linedef indices
			c.allLines = make(map[uint16][]uint16)       // sector index -> array of linedef indices
			c.spewOut = make([]uint16, 0)
		}
	default:
		{
			panic("Unknown mode for culler.")
		}
	}
}

func (c *Culler) GetMode() int {
	return c.mode
}

// For modes that revolve around creating segs, SetWriteableLines is a mandatory
// call besides SetMode
func (c *Culler) SetWriteableLines(lines WriteableLines) {
	if c == nil {
		return
	}
	c.writLines = lines
	c.absLines = lines
}

// For CREATE_REJECTS, SetAbstractLines is a mandatory call besides SetMode
func (c *Culler) SetAbstractLines(lines AbstractLines) {
	if c == nil {
		return
	}
	c.absLines = lines
}

func (c *Culler) EnablePerimeterSink(enable bool) {
	if enable {
		c.perimeterSink = make(map[uint16][][]uint16)
	} else {
		c.perimeterSink = nil
	}
}

// Evaluates line identified by idx, and returns true if caller should skip it
func (c *Culler) AddLine(idx uint16) bool {
	if c == nil {
		return false
	}
	firstSdef := c.absLines.GetSidedefIndex(idx, true)
	secondSdef := c.absLines.GetSidedefIndex(idx, false)
	switch c.mode {
	case CREATE_SEGS:
		{
			return c.addLineForProperSegs(c.writLines, c.sidedefs, idx,
				firstSdef, secondSdef)
		}
	case CREATE_SEGS_SLOPPY:
		{
			return c.addLineForSloppySegs(c.writLines, c.sidedefs, idx,
				firstSdef, secondSdef)
		}
	case CREATE_REJECT:
		{
			return c.addLineForReject(c.absLines, c.sidedefs, idx,
				firstSdef, secondSdef)
		}
	}
	return false // should never reach here
}

func (c *Culler) Analyze() {
	if c == nil {
		return
	}
	c.spewIdx = -1
	switch c.mode {
	case CREATE_SEGS:
		{
			c.analyzeForProperSegs()
		}
	case CREATE_SEGS_SLOPPY:
		{
			c.analyzeForSloppySegs()
		}
	case CREATE_REJECT:
		{
			c.analyzeForReject()
		}
	}
	// Let's do it unconditionally. Do not want inconsistent visplane results
	// over subsequent runs because segs are in different order
	//if config.Deterministic { // reference to global: config
	sort.Sort(Uint16Slice(c.spewOut))
	// FIXME perimeterSink is currently a map, another probable source of
	// inconsistency. Redefine it as an array, sort it by sector number
	//}
}

func (c *Culler) SpewBack() bool {
	if c == nil {
		return false
	}
	if c.spewIdx == -1 {
		c.spewIdx = 0
	} else {
		c.spewIdx++
	}
	return c.spewIdx < len(c.spewOut)
}

func (c *Culler) GetLine() uint16 {
	// No check for c == nil, this must never be called without a SpewBack() ==
	// true call
	return c.spewOut[c.spewIdx]
}

func (c *Culler) GetPerimeters() map[uint16][][]uint16 {
	return c.perimeterSink
}

// Returns true if segs need not to be created for this line in first pass
// (prior to having knowledge about which sectors are self-referencing)
func canBeCulled_ForSegs(lines WriteableLines, sidedefs []Sidedef,
	i uint16, firstSdef, secondSdef uint16) bool {
	// In order to be invisible, must not:
	// 1. be precious
	// 2. be one-sided
	// 3. have different sector on both sides
	// 4. have any of the textures set
	return !lines.IsTaggedPrecious(i) &&
		uint16(lines.GetFlags(i))&LF_TWOSIDED != 0 &&
		(firstSdef != SIDEDEF_NONE) && (secondSdef != SIDEDEF_NONE) &&
		(sidedefs[firstSdef].Sector == sidedefs[secondSdef].Sector) &&
		IsEmptyTexture(sidedefs[firstSdef].LoName[:]) &&
		IsEmptyTexture(sidedefs[firstSdef].MidName[:]) &&
		IsEmptyTexture(sidedefs[firstSdef].UpName[:]) &&
		IsEmptyTexture(sidedefs[secondSdef].LoName[:]) &&
		IsEmptyTexture(sidedefs[secondSdef].MidName[:]) &&
		IsEmptyTexture(sidedefs[secondSdef].UpName[:])
}

func (c *Culler) addLineForSloppySegs(lines WriteableLines, sidedefs []Sidedef,
	i uint16, firstSdef, secondSdef uint16) bool {
	canCull := canBeCulled_ForSegs(lines, sidedefs, i, firstSdef, secondSdef)
	if canCull {
		if lines.IsDoNotRender(i) {
			// Still needs to be analyzed for proper border determination
			hashmapAddLineToSector(c.invisibleLines, sidedefs[firstSdef].Sector,
				i)
			// Return false, because although the line will not be rendered,
			// the caller must acknowledge (and print to user if verbosity
			// allows) that user made this line explicitly invisible
			return false
		} else {
			// We are only setting "check this sector" to true when that sector
			// has at least one line that we suspect can be invisible that was
			// not marked as such by user explicitly
			c.checkThisSector[sidedefs[firstSdef].Sector] = true
			hashmapAddLineToSector(c.invisibleLines, sidedefs[firstSdef].Sector,
				i)
			return true
		}
	} else {
		canCredit := !(uint16(lines.GetFlags(i))&LF_TWOSIDED != 0 &&
			firstSdef != SIDEDEF_NONE && secondSdef != SIDEDEF_NONE &&
			sidedefs[firstSdef].Sector == sidedefs[secondSdef].Sector)
		if canCredit {
			if firstSdef != SIDEDEF_NONE {
				hashmapAddLineToSector(c.visibleLines, sidedefs[firstSdef].Sector,
					i)
			}
			if secondSdef != SIDEDEF_NONE {
				hashmapAddLineToSector(c.visibleLines, sidedefs[secondSdef].Sector,
					i)
			}
		}
	}
	return false
}

func (c *Culler) addLineForProperSegs(lines WriteableLines, sidedefs []Sidedef,
	i uint16, firstSdef, secondSdef uint16) bool {
	result := false
	canCull := canBeCulled_ForSegs(lines, sidedefs, i, firstSdef, secondSdef)
	if canCull {
		if lines.IsDoNotRender(i) {
			// Still needs to be analyzed for proper border determination
			hashmapAddLineToSector(c.invisibleLines, sidedefs[firstSdef].Sector,
				i)
		} else {
			// We are only setting "check this sector" to true when that sector
			// has at least one line that we suspect can be invisible that was
			// not marked as such by user explicitly
			c.checkThisSector[sidedefs[firstSdef].Sector] = true
			hashmapAddLineToSector(c.invisibleLines, sidedefs[firstSdef].Sector,
				i)
			result = true
		}
	}
	if firstSdef != SIDEDEF_NONE {
		hashmapAddLineToSector(c.allLines, sidedefs[firstSdef].Sector,
			i)
	}
	if secondSdef != SIDEDEF_NONE {
		hashmapAddLineToSector(c.allLines, sidedefs[secondSdef].Sector,
			i)
	}
	return result
}

func (c *Culler) addLineForReject(lines AbstractLines, sidedefs []Sidedef,
	i uint16, firstSdef, secondSdef uint16) bool {
	result := false
	canCull := (uint16(lines.GetFlags(i))&LF_TWOSIDED == LF_TWOSIDED) &&
		firstSdef != SIDEDEF_NONE && secondSdef != SIDEDEF_NONE &&
		sidedefs[firstSdef].Sector == sidedefs[secondSdef].Sector
	if canCull {
		// c.checkThisSector[sidedefs[firstSdef].Sector] = true
		hashmapAddLineToSector(c.invisibleLines, sidedefs[firstSdef].Sector,
			i)
		result = true
	}
	// Here the visibleLines will contain ALL lines in sector, not just the
	// really visible lines. This will allow us to compute perimeter of sector
	// - the lines forming the border - in Analyze, and then all invisibleLines
	// that are in perimeter can be said to be implementing self-referencing
	// effect.
	if firstSdef != SIDEDEF_NONE {
		hashmapAddLineToSector(c.allLines, sidedefs[firstSdef].Sector,
			i)
	}
	if secondSdef != SIDEDEF_NONE {
		hashmapAddLineToSector(c.allLines, sidedefs[secondSdef].Sector,
			i)
	}
	return result
}

// Sloppy simply looks if whole sector may be declared self-referencing,
// by comparing the bounds derived from culled lines of a sector N with bounds
// of all solid or 2-sided lines with different sector that come from sector N
// It may fail to detect a self-referencing sector under certain conditions,
// it also tends to report a lot of sectors as self-referencing when they aren't,
// and it doesn't attempt to identify lines that can be hidden even within self-
// referencing sector
// It has both false negative and false positive cases
func (c *Culler) analyzeForSloppySegs() {
	for sector, culledLines := range c.invisibleLines {
		if !c.checkThisSector[sector] {
			// all invisible lines were already made invisible by explicit
			// request by user, thus no analysis is needed
			continue
		}
		properLines := c.visibleLines[sector]
		// If no other lines make up the sector, this IS a self-referencing
		// one. About the only one time when you can guarantee there is no
		// mistake
		mustUncull := properLines == nil
		if !mustUncull {
			// "Detects" self-referencing sector by comparing borders derived
			// from 2-sided lines with same sector on both sides with borders
			// derived from normal lines (solid of 2-sided with different sector)
			// of the same sector. If the first one is STRICTLY smaller then
			// whole sector is seen as self-referencing, and ALL lines of it
			// are spewen back as part of self-referencing effect (although this
			// is not necessary true)
			// Known false negative: self-referencing sector that was linked to
			// TWO distant non-self-referencing sectors. The self-referencing
			// part's box is inside the proper sector box, so it is seen as
			// sector that doesn't have self-referencing effect anywhere at all
			// (WRONG!)
			// Frequent false positive: monster closets with lots of
			// teleporting lines
			properLimits := findLimitsFromLines(c.writLines, properLines)
			culledLimits := findLimitsFromLines(c.writLines, culledLines)
			wasEligible := (properLimits.Xmax >= culledLimits.Xmax) &&
				(properLimits.Xmin <= culledLimits.Xmin) &&
				(properLimits.Ymax >= culledLimits.Ymax) &&
				(properLimits.Ymin <= culledLimits.Ymin) &&
				((properLimits.Xmax != culledLimits.Xmax) ||
					(properLimits.Ymax != culledLimits.Ymax) ||
					(properLimits.Ymin != culledLimits.Ymin) ||
					(properLimits.Xmin != culledLimits.Xmin))
			mustUncull = !wasEligible
		}
		if mustUncull {
			for i := 0; i < len(culledLines); i++ {
				line := culledLines[i]
				// don't spew out something that was deliberately not rendered
				// by explicit user request
				if !c.writLines.IsDoNotRender(line) {
					c.spewOut = append(c.spewOut, line)
				}
			}
		} else {
			for i := 0; i < len(culledLines); i++ {
				line := culledLines[i]
				Log.Verbose(1, "Linedef %d was removed from being rendered, I didn't find evidence to convince me sector %d was self-referencing.\n",
					line, sector)
			}
		}
	}
}

func (c *Culler) analyzeForProperSegs() {
	for sector, culledLines := range c.invisibleLines {
		if !c.checkThisSector[sector] {
			// all invisible lines were already excluded by explicit request
			// by user, thus no analysis is needed
			continue
		}
		properLines := c.allLines[sector]
		// If no other lines make up the sector, this IS a self-referencing
		// one. About the only one time when you can guarantee there is no
		// mistake
		mustUncull := len(properLines) == len(culledLines)
		if !mustUncull {
			// More expensive heuristics
			handled := c.deepAnalysis(sector, properLines, culledLines, true)
			if handled {
				continue
			} else {
				Log.Verbose(1, "Sector analysis failed for sector #%d. Will assume sector could be self-referencing and render all its lines...\n", sector)
				mustUncull = true
			}
		}
		// mustUncull == true ALWAYS now
		for i := 0; i < len(culledLines); i++ {
			line := culledLines[i]
			// don't spew out something that was deliberately not rendered
			if !c.writLines.IsDoNotRender(line) {
				c.spewOut = append(c.spewOut, line)
			}
		}

	}
}

func (c *Culler) analyzeForReject() {
	for sector, culledLines := range c.invisibleLines {
		properLines := c.allLines[sector]
		// If no other lines make up the sector, this IS a self-referencing
		// one. About the only one time when you can guarantee there is no
		// mistake
		mustUncull := len(properLines) == len(culledLines)
		if !mustUncull || c.perimeterSink != nil {
			// More expensive heuristics
			handled := c.deepAnalysis(sector, properLines, culledLines, false)
			if handled {
				continue
			} else {
				Log.Verbose(1, "Sector analysis failed for sector #%d. Will assume sector could be self-referencing and mark it as always visible.\n", sector)
				mustUncull = true
			}
		}
		// mustUncull = true ALWAYS now
		for i := 0; i < len(culledLines); i++ {
			line := culledLines[i]
			c.spewOut = append(c.spewOut, line)
		}
	}
}

// deepAnalysis Computes the border (perimeter) of sector secIdx to find
// lines that definitely need to be rendered. All culledLines that are part
// of perimeter are to be spewedBack (except when doingForSegs == true and
// the specific line in question is doNotRender)
func (c *Culler) deepAnalysis(secIdx uint16, allLines, culledLines []uint16,
	doingForSegs bool) bool {
	// Represent sector as graph
	graph := c.convertLinesToGraph(allLines)
	// Split graph into connected components (thus sector is split into
	// connected parts. Most sectors will have only one such part, but sectors
	// that were produced as a result of linking distant sectors will have
	// 2 or more parts)
	graphs, ok := graph.split()
	if !ok {
		return false
	}
	Log.Verbose(2, "Sector %d has %d connected components.", secIdx, len(graphs))
	// FIXME now if one component is completely surrounded by another (maybe
	// this can be done after identifying perimeter of each component), it needs
	// to be omitted. Hexen map01 can be used to test this... although might
	// need also verify that it is not possible to nest a different sector in
	// between the "outer" and the "inner" perimeter, fuck.
	// Anyway, there IS a problem of some lines seen as self-referencing effect
	// when they are 2-sided same-sector lines inside the proper sector (inner
	// polygon merged to the outer)

	culledLinesVisibility := make([]bool, len(culledLines))

	var perimeters [][]uint16
	if c.perimeterSink != nil {
		perimeters = make([][]uint16, 0, len(graphs))
	}

	// Now we need to determine perimeter of every component. All lines that
	// constitute the perimeter should not have been culled - if they are
	// 2-sided with same sector reference on both sides, they make up
	// self-referencing effect. Any 2-sided line that is not in the perimeter,
	// is not part of self-referencing effect
	for i, graphI := range graphs {
		if len(graphI.lines) < 3 {
			// Hm. Removing appendages in convertLinesToGraph should have
			// already taken care of this, but maybe some rare cases like that
			// of two fully overlapping lines (and nothing else) might trigger
			// this treatment
			Log.Verbose(2, "Rejecting component #%d of sector %d because it has fewer than 3 lines.\n   total lines: %s",
				i, secIdx, debugToStrLine(graphI.lines))
			continue
		}
		ok, borderLines := c.getPerimeter(graphI)
		if ok {
			Log.Verbose(2, "Sector %d component %d has %d lines in perimeter: %s",
				secIdx, i, len(borderLines), debugToStrSimple(borderLines))
			// Good. Any line that is both in culledLines and in borderLines
			// must be rendered
			if perimeters != nil {
				// We need to store whole perimeters, but only when there is
				// at least one culled line in this perimeter
				store := false
				for _, lineJ := range borderLines {
					for _, lineK := range culledLines {
						if lineJ == lineK {
							// NOTE check for doingForSegs/doNotRender is absent
							// here. Perimeter is supposed to be only stored
							// for reject-making (and even then not always),
							// segs don't make use of perimeter sink at all
							store = true
							break
						}
					}
					if store {
						break
					}
				}
				if store {
					perimeters = append(perimeters, borderLines)
				}
			} else {
				// Straightforward: all culledLines that are in borderLines are
				// added to spewOut array
				for _, lineJ := range borderLines {
					for k, lineK := range culledLines {
						if lineJ == lineK {
							if !doingForSegs || !c.writLines.IsDoNotRender(lineJ) {
								c.spewOut = append(c.spewOut, lineJ)
								culledLinesVisibility[k] = true
							}
						}
					}
				}
			}
		} else {
			Log.Verbose(2, "Failed to determine perimeter of component %d of sector %d.\n   total lines: %s",
				i, secIdx, debugToStrLine(graphI.lines))
			// So I will spewBack all lines from this graph component that I
			// speculated were invisible
			for _, lineJ := range graph.lines {
				for k, lineK := range culledLines {
					if lineJ.id == lineK {
						if !doingForSegs || !c.writLines.IsDoNotRender(lineK) {
							c.spewOut = append(c.spewOut, lineK)
							culledLinesVisibility[k] = true
						}
					}
				}
			}
			if perimeters != nil {
				// And I'll have to also spewBack all lines from perimeters,
				// as perimeters aren't going to be used
				for _, perimeter := range perimeters {
					for _, lineJ := range perimeter {
						for k, lineK := range culledLines {
							if lineJ == lineK {
								if !doingForSegs || !c.writLines.IsDoNotRender(lineK) {
									c.spewOut = append(c.spewOut, lineK)
									culledLinesVisibility[k] = true
								}
							}
						}
					}
				}
				perimeters = nil
			}
		}
	}
	if perimeters != nil && len(perimeters) > 0 {
		c.perimeterSink[secIdx] = perimeters
	}

	if doingForSegs {
		for i := 0; i < len(culledLinesVisibility); i++ {
			if !culledLinesVisibility[i] && !c.writLines.IsDoNotRender(culledLines[i]) {
				Log.Verbose(1, "2-sided line %d (sector %d) will not be rendered - is not bordering this sector.",
					culledLines[i], secIdx)
			}
		}
	}
	return true
}

func debugStrVerticesCoords(vs []NodeVertex) string {
	result := ""
	for i, v := range vs {
		result += fmt.Sprintf(",%d=(%d,%d)", i, v.X, v.Y)
	}
	return result
}

func debugToStrSimple(lines []uint16) string {
	result := ""
	for _, v := range lines {
		result += fmt.Sprintf(",%d", v)
	}
	return result
}

func debugToStrLine(lines []CullerLine) string {
	result := ""
	for _, v := range lines {
		result += fmt.Sprintf(",%d=>(%d,%d)", v.id, v.startVertex, v.endVertex)
	}
	return result
}

// getPerimeter is derived from public domain code by Darel Rex Finley
// (with acknowledgment to Moritz Ringler) as found on the page:
// https://arienryderflex.com/polygon_perimeter
// getPerimeter traces the outside edge (perimeter) of a complex (potentially
// self-crossing) polygon in a clockwise direction
func (o *Culler) getPerimeter(graph CullerGraph) (bool, []uint16) {
	// TODO some things need implementation still, look for FIXMEs etc.
	if len(graph.lines) < 3 { // not a polygon if there aren't even 3 edges
		result := make([]uint16, len(graph.lines))
		for i, line := range graph.lines {
			result[i] = line.id
		}
		return true, result
	}

	// Must choose starting pointer that is on perimeter. For this will find
	// the point with greatest Y, and (if there are multiple such points) with
	// lowest X amongst those
	startX := graph.vertices[0].X
	startY := graph.vertices[0].Y
	vertId := 0
	for i, vertex := range graph.vertices {
		if (vertex.Y > startY) || (vertex.Y == startY && vertex.X < startX) {
			startX = vertex.X
			startY = vertex.Y
			vertId = i
		}
	}

	// Now we need to see if any of the linedefs intersect or are collinear
	// with each other. (to be done, will affect performance most likely)
	// FIXME implement support for overlapping (intersecting / collinear) lines
	// Currently they would result in failing to compute perimeter. I expect
	// fallbacks in code of the culler to take care of that by just assuming
	// this sector is to be "protected" as a potentially self-referencing one,
	// but this support perhaps should be properly implemented

	// Calculate the angle of each segment
	segAngle := make([]float64, len(graph.lines))
	for i, line := range graph.lines {
		dx := float64(graph.vertices[line.endVertex].X -
			graph.vertices[line.startVertex].X)
		dy := float64(graph.vertices[line.endVertex].Y -
			graph.vertices[line.startVertex].Y)
		segAngle[i] = angleOf_InRadians(dx, dy)
		if math.IsNaN(segAngle[i]) {
			Log.Verbose(2, "getPerimeter: Aborting!!! Angle is NaN %d.\n", i)
			return false, nil
		}
	}

	pVertices := make([]int, 1)
	pVertices[0] = vertId
	pLines := make([]int, 0)
	c := startX
	d := startY
	a := c - 1.
	b := d
	var e, f int
	numVertices := 1
	var angleDif float64
	lastAngle := .5 * CIRCLE_RADIANS
	lineIdx := 0
	// defense against infinite loops
	abortLength := len(graph.lines) + 1
	// end defense against infinite loops
	for true {
		bestAngleDif := CIRCLE_RADIANS
		// Now seek out line which starts at the same vertex as the last
		// vertex of perimeter we identified, and that has the smallest angle
		// from the previous segment in the clockwise direction
		for i, line := range graph.lines {
			if graph.vertices[line.startVertex].X == c &&
				graph.vertices[line.startVertex].Y == d &&
				(a != graph.vertices[line.endVertex].X ||
					b != graph.vertices[line.endVertex].Y) {

				angleDif = lastAngle - segAngle[i]
				for angleDif >= CIRCLE_RADIANS {
					angleDif -= CIRCLE_RADIANS
				}
				for angleDif < 0 {
					angleDif += CIRCLE_RADIANS
				}
				if angleDif < bestAngleDif {
					bestAngleDif = angleDif
					e = graph.vertices[line.endVertex].X
					f = graph.vertices[line.endVertex].Y
					vertId = line.endVertex
					lineIdx = i
				}
			}

			if graph.vertices[line.endVertex].X == c &&
				graph.vertices[line.endVertex].Y == d &&
				(a != graph.vertices[line.startVertex].X ||
					b != graph.vertices[line.startVertex].Y) {

				angleDif = lastAngle - segAngle[i] + .5*CIRCLE_RADIANS
				for angleDif >= CIRCLE_RADIANS {
					angleDif -= CIRCLE_RADIANS
				}
				for angleDif < 0 {
					angleDif += CIRCLE_RADIANS
				}
				if angleDif < bestAngleDif {
					bestAngleDif = angleDif
					e = graph.vertices[line.startVertex].X
					f = graph.vertices[line.startVertex].Y
					vertId = line.startVertex
					lineIdx = i
				}
			}

		}
		if bestAngleDif == CIRCLE_RADIANS {
			Log.Verbose(2, "getPerimeter: quiting because of bestAngleDif == CIRCLE_RADIANS at numVertices = %d\n", numVertices)
			return false, nil
		}
		if len(pLines) == abortLength {
			// defense against infinite loops: number of perimeter lines got
			// greater than the total number of edges in the graph!
			// TODO when we will have splitting segments, this is going to be
			// adjusted for the number segments AFTER split
			Log.Verbose(2, "getPerimeter: aborting infinite loop at numVertices = %d\n", numVertices)
			return false, nil
		}
		// Search ends when we made full circle around the polygon
		if numVertices > 1 && c == graph.vertices[pVertices[0]].X &&
			d == graph.vertices[pVertices[0]].Y &&
			e == graph.vertices[pVertices[1]].X &&
			f == graph.vertices[pVertices[1]].Y {
			numVertices--
			break
		}
		pVertices = append(pVertices, vertId)
		pLines = append(pLines, lineIdx)
		numVertices++
		lastAngle -= bestAngleDif + .5*CIRCLE_RADIANS
		a = c
		b = d
		c = e
		d = f
	}

	// TODO the path might be shortened, but only when there was no overlapping
	// lines and no intersecting lines. Otherwise this hurdle may be necessary
	// - investigate
	result := make([]uint16, len(pLines))
	for i, lineIdx := range pLines {
		result[i] = graph.lines[lineIdx].id
	}

	return true, result
}

const CIRCLE_RADIANS = float64(6.283185307179586476925286766559)

func angleOf_InRadians(x, y float64) float64 {
	dist := math.Sqrt(x*x + y*y)
	if y >= 0. {
		return math.Acos(x / dist)
	} else {
		return math.Acos(-x/dist) + .5*CIRCLE_RADIANS
	}
}

func (c *Culler) convertLinesToGraph(lines []uint16) CullerGraph {
	var g CullerGraph
	g.lines = make([]CullerLine, 0)
	g.vertices = make([]NodeVertex, 0)
	verticeMap := make(map[BareVertex]int) // to index in vertices array
	verticesUse := make([]int, 0)
	for _, line := range lines {
		X1, Y1, X2, Y2 := c.absLines.GetAllXY(line)
		vBegin := BareVertex{X: X1, Y: Y1}
		vEnd := BareVertex{X: X2, Y: Y2}
		iBegin, ok := verticeMap[vBegin]
		if !ok {
			iBegin = len(g.vertices)
			verticeMap[vBegin] = iBegin
			g.vertices = append(g.vertices, NodeVertex{
				X:   X1,
				Y:   Y1,
				idx: uint32(iBegin), // will be used as graphId; we start with disjoint sets
			})
			verticesUse = append(verticesUse, 0)
		}
		iEnd, ok := verticeMap[vEnd]
		if !ok {
			iEnd = len(g.vertices)
			verticeMap[vEnd] = iEnd
			g.vertices = append(g.vertices, NodeVertex{
				X:   X2,
				Y:   Y2,
				idx: uint32(iEnd), // will be used as graphId; we start with disjoint sets
			})
			verticesUse = append(verticesUse, 0)
		}
		g.lines = append(g.lines, CullerLine{
			id:          line,
			startVertex: iBegin,
			endVertex:   iEnd,
		})
		verticesUse[iBegin]++
		verticesUse[iEnd]++
	}
	removeAppendages(&g, verticesUse)
	return g
}

// Removes all sequences of lines that end in vertex that belongs only to
// a single line. These lines sequences don't form a loop and thus can't be a
// polygon, and can't define a border of the sector. Examples of such would be
// trigger lines inside the sector that don't touch the sector borders, etc.
// TODO no thought was given to code efficiency when writing this function
func removeAppendages(g *CullerGraph, verticesUse []int) {
	danglingVertices := make([]int, 0)
	allVerticesToRemove := make([]int, 0)
	proceed := true
	for proceed {
		removed := false
		for verticeIdx, _ := range verticesUse {
			if verticesUse[verticeIdx] == 1 {
				danglingVertices = append(danglingVertices, verticeIdx)
				allVerticesToRemove = append(allVerticesToRemove, verticeIdx)
			}
		}
		for _, verticeIdx := range danglingVertices {
			for i, line := range g.lines {
				if line.startVertex == verticeIdx || line.endVertex == verticeIdx {
					verticesUse[line.startVertex]--
					verticesUse[line.endVertex]--
					g.lines[i].startVertex = -1
					g.lines[i].endVertex = -1
					removed = true
				}
			}
		}
		danglingVertices = danglingVertices[:0]
		proceed = removed
	}
	if len(allVerticesToRemove) == 0 {
		return
	}
	// Now remove lines that had their vertices marked
	lineCount := len(g.lines)
	for i := 0; i < lineCount; i++ {
		if g.lines[i].startVertex == -1 {
			Log.Verbose(2, "Line %d is removed from perimeter calculations (part of appendage).\n",
				g.lines[i].id)
			copy(g.lines[i:], g.lines[i+1:])
			lineCount--
			i--
		}
	}
	g.lines = g.lines[:lineCount]
	// Now remove vertices themselves
	sort.Sort(sort.IntSlice(allVerticesToRemove))
	verticesRemoved := 0
	for _, removeIdx := range allVerticesToRemove {
		// Keep in mind that as vertices are deleted, removeIdx is itself no
		// longer valid until adjusted
		removeIdx = removeIdx - verticesRemoved
		copy(g.vertices[removeIdx:], g.vertices[removeIdx+1:])
		verticesRemoved++
		// Keep line->vertice links up to date!
		for i, _ := range g.lines {
			if g.lines[i].startVertex >= removeIdx {
				g.lines[i].startVertex--
			}
			if g.lines[i].endVertex >= removeIdx {
				g.lines[i].endVertex--
			}
		}
	}
	g.vertices = g.vertices[:(len(g.vertices) - verticesRemoved)]
	// Now update vertice.idx to match the index in array as it is supposed to
	// be
	for i, _ := range g.vertices {
		g.vertices[i].idx = uint32(i)
	}
}

func mergeGraphIdx(parent []NodeVertex, x uint32) uint32 {
	if parent[x].idx == x {
		return x
	}
	return mergeGraphIdx(parent, parent[x].idx)
}

// Identifies all of connected components in CullerGraph using "disjoint set
// union" method
func (g *CullerGraph) split() ([]CullerGraph, bool) {
	// Each vertice starts with graphId (idx field) set to its index, then we
	// merge them based on edges (lines)
	for _, x := range g.lines {
		g.vertices[mergeGraphIdx(g.vertices, uint32(x.startVertex))].idx =
			mergeGraphIdx(g.vertices, uint32(x.endVertex))
	}
	// After the merge, we count the number of connected components
	numComponents := 0
	compReverseMap := make([]int, 0)
	for i := 0; i < len(g.vertices); i++ {
		if g.vertices[i].idx == uint32(i) {
			compReverseMap = append(compReverseMap, i)
			numComponents++
		}
	}
	if numComponents == 1 { // shortcut when whole graph is connected
		return []CullerGraph{*g}, true
	} else if numComponents == 0 {
		// should never happen
		Log.Verbose(1, "Couldn't find components in a graph representing sector. (culler)\n")
		return nil, false
	}
	// And now, we create a graph from each component
	graphs := make([]CullerGraph, numComponents)
	for i, _ := range graphs {
		graphs[i].lines = make([]CullerLine, 0)
		graphs[i].vertices = make([]NodeVertex, 0)
	}
	for i := 0; i < len(g.vertices); i++ {
		g.vertices[i].idx = mergeGraphIdx(g.vertices, g.vertices[i].idx)
	}
	verticeMap := make([]int, len(g.vertices))
	for i, _ := range verticeMap {
		verticeMap[i] = -1
	}
	// As we populate the graphs with lines and vertices, we have to maintain
	// links from lines to vertices by converting vertices' indices from
	// array of original graph vertices to those they have within the array of
	// vertices of new graph(s) they are going into
	for i := 0; i < len(g.lines); i++ {
		sVertex := g.lines[i].startVertex
		var graphNum int
		for j, graphIdent := range compReverseMap {
			if g.vertices[sVertex].idx == uint32(graphIdent) {
				graphNum = j
			}
		}
		if graphNum >= numComponents {
			Log.Verbose(2, "Extreme error: %d >= %d. %s", graphNum, numComponents, debugStrVerticesIdx(g.vertices))
		}
		newVSId := verticeMap[sVertex]
		if newVSId == -1 {
			newVSId = len(graphs[graphNum].vertices)
			verticeMap[sVertex] = newVSId
			graphs[graphNum].vertices = append(graphs[graphNum].vertices, g.vertices[sVertex])
		}
		eVertex := g.lines[i].endVertex
		newVEId := verticeMap[eVertex]
		if newVEId == -1 {
			newVEId = len(graphs[graphNum].vertices)
			verticeMap[eVertex] = newVEId
			graphs[graphNum].vertices = append(graphs[graphNum].vertices, g.vertices[eVertex])
		}
		g.lines[i].startVertex = newVSId
		g.lines[i].endVertex = newVEId
		graphs[graphNum].lines = append(graphs[graphNum].lines, g.lines[i])
	}
	return graphs, true
}

func debugStrVerticesIdx(vs []NodeVertex) string {
	result := ""
	for _, v := range vs {
		result += fmt.Sprintf(",%d", v.idx)
	}
	return result
}

func hashmapAddLineToSector(lines map[uint16][]uint16, sector uint16, line uint16) {
	slot := lines[sector]
	if slot == nil {
		slot = make([]uint16, 0)
	} else {
		// Taking advantage that insertions are consecutive, this prevents
		// duplicates from being added
		if slot[len(slot)-1] == line {
			return
		}
	}
	slot = append(slot, line)
	lines[sector] = slot
}

func findLimitsFromLines(lines WriteableLines, idx []uint16) *NodeBounds {
	var r NodeBounds
	// using 32-bit signed min/max values as initial, even though most engines
	// don't have coords this big
	r.Xmax = -2147483648
	r.Ymax = -2147483648
	r.Xmin = 2147483647
	r.Ymin = 2147483647
	for i := 0; i < len(idx); i++ {
		x1, y1, x2, y2 := lines.GetAllXY(idx[i])

		if x1 < r.Xmin {
			r.Xmin = x1
		}
		if x1 > r.Xmax {
			r.Xmax = x1
		}
		if y1 < r.Ymin {
			r.Ymin = y1
		}
		if y1 > r.Ymax {
			r.Ymax = y1
		}

		if x2 < r.Xmin {
			r.Xmin = x2
		}
		if x2 > r.Xmax {
			r.Xmax = x2
		}
		if y2 < r.Ymin {
			r.Ymin = y2
		}
		if y2 > r.Ymax {
			r.Ymax = y2
		}
	}
	return &r
}
