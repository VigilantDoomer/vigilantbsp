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

// reject
package main

import (
	"sort"
	"time"
)

// NOTE Programmers beware: zennode's original reject code was already not
// exactly easy to understand (although I'm deeply indebted to its existence),
// and the code you are seeing here originated from translating that code from
// C++ to Go. Afterwards, a row of optimization and debugging followed, as
// things that are good in C++ may have bad performance in Go + implementing
// support for self-referencing sectors is not exactly walk in the park.
// TODO RMB effects are not AT ALL implemented yet.
// NOTE The output is unlikely to be identical to zennode/zokumbsp's output (not
// to say those two even differ between themselves). It has to do with the fact
// that reject computation depends on blockmap computation, and the algorithm
// used is different, as it was derived from ZDBSP's code not zennodes' one.

const VIS_UNKNOWN uint8 = 0x00
const VIS_VISIBLE uint8 = 0x01     // At least 1 valid LOS found
const VIS_HIDDEN uint8 = 0x02      // No actual LOS exists
const VIS_RMB_VISIBLE uint8 = 0x04 // Special - ignores VIS_RMB_HIDDEN
const VIS_RMB_HIDDEN uint8 = 0x08  // Special - force sector to be hidden
const VIS_RMB_MASK uint8 = 0x0C    // Special - RMB option present

// RejectWork stands for reject work data
type RejectWork struct {
	numSectors  int
	rejectTable []byte
	input       *RejectInput
	// the bulk of working data begins
	solidLines   []SolidLine
	transLines   []TransLine
	indexToSolid []*SolidLine
	// lineVisTable was replaced. Reason: computations wrote both VIS_VISIBLE
	// and VIS_HIDDEN to lineVisTable cells, but what was read is whether cell
	// value is equal to VIS_UNKNOWN (the initial one) or was modified to one of
	// those two.
	// This ternary system was replaced with a binary one in lineVisDone. True
	// means line pair visibility was already computed, false means it is
	// VIS_UNKNOWN. That's all that's needed, even when RMB effects will be
	// implemented.
	//lineVisTable     []uint8
	lineVisDone      []uint8 // bitarray, replaces lineVisTable
	sectors          []RejSector
	slyLinesInSector map[uint16]bool // line with equal sector references on both sides. May indicate self-referencing sector
	maxDistance      uint32
	testLines        []*SolidLine
	polyPoints       []*IntVertex
	reSectors        []*RejSector
	lineMap          []*TransLine
	blockmap         *Blockmap
	loRow            int
	hiRow            int
	//blockity         *BlockityLines // was used to avoid 3D arrays and blockmap copying, but then I've found an even faster way
	blockMapBounds []BlockMapBounds
	src, tgt       TransLine        // memory optimization: used in testLinePair, which is not reentrant. Allocate once, reuse every time.
	p1, p2, p3, p4 IntVertex        // memory optimization: used in rejectLOS.go -> initializeWorld, another non-reentrant function
	seenLines      [65536 / 8]uint8 // replaces checkLine bool array, here we use unsigned bytes to store them compactly
	// rejectDFS.go junk
	graphTable GraphTable
}

type RejectInput struct {
	lines      AbstractLines
	bounds     LevelBounds
	sectors    []Sector
	sidedefs   []Sidedef
	bcontrol   chan BconRequest
	bgenerator chan BgenRequest
	rejectChan chan<- []byte
}

type IntVertex struct {
	X int
	Y int
}

type SolidLine struct {
	index  uint16
	start  *IntVertex
	end    *IntVertex
	ignore bool
}

type TransLine struct {
	index          uint16
	start          *IntVertex
	end            *IntVertex
	frontSector    uint16
	backSector     uint16
	DY, DX, H      int
	lo, hi         float64
	loPoint        *IntVertex
	hiPoint        *IntVertex
	indexInLineVis int
}

type RejSector struct {
	index        int
	lines        []*TransLine
	neighbors    []*RejSector
	numLines     int
	numNeighbors int
	//isNeighbor   map[int]bool
	// graph stuff
	metric           int
	baseGraph, graph *RejGraph
	graphParent      *RejSector
	isArticulation   bool
	indexDFS         int
	loDFS            int
	hiDFS            int
}

type RejGraph struct {
	numSectors int
	sectors    []*RejSector
}

type GraphTable struct {
	numGraphs   int
	graphs      []RejGraph
	sectorStart []*RejSector
	sectorPool  []*RejSector
}

type Neighboring struct {
	smallIndex int
	bigIndex   int
}

// goroutine
func RejectGenerator(input RejectInput) {
	start := time.Now()
	r := &RejectWork{
		numSectors: len(input.sectors),
		input:      &input,
	}
	r.prepareReject()
	if r.setupLines() {
		input.bcontrol <- BconRequest{
			Sender:  SOLIDBLOCKS_REJECT,
			Message: BCON_NEED_SOLID_BLOCKMAP,
		}
		// Blockmap will be needed a little bit later, do other tasks meanwhile
		r.createSectorInfo()
		r.finishLineSetup()
		r.eliminateTrivialCases()
		r.maxDistance = 0xFFFFFFFF // TODO RMB support
		r.testLines = make([]*SolidLine, len(r.solidLines))
		r.polyPoints = make([]*IntVertex, 2*(len(r.solidLines)+2)) // this is going to be reused many times to hold both lower and upper polygons (rejectLOS.go!InitializeWorld, etc)
		r.reSectors = make([]*RejSector, len(r.sectors))
		for i := 0; i < len(r.sectors); i++ {
			r.reSectors[i] = &(r.sectors[i])
		}

		if config.UseGraphsForLOS {
			// --- Graphs code starts here
			r.InitializeGraphs(r.numSectors)
			// Try to order lines to maximize our chances of culling child sectors (zennode)
			// VigilantDoomer: we use a different sort function in graphs branch
			// so that function in bruteforce branch is not slowed by checking
			// stuff that never gets initialized for it
			sort.Sort(reSectors_SorterWithGraphs(r.reSectors))
			// Here, lineMap is used to localize lineVisibility access with
			// respect to sectors. As I plan to compress lineVisibility array
			// in-memory when it is too big, I expect localization to reduce the
			// number of compress/decompress operations -- VigilantDoomer
			r.lineMap = make([]*TransLine, len(r.transLines))
			SetupLineMap(r.lineMap, r.reSectors, len(r.sectors))
			// Get blockmap, it will be used in call tree initiated by testLinePair
			// method
			bmResponse := make(chan *Blockmap)
			input.bgenerator <- BgenRequest{
				Action:  BGEN_RETRIEVE_BLOCKMAP,
				ReplyTo: bmResponse,
			}
			r.blockmap = <-bmResponse
			r.prepareBlockmapForLOS() // arrangements for hard porn in rejectLOS.go

			for i := 0; i < r.numSectors; i++ {
				//UpdateProgress ( 1, 100.0 * ( double ) i / ( double ) noSectors );
				r.ProcessSector(r.reSectors[i])
			}
			// --- Graphs code ends here
		} else {
			// --- Bruteforce code starts here

			// Try to order lines to maximize our chances of culling child sectors (zennode)
			sort.Sort(reSectorsType(r.reSectors))
			r.lineMap = make([]*TransLine, len(r.transLines))
			lineMapSize := SetupLineMap(r.lineMap, r.reSectors, len(r.sectors))

			// Get blockmap, it will be used in call tree initiated by testLinePair
			// method
			bmResponse := make(chan *Blockmap)
			input.bgenerator <- BgenRequest{
				Action:  BGEN_RETRIEVE_BLOCKMAP,
				ReplyTo: bmResponse,
			}
			r.blockmap = <-bmResponse
			r.prepareBlockmapForLOS() // arrangements for hard porn in rejectLOS.go

			// Check all lines against each other
			for i := 0; i < lineMapSize; i++ {
				srcLine := r.lineMap[i]
				for j := lineMapSize - 1; j > i; j-- {
					tgtLine := r.lineMap[j]
					if r.testLinePair(srcLine, tgtLine) {
						r.markPairVisible(srcLine, tgtLine)
					}
				}
			}
			// --- Bruteforce code ends here
		}

		input.bcontrol <- BconRequest{
			Sender:  SOLIDBLOCKS_REJECT,
			Message: BCON_DONE_WITH_SOLID_BLOCKMAP,
		}
		//
	} else {
		input.bcontrol <- BconRequest{
			Sender:  SOLIDBLOCKS_REJECT,
			Message: BCON_NONEED_SOLID_BLOCKMAP,
		}
		Log.Printf("Reject builder: not a single two-sided linedef between two distinct sectors was found. You will have an empty, zero-filled REJECT.")
	}
	Log.Printf("Reject took %s\n", time.Since(start))
	// TODO some RMB effects are to be applied (when implemented) after the pass above
	input.rejectChan <- r.getResult()
}

func isHidden(vis uint8) bool {
	// zennode does it this way
	return !(vis&VIS_VISIBLE == VIS_VISIBLE) || (vis&VIS_RMB_MASK == VIS_RMB_HIDDEN)
}

func (r *RejectWork) prepareReject() {
	// The working table size (uses bytes not bits).
	// Extra 7 bytes to simplify getResult() method
	tableSize := r.numSectors*r.numSectors + 7
	r.rejectTable = make([]uint8, tableSize, tableSize)
	for i, _ := range r.rejectTable {
		r.rejectTable[i] = 0
	}
}

func (r *RejectWork) getResult() []byte {
	rejectSize := int((r.numSectors*r.numSectors)+7) / 8
	result := make([]byte, rejectSize, rejectSize)
	tbIdx := 0
	for i := 0; i < rejectSize; i++ {
		bits := 0
		if isHidden(r.rejectTable[tbIdx]) {
			bits = bits | 0x01
		}
		tbIdx++
		if isHidden(r.rejectTable[tbIdx]) {
			bits = bits | 0x02
		}
		tbIdx++
		if isHidden(r.rejectTable[tbIdx]) {
			bits = bits | 0x04
		}
		tbIdx++
		if isHidden(r.rejectTable[tbIdx]) {
			bits = bits | 0x08
		}
		tbIdx++
		if isHidden(r.rejectTable[tbIdx]) {
			bits = bits | 0x10
		}
		tbIdx++
		if isHidden(r.rejectTable[tbIdx]) {
			bits = bits | 0x20
		}
		tbIdx++
		if isHidden(r.rejectTable[tbIdx]) {
			bits = bits | 0x40
		}
		tbIdx++
		if isHidden(r.rejectTable[tbIdx]) {
			bits = bits | 0x80
		}
		tbIdx++
		result[i] = uint8(bits)
	}
	return result
}

func (r *RejectWork) setupLines() bool {
	numLines := r.input.lines.Len()
	// REMARK lineProcessed array - was never used in zennode. Was just created,
	// then/ deleted, never written to or read from.
	r.solidLines = make([]SolidLine, numLines)
	r.transLines = make([]TransLine, numLines)
	r.indexToSolid = make([]*SolidLine, numLines) // maps index of a line in input.lines to its record in solidLines array, if one exists
	r.slyLinesInSector = make(map[uint16]bool)
	for i := uint16(0); i < numLines; i++ {
		r.indexToSolid[i] = nil
	}
	vertices := make([]IntVertex, int(numLines)*2) // cast to int important!
	numSolidLines := 0
	numTransLines := 0
	var cull *Culler
	if config.RejectSelfRefMode != REJ_SELFREF_TRIVIAL { // reference to global: config
		cull = new(Culler)
		cull.SetMode(CREATE_REJECT, r.input.sidedefs)
		cull.SetAbstractLines(r.input.lines)
		cull.EnablePerimeterSink(config.RejectSelfRefMode == REJ_SELFREF_PEDANTIC) // reference to global: config
	}
	for i := uint16(0); i < numLines; i++ {
		x1, y1, x2, y2 := r.input.lines.GetAllXY(i)
		vertices[int(i)<<1] = IntVertex{X: x1, Y: y1}
		vertices[int(i)<<1+1] = IntVertex{X: x2, Y: y2}
		if x1 == x2 && y1 == y2 {
			continue
		}
		culled := cull.AddLine(i)
		if (uint16(r.input.lines.GetFlags(i)) & LF_TWOSIDED) == LF_TWOSIDED {
			fSide := r.input.lines.GetSidedefIndex(i, true)
			bSide := r.input.lines.GetSidedefIndex(i, false)
			if fSide == SIDEDEF_NONE || bSide == SIDEDEF_NONE {
				continue
			}
			fSector := r.input.sidedefs[fSide].Sector
			bSector := r.input.sidedefs[bSide].Sector
			if fSector == bSector || culled { // could be (or not be) a border of self-referencing (part of) sector
				if config.RejectSelfRefMode == REJ_SELFREF_TRIVIAL { // reference to global: config
					// In trivial mode, we mark this straight away. Otherwise,
					// we *might* mark this later OR (for pedantic mode) create
					// a transient line by finding which sector surrounds the
					// self-referencing one
					r.slyLinesInSector[fSector] = true
				}
				Log.Verbose(2, "Reject: sector %d has line %d which references it on both sides.\n",
					fSector, i)
				continue // can't put this in transient lines while both sides are referencing one sector
			}
			r.transLines[numTransLines] = TransLine{
				index:          i,
				start:          &vertices[int(i)<<1],
				end:            &vertices[int(i)<<1+1],
				frontSector:    fSector,
				backSector:     bSector,
				DX:             x2 - x1,
				DY:             y2 - y1,
				H:              (x2-x1)*(x2-x1) + (y2-y1)*(y2-y1),
				indexInLineVis: int(numTransLines),
			}
			numTransLines++
		} else {
			r.solidLines[numSolidLines] = SolidLine{
				index:  i,
				start:  &vertices[int(i)<<1],
				end:    &vertices[int(i)<<1+1],
				ignore: false,
			}
			r.indexToSolid[i] = &r.solidLines[numSolidLines]
			numSolidLines++
		}
	}
	cull.Analyze()
	if config.RejectSelfRefMode == REJ_SELFREF_PEDANTIC { // reference to global: config
		// In pedantic mode, we are generating transient line from all
		// self-referencing effect lines in perimeter by finding the outer
		// sector to which one of these lines' sides can be assigned to. This
		// is done by tracing a line through a map and finding the closest
		// proper line it intersects that is outside the line segment.
		// Blockmap is used to speed up the computation
		// NOTE it's possible for sectors to have multiple borders and thus
		// multiple neighbors
		var it *BlockityLines
		var blXMin, blXMax, blYMin, blYMax int
		bm := CreateBlockmap(BlockmapInput{
			lines:           r.input.lines.GetAuxVersion(),
			bounds:          r.input.bounds,
			XOffset:         0,
			YOffset:         0,
			useZeroHeader:   false,
			internalPurpose: true,
		})
		it = GetBlockityLines(bm)
		lineTraces := make(map[[2]OrientedVertex]CollinearOrientedVertices)
		blXMin = int(bm.header.XMin)
		blXMax = bm.XMax
		blYMin = int(bm.header.YMin)
		blYMax = bm.YMax

		sectorPerimeters := cull.GetPerimeters()
		for sector, perimeters := range sectorPerimeters {
			for _, perimeter := range perimeters {
				whichSector := r.traceSelfRefLines(&numTransLines, sector, perimeter, it,
					lineTraces, blXMin, blXMax, blYMin, blYMax, vertices)
				if whichSector != -1 {
					Log.Verbose(1, "Self-referencing sector %d has sector %d as its neighbor.",
						sector, whichSector)
				}
			}
		}
	}

	// This is used for all other reject treatment modes, and also as fallback
	// in pedantic mode when perimeter computation failed for any reason.
	// It just tells which sectors contain self-referencing lines and need to
	// be made always visible because of that.
	for cull.SpewBack() {
		i := cull.GetLine()
		fSide := r.input.lines.GetSidedefIndex(i, true)
		fSector := r.input.sidedefs[fSide].Sector
		_, ok := r.slyLinesInSector[fSector]
		r.slyLinesInSector[fSector] = true
		if !ok { // marking sector for the first time
			if config.RejectSelfRefMode == REJ_SELFREF_PEDANTIC { // reference to global: config
				Log.Verbose(1, "Reject: sector %d seems to be self-referencing, I failed to be pedantic about which lines make up the border though: will have to resort to a hack to make it always visible.\n", fSector)
			} else {
				Log.Verbose(1, "Reject: sector %d is self-referencing.\n",
					fSector)
			}
		}
	}

	// set correct length
	r.solidLines = r.solidLines[:numSolidLines]
	r.transLines = r.transLines[:numTransLines]
	return numTransLines > 0
}

// FIXME there remains errors in this function. Traces one of the sectors in
// first map of Hexen as self-referencing because it picks wrong side of the
// line by mistake. Looks like there are things to improve...
func (r *RejectWork) traceSelfRefLines(numTransLines *int, sector uint16,
	perimeter []uint16, it *BlockityLines,
	lineTraces map[[2]OrientedVertex]CollinearOrientedVertices,
	blXMin, blXMax, blYMin, blYMax int, vertices []IntVertex) int {
	// Ok, let's see if we failed miserably for this sector on some other
	// perimeter (couldn't trace) and so already using the fallback to
	// sly lines in sector marker treatment
	if r.slyLinesInSector[sector] {
		return -1
	}
	// All good, proceed. Here is the plan:
	// 1. We have a blockmap (passed to us via BlockityLines object)
	// 2. We trace every "self-referencing effect" line like it was a partition
	// line throughout the whole blockmap, until the following sequence
	// succeeds:
	// - identify all intersection points between this line and whole
	// map lines (with the use of blockmap)
	// - find nearest intersection point (produced by a solid line or
	// a proper 2-sided line) outside the line segment, use sector
	// from that (from the side that faces towards the "partition line"
	// segment of course)
	// - if it was a success, the rest of lines should result in the same
	// sector reference, so exit the loop
	// 3. Now we go through each self-referencing line from the perimeter (which
	// perimeter is ALWAYS clockwise-ordered sequence of lines), and assign
	// the calculated sector from stage 2 to the sidedef that points outside the
	// self-referencing sector
	sectorFound := false
	whichSector := uint16(0)
	tracedSelf := false
	for _, i := range perimeter {
		// First, we make sure this line is 2-sided and reference same sector
		// on both sides
		fSide := r.input.lines.GetSidedefIndex(i, true)
		bSide := r.input.lines.GetSidedefIndex(i, false)
		if fSide == SIDEDEF_NONE || bSide == SIDEDEF_NONE {
			continue
		}
		if r.input.sidedefs[fSide].Sector != sector || r.input.sidedefs[bSide].Sector != sector {
			continue
		}
		// Ok, it is/does
		X1, Y1, X2, Y2 := r.input.lines.GetAllXY(i)
		srLine := NodeSeg{
			StartVertex: &NodeVertex{
				X: X1,
				Y: Y1,
			},
			EndVertex: &NodeVertex{
				X: X2,
				Y: Y2,
			},
			Angle:   0, // unused
			Linedef: i,
			Flip:    0,
			Offset:  0,
			perp:    0,
			sector:  sector,
			alias:   0,
			len:     0,
			partner: nil,
		}
		srLine.psx = X1
		srLine.psy = Y1
		srLine.pex = X2
		srLine.pey = Y2
		srLine.pdx = X2 - X1
		srLine.pdy = Y2 - Y1
		partSegCoords := srLine.toVertexPairC()
		var c IntersectionContext
		c.psx = partSegCoords.StartVertex.X
		c.psy = partSegCoords.StartVertex.Y
		c.pex = partSegCoords.EndVertex.X
		c.pey = partSegCoords.EndVertex.Y
		c.pdx = c.pex - c.psx
		c.pdy = c.pey - c.psy
		ov1, ov2 := PartitionInBoundary(&srLine, &c, blXMax, blYMax, blXMin,
			blYMin, partSegCoords)
		if ov1 == nil || ov2 == nil {
			Log.Verbose(2, "Reject: PartitionInBoundary failed for line %d.",
				i)
			// Try next line
			continue
		}
		traceKey := [2]OrientedVertex{*ov1, *ov2}
		trace, ok := lineTraces[traceKey]
		if !ok {
			trace = CollinearOrientedVertices(make([]OrientedVertex, 1))
			trace[0] = *ov1
			it.SetContext(ov1.v.X, ov1.v.Y, ov2.v.X, ov2.v.Y)
			for it.NextLine() {
				aline := it.GetLine()
				c.lsx, c.lsy, c.lex, c.ley = r.input.lines.GetAllXY(aline)
				if c.lsx == c.lex && c.lsy == c.ley { // skip zero-length lines
					continue
				}
				pt1, pt2 := c.getIntersectionOrIndicence()
				if pt1 != nil {
					if pt2 != nil {
						// Incidence of no use to us
					} else { // pt2 == nil
						// Intersection
						left := IsClockwiseTriangle(&NodeVertex{X: c.lsx, Y: c.lsy},
							&NodeVertex{X: c.lex, Y: c.ley}, ov1.v)
						pt1.idx = uint32(aline)
						ov := OrientedVertex{
							v:    pt1,
							left: left,
						}
						trace = append(trace, ov)
					}
				}
			}
			trace = append(trace, *ov2)
			sort.Stable(trace)
			if trace[0] != *ov1 || trace[len(trace)-1] != *ov2 {
				Log.Verbose(2, "Reject: failed to produce a trace %t %t (%d, %d) != (%d, %d).\n",
					trace[0] != *ov1, trace[len(trace)-1] != *ov2,
					trace[len(trace)-1].v.X, trace[len(trace)-1].v.Y,
					ov2.v.X, ov2.v.Y)
				trace = nil
			}
			// Cache it!
			lineTraces[traceKey] = trace
		}
		if trace == nil {
			// Try next line
			continue
		}

		leftStart := -1
		for i := 0; i < len(trace); i++ {
			if VertexPairCOrdering(trace[i].v, partSegCoords.StartVertex, true) {
				leftStart = i
			} else {
				break
			}
		}
		rightStart := len(trace)
		for i := len(trace) - 1; i >= 0; i-- {
			if VertexPairCOrdering(partSegCoords.EndVertex, trace[i].v, true) {
				rightStart = i
			} else {
				break
			}
		}
		if leftStart == -1 || rightStart == len(trace) || leftStart >= rightStart {
			// Try next line
			continue
		}
		// Pick any point (let's take the first one) of currently traced line
		// segment to evaluate which side this point is from line segment
		// intersecting the trace
		nv3 := &NodeVertex{
			X: X1,
			Y: Y1,
		}
		lineThatWasTraced := i
		for i := leftStart; i >= 0; i++ {
			aline := uint16(trace[i].v.idx)
			// Multiple self-referencing sectors can coexist inside one sector
			// that surrounds them. So, make sure line didn't come from another
			// self-referencing sector. We will hit a normal sector transient
			// line or a solid line sooner or later
			// TODO investigate whether nested self-referencing sectors
			// are possible. The surrounding sector for self-referencing sectors
			// inside another self-referencing sector wouldn't be correctly
			// identified then with the current code and needs to be fixed.
			if uint16(r.input.lines.GetFlags(aline))&LF_TWOSIDED == LF_TWOSIDED {
				fSide := r.input.lines.GetSidedefIndex(aline, true)
				bSide := r.input.lines.GetSidedefIndex(aline, false)
				if fSide != SIDEDEF_NONE || bSide != SIDEDEF_NONE {
					fSector := r.input.sidedefs[fSide].Sector
					bSector := r.input.sidedefs[bSide].Sector
					if fSector == bSector {
						continue
					}
				}
			}
			// Ok, we are hitting a good line it seems
			TX1, TY1, TX2, TY2 := r.input.lines.GetAllXY(aline)
			nv1 := &NodeVertex{
				X: TX1,
				Y: TY1,
			}
			nv2 := &NodeVertex{
				X: TX2,
				Y: TY2,
			}
			// FIXME there may still be an error here somewhere. Not to say that
			// apparently changes in selfref.go are warranted. Something to
			// rule out polygon of 2-sided lines nested in regular sector's true
			// perimeter becoming a "self-referencing" perimeter
			// Now calculate which side of line the point is. Whether line goes
			// AB or BA, there shouldn't be a difference of where the point is
			// relative to it. The calculation, however, depends on us starting
			// triangle by walking the line in specific direction so that
			// nv1'<nv2', which may or may not match the vector direction of
			// line segment
			if (nv1.X > nv2.X) || (nv1.X == nv2.X && nv1.Y > nv2.Y) {
				// Swap the points
				nv1, nv2 = nv2, nv1
			} else if nv1.X < nv2.X && nv1.Y > nv2.Y { // recent addition!
				// Also swap? This seems to fix Hexen map01 (no sectors shall
				// be self-referencing), but I'm afraid of breaking something
				// else. Without this, 210 is becoming self-referencing with
				// 207 assigned to the interior 2-sided lines
				nv1, nv2 = nv2, nv1
			}
			isLeft := !IsClockwiseTriangle(nv1, nv2, nv3)
			Log.Verbose(3, "Reject: sector %d traced line %d hit line %d, left = %t, traceLeft = %t.\n",
				sector, lineThatWasTraced, aline, isLeft, trace[i].left)
			// Compare where the point is to where the intersecting line looks
			if isLeft == trace[i].left { // line is looking towards the point
				// front side is where our sector is
				fSide := r.input.lines.GetSidedefIndex(aline, true)
				if fSide == SIDEDEF_NONE {
					Log.Verbose(2, "Reject: sector %d traced line %d hit line %d, expected to yield sector from the latter's front side but there was no front sidedef.\n",
						sector, lineThatWasTraced, aline)
					break
				}
				fSector := r.input.sidedefs[fSide].Sector
				whichSector = fSector
			} else { // line is looking away from the point
				// back side is where our sector is
				bSide := r.input.lines.GetSidedefIndex(aline, false)
				if bSide == SIDEDEF_NONE {
					Log.Verbose(2, "Reject: sector %d traced line %d hit line %d, expected to yield sector from the latter's back side but there was no back sidedef.\n",
						sector, lineThatWasTraced, aline)
					break
				}
				bSector := r.input.sidedefs[bSide].Sector
				whichSector = bSector
			}
			if whichSector != sector {
				sectorFound = true
				Log.Verbose(2, "Reject: sector %d traced line %d hit line %d and yielded sector %d from the latter.\n",
					sector, lineThatWasTraced, aline, whichSector)
				break
			} else { // new case catched in Hexen map01
				tracedSelf = true
				break
			}
		}
		if sectorFound {
			break
		}
	}
	if !sectorFound && tracedSelf { // new case catched in Hexen map01
		// This is not self-referencing sector, but just some lines forming
		// a polygon set to the same sector they are surrounded by
		Log.Verbose(2, "The entire perimeter of subgraph of sector %d traced to be surrounded by the same sector. Verdict: this part is not self-referencing.\n",
			sector)
		return -1
	}
	if !sectorFound {
		r.slyLinesInSector[sector] = true
		Log.Verbose(1, "Reject: sector %d is self-referencing, but I failed to trace any of its lines to an outside sector. I will have to resort to 'make it visible to all' hack.\n",
			sector)
		return -1
	}
	// Now iterate through perimeter again and create transient lines where
	// inner sector is self-referencing and outer one is whichSector
	for ii, i := range perimeter {
		// First, we make sure this line is 2-sided and reference same sector
		// on both sides
		fSide := r.input.lines.GetSidedefIndex(i, true)
		bSide := r.input.lines.GetSidedefIndex(i, false)
		if fSide == SIDEDEF_NONE || bSide == SIDEDEF_NONE {
			continue
		}
		if r.input.sidedefs[fSide].Sector != sector || r.input.sidedefs[bSide].Sector != sector {
			continue
		}
		// Now must analyze two consecutive lines (current one and next one) in
		// the perimeter to see whether current one's direction matches the
		// clockwise perimeter order (front side references inner,
		// self-referencing sector, back side the outer one and is the one to
		// be reassigned) or not (vice versa)
		iiPlusOne := ii + 1
		if iiPlusOne == len(perimeter) {
			iiPlusOne = 0
		}
		// How to: Two lines share one vertex, let's name it B. The vertex
		// of current line that is not B will be called A, and the vertex of
		// next line that is not B will be called C.
		// It the current line is ordered AB, it matches the clockwise ordering,
		// else (if it is BA) it doesn't.
		X1, Y1, X2, Y2 := r.input.lines.GetAllXY(i)
		NX1, NY1, NX2, NY2 := r.input.lines.GetAllXY(perimeter[iiPlusOne])
		BIsWhere := 2 // imply AB
		if (X1 == NX1 && Y1 == NY1) || (X1 == NX2 && Y1 == NY2) {
			BIsWhere = 1 // it's BA instead
		}
		// Perform the sector assignment
		newFrontSector := sector
		newBackSector := sector
		if BIsWhere == 2 { // AB
			newBackSector = whichSector
			Log.Verbose(2, "Reject: line %d (original sector %d) back sector will be reassigned to %d when representing it as transient line for reject computations.\n",
				i, sector, whichSector)
		} else { // BA
			newFrontSector = whichSector
			Log.Verbose(2, "Reject: line %d (original sector %d) front sector will be reassigned to %d when representing it as transient line for reject computations.\n",
				i, sector, whichSector)
		}
		r.transLines[*numTransLines] = TransLine{
			index:          i,
			start:          &vertices[int(i)<<1],
			end:            &vertices[int(i)<<1+1],
			frontSector:    newFrontSector,
			backSector:     newBackSector,
			DX:             X2 - X1,
			DY:             Y2 - Y1,
			H:              (X2-X1)*(X2-X1) + (Y2-Y1)*(Y2-Y1),
			indexInLineVis: *numTransLines,
		}
		(*numTransLines)++
	}

	return int(whichSector)
}

// Refactored away from setupLines so that it can be called after
// createSectorInfo(). Reason: createSectorInfo() allocates map of sectors
// (for tracking sectors made neighbors already), which, just like lineVisTable,
// can be big. By placing that call before lineVisTable is allocated, the map
// can be deallocated before lineVisTable would be allocated, rather than
// coexisting with it at the same time
func (r *RejectWork) finishLineSetup() {
	numTransLines := len(r.transLines)
	// From zokumbsp: lineVisSize needs to be computed using 64-bit math
	// The final size always fits signed 32-bit integer, however.
	// + After studying zennode's code closely, I realized that the actual
	// visibility information stored in lineVisTable doesn't matter (not even
	// for RMB implementation). All that matters is whether it was computed,
	// or not, which means that in reality what is needed is a bit array where
	// each bit corresponds to whether a certain pair is visible. This role is
	// now covered by lineVisDone

	/*
		lineVisSize := int(uint64(numTransLines-1) * uint64(numTransLines) / 2)
		r.lineVisTable = make([]uint8, lineVisSize)
		for i, _ := range r.lineVisTable {
			r.lineVisTable[i] = 0
		}
	*/

	// How is value derived:
	// 1. We need to store a triangle, rather than whole matrix, and without
	// the dominant diagonal, of numTransLines*numTransLines matrix
	// 2. The number of cells in such triangle is equivalent to a number of cells
	// in triangle WITH the diagonal of (numTransLines-1)*(numTransLines-1)
	// matrix
	// 3. The number of cells in triangle with the major diagonal of matrix N*N
	// is N*(N+1)/2, where N = (numTransLines-1) hence cell count is
	// (numTransLines-1)*numTransLines/2 in our case
	// 4. Each cell is a bit rather than a byte, number of bits is then
	// (numCells + 7) / 8 where / is integer division
	lineVisSize := int((uint64(numTransLines-1)*uint64(numTransLines)/2 + 7) / 8)
	r.lineVisDone = make([]uint8, lineVisSize)
	for i, _ := range r.lineVisDone {
		r.lineVisDone[i] = 0
	}

}

func (r *RejectWork) createSectorInfo() {
	numSectors := len(r.input.sectors)

	r.sectors = make([]RejSector, numSectors)
	for i := 0; i < numSectors; i++ {
		r.sectors[i] = RejSector{
			index:        i,
			numNeighbors: 0,
			numLines:     0,
			// initialize anything else that needs to start from blank state
		}
	}

	isNeighbor := make(map[Neighboring]bool)
	numTransLines := len(r.transLines)
	// Count the number of lines for each sector first
	for i := 0; i < numTransLines; i++ {
		line := &r.transLines[i]
		r.sectors[line.frontSector].numLines++
		r.sectors[line.backSector].numLines++
	}

	// Allocate contiguous arrays (efficient allocation). Each transient line
	// belongs to two sectors
	sectorLines := make([]*TransLine, uint64(numTransLines)*2)
	neighborList := make([]*RejSector, uint64(numTransLines)*2)

	lines := sectorLines
	neighbors := neighborList

	// Set up the line & neighbor array for each sector
	for i := 0; i < numSectors; i++ {
		// cut the slice part designated for this sector
		r.sectors[i].lines = lines[:r.sectors[i].numLines]
		r.sectors[i].neighbors = neighbors[:r.sectors[i].numLines]
		// skip the cut part
		lines = lines[r.sectors[i].numLines:]
		neighbors = neighbors[r.sectors[i].numLines:]
		r.sectors[i].numLines = 0
	}

	// Fill in line information & mark off neighbors
	for i := 0; i < numTransLines; i++ {
		line := &r.transLines[i]
		sec1 := &r.sectors[line.frontSector]
		sec2 := &r.sectors[line.backSector]
		sec1.lines[sec1.numLines] = line
		sec1.numLines++
		sec2.lines[sec2.numLines] = line
		sec2.numLines++
		r.makeNeighbors(sec1, sec2, isNeighbor)
	}
}

func (r *RejectWork) makeNeighbors(sec1, sec2 *RejSector, isNeighbor map[Neighboring]bool) {
	nei := Neighboring{
		smallIndex: sec1.index,
		bigIndex:   sec2.index,
	}
	if nei.smallIndex > nei.bigIndex {
		nei.smallIndex, nei.bigIndex = nei.bigIndex, nei.smallIndex
	}
	_, ok := isNeighbor[nei]
	if ok {
		return
	}
	isNeighbor[nei] = true
	sec1.neighbors[sec1.numNeighbors] = sec2
	sec1.numNeighbors++
	sec2.neighbors[sec2.numNeighbors] = sec1
	sec2.numNeighbors++
}

func (r *RejectWork) markVisibility(i, j int, visibility uint8) {
	cell1 := r.rejectTableIJ(i, j)
	if *cell1 == VIS_UNKNOWN {
		*cell1 = visibility
	}

	cell2 := r.rejectTableIJ(j, i)
	if *cell2 == VIS_UNKNOWN {
		*cell2 = visibility
	}
}

func (r *RejectWork) markPairVisible(srcLine, tgtLine *TransLine) {
	// there is a direct LOS between the two lines - mark all affected sectors (zennode)
	// that is, mark both sectors of one line as visible from both of another and vice versa
	r.markVisibility(int(srcLine.frontSector), int(tgtLine.frontSector), VIS_VISIBLE)
	r.markVisibility(int(srcLine.backSector), int(tgtLine.frontSector), VIS_VISIBLE)
	r.markVisibility(int(srcLine.frontSector), int(tgtLine.backSector), VIS_VISIBLE)
	r.markVisibility(int(srcLine.backSector), int(tgtLine.backSector), VIS_VISIBLE)

}

func (r *RejectWork) rejectTableIJ(i, j int) *uint8 {
	return &(r.rejectTable[i*len(r.input.sectors)+j])
}

func (r *RejectWork) eliminateTrivialCases() {
	if config.DebugNoSlyForReject { // reference to global: config
		// debug. Revert to zennode's behavior of not counting lines which
		// refer to the same sector on both sides as transient. (empty map
		// substituted for the real map of which sectors contain such lines)
		r.slyLinesInSector = make(map[uint16]bool)
	}
	for i := 0; i < len(r.input.sectors); i++ {
		// each sector can see itself
		*(r.rejectTableIJ(i, i)) = VIS_VISIBLE

		// zennode thinks: all sectors with no seethrough lines can't see
		// (be seen by) others
		// VigilantDoomer: self-referencing sectors must not be hidden. This is
		// the way they are kept visible in modes other than pedantic, and as
		// a fallback in pedantic mode
		if r.sectors[i].numLines == 0 {
			sly, _ := r.slyLinesInSector[uint16(i)]
			if !sly {
				for j := 0; j < len(r.input.sectors); j++ {
					r.markVisibility(i, j, VIS_HIDDEN)
				}
			} else {
				// If any non-solid lines with both references set to same
				// sector exist, assume this could be self-referencing sector
				// and keep it visible to ALL
				Log.Verbose(1, "REJECT: HACK Sector %d marked as visible to all (type: 1).\n", i)
				for j := 0; j < len(r.input.sectors); j++ {
					r.markVisibility(i, j, VIS_VISIBLE)
				}
			}
		} else {
			// This acknowledges that one of remotely located parts of sector
			// can be self-referencing - made of only sly lines - the other may
			// be a normal one. Thus anything with suspicious lines is marked
			// as visible to all (and seeing all) sectors
			sly, _ := r.slyLinesInSector[uint16(i)]
			if sly {
				Log.Verbose(1, "REJECT: HACK Sector %d marked as visible to all (type: 2).\n", i)
				for j := 0; j < len(r.input.sectors); j++ {
					r.markVisibility(i, j, VIS_VISIBLE)
				}
			}
		}

		// each sector can see it's immediate neighbors
		// VigilantDoomer: in pedantic mode, self-referencing sectors too can
		// see neighboring sectors through this code, because their neighbors
		// are identified in traceSelfRefLines method
		sec := &(r.sectors[i])
		for j := 0; j < sec.numNeighbors; j++ {
			nei := sec.neighbors[j]
			if nei.index > sec.index {
				r.markVisibility(sec.index, nei.index, VIS_VISIBLE)
			}
		}
	}
}

func reSectorsCompare(x reSectorsType, i, j int) int {
	sec1 := x[i]
	sec2 := x[j]

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

func (r *RejectWork) testLinePair(srcLine, tgtLine *TransLine) bool {
	done := r.getLineVisibilityDone(srcLine, tgtLine)
	if done || r.dontBother(srcLine, tgtLine) {
		return false
	}
	// TODO if r.linesTooFarApart -- this is rmb logic which is not yet

	isVisible := false
	r.src = *srcLine
	r.tgt = *tgtLine
	bisect := false
	if adjustLinePair(&(r.src), &(r.tgt), &bisect) {
		if bisect {
			isVisible = r.divideRegion(&(r.src), &(r.tgt))
		} else {
			isVisible = r.checkLOS(&(r.src), &(r.tgt))
		}
	}

	r.setLineVisibilityDone(srcLine, tgtLine)
	return isVisible
}

/*
func (r *RejectWork) testLinePair(srcLine, tgtLine *TransLine) bool {
	vis := r.getLineVisibility(srcLine, tgtLine)
	if vis != VIS_UNKNOWN || r.dontBother(srcLine, tgtLine) {
		return false
	}
	// TODO if r.linesTooFarApart -- this is rmb logic which is not yet

	isVisible := false
	r.src = *srcLine
	r.tgt = *tgtLine
	bisect := false
	if adjustLinePair(&(r.src), &(r.tgt), &bisect) {
		if bisect {
			isVisible = r.divideRegion(&(r.src), &(r.tgt))
		} else {
			isVisible = r.checkLOS(&(r.src), &(r.tgt))
		}
	}

	if isVisible {
		vis = VIS_VISIBLE
	} else {
		vis = VIS_HIDDEN
	}
	r.setLineVisibility(srcLine, tgtLine, vis)
	return isVisible
}
*/

func getVisTableOffsetPrefab(srcLine, tgtLine *TransLine, numTransLines int) uint64 {
	var row uint64
	var col uint64
	if srcLine.indexInLineVis < tgtLine.indexInLineVis {
		row = uint64(srcLine.indexInLineVis)
		col = uint64(tgtLine.indexInLineVis)
	} else {
		row = uint64(tgtLine.indexInLineVis)
		col = uint64(srcLine.indexInLineVis)
	}

	return row*(uint64(numTransLines)<<1-1-row)>>1 + (col - row - 1)
}

func (r *RejectWork) getLineVisibilityDone(srcLine, tgtLine *TransLine) bool {
	if srcLine == tgtLine {
		return true
	}

	offset := getVisTableOffsetPrefab(srcLine, tgtLine, len(r.transLines))
	data := r.lineVisDone[offset>>3]
	bit := uint8(1 << (offset % 8))

	return data&bit == bit
}

func (r *RejectWork) setLineVisibilityDone(srcLine, tgtLine *TransLine) {

	if srcLine == tgtLine {
		return
	}

	offset := getVisTableOffsetPrefab(srcLine, tgtLine, len(r.transLines))
	data := r.lineVisDone[offset>>3]
	bit := uint8(1 << (offset % 8))

	r.lineVisDone[offset>>3] = data | bit
}

/*
func (r *RejectWork) getLineVisibility(srcLine, tgtLine *TransLine) uint8 {
	if srcLine == tgtLine {
		return VIS_VISIBLE
	}

	offset := getVisTableOffsetPrefab(srcLine, tgtLine, len(r.transLines))
	data := r.lineVisTable[offset>>2]

	return uint8(0x03 & (data >> ((offset % 4) << 1)))
}

func (r *RejectWork) setLineVisibility(srcLine, tgtLine *TransLine, vis uint8) {

	if srcLine == tgtLine {
		return
	}

	offset := getVisTableOffsetPrefab(srcLine, tgtLine, len(r.transLines))
	data := r.lineVisTable[offset>>2]

	// golang uses unary ^ (instead of unary ~ common in C-like language syntax
	// family) to signify bitwise NOT
	data &= ^(0x03 << ((offset % 4) << 1))
	data |= vis << ((offset % 4) << 1)

	r.lineVisTable[offset>>2] = data
}
*/

func (r *RejectWork) dontBother(srcLine, tgtLine *TransLine) bool {

	if (*r.rejectTableIJ(int(srcLine.frontSector), int(tgtLine.frontSector)) != VIS_UNKNOWN) &&
		(*r.rejectTableIJ(int(srcLine.frontSector), int(tgtLine.backSector)) != VIS_UNKNOWN) &&
		(*r.rejectTableIJ(int(srcLine.backSector), int(tgtLine.frontSector)) != VIS_UNKNOWN) &&
		(*r.rejectTableIJ(int(srcLine.backSector), int(tgtLine.backSector)) != VIS_UNKNOWN) {
		return true
	}

	return false
}

// adjustLinePair adjusts the two lines so that:
//   1) If one line bisects the other:
//      a) The bisecting line is tgt
//      b) The point farthest from src is made both start & end
//   2) tgt is on the left side of src
//   3) src and tgt go in 'opposite' directions
//
func adjustLinePair(src, tgt *TransLine, bisects *bool) bool {

	// Rotate & Translate so that src lies along the +X asix
	y1 := src.DX*(tgt.start.Y-src.start.Y) - src.DY*(tgt.start.X-src.start.X)
	y2 := src.DX*(tgt.end.Y-src.start.Y) - src.DY*(tgt.end.X-src.start.X)

	// The two lines are co-linear and should be ignored
	if (y1 == 0) && (y2 == 0) {
		return false
	}

	// Make sure that src doesn't bi-sect tgt
	if ((y1 > 0) && (y2 < 0)) || ((y1 < 0) && (y2 > 0)) {
		// Swap src & tgt then recalculate the endpoints
		src, tgt = tgt, src

		y1 = src.DX*(tgt.start.Y-src.start.Y) - src.DY*(tgt.start.X-src.start.X)
		y2 = src.DX*(tgt.end.Y-src.start.Y) - src.DY*(tgt.end.X-src.start.X)
		// See if these two lines actually intersect
		if ((y1 > 0) && (y2 < 0)) || ((y1 < 0) && (y2 > 0)) {
			// TODO this should be logged to stderr rather than stdout
			// or maybe not. I don't see what's so big a problem about it.
			// Demoted to verbose level=1 -- VigilantDoomer
			Log.Verbose(1, "ERROR: Two lines (%d & %d) intersect\n", src.index, tgt.index)
			return false
		}
	}

	// Make sure that tgt will end up on the correct (left) side
	if (y1 <= 0) && (y2 <= 0) {
		// Flip src
		src.start, src.end = src.end, src.start
		// Adjust values y1 and y2 end reflect new src
		src.DX = -src.DX
		src.DY = -src.DY
		y1 = -y1
		y2 = -y2
	}

	// See if the lines are parallel
	if y2 == y1 {
		x1 := src.DX*(tgt.start.X-src.start.X) + src.DY*(tgt.start.Y-src.start.Y)
		x2 := src.DX*(tgt.end.X-src.start.X) + src.DY*(tgt.end.Y-src.start.Y)
		if x1 < x2 {
			tgt.start, tgt.end = tgt.end, tgt.start
			tgt.DX = -tgt.DX
			tgt.DY = -tgt.DY
		}
		return true
	}

	// Now look at src from tgt's point of view
	x1 := tgt.DX*(src.start.Y-tgt.start.Y) - tgt.DY*(src.start.X-tgt.start.X)
	x2 := tgt.DX*(src.end.Y-tgt.start.Y) - tgt.DY*(src.end.X-tgt.start.X)

	// See if a line along tgt intersects src
	if ((x1 < 0) && (x2 > 0)) || ((x1 > 0) && (x2 < 0)) {
		*bisects = true
		// Make sure tgt points away from src
		if y1 > y2 {
			tgt.start, tgt.end = tgt.end, tgt.start
			tgt.DX = -tgt.DX
			tgt.DY = -tgt.DY
		}
	} else if (x1 <= 0) && (x2 <= 0) {
		tgt.start, tgt.end = tgt.end, tgt.start
		tgt.DX = -tgt.DX
		tgt.DY = -tgt.DY
	}

	return true
}

func (r *RejectWork) checkLOS(src, tgt *TransLine) bool {
	// all functions called from here were placed in rejectLOS.go
	var myWorld WorldInfo
	r.initializeWorld(&myWorld, src, tgt)
	r.markBlockMap(&myWorld)
	if r.findInterveningLines(&(myWorld.solidSet)) {
		// If src & tgt touch, look for a quick way out - no lines passing through the common point
		var common *IntVertex
		common = src.end
		linesTouch := false
		if common == tgt.start {
			linesTouch = true
		} else {
			common = src.start
			if common == tgt.end {
				linesTouch = true
			}
		}
		if linesTouch {
			more := false
			for i := myWorld.solidSet.loIndex; i <= myWorld.solidSet.hiIndex; i++ {
				line := myWorld.solidSet.lines[i]
				if (line.start == common) || (line.end == common) {
					more = true
				}
			}
			// The two lines touch and there are no lines blocking them
			if !more {
				return true
			}
		}
		// Do a more refined check to see if there are any lines
		switch TrimLines(myWorld.src, myWorld.tgt, &myWorld.solidSet) {

		case -1:
			{ // A single line completely blocks the view
				return false
			}

		case 0:
			{ // No intervening lines left - end check

			}

		default:
			{
				// Do an even more refined check
				if !FindPolyLines(&myWorld) {
					return false
				}
				// Now see if there are any obstacles that may block the LOS
				if FindObstacles(&myWorld) {
					return false
				}
			}
		}
	}
	return true
}

func (r *RejectWork) divideRegion(src, tgt *TransLine) bool {
	// Find the point of intersection on src
	num := tgt.DX*(src.start.Y-tgt.start.Y) - tgt.DY*(src.start.X-tgt.start.X)
	det := src.DX*tgt.DY - src.DY*tgt.DX
	t := float64(num) / float64(det)

	crossPoint := IntVertex{
		X: int(int64(src.start.X) + int64(t*float64(src.DX))),
		Y: int(int64(src.start.Y) + int64(t*float64(src.DY))),
	}

	// See if we ran into an integer truncation problem (shortcut if we did)
	if (crossPoint == *(src.start)) || (crossPoint == *(src.end)) {
		Log.Verbose(3, "Debug: integer truncation detected in divideRegion for lines %d & %d\n", src.index, tgt.index)
		return r.checkLOS(src, tgt)
	}

	newSrc := *src

	newSrc.end = &crossPoint

	isVisible := r.checkLOS(&newSrc, tgt)

	if !isVisible {
		newSrc.start = &crossPoint
		newSrc.end = src.end
		tgt.start, tgt.end = tgt.end, tgt.start
		tgt.DX = -tgt.DX
		tgt.DY = -tgt.DY
		isVisible = r.checkLOS(&newSrc, tgt)
	}

	return isVisible
}

type reSectorsType []*RejSector

func (x reSectorsType) Len() int { return len(x) }
func (x reSectorsType) Less(i, j int) bool {
	return reSectorsCompare(x, i, j) < 0
}
func (x reSectorsType) Swap(i, j int) { x[i], x[j] = x[j], x[i] }

func SetupLineMap(lineMap []*TransLine, reSectors []*RejSector, numSectors int) int {
	inMap := make(map[uint16]bool)
	maxIndex := 0
	for i := 0; i < numSectors; i++ {
		for j := 0; j < reSectors[i].numLines; j++ {
			line := reSectors[i].lines[j]
			val, _ := inMap[(*line).index]
			if !val {
				inMap[line.index] = true
				lineMap[maxIndex] = line
				line.indexInLineVis = maxIndex // reorder so that lineVisibility accesses are closer to consecutive
				maxIndex++
			}
		}
	}

	return maxIndex
}
