// Code generated from other source files. DO NOT EDIT.
// Copyright (C) 2022-2025, VigilantDoomer
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
	"io"
	"sort"
)

type FastRejectWork struct {
	extra       RejectNoExtra
	numSectors  int
	rejectTable []byte
	input       *RejectInput

	solidLines       []SolidLine
	transLines       []TransLine
	indexToSolid     []uint16
	lineVisDone      []uint8
	sectors          []RejSector
	slyLinesInSector map[uint16]bool
	maxDistance      uint64
	testLines        []*SolidLine
	polyPoints       []*IntVertex
	reSectors        []*RejSector
	lineMap          []*TransLine
	blockmap         *Blockmap
	solidList        []SolidmapSlice
	solidMap         []uint16
	loRow            int
	hiRow            int

	blockMapBounds []BlockMapBounds
	src, tgt       TransLine
	p1, p2, p3, p4 IntVertex
	seenLines      []uint8

	hasGroups bool
	groups    []RejGroup

	groupShareVis bool

	graphTable GraphTable

	linesToIgnore []bool

	rmbFrame          *RMBFrame
	RejectSelfRefMode int
	PedanticFailMode  int
	distanceTable     []uint16
	maxLength         uint16
	fileControl       *FileControl
	mapName           string
	lineEffects       map[uint16]uint8
	specialSolids     []uint16
	drLine            *TransLine
	symmShim          int64
}

func (r *FastRejectWork) main(input RejectInput, hasGroups bool, groupShareVis bool,
	groups []RejGroup) []byte {
	*r = FastRejectWork{
		numSectors:    len(input.sectors),
		input:         &input,
		linesToIgnore: input.linesToIgnore,
		rmbFrame:      input.rmbFrame,

		RejectSelfRefMode: config.RejectSelfRefMode,

		PedanticFailMode: PEDANTIC_FAIL_NOTMATTER,
		fileControl:      input.fileControl,
		mapName:          input.mapName,
		hasGroups:        hasGroups,
		groups:           groups,
		groupShareVis:    groupShareVis,
	}

	needDistances := input.rmbFrame.NeedDistances()
	if needDistances {
		r.PedanticFailMode = PEDANTIC_FAIL_REPORT
		if r.RejectSelfRefMode != REJ_SELFREF_PEDANTIC {
			r.RejectSelfRefMode = REJ_SELFREF_PEDANTIC
			Log.Printf("Forcing pedantic mode for self-referencing sectors because RMB options make use of length aka 'distance in sector units'\n")
		}
	}

	if input.rmbFrame.HasLineEffects() {
		r.PedanticFailMode = PEDANTIC_FAIL_REPORT
		if r.RejectSelfRefMode != REJ_SELFREF_PEDANTIC {
			r.RejectSelfRefMode = REJ_SELFREF_PEDANTIC
			Log.Printf("Forcing pedantic mode for self-referencing sectors because RMB options that block sight across a line are present\n")
		}
	}

	maxDistOk, maxDist := input.rmbFrame.GetDISTANCEValue()
	if maxDistOk {
		r.PedanticFailMode = PEDANTIC_FAIL_REPORT
		r.maxDistance = maxDist
		if r.RejectSelfRefMode != REJ_SELFREF_PEDANTIC {

			r.RejectSelfRefMode = REJ_SELFREF_PEDANTIC
			Log.Printf("Forcing pedantic mode for self-referencing sectors because DISTANCE is present in RMB options")
		}
	} else {
		r.maxDistance = UNLIMITED_DISTANCE
	}

	r.prepareReject()
	if r.setupLines() {
		r.ScheduleSolidBlockmap()

		r.createSectorInfo()
		if r.hasGroups {
			computeGroupNeighbors(r.groups, r.sectors)
		}
		r.finishLineSetup()
		r.eliminateTrivialCases()

		if needDistances {

			r.CreateDistanceTable()
			r.ApplyDistanceLimits()
		}

		r.testLines = make([]*SolidLine, len(r.solidLines)+1)
		r.polyPoints = make([]*IntVertex, 2*(len(r.solidLines)+2))
		r.reSectors = make([]*RejSector, len(r.sectors))
		for i := 0; i < len(r.sectors); i++ {
			r.reSectors[i] = &(r.sectors[i])
		}

		if config.UseGraphsForLOS {

			mixerSetup := false
			if r.groupShareVis {
				r.setupMixer()
				mixerSetup = true
			}
			r.InitializeGraphs(r.numSectors)

			sort.Sort(reSectors_SorterWithGraphs(r.reSectors))

			r.lineMap = make([]*TransLine, len(r.transLines))
			SetupLineMap(r.lineMap, r.reSectors, len(r.sectors))

			r.RetrieveSolidBlockmap()
			r.prepareBlockmapForLOS()

			for i := 0; i < r.numSectors; i++ {

				r.ProcessSector(r.reSectors[i])
			}
			if mixerSetup {
				r.mergeAndDestroyMixer()
			}

		} else {

			sort.Sort(reSectorsType(r.reSectors))
			r.lineMap = make([]*TransLine, len(r.transLines))
			lineMapSize := SetupLineMap(r.lineMap, r.reSectors, len(r.sectors))

			r.RetrieveSolidBlockmap()
			r.prepareBlockmapForLOS()

			for i := 0; i < lineMapSize; i++ {
				srcLine := r.lineMap[i]
				for j := lineMapSize - 1; j > i; j-- {
					tgtLine := r.lineMap[j]
					if r.testLinePair(srcLine, tgtLine) {
						r.markPairVisible(srcLine, tgtLine)
					}
				}
			}

		}

		r.DoneWithSolidBlockmap()

	} else {
		r.NoNeedSolidBlockmap()
		Log.Printf("Reject builder: not a single two-sided linedef between two distinct sectors was found. You will have an empty, zero-filled REJECT.")
	}

	r.rmbFrame.FastProcessOptionsRMB(r)

	return r.getResult()
}

func (r *FastRejectWork) ScheduleSolidBlockmap() {
	if r.specialSolids == nil {

		Log.Panic("Programmer error: setupLines must be called before ScheduleSolidBlockmap()")
	}
	if len(r.specialSolids) > 0 {
		r.createSolidBlockmapNow()

		r.NoNeedSolidBlockmap()
	} else {

		r.input.bcontrol <- BconRequest{
			Sender:  SOLIDBLOCKS_REJECT,
			Message: BCON_NEED_SOLID_BLOCKMAP,
		}
	}
}

func (r *FastRejectWork) RetrieveSolidBlockmap() {
	if len(r.specialSolids) > 0 {
		return
	}
	bmResponse := make(chan *Blockmap)
	r.input.bgenerator <- BgenRequest{
		Action:  BGEN_RETRIEVE_BLOCKMAP,
		ReplyTo: bmResponse,
	}
	r.blockmap = <-bmResponse
}

func (r *FastRejectWork) DoneWithSolidBlockmap() {
	if len(r.specialSolids) > 0 {
		return
	}
	r.input.bcontrol <- BconRequest{
		Sender:  SOLIDBLOCKS_REJECT,
		Message: BCON_DONE_WITH_SOLID_BLOCKMAP,
	}
}

func (r *FastRejectWork) NoNeedSolidBlockmap() {
	r.input.bcontrol <- BconRequest{
		Sender:  SOLIDBLOCKS_REJECT,
		Message: BCON_NONEED_SOLID_BLOCKMAP,
	}
}

func (r *FastRejectWork) createSolidBlockmapNow() {
	lines := r.input.lines.GetSolidVersion()
	bmi := BlockmapInput{
		bounds:          r.input.bounds,
		XOffset:         0,
		YOffset:         0,
		useZeroHeader:   false,
		internalPurpose: true,
		gcShield:        nil,
		linesToIgnore:   r.input.linesToIgnore,
	}
	for _, lidx := range r.specialSolids {
		lines.TreatAsSolid(lidx)
	}
	bmi.lines = lines
	r.blockmap = CreateBlockmap(bmi)
}

func (r *FastRejectWork) prepareReject() {

	tableSize := r.numSectors*r.numSectors + 7
	r.rejectTable = make([]uint8, tableSize, tableSize)
	for i, _ := range r.rejectTable {
		r.rejectTable[i] = 0
	}
}

func (r *FastRejectWork) getResult() []byte {
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

func (r *FastRejectWork) setupLines() bool {
	if r.NoProcess_TryLoad() {
		return false
	}
	numLines := r.input.lines.Len()
	r.RMBLoadLineEffects()
	r.specialSolids = make([]uint16, 0)

	r.solidLines = make([]SolidLine, numLines)
	r.transLines = make([]TransLine, numLines)
	r.drLine = new(TransLine)
	r.indexToSolid = make([]uint16, numLines)
	r.slyLinesInSector = make(map[uint16]bool)
	vertices := make([]IntVertex, int(numLines)*2)
	numSolidLines := 0
	numTransLines := 0
	var cull *Culler
	if r.RejectSelfRefMode != REJ_SELFREF_TRIVIAL {
		cull = new(Culler)
		cull.SetMode(CREATE_REJECT, r.input.sidedefs)
		cull.SetAbstractLines(r.input.lines)
		cull.EnablePerimeterSink(r.RejectSelfRefMode == REJ_SELFREF_PEDANTIC)
	}
	for i := uint16(0); i < numLines; i++ {

		if r.linesToIgnore != nil && r.linesToIgnore[i] {
			continue
		}
		x1, y1, x2, y2 := r.input.lines.GetAllXY(i)
		vertices[int(i)<<1] = IntVertex{X: x1, Y: y1}
		vertices[int(i)<<1+1] = IntVertex{X: x2, Y: y2}
		if x1 == x2 && y1 == y2 {
			continue
		}
		culled := cull.AddLine(i)
		twoSided := uint16(r.input.lines.GetFlags(i))&LF_TWOSIDED == LF_TWOSIDED
		if twoSided && !r.HasRMBEffectLINE(i) {
			fSide := r.input.lines.GetSidedefIndex(i, true)
			bSide := r.input.lines.GetSidedefIndex(i, false)
			if fSide == SIDEDEF_NONE || bSide == SIDEDEF_NONE {
				continue
			}
			fSector := r.input.sidedefs[fSide].Sector
			bSector := r.input.sidedefs[bSide].Sector
			if fSector == bSector || culled {
				if r.RejectSelfRefMode == REJ_SELFREF_TRIVIAL {

					r.slyLinesInSector[fSector] = true
				}
				Log.Verbose(2, "Reject: sector %d has line %d which references it on both sides.\n",
					fSector, i)
				continue
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

			if twoSided {
				r.specialSolids = append(r.specialSolids, i)
			}
			r.solidLines[numSolidLines] = SolidLine{
				index:  i,
				start:  vertices[int(i)<<1],
				end:    vertices[int(i)<<1+1],
				ignore: false,
			}
			r.indexToSolid[i] = uint16(numSolidLines)
			numSolidLines++
		}
	}
	cull.Analyze()
	if r.RejectSelfRefMode == REJ_SELFREF_PEDANTIC {

		var it *BlockityLines
		var blXMin, blXMax, blYMin, blYMax int
		bm := CreateBlockmap(BlockmapInput{
			lines:           r.input.lines.GetAuxVersion(),
			bounds:          r.input.bounds,
			XOffset:         0,
			YOffset:         0,
			useZeroHeader:   false,
			internalPurpose: true,
			linesToIgnore:   r.linesToIgnore,
		})
		it = GetBlockityLines(bm)
		lineTraces := make(map[[2]IntOrientedVertex]IntCollinearOrientedVertices)
		blXMin = int(bm.header.XMin)
		blXMax = bm.XMax
		blYMin = int(bm.header.YMin)
		blYMax = bm.YMax

		sectorPerimeters := cull.GetPerimeters()
		for sector, perimeters := range sectorPerimeters {
			for _, perimeter := range perimeters {
				whichSector := r.traceSelfRefLines(&numTransLines,
					&numSolidLines, sector, perimeter, it,
					lineTraces, blXMin, blXMax, blYMin, blYMax, vertices)
				if whichSector != -1 {
					Log.Verbose(1, "Self-referencing sector %d has sector %d as its neighbor.\n",
						sector, whichSector)
				}
			}
		}
	}

	for cull.SpewBack() {
		i := cull.GetLine()
		if r.HasRMBEffectLINE(i) {

			if r.RejectSelfRefMode == REJ_SELFREF_PEDANTIC {
				fSide := r.input.lines.GetSidedefIndex(i, true)
				fSector := r.input.sidedefs[fSide].Sector
				Log.Verbose(1, "Reject: line %d from sector %d is affected by line effect, but I failed to classify sector as self-referencing or not.\nIf EVERY 2-sided line that references this sector on both sides is like this, this sector will be hidden from all, but if at least one such line is not marked, it will be hacked to be visible to all.\n",
					i, fSector)
			}
			continue
		}
		fSide := r.input.lines.GetSidedefIndex(i, true)
		fSector := r.input.sidedefs[fSide].Sector
		_, ok := r.slyLinesInSector[fSector]
		r.slyLinesInSector[fSector] = true
		if !ok {
			if r.RejectSelfRefMode == REJ_SELFREF_PEDANTIC {
				Log.Verbose(1, "Reject: sector %d seems to be self-referencing, I failed to be pedantic about which lines make up the border though: will resort to a hack to make it always visible.\n", fSector)
				if r.PedanticFailMode == PEDANTIC_FAIL_REPORT {
					r.PedanticFailMode = PEDANTIC_FAIL_REPORTED
				}
			} else {
				Log.Verbose(1, "Reject: sector %d is self-referencing.\n",
					fSector)
			}
		}
	}

	r.solidLines = r.solidLines[:numSolidLines]
	r.transLines = r.transLines[:numTransLines]
	r.seenLines = make([]uint8, numSolidLines>>3+1)
	return numTransLines > 0
}

func (r *FastRejectWork) traceSelfRefLines(numTransLines *int, numSolidLines *int,
	sector uint16, perimeter []uint16, it *BlockityLines,
	lineTraces map[[2]IntOrientedVertex]IntCollinearOrientedVertices,
	blXMin, blXMax, blYMin, blYMax int, vertices []IntVertex) int {

	if r.slyLinesInSector[sector] {
		return -1
	}

	sectorFound := false
	whichSector := uint16(0)
	tracedSelf := false
	lineEffect := false
	for _, i := range perimeter {

		fSide := r.input.lines.GetSidedefIndex(i, true)
		bSide := r.input.lines.GetSidedefIndex(i, false)
		if fSide == SIDEDEF_NONE || bSide == SIDEDEF_NONE {
			continue
		}
		if r.input.sidedefs[fSide].Sector != sector || r.input.sidedefs[bSide].Sector != sector {
			continue
		}
		if r.HasRMBEffectLINE(i) {
			lineEffect = true
		}

		X1, Y1, X2, Y2 := r.input.lines.GetAllXY(i)
		srLine := NodeSeg{
			StartVertex: &NodeVertex{
				X: Number(X1),
				Y: Number(Y1),
			},
			EndVertex: &NodeVertex{
				X: Number(X2),
				Y: Number(Y2),
			},
			Angle:   0,
			Linedef: i,
			flags:   0,
			Offset:  0,
			perp:    0,
			sector:  sector,
			alias:   0,
			len:     0,
			partner: nil,
		}
		srLine.psx = Number(X1)
		srLine.psy = Number(Y1)
		srLine.pex = Number(X2)
		srLine.pey = Number(Y2)
		srLine.pdx = Number(X2 - X1)
		srLine.pdy = Number(Y2 - Y1)
		partSegCoords := srLine.toIntVertexPairC()
		var c IntersectionContext
		c.psx = partSegCoords.StartVertex.X
		c.psy = partSegCoords.StartVertex.Y
		c.pex = partSegCoords.EndVertex.X
		c.pey = partSegCoords.EndVertex.Y
		c.pdx = c.pex - c.psx
		c.pdy = c.pey - c.psy
		ov1, ov2 := IntPartitionInBoundary(&srLine, &c, blXMax, blYMax, blXMin,
			blYMin, partSegCoords)
		if ov1 == nil || ov2 == nil {
			Log.Verbose(2, "Reject: PartitionInBoundary failed for line %d.",
				i)

			continue
		}
		traceKey := [2]IntOrientedVertex{*ov1, *ov2}
		trace, ok := lineTraces[traceKey]
		if !ok {
			trace = IntCollinearOrientedVertices(make([]IntOrientedVertex, 1))
			trace[0] = *ov1
			it.SetContext(int(ov1.v.X), int(ov1.v.Y), int(ov2.v.X), int(ov2.v.Y))
			for it.NextLine() {
				aline := it.GetLine()
				lsx, lsy, lex, ley := r.input.lines.GetAllXY(aline)
				c.lsx = Number(lsx)
				c.lsy = Number(lsy)
				c.lex = Number(lex)
				c.ley = Number(ley)
				if c.lsx == c.lex && c.lsy == c.ley {
					continue
				}
				pt1, pt2 := c.intGetIntersectionOrIndicence()
				if pt1 != nil {
					if pt2 != nil {

					} else {

						left := IntIsClockwiseTriangle(&NodeVertex{X: c.lsx, Y: c.lsy},
							&NodeVertex{X: c.lex, Y: c.ley}, ov1.v)
						pt1.idx = uint32(aline)
						ov := IntOrientedVertex{
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

			lineTraces[traceKey] = trace
		}
		if trace == nil {

			continue
		}

		leftStart := -1
		for i := 0; i < len(trace); i++ {
			if IntVertexPairCOrdering(trace[i].v, partSegCoords.StartVertex, true) {
				leftStart = i
			} else {
				break
			}
		}
		rightStart := len(trace)
		for i := len(trace) - 1; i >= 0; i-- {
			if IntVertexPairCOrdering(partSegCoords.EndVertex, trace[i].v, true) {
				rightStart = i
			} else {
				break
			}
		}
		if leftStart == -1 || rightStart == len(trace) || leftStart >= rightStart {

			continue
		}

		nv3 := &NodeVertex{
			X: Number(X1),
			Y: Number(Y1),
		}
		lineThatWasTraced := i
		for i := leftStart; i >= 0; i++ {
			aline := uint16(trace[i].v.idx)

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

			TX1, TY1, TX2, TY2 := r.input.lines.GetAllXY(aline)
			nv1 := &NodeVertex{
				X: Number(TX1),
				Y: Number(TY1),
			}
			nv2 := &NodeVertex{
				X: Number(TX2),
				Y: Number(TY2),
			}

			if (nv1.X > nv2.X) || (nv1.X == nv2.X && nv1.Y > nv2.Y) {

				nv1, nv2 = nv2, nv1
			} else if nv1.X < nv2.X && nv1.Y > nv2.Y {

				nv1, nv2 = nv2, nv1
			}
			isLeft := !IntIsClockwiseTriangle(nv1, nv2, nv3)
			Log.Verbose(3, "Reject: sector %d traced line %d hit line %d, left = %t, traceLeft = %t.\n",
				sector, lineThatWasTraced, aline, isLeft, trace[i].left)

			if isLeft == trace[i].left {

				fSide := r.input.lines.GetSidedefIndex(aline, true)
				if fSide == SIDEDEF_NONE {
					Log.Verbose(2, "Reject: sector %d traced line %d hit line %d, expected to yield sector from the latter's front side but there was no front sidedef.\n",
						sector, lineThatWasTraced, aline)
					break
				}
				fSector := r.input.sidedefs[fSide].Sector
				whichSector = fSector
			} else {

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
			} else {
				tracedSelf = true
				break
			}
		}
		if sectorFound {
			break
		}
	}
	if !sectorFound && tracedSelf {

		Log.Verbose(2, "The entire perimeter of subgraph of sector %d traced to be surrounded by the same sector. Verdict: this part is not self-referencing.\n",
			sector)
		return -1
	}
	if !sectorFound {
		if lineEffect {

			Log.Verbose(1, "Reject: sector %d contains lines that have RMB effect LINE applied to them.\n",
				sector)
		} else {
			r.slyLinesInSector[sector] = true
			Log.Verbose(1, "Reject: sector %d is self-referencing, but I failed to trace any of its lines to an outside sector. I will have to resort to 'make it visible to all' hack.\n",
				sector)
		}
		return -1
	}

	for ii, i := range perimeter {

		fSide := r.input.lines.GetSidedefIndex(i, true)
		bSide := r.input.lines.GetSidedefIndex(i, false)
		if fSide == SIDEDEF_NONE || bSide == SIDEDEF_NONE {
			continue
		}
		if r.input.sidedefs[fSide].Sector != sector || r.input.sidedefs[bSide].Sector != sector {
			continue
		}
		if r.HasRMBEffectLINE(i) {

			Log.Verbose(2, "Reject: line %d is border of self-referencing sector %d, however RMB option LINE is applied to it, so I will treat it as solid not transient.\n",
				i, sector)
			continue
		}

		iiPlusOne := ii + 1
		if iiPlusOne == len(perimeter) {
			iiPlusOne = 0
		}

		X1, Y1, X2, Y2 := r.input.lines.GetAllXY(i)
		NX1, NY1, NX2, NY2 := r.input.lines.GetAllXY(perimeter[iiPlusOne])
		BIsWhere := 2
		if (X1 == NX1 && Y1 == NY1) || (X1 == NX2 && Y1 == NY2) {
			BIsWhere = 1
		}

		newFrontSector := sector
		newBackSector := sector
		if BIsWhere == 2 {
			newBackSector = whichSector
			Log.Verbose(2, "Reject: line %d (original sector %d) back sector will be reassigned to %d when representing it as transient line for reject computations.\n",
				i, sector, whichSector)
		} else {
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

func (r *FastRejectWork) finishLineSetup() {
	numTransLines := len(r.transLines)

	lineVisSize := int((uint64(numTransLines-1)*uint64(numTransLines)/2 + 7) / 8)
	r.lineVisDone = make([]uint8, lineVisSize)
	for i, _ := range r.lineVisDone {
		r.lineVisDone[i] = 0
	}

	if r.PedanticFailMode == PEDANTIC_FAIL_REPORTED {
		Log.Error("RMB's accuracy is going to be decreased, because I failed to be pedantic about self-referencing sector effects.\n")
	}

}

func (r *FastRejectWork) createSectorInfo() {
	numSectors := r.numSectors

	r.sectors = make([]RejSector, numSectors)
	for i := 0; i < numSectors; i++ {
		r.sectors[i] = RejSector{
			index:        i,
			numNeighbors: 0,
			numLines:     0,
		}
	}

	isNeighbor := make(map[Neighboring]bool)
	numTransLines := len(r.transLines)

	for i := 0; i < numTransLines; i++ {
		line := &r.transLines[i]
		r.sectors[line.frontSector].numLines++
		r.sectors[line.backSector].numLines++
	}

	sectorLines := make([]*TransLine, uint64(numTransLines)*2)
	neighborList := make([]*RejSector, uint64(numTransLines)*2)

	lines := sectorLines
	neighbors := neighborList

	for i := 0; i < numSectors; i++ {

		r.sectors[i].lines = lines[:r.sectors[i].numLines]
		r.sectors[i].neighbors = neighbors[:r.sectors[i].numLines]

		lines = lines[r.sectors[i].numLines:]
		neighbors = neighbors[r.sectors[i].numLines:]
		r.sectors[i].numLines = 0
	}

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

func (r *FastRejectWork) makeNeighbors(sec1, sec2 *RejSector, isNeighbor map[Neighboring]bool) {
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

func (r *FastRejectWork) markVisibilitySector(i, j int, visibility uint8) {
	cell1 := r.rejectTableIJ(i, j)
	if *cell1 == VIS_UNKNOWN {
		*cell1 = visibility
	}

	cell2 := r.rejectTableIJ(j, i)
	if *cell2 == VIS_UNKNOWN {
		*cell2 = visibility
	}
}

func (r *FastRejectWork) sectorIsNotGroup(i int) bool {
	return r.groups[i].legal && len(r.groups[i].sectors) == 1
}

func (r *FastRejectWork) markPairVisible(srcLine, tgtLine *TransLine) {

	r.markVisibility(int(srcLine.frontSector), int(tgtLine.frontSector), VIS_VISIBLE)
	r.markVisibility(int(srcLine.backSector), int(tgtLine.frontSector), VIS_VISIBLE)
	r.markVisibility(int(srcLine.frontSector), int(tgtLine.backSector), VIS_VISIBLE)
	r.markVisibility(int(srcLine.backSector), int(tgtLine.backSector), VIS_VISIBLE)
}

func (r *FastRejectWork) mixerSetVisibility(i, j int, visibility uint8) {
	cell1 := r.mixerIJ(i, j)
	if *cell1 == VIS_UNKNOWN {
		*cell1 = visibility
	}

	cell2 := r.mixerIJ(j, i)
	if *cell2 == VIS_UNKNOWN {
		*cell2 = visibility
	}
}

func (r *FastRejectWork) rejectTableIJ(i, j int) *uint8 {
	return &(r.rejectTable[i*r.numSectors+j])
}

func (r *FastRejectWork) eliminateTrivialCases() {
	if config.DebugNoSlyForReject {

		r.slyLinesInSector = make(map[uint16]bool)
	}

	if r.groupShareVis {

		for _, g := range r.groups {
			if !g.legal || len(g.sectors) == 1 {

				continue
			}
			for _, s1 := range g.sectors {
				for _, s2 := range g.sectors {
					*(r.rejectTableIJ(s1, s2)) = VIS_VISIBLE
				}
			}
		}
	}

	for i := 0; i < r.numSectors; i++ {

		*(r.rejectTableIJ(i, i)) = VIS_VISIBLE

		if r.sectors[i].numLines == 0 {
			sly, _ := r.slyLinesInSector[uint16(i)]
			if !sly {
				for j := 0; j < r.numSectors; j++ {
					r.markVisibilitySector(i, j, VIS_HIDDEN)
				}
			} else {

				Log.Verbose(1, "REJECT: HACK Sector %d marked as visible to all (type: 1).\n", i)
				for j := 0; j < r.numSectors; j++ {
					r.markVisibilitySector(i, j, VIS_VISIBLE)
				}
			}
		} else {

			sly, _ := r.slyLinesInSector[uint16(i)]
			if sly {
				Log.Verbose(1, "REJECT: HACK Sector %d marked as visible to all (type: 2).\n", i)
				for j := 0; j < r.numSectors; j++ {
					r.markVisibilitySector(i, j, VIS_VISIBLE)
				}
			}
		}

		sec := &(r.sectors[i])
		for j := 0; j < sec.numNeighbors; j++ {
			nei := sec.neighbors[j]
			if nei.index > sec.index {
				r.markVisibilitySector(sec.index, nei.index, VIS_VISIBLE)
			}
		}
	}
}

func (r *FastRejectWork) entireGroupHiddenNoSly(sector int) bool {
	if r.sectors[sector].numLines != 0 {
		return false
	}

	for _, sec := range r.groups[r.groups[sector].parent].sectors {
		if r.sectors[sec].numLines != 0 {
			return false
		}
		if r.slyLinesInSector[uint16(sec)] {
			return false
		}
	}
	return true
}

func (r *FastRejectWork) testLinePair(srcLine, tgtLine *TransLine) bool {
	done := r.getLineVisibilityDone(srcLine, tgtLine)
	if done || r.dontBother(srcLine, tgtLine) {
		return false
	}

	if r.linesTooFarApart(srcLine, tgtLine) {
		r.setLineVisibilityDone(srcLine, tgtLine)
		return false
	}

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

func (r *FastRejectWork) getLineVisibilityDone(srcLine, tgtLine *TransLine) bool {
	if srcLine == tgtLine {
		return true
	}

	offset := getVisTableOffsetPrefab(srcLine, tgtLine, len(r.transLines))
	data := r.lineVisDone[offset>>3]
	bit := uint8(1 << (offset % 8))

	return data&bit == bit
}

func (r *FastRejectWork) setLineVisibilityDone(srcLine, tgtLine *TransLine) {

	if srcLine == tgtLine {
		return
	}

	offset := getVisTableOffsetPrefab(srcLine, tgtLine, len(r.transLines))
	data := r.lineVisDone[offset>>3]
	bit := uint8(1 << (offset % 8))

	r.lineVisDone[offset>>3] = data | bit
}

func (r *FastRejectWork) dontBother(srcLine, tgtLine *TransLine) bool {

	if (*r.rejectTableIJ(int(srcLine.frontSector), int(tgtLine.frontSector)) != VIS_UNKNOWN) &&
		(*r.rejectTableIJ(int(srcLine.frontSector), int(tgtLine.backSector)) != VIS_UNKNOWN) &&
		(*r.rejectTableIJ(int(srcLine.backSector), int(tgtLine.frontSector)) != VIS_UNKNOWN) &&
		(*r.rejectTableIJ(int(srcLine.backSector), int(tgtLine.backSector)) != VIS_UNKNOWN) {
		return true
	}

	return false
}

func (r *FastRejectWork) checkLOS(src, tgt *TransLine) bool {

	var myWorld WorldInfo
	r.initializeWorld(&myWorld, src, tgt)
	r.markBlockMap(&myWorld)
	if r.findInterveningLines(&(myWorld.solidSet)) {

		var common *IntVertex
		common = src.end
		linesTouch := false
		if *common == *tgt.start {
			linesTouch = true
		} else {
			common = src.start
			if *common == *tgt.end {
				linesTouch = true
			}
		}
		if linesTouch {
			more := false
			for i := myWorld.solidSet.loIndex; i <= myWorld.solidSet.hiIndex; i++ {
				line := myWorld.solidSet.lines[i]
				if (line.start == *common) || (line.end == *common) {
					more = true
					break
				}
			}
			if !more {

				return true
			}
		}

		switch TrimLines(myWorld.src, myWorld.tgt, &myWorld.solidSet) {

		case -1:
			{
				return false
			}

		case 0:
			{

			}

		default:
			{

				if !FindPolyLines(&myWorld) {
					return false
				}

				if FindObstacles(&myWorld) {
					return false
				}
			}
		}
	}
	return true
}

func (r *FastRejectWork) divideRegion(src, tgt *TransLine) bool {

	num := tgt.DX*(src.start.Y-tgt.start.Y) - tgt.DY*(src.start.X-tgt.start.X)
	det := src.DX*tgt.DY - src.DY*tgt.DX
	t := float64(num) / float64(det)

	crossPoint := IntVertex{
		X: int(int64(src.start.X) + int64(t*float64(src.DX))),
		Y: int(int64(src.start.Y) + int64(t*float64(src.DY))),
	}

	if (crossPoint == *(src.start)) || (crossPoint == *(src.end)) {
		Log.Verbose(3, "Reject: integer truncation detected in divideRegion for lines %d & %d\n", src.index, tgt.index)
		return r.checkLOS(src, tgt)
	}

	*(r.drLine) = *src
	r.drLine.end = &crossPoint

	isVisible := r.checkLOS(r.drLine, tgt)

	if !isVisible {
		r.drLine.start = &crossPoint
		r.drLine.end = src.end
		tgt.start, tgt.end = tgt.end, tgt.start
		tgt.DX = -tgt.DX
		tgt.DY = -tgt.DY
		isVisible = r.checkLOS(r.drLine, tgt)
	}

	return isVisible
}

func (r *FastRejectWork) forceVisibility(i, j int, visibility uint8) {
	if !r.hasGroups {
		*(r.rejectTableIJ(i, j)) = visibility
		return
	}
	groupI := r.groups[i].sectors
	groupJ := r.groups[j].sectors
	for _, i2 := range groupI {
		for _, j2 := range groupJ {
			*(r.rejectTableIJ(i2, j2)) = visibility
		}
	}
}

func (r *FastRejectWork) NoProcess_TryLoad() bool {
	if !(r.rmbFrame.IsNOPROCESSInEffect()) {
		return false
	}
	return false
}

func FastgetRejectWorkIntf() RejectWorkIntf {
	return &FastRejectWork{}
}

func (fr *RMBFrame) FastProcessOptionsRMB(r *FastRejectWork) {
	if fr == nil {
		return
	}

	fr.FastprocessDistanceUsingOptions(r, nil)
	fr.FastprocessSimpleBlindSafeOptions(r, nil)

	fr.FastprocessINCLUDEs(r)

	fr.FastprocessEXCLUDEs(r)

	r.generateReport()
	r.distanceTable = nil
}

func (fr *RMBFrame) FastprocessDistanceUsingOptions(r *FastRejectWork, sectors []SectorRMB) {
	if fr == nil || r.distanceTable == nil {
		return
	}

	madeSectors := false
	if sectors == nil {
		sectors = make([]SectorRMB, r.numSectors)
		madeSectors = true
	}

	fr.Parent.FastprocessDistanceUsingOptions(r, sectors)

	for _, command := range fr.Commands {
		if command.Type == RMB_BLIND {
			r.prepareBlind(command, sectors)
		}

		if command.Type == RMB_SAFE {
			r.prepareSafe(command, sectors)
		}
	}

	if !madeSectors {

		return
	}

	for i, sector := range sectors {
		if sector.Blind > 0 {
			r.applyBlind(sector, i)
		}
		if sector.Safe > 0 {
			r.applySafe(sector, i)
		}
	}
}

func (r *FastRejectWork) prepareBlind(command RMBCommand, sectors []SectorRMB) {
	if command.Band {
		for _, num := range command.List[0] {
			group := r.rmbGetGroup(sectors, num, command)
			for _, si := range group {
				sector := rmbGetSector(sectors, si, command)
				if sector == nil {
					continue
				}
				if command.Invert {
					sector.Blind = 7
				} else {
					sector.Blind = 4
				}
				sector.BlindLo = command.Data[0]
				sector.BlindHi = command.Data[1]
			}
		}
	} else {
		for _, num := range command.List[0] {
			group := r.rmbGetGroup(sectors, num, command)
			for _, si := range group {
				sector := rmbGetSector(sectors, si, command)
				if sector == nil {
					continue
				}
				if sector.Blind < 4 {
					if command.Invert {
						sector.Blind |= 2
						sector.BlindHi = command.Data[0]
					} else {
						sector.Blind |= 1
						sector.BlindLo = command.Data[0]
					}
				}
			}
		}
	}
}

func (r *FastRejectWork) prepareSafe(command RMBCommand, sectors []SectorRMB) {
	if command.Band {
		for _, num := range command.List[0] {
			group := r.rmbGetGroup(sectors, num, command)
			for _, si := range group {
				sector := rmbGetSector(sectors, si, command)
				if sector == nil {
					continue
				}
				if command.Invert {
					sector.Safe = 7
				} else {
					sector.Safe = 4
				}
				sector.SafeLo = command.Data[0]
				sector.SafeHi = command.Data[1]
			}
		}
	} else {
		for _, num := range command.List[0] {
			group := r.rmbGetGroup(sectors, num, command)
			for _, si := range group {
				sector := rmbGetSector(sectors, si, command)
				if sector == nil {
					continue
				}
				if sector.Safe < 4 {
					if command.Invert {
						sector.Safe |= 2
						sector.SafeHi = command.Data[0]
					} else {
						sector.Safe |= 1
						sector.SafeLo = command.Data[0]
					}
				}
			}
		}
	}
}

func (r *FastRejectWork) rmbGetGroup(sectors []SectorRMB, num int, command RMBCommand) []int {
	asector := rmbGetSector(sectors, num, command)
	if asector == nil {
		return nil
	}
	if r.hasGroups && !r.groups[num].legal {
		command.Error("command issued on sector %d but it is part of group. Will substitute it for group instead%d\n",
			num, r.groups[num].parent)
		num = r.groups[num].parent
	}
	return r.groups[num].sectors
}

func (r *FastRejectWork) rmbCheckSectorInRange(i int, command RMBCommand) bool {
	if i >= r.numSectors || i < 0 {
		command.Error("specified sector number out of range: %d\n", i)
		return false
	}
	return true
}

func (r *FastRejectWork) applyBlind(sector SectorRMB, i int) {
	if sector.Blind == 3 {
		if sector.BlindLo > sector.BlindHi {
			sector.BlindLo, sector.BlindHi = sector.BlindHi, sector.BlindLo
			sector.Blind = 4
		}
	}

	for j := 0; j < r.numSectors; j++ {

		if sector.Blind&1 == 1 {

			if int(*r.distanceTableIJ(i, j)) >= sector.BlindLo {
				*r.rejectTableIJ(i, j) = VIS_HIDDEN
			}
		}
		if sector.Blind&2 == 2 {

			if int(*r.distanceTableIJ(i, j)) < sector.BlindHi {
				*r.rejectTableIJ(i, j) = VIS_HIDDEN
			}
		}
		if sector.Blind == 4 {

			if int(*r.distanceTableIJ(i, j)) >= sector.BlindLo &&
				int(*r.distanceTableIJ(i, j)) < sector.BlindHi {
				*r.rejectTableIJ(i, j) = VIS_HIDDEN
			}

		}
	}
}

func (r *FastRejectWork) applySafe(sector SectorRMB, i int) {
	if sector.Safe == 3 {
		if sector.SafeLo > sector.SafeHi {
			sector.SafeLo, sector.SafeHi = sector.SafeHi, sector.SafeLo
			sector.Safe = 4
		}
	}

	for j := 0; j < r.numSectors; j++ {

		if sector.Safe&1 == 1 {

			if int(*r.distanceTableIJ(i, j)) >= sector.SafeLo {
				*r.rejectTableIJ(j, i) = VIS_HIDDEN
			}
		}
		if sector.Safe&2 == 2 {

			if int(*r.distanceTableIJ(i, j)) < sector.SafeHi {
				*r.rejectTableIJ(j, i) = VIS_HIDDEN
			}
		}
		if sector.Safe == 4 {

			if int(*r.distanceTableIJ(i, j)) >= sector.SafeLo &&
				int(*r.distanceTableIJ(i, j)) < sector.SafeHi {
				*r.rejectTableIJ(j, i) = VIS_HIDDEN
			}
		}
	}
}

func (fr *RMBFrame) FastprocessINCLUDEs(r *FastRejectWork) {
	if fr == nil {
		return
	}
	fr.Parent.FastprocessINCLUDEs(r)

	for _, cmd := range fr.Commands {
		if cmd.Type == RMB_INCLUDE {
			for _, i := range cmd.List[0] {
				if !r.rmbCheckSectorInRange(i, cmd) {
					continue
				}
				for _, j := range cmd.List[1] {
					if !r.rmbCheckSectorInRange(j, cmd) {
						continue
					}

					r.forceVisibility(i, j, VIS_VISIBLE)
				}
			}
		}
	}
}

func (fr *RMBFrame) FastprocessEXCLUDEs(r *FastRejectWork) {
	if fr == nil {
		return
	}
	fr.Parent.FastprocessEXCLUDEs(r)

	for _, cmd := range fr.Commands {
		if cmd.Type == RMB_EXCLUDE {
			for _, i := range cmd.List[0] {
				if !r.rmbCheckSectorInRange(i, cmd) {
					continue
				}
				for _, j := range cmd.List[1] {
					if !r.rmbCheckSectorInRange(j, cmd) {
						continue
					}

					r.forceVisibility(i, j, VIS_HIDDEN)
				}
			}
		}
	}
}

func (r *FastRejectWork) CreateDistanceTable() {
	Log.Verbose(1, "Reject: calculating sector distances for RMB effects (this may allocate a lot of memory)\n")

	tableSize := uint64(r.numSectors+1)*uint64(r.numSectors) -
		uint64(r.numSectors-1)*uint64(r.numSectors)/2
	dtableWorkData := make([]uint16, tableSize)
	r.distanceTable = dtableWorkData[:tableSize-uint64(r.numSectors)]
	for i, _ := range dtableWorkData {
		dtableWorkData[i] = 65535
	}

	numSectors := r.numSectors
	queue := CreateRingU16(uint32(numSectors))
	if r.hasGroups {

		r.maxLength = r.distanceTableFromGroups(dtableWorkData, queue,
			numSectors)
		return
	}
	length := uint16(0)
	offset := 0
	for i := 0; i < numSectors; i++ {
		itLength := BFS(r.sectors, dtableWorkData[offset:offset+r.numSectors],
			uint16(i), queue)
		if length < itLength {
			length = itLength
		}

		copy(dtableWorkData[offset:offset+numSectors-i],
			dtableWorkData[offset+i:offset+numSectors])
		offset += numSectors - i

		queue.Reset()
	}

	r.maxLength = length
}

func (r *FastRejectWork) distanceTableFromGroups(distanceTable []uint16,
	queue *RingU16, numSectors int) uint16 {
	length := uint16(0)
	offset := 0

	for i := 0; i < numSectors; i++ {

		if r.groups[i].minRepresentative != i {
			offset += numSectors - i
			continue
		}
		parent := uint16(r.groups[i].parent)
		distanceRow := distanceTable[offset : offset+numSectors]
		itLength := GroupBFS(r.groups, distanceRow, parent, queue)
		for grI, _ := range r.groups {

			if !r.groups[grI].legal {
				distanceRow[grI] = distanceRow[r.groups[grI].parent]
			}
		}
		if length < itLength {
			length = itLength
		}

		copy(distanceRow[:numSectors-i], distanceRow[i:])
		offset += numSectors - i

		queue.Reset()
	}

	for i := 0; i < numSectors; i++ {
		herald := uint64(r.groups[i].minRepresentative)
		ui := uint64(i)
		if ui != herald {

			heraldStart := herald * (uint64(numSectors)<<1 + 1 - herald) >> 1
			iStart := ui * (uint64(numSectors)<<1 + 1 - ui) >> 1

			srcRow := distanceTable[heraldStart+
				(ui-herald) : heraldStart+uint64(numSectors)-herald]
			destRow := distanceTable[iStart : iStart+uint64(numSectors)-ui]
			copy(destRow, srcRow)
		}
	}
	return length
}

func (r *FastRejectWork) ApplyDistanceLimits() {
	ok, maxLength := r.rmbFrame.GetLENGTHValue()
	if ok {

		if maxLength < r.maxLength {
			r.maxLength = maxLength
		}
	}

	if r.maxLength == 65535 {
		Log.Verbose(2, "Reject: effective maxlength ended up being 65535 and therefore is redundant.\n")
		return
	}

	Log.Verbose(2, "Reject: maxlength in effect is %d\n", r.maxLength)

	for i := 0; i < r.numSectors; i++ {
		for j := i + 1; j < r.numSectors; j++ {

			if *(r.distanceTableIJ(i, j)) > r.maxLength {

				r.markVisibilitySector(i, j, VIS_HIDDEN)
			}
		}
	}

}

func (r *FastRejectWork) distanceTableIJ(i, j int) *uint16 {
	if i > j {
		i, j = j, i
	}
	return &r.distanceTable[uint64(i)*(uint64(r.numSectors)<<1-1-uint64(i))>>1+
		uint64(j)]
}

func (r *FastRejectWork) linesTooFarApart(srcLine, tgtLine *TransLine) bool {
	if r.maxDistance != UNLIMITED_DISTANCE &&
		r.pointTooFar(srcLine.start, tgtLine) &&
		r.pointTooFar(srcLine.end, tgtLine) &&
		r.pointTooFar(tgtLine.start, srcLine) &&
		r.pointTooFar(tgtLine.end, srcLine) {
		Log.Verbose(2, "Reject: lines %d and %d are too far apart (rmb DISTANCE option applied)\n",
			srcLine.index, tgtLine.index)
		return true
	}
	return false
}

func (r *FastRejectWork) pointTooFar(p *IntVertex, line *TransLine) bool {
	p1 := line.start
	p2 := line.end

	c1 := int64(line.DX)*int64(p.X-p1.X) + int64(line.DY)*int64(p.Y-p1.Y)

	if c1 <= 0 {
		return !(r.mapDistance(p, p1) < r.maxDistance)
	}

	if c1 >= int64(line.H) {
		return !(r.mapDistance(p, p2) < r.maxDistance)
	}

	d := (int64(line.DX)*int64(p.Y-p1.Y) - int64(line.DY)*int64(p.X-p1.X)) / int64(line.H)
	if d < 0 {
		return false
	}
	return !(uint64(d) < r.maxDistance)
}

func (r *FastRejectWork) mapDistance(p1, p2 *IntVertex) uint64 {
	dx := int64(p1.X - p2.X)
	dy := int64(p1.Y - p2.Y)
	return uint64(dx*dx) + uint64(dy*dy)
}

func (r *FastRejectWork) generateReport() {
	if r.distanceTable == nil {
		return
	}
	r.generateReportForFrame(r.rmbFrame)
}

func (r *FastRejectWork) generateReportForFrame(rmbFrame *RMBFrame) bool {
	if rmbFrame == nil {
		return false
	}
	ret := false
	for _, cmd := range rmbFrame.Commands {

		if cmd.Type == RMB_REPORT {
			ret = true
			if int(uint16(cmd.Data[0])) != cmd.Data[0] {
				cmd.Error("Distance specified for REPORT command is out of range (must be 0 <= %d <= 65535), will be ignored\n",
					cmd.Data[0])
				continue
			}
			writ := r.reportGetWriter()
			if writ != nil {
				r.reportDoForDistance(writ, uint16(cmd.Data[0]))
			}
		}
	}
	if !ret {

		ret = r.generateReportForFrame(rmbFrame.Parent)
	}
	return ret
}

func (r *FastRejectWork) reportDoForDistance(w io.Writer, distance uint16) {
	r.printfln(w, "# %s All sectors with LOS distance>%d are reported",
		r.mapName, distance)
	for i := 0; i < r.numSectors; i++ {
		for j := i + 1; j < r.numSectors; j++ {

			if *(r.distanceTableIJ(i, j)) > distance &&
				!isHidden(*(r.rejectTableIJ(i, j))) &&
				!isHidden(*(r.rejectTableIJ(j, i))) {
				r.printfln(w, "%d,%d", i, j)
			}
		}
	}
}

func (r *FastRejectWork) printfln(w io.Writer, format string, a ...interface{}) {
	WriterPrintfln(w, r.rmbFrame.RMB.CRLF, format, a...)
}

func (r *FastRejectWork) reportGetWriter() io.Writer {
	if r.fileControl.freport != nil {
		return r.fileControl.freport
	}
	wri, err := r.fileControl.OpenReportFile()
	if err != nil {
		Log.Error("Couldn't create file %s: %s", r.fileControl.reportFileName,
			err.Error())
		return nil
	}

	r.printfln(wri, "# %s %s", PROG_CAPIT_NAME, VERSION)
	return wri
}

func (r *FastRejectWork) HasRMBEffectLINE(lineIdx uint16) bool {
	if r.lineEffects == nil {
		return false
	}
	return r.lineEffects[lineIdx] == LINE_EFFECT_SOLID
}

func (r *FastRejectWork) RMBLoadLineEffects() {
	r.lineEffects = make(map[uint16]uint8)
	did := r.loadLineEffectsForFrame(r.rmbFrame)
	if !did {
		r.lineEffects = nil
	}
}

func (r *FastRejectWork) loadLineEffectsForFrame(rmbFrame *RMBFrame) bool {
	if rmbFrame == nil {
		return false
	}
	ret := r.loadLineEffectsForFrame(rmbFrame.Parent)

	for _, cmd := range rmbFrame.Commands {
		switch cmd.Type {
		case RMB_LEFT:
			{
				idx := uint16(cmd.Data[0])
				oldEffect := r.lineEffects[idx]
				if oldEffect == LINE_EFFECT_NONE {
					r.lineEffects[idx] = LINE_EFFECT_LEFT
				} else if oldEffect == LINE_EFFECT_RIGHT {

					r.lineEffects[idx] = LINE_EFFECT_SOLID
				}
				ret = true
			}
		case RMB_RIGHT:
			{
				idx := uint16(cmd.Data[0])
				oldEffect := r.lineEffects[idx]
				if oldEffect == LINE_EFFECT_NONE {
					r.lineEffects[idx] = LINE_EFFECT_RIGHT
				} else if oldEffect == LINE_EFFECT_LEFT {

					r.lineEffects[idx] = LINE_EFFECT_SOLID
				}
				ret = true
			}
		case RMB_LINE:
			{
				r.lineEffects[uint16(cmd.Data[0])] = LINE_EFFECT_SOLID
				ret = true
			}
		}
	}
	return ret
}

func (fr *RMBFrame) FastprocessSimpleBlindSafeOptions(r *FastRejectWork, sectors []SectorRMB) {
	if fr == nil || !fr.hasSimpleBlindSafe() {
		return
	}

	madeSectors := false
	if sectors == nil {
		sectors = make([]SectorRMB, r.numSectors)
		madeSectors = true
	}

	fr.Parent.FastprocessSimpleBlindSafeOptions(r, sectors)

	for _, command := range fr.Commands {
		if command.Type == RMB_SIMPLE_BLIND {
			r.prepareBlind(command, sectors)
		}

		if command.Type == RMB_SIMPLE_SAFE {
			r.prepareSafe(command, sectors)
		}
	}

	if !madeSectors {

		return
	}

	for i, sector := range sectors {
		if sector.Blind > 0 {
			r.applySimpleBlind(sector, i)
		}
		if sector.Safe > 0 {
			r.applySimpleSafe(sector, i)
		}
	}
}

func (r *FastRejectWork) applySimpleBlind(sector SectorRMB, i int) {

	grI := r.groups[i].parent
	for j := 0; j < r.numSectors; j++ {
		if !r.groups[j].legal {
			continue
		}
		grJ := r.groups[j].parent
		for _, k := range r.groups[grJ].sectors {
			if sector.Blind&1 == 1 {

				if sector.BlindLo == 0 || grJ != grI {
					*r.rejectTableIJ(i, k) = VIS_HIDDEN
				}
			}
			if sector.Blind&2 == 2 {

				if sector.BlindHi == 1 && grI == grJ {
					*r.rejectTableIJ(i, k) = VIS_HIDDEN
				}
			}
		}
	}
}

func (r *FastRejectWork) applySimpleSafe(sector SectorRMB, i int) {

	grI := r.groups[i].parent
	for j := 0; j < r.numSectors; j++ {
		if !r.groups[j].legal {
			continue
		}
		grJ := r.groups[j].parent
		for _, k := range r.groups[grJ].sectors {
			if sector.Safe&1 == 1 {

				if sector.SafeLo == 0 || grJ != grI {
					*r.rejectTableIJ(k, i) = VIS_HIDDEN
				}
			}
			if sector.Safe&2 == 2 {

				if sector.SafeHi == 1 && grI == grJ {
					*r.rejectTableIJ(k, i) = VIS_HIDDEN
				}
			}
		}
	}
}

func (w *FastRejectWork) DFS(graph *RejGraph, sector *RejSector) int {

	sector.graph = graph
	sector.indexDFS = graph.numSectors
	sector.loDFS = graph.numSectors
	sector.isArticulation = false

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

func (w *FastRejectWork) CreateGraph(root *RejSector) *RejGraph {
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

func (w *FastRejectWork) HideComponents(oldGraph, newGraph *RejGraph) {
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

func (w *FastRejectWork) SplitGraph(oldGraph *RejGraph) {
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

func (w *FastRejectWork) InitializeGraphs(numSectors int) {
	Log.Verbose(1, "Creating sector graphs...\n")

	w.graphTable.numGraphs = 0
	w.graphTable.graphs = make([]RejGraph, w.numSectors*2)
	sectorPool := make([]*RejSector, w.numSectors*4)
	w.graphTable.sectorStart = sectorPool

	for _, v := range w.graphTable.graphs {
		v.numSectors = 0
		v.sectors = nil
	}

	for i, _ := range sectorPool {
		sectorPool[i] = nil
	}

	graph := &(w.graphTable.graphs[0])
	graph.numSectors = w.numSectors
	graph.sectors = w.graphTable.sectorStart
	w.graphTable.sectorStart = w.graphTable.sectorStart[w.numSectors:]
	w.graphTable.numGraphs++

	for i := 0; i < w.numSectors; i++ {
		w.sectors[i].graph = graph
		graph.sectors[i] = &(w.sectors[i])
	}

	w.SplitGraph(graph)

	for i := 0; i < w.numSectors; i++ {
		w.sectors[i].baseGraph = w.sectors[i].graph
	}

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

	Log.Verbose(1, "Reject: created %d graphs.\n", w.graphTable.numGraphs)
}

func (w *FastRejectWork) HideSectorFromComponents(root, sec *RejSector) {
	graph := sec.graph

	for _, sec2 := range graph.sectors[:root.indexDFS] {
		w.markVisibility(sec.index, sec2.index, VIS_HIDDEN)
	}
	for _, sec2 := range graph.sectors[root.hiDFS+1 : graph.numSectors] {
		w.markVisibility(sec.index, sec2.index, VIS_HIDDEN)
	}
}

func (w *FastRejectWork) AddGraph(graph *RejGraph, sector *RejSector) {

	sector.graph = graph
	sector.indexDFS = graph.numSectors
	sector.loDFS = graph.numSectors

	graph.sectors[graph.numSectors] = sector
	graph.numSectors++

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

func (w *FastRejectWork) QuickGraph(root *RejSector) *RejGraph {
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

func (w *FastRejectWork) ProcessSectorLines(key, root, sector *RejSector,
	lines []*TransLine) {

	isVisible := *(w.rejectTableIJ(key.index, sector.index)) == VIS_VISIBLE
	isUnknown := *(w.rejectTableIJ(key.index, sector.index)) == VIS_UNKNOWN

	if isUnknown {
		ptr := lines
		for ptr[0] != nil {
			srcLine := ptr[0]
			ptr = ptr[1:]
			for i := 0; i < sector.numNeighbors; i++ {
				child := sector.neighbors[i]

				if child.loDFS <= sector.indexDFS {
					for j := 0; j < sector.numLines; j++ {
						tgtLine := sector.lines[j]
						if (tgtLine.backSector == uint16(child.index)) || (tgtLine.frontSector == uint16(child.index)) {
							if ShouldTest(srcLine, uint16(key.index), tgtLine, uint16(sector.index)) {
								if w.testLinePair(srcLine, tgtLine) {
									w.markPairVisible(srcLine, tgtLine)
									goto done
								}
							}
						}
					}
				}
			}
		}
	}

	if !isVisible {

		graph := sector.graph

		if sector.loDFS == sector.indexDFS {

			for i := sector.indexDFS; i <= sector.hiDFS; i++ {
				w.HideSectorFromComponents(root, graph.sectors[i])
			}

		} else {

			w.HideSectorFromComponents(root, sector)

			for i := 0; i < sector.numNeighbors; i++ {
				child := sector.neighbors[i]
				if child.graphParent == sector {

					if child.loDFS >= sector.indexDFS {
						for j := child.indexDFS; j <= child.hiDFS; j++ {
							w.HideSectorFromComponents(root, graph.sectors[j])
						}
					} else {
						w.ProcessSectorLines(key, root, child, lines)
					}
				}
			}
		}

		return
	}

done:

	for i := 0; i < sector.numNeighbors; i++ {
		child := sector.neighbors[i]
		if child.graphParent == sector {
			w.ProcessSectorLines(key, root, child, lines)
		}
	}
}

func (w *FastRejectWork) ProcessSector(sector *RejSector) {
	if sector.isArticulation {

		w.QuickGraph(sector)

		lines := make([]*TransLine, sector.numLines+1)

		for i := 0; i < sector.numNeighbors; i++ {

			child := sector.neighbors[i]

			if child.graphParent == sector {

				index := 0
				for j := 0; j < sector.numLines; j++ {
					line := sector.lines[j]
					if (line.backSector == uint16(child.index)) || (line.frontSector == uint16(child.index)) {
						lines[index] = line
						index++
					}
				}

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
				for j := 0; j < sector.numLines; j++ {
					srcLine := sector.lines[j]
					for k := 0; k < tgtSector.numLines; k++ {
						tgtLine := tgtSector.lines[k]
						if w.testLinePair(srcLine, tgtLine) {
							w.markPairVisible(srcLine, tgtLine)
							goto next
						}
					}
				}
				w.markVisibility(sector.index, tgtSector.index, VIS_HIDDEN)
			next:
			}
		}
	}
}

func (r *FastRejectWork) initializeWorld(world *WorldInfo, src, tgt *TransLine) {
	world.src = src
	world.tgt = tgt

	world.solidSet.lines = r.testLines
	world.solidSet.loIndex = 0
	world.solidSet.hiIndex = -1

	r.p1 = *(src.start)
	r.p2 = *(src.end)
	r.p3 = *(tgt.start)
	r.p4 = *(tgt.end)

	src.loPoint = &(r.p1)
	src.lo = 0.0
	src.hiPoint = &(r.p2)
	src.hi = 1.0
	tgt.loPoint = &(r.p3)
	tgt.lo = 0.0
	tgt.hiPoint = &(r.p4)
	tgt.hi = 1.0

	lowerPoly := &(world.lowerPoly)
	lowerPoly.points = r.polyPoints[:len(r.solidLines)+2]
	lowerPoly.numPoints = 2
	lowerPoly.lastPoint = -1
	lowerPoly.points[0] = src.hiPoint
	lowerPoly.points[1] = tgt.loPoint

	upperPoly := &(world.upperPoly)
	upperPoly.points = r.polyPoints[len(r.solidLines)+2:]
	upperPoly.numPoints = 2
	upperPoly.lastPoint = -1
	upperPoly.points[0] = tgt.hiPoint
	upperPoly.points[1] = src.loPoint
}

func (r *FastRejectWork) markBlockMap(world *WorldInfo) {
	r.loRow = int(r.blockmap.header.YBlocks)
	r.hiRow = -1

	r.drawBlockMapLine(world.src.start, world.src.end)
	r.drawBlockMapLine(world.tgt.start, world.tgt.end)
	r.drawBlockMapLine(world.src.start, world.tgt.end)
	r.drawBlockMapLine(world.tgt.start, world.src.end)

}

func (r *FastRejectWork) prepareBlockmapForLOS() {
	rowCount := int(r.blockmap.header.YBlocks)
	colCount := int(r.blockmap.header.XBlocks)
	r.blockMapBounds = make([]BlockMapBounds, rowCount)
	for row := 0; row < rowCount; row++ {
		r.blockMapBounds[row].lo = colCount
		r.blockMapBounds[row].hi = -1
	}

	r.solidList = make([]SolidmapSlice, len(r.blockmap.blocklist))
	totalEntries := 0
	for i, list := range r.blockmap.blocklist {
		r.solidList[i].offset = totalEntries
		r.solidList[i].length = len(list)
		totalEntries += r.solidList[i].length
	}
	r.solidMap = make([]uint16, totalEntries)
	i := 0
	for _, list := range r.blockmap.blocklist {
		for _, v := range list {
			r.solidMap[i] = r.indexToSolid[v]
			i++
		}
	}
}

func (r *FastRejectWork) drawBlockMapLine(p1, p2 *IntVertex) {
	x0 := p1.X - int(r.blockmap.header.XMin)
	y0 := p1.Y - int(r.blockmap.header.YMin)
	x1 := p2.X - int(r.blockmap.header.XMin)
	y1 := p2.Y - int(r.blockmap.header.YMin)

	startX := x0 >> BLOCK_BITS
	startY := y0 >> BLOCK_BITS
	endX := x1 >> BLOCK_BITS
	endY := y1 >> BLOCK_BITS

	if startY < r.loRow {
		r.loRow = startY
	}
	if startY > r.hiRow {
		r.hiRow = startY
	}

	if endY < r.loRow {
		r.loRow = endY
	}
	if endY > r.hiRow {
		r.hiRow = endY
	}

	r.updateRow(startX, startY)

	if startX == endX {

		if startY != endY {

			var dy int
			if endY > startY {
				dy = 1
			} else {
				dy = -1
			}
			b := true
			for b {
				startY += dy
				r.updateRow(startX, startY)
				b = startY != endY
			}

		}

	} else {

		if startY != endY {

			var deltaX int
			deltaY := (y1 - y0) << BLOCK_BITS
			nextX := x0 * (y1 - y0)

			var dy int
			if endY > startY {

				nextX += (startY<<BLOCK_BITS + BLOCK_WIDTH - y0) * (x1 - x0)
				deltaX = ((x1 - x0) << BLOCK_BITS)
				dy = 1
			} else {

				nextX += (startY<<BLOCK_BITS - y0) * (x1 - x0)
				deltaX = ((x0 - x1) << BLOCK_BITS)
				dy = -1
			}

			lastX := nextX / deltaY
			r.updateRow(lastX, startY)

			curY := startY
			if x0 < x1 {
				for true {

					curY = curY + dy
					bound := &(r.blockMapBounds[curY])
					if lastX < bound.lo {
						bound.lo = lastX
					}

					if curY == endY {
						break
					}
					nextX += deltaX
					lastX = nextX / deltaY
					if lastX > bound.hi {
						bound.hi = lastX
					}
				}
			} else {
				for true {

					curY = curY + dy
					bound := &(r.blockMapBounds[curY])
					if lastX > bound.hi {
						bound.hi = lastX
					}

					if curY == endY {
						break
					}
					nextX += deltaX
					lastX = nextX / deltaY
					if lastX < bound.lo {
						bound.lo = lastX
					}
				}
			}
		}

		r.updateRow(endX, endY)
	}
}

func (r *FastRejectWork) updateRow(col, row int) {
	bound := &(r.blockMapBounds[row])
	if col < bound.lo {
		bound.lo = col
	}
	if col > bound.hi {
		bound.hi = col
	}
}

func (r *FastRejectWork) findInterveningLines(set *LineSet) bool {

	lineCount := 0

	for i, _ := range r.seenLines {
		r.seenLines[i] = 0
	}

	xblocks := int(r.blockmap.header.XBlocks)
	for row := r.loRow; row <= r.hiRow; row++ {
		bound := &(r.blockMapBounds[row])
		blocklistIdx := row*xblocks + bound.lo
		for col := bound.lo; col <= bound.hi; col++ {
			obeg := r.solidList[blocklistIdx].offset
			oend := obeg + r.solidList[blocklistIdx].length
			for _, line := range r.solidMap[obeg:oend] {
				if !r.markAndRecall(line) {
					set.lines[lineCount] = &(r.solidLines[line])
					lineCount++
				}
			}
			blocklistIdx++
		}
		bound.lo = int(r.blockmap.header.XBlocks)
		bound.hi = -1
	}

	set.loIndex = 0
	set.hiIndex = lineCount - 1
	set.lines[lineCount] = nil

	if lineCount > 0 {
		return true
	}

	return false
}

func (r *FastRejectWork) markAndRecall(cur uint16) bool {
	bte := cur >> 3
	bit := uint8(1 << (cur % 8))
	retA := r.seenLines[bte] & bit
	if retA == bit {
		return true
	} else {
		r.seenLines[bte] = r.seenLines[bte] | bit
		return false
	}
}

func (r *FastRejectWork) markVisibility(i, j int, visibility uint8) {
	cell1 := r.rejectTableIJ(i, j)
	if *cell1 == VIS_UNKNOWN {
		*cell1 = visibility
	}

	cell2 := r.rejectTableIJ(j, i)
	if *cell2 == VIS_UNKNOWN {
		*cell2 = visibility
	}
}

func (w *FastRejectWork) setupMixer() {

}

func (w *FastRejectWork) mergeAndDestroyMixer() {

}

func (r *FastRejectWork) markVisibilityGroup(i, j int, visibility uint8) {

}

func (r *FastRejectWork) mixerIJ(i, j int) *uint8 {
	return nil
}

func (r *FastRejectWork) DFSGetNeighborsAndGroupsiblings(s *RejSector) []*RejSector {
	return s.neighbors
}
func init() {
	getFastRejectWorkIntf = FastgetRejectWorkIntf
}
