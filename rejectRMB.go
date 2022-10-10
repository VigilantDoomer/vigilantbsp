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

// rejectRMB.go
package main

import (
	"fmt"
	"io"
)

// This file applies RMB effects to reject and so hosts code that is used
// only by rejectXXX.go
// Parser is in rmbparse.go, and shared declarations are in rmbunit.go

// Let {x} mean BLIND or SAFE (depending on context} then
// SectorRMB.{x} field can take following values:
// 0 - uninitialized (Zennode used -1)
// 1 - normal {x}
// 2 - inverse {x}
// 4 - normal BAND {x}
// 7 - inverse BAND {x}
type SectorRMB struct {
	Safe, SafeLo, SafeHi    int
	Blind, BlindLo, BlindHi int
}

const (
	LINE_EFFECT_NONE = iota // cause this will get 0, and I need SOLID to be distinct from 0
	LINE_EFFECT_SOLID
	LINE_EFFECT_LEFT
	LINE_EFFECT_RIGHT
)

// Check if RMB contains command that need table of distances where distances
// are measured in NUMBER OF SECTORS (think LENGTH rmb option, not DISTANCE)
// Fully analogues to Zennode's function of the same name in ZenReject.cpp
func (fr *RMBFrame) NeedDistances() bool {
	if fr == nil {
		return false
	}
	for _, cmd := range fr.Commands {
		switch cmd.Type {
		case RMB_BLIND, RMB_LENGTH, RMB_SAFE, RMB_REPORT:
			{
				return true
			}
		}
	}
	// If this frame doesn't contain these options, check parent one
	return fr.Parent.NeedDistances()
}

// Scans frame for LENGTH option and returns the LAST encountered value
// specified in it
// If not found in current frame, will scan parent frame
// If not found at all, first result is false.
// If found, first result is true and the second one is the LENGTH option value
func (fr *RMBFrame) GetLENGTHValue() (bool, uint16) {
	if fr == nil {
		return false, uint16(0)
	}
	for i := len(fr.Commands) - 1; i >= 0; i-- {
		if fr.Commands[i].Type == RMB_LENGTH {
			chk := fr.Commands[i].Data[0]
			if chk > 65535 {
				// Value of 65535 already means that DistanceLimits will
				// effectively not be applied, and I can't go beyond it without
				// increasing the memory size for distanceTable etc. + very hard
				// to reach even this number of sectors, much less arrange
				// them so that you could produce such length at the same time
				Log.Error("RMB: LENGTH greater than 65535 is truncated to 65535.\n")
				chk = 65535
			} else if chk < 0 {
				Log.Error("RMB: Negative LENGTH truncated to 0\n")
				chk = 0
			}
			return true, uint16(chk)
		}
	}
	b, val := fr.Parent.GetLENGTHValue()
	return b, val
}

// Scans frame for DISTANCE option and returns SQUARE of the LAST encountered
// value
// Like GetLENGTHValue(), it scan parent frame if not found in current one,
// and will signal absence of any DISTANCE option through setting first result
// to false
func (fr *RMBFrame) GetDISTANCEValue() (bool, uint64) {
	if fr == nil {
		return false, 0
	}
	for i := len(fr.Commands) - 1; i >= 0; i-- {
		if fr.Commands[i].Type == RMB_DISTANCE {
			v := uint64(fr.Commands[i].Data[0])
			// From Zennode: store the square of the distance (avoid floating
			// point later on)
			return true, v * v
		}
	}
	b, val := fr.Parent.GetDISTANCEValue()
	return b, val
}

func (fr *RMBFrame) ProcessOptionsRMB(r *RejectWork) {
	if fr == nil {
		return
	}
	fr.processDistanceUsingOptions(r, nil)

	// INCLUDE is the 2nd highest priority option
	fr.processINCLUDEs(r)

	// EXCLUDE is the highest priority option
	fr.processEXCLUDEs(r)

	// REPORT, though, is executed last. It doesn't apply any effects to the
	// map, so it doesn't disturb the priority promise
	r.generateReport()
	r.distanceTable = nil // allow GC to delete distanceTable now or soon
}

// Applies (BAND) BLIND and SAFE effects, both normal and inverted. Respects
// rules detailed in "Combining options" sections of RMB manual
func (fr *RMBFrame) processDistanceUsingOptions(r *RejectWork,
	sectors []SectorRMB) {
	if fr == nil || r.distanceTable == nil {
		return
	}

	madeSectors := false // remains false for parent frame
	if sectors == nil {
		sectors = make([]SectorRMB, r.numSectors)
		madeSectors = true // becomes true for the most local map frame
	}

	// Let's aggregate information from the parent frame before the current one
	fr.Parent.processDistanceUsingOptions(r, sectors)

	// Populate sectors array with the BLIND/SAFE information
	for _, command := range fr.Commands {
		if command.Type == RMB_BLIND {
			prepareBlind(command, sectors)
		}

		if command.Type == RMB_SAFE {
			prepareSafe(command, sectors)
		}
	}

	if !madeSectors {
		// parent sector information is aggregated into original callers array
		// and processed there
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

func prepareBlind(command RMBCommand, sectors []SectorRMB) {
	if command.Band {
		for _, num := range command.List[0] {
			sector := rmbGetSector(sectors, num, command)
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
	} else {
		for _, num := range command.List[0] {
			sector := rmbGetSector(sectors, num, command)
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

func prepareSafe(command RMBCommand, sectors []SectorRMB) {
	if command.Band {
		for _, num := range command.List[0] {
			sector := rmbGetSector(sectors, num, command)
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
	} else {
		for _, num := range command.List[0] {
			sector := rmbGetSector(sectors, num, command)
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

func rmbGetSector(sectors []SectorRMB, i int, command RMBCommand) *SectorRMB {
	if i >= len(sectors) || i < 0 {
		// TODO should refer to source file
		Log.Error("Reject: RMB specified sector number out of range: %d at line %d\n", i,
			command.SrcLine)
		return nil
	}
	return &(sectors[i])
}

func (r *RejectWork) rmbCheckSectorInRange(i int, command RMBCommand) bool {
	if i >= r.numSectors || i < 0 {
		// TODO should refer to source file
		Log.Error("Reject: RMB specified sector number out of range: %d at line %d\n", i,
			command.SrcLine)
		return false
	}
	return true
}

func (r *RejectWork) applyBlind(sector SectorRMB, i int) {
	if sector.Blind == 3 {
		if sector.BlindLo > sector.BlindHi {
			sector.BlindLo, sector.BlindHi = sector.BlindHi, sector.BlindLo
			sector.Blind = 4
		}
	}

	for j := 0; j < r.numSectors; j++ {
		if sector.Blind&1 == 1 {
			// Handle normal BLIND
			// Also handles first half of inverse BAND BLIND
			if int(*r.distanceTableIJ(i, j)) >= sector.BlindLo {
				*r.rejectTableIJ(i, j) |= VIS_RMB_HIDDEN
			}
		}
		if sector.Blind&2 == 2 {
			// Handle inverse BLIND
			// Also handles second half of inverse BAND BLIND
			if int(*r.distanceTableIJ(i, j)) < sector.BlindHi {
				*r.rejectTableIJ(i, j) |= VIS_RMB_HIDDEN
			}
		}
		if sector.Blind == 4 {
			// Handle normal BAND BLIND
			if int(*r.distanceTableIJ(i, j)) >= sector.BlindLo &&
				int(*r.distanceTableIJ(i, j)) < sector.BlindHi {
				*r.rejectTableIJ(i, j) |= VIS_RMB_HIDDEN
			}

		}
	}
}

func (r *RejectWork) applySafe(sector SectorRMB, i int) {
	if sector.Safe == 3 {
		if sector.SafeLo > sector.SafeHi {
			sector.SafeLo, sector.SafeHi = sector.SafeHi, sector.SafeLo
			sector.Safe = 4
		}
	}

	for j := 0; j < r.numSectors; j++ {
		if sector.Safe&1 == 1 {
			// Handle normal SAFE
			// Also handles first half of inverse BAND SAFE
			if int(*r.distanceTableIJ(i, j)) >= sector.SafeLo {
				*r.rejectTableIJ(j, i) |= VIS_RMB_HIDDEN
			}
		}
		if sector.Safe&2 == 2 {
			// Handle inverse SAFE
			// Also handles second half of inverse BAND SAFE
			if int(*r.distanceTableIJ(i, j)) < sector.SafeHi {
				*r.rejectTableIJ(j, i) |= VIS_RMB_HIDDEN
			}
		}
		if sector.Safe == 4 {
			// Handle normal BAND SAFE
			if int(*r.distanceTableIJ(i, j)) >= sector.SafeLo &&
				int(*r.distanceTableIJ(i, j)) < sector.SafeHi {
				*r.rejectTableIJ(j, i) |= VIS_RMB_HIDDEN
			}
		}
	}
}

func (fr *RMBFrame) processINCLUDEs(r *RejectWork) {
	if fr == nil {
		return
	}
	fr.Parent.processINCLUDEs(r)

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
					*(r.rejectTableIJ(i, j)) |= VIS_RMB_VISIBLE
				}
			}
		}
	}
}

func (fr *RMBFrame) processEXCLUDEs(r *RejectWork) {
	if fr == nil {
		return
	}
	fr.Parent.processEXCLUDEs(r)

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
					// Yes, overwrite with VIS_HIDDEN - intentional
					// because EXCLUDE has the highest priority of all RMB
					// options
					*(r.rejectTableIJ(i, j)) = VIS_HIDDEN
				}
			}
		}
	}
}

// CreateDistanceTable() evaluates distance (in the number of sectors) between
// ALL pairs of sectors
// Rolled out my own implementation (rather than taking Zennode's), should use
// less memory, be easier to understand and parallelize.
func (r *RejectWork) CreateDistanceTable() {
	Log.Verbose(1, "Reject: calculating sector distances for RMB effects (this may allocate a lot of memory)\n")

	// Given that it is hard to reach 65536 count of sectors (sectors need
	// several linedefs to be defined, which are also bound by 65536 limit),
	// memory consumption, in practice, will be several hundreds of megabytes
	// at most. Gigabytes are unlikely.
	// This means we are not expecting an overflow condition to be reached on
	// 32-bit systems when computing table size
	r.distanceTable = make([]uint16, r.numSectors*r.numSectors)
	for i, _ := range r.distanceTable {
		r.distanceTable[i] = 65535 // maximum possible value for uint16
	}

	length := uint16(0)

	// Problem definition:
	// All-pairs shortest paths (path lengths) for unweighted undirected
	// graph (vertexes = sectors, edges = neighborship relation)
	// Straightforward solution: BFS (breadth first search) per each starting
	// point.
	numSectors := r.numSectors
	queue := CreateRingU16(uint32(numSectors))
	for i := 0; i < numSectors; i++ {
		itLength := r.BFS(r.distanceTable[r.numSectors*i:r.numSectors*(i+1)],
			uint16(i), queue)
		if length < itLength {
			length = itLength
		}
		// Resetting queue for reuse - probably not needed, since Queue should
		// be empty always by the end of BFS
		queue.Reset()
	}

	// So we set length to the maximum value that was encountered
	r.maxLength = length
}

// Fills a row of distanceTable with lengths of paths between source sector
// (which is the number of row) and all other sectors (columns). Lengths are
// computed using BFS, hence the name of function
func (r *RejectWork) BFS(distanceRow []uint16, source uint16,
	queue *RingU16) uint16 {
	visited := make([]bool, r.numSectors)
	visited[source] = true
	distanceRow[source] = 0
	length := uint16(0)
	queue.Enqueue(source)
	for !queue.Empty() {
		item := queue.Dequeue()
		for _, neighbor := range r.sectors[item].neighbors {
			if neighbor == nil {
				break
			}
			nIndex := uint16(neighbor.index)
			if !visited[nIndex] {
				visited[nIndex] = true
				queue.Enqueue(nIndex)
				newDist := distanceRow[item] + 1
				distanceRow[nIndex] = newDist
				if length < newDist {
					length = newDist
				}
			}
		}
	}
	return length
}

func (r *RejectWork) ApplyDistanceLimits() {
	ok, maxLength := r.rmbFrame.GetLENGTHValue()
	if ok {
		// If user-supplied length is less than maximum computed value (and so
		// presents stronger requirement), then we apply user-supplied length.
		// Otherwise we will apply maximum computed value to weed out just those
		// sectors that are unreachable from certain others
		if maxLength < r.maxLength {
			r.maxLength = maxLength
		}
	}

	if r.maxLength == 65535 { // distanceTable cell can never exceed this value
		Log.Verbose(2, "Effective maxlength ended up being 65535 and therefore is redundant.\n")
		return
	}

	Log.Verbose(2, "Maxlength in effect is %d\n", r.maxLength)

	for i := 0; i < r.numSectors; i++ {
		for j := i + 1; j < r.numSectors; j++ {
			if *(r.distanceTableIJ(i, j)) > r.maxLength {
				// Yes, markVisibility, Anything hacked to be visible in
				// eliminateTrivialCases is going to stay that way
				r.markVisibility(i, j, VIS_HIDDEN)
			}
		}
	}

}

func (r *RejectWork) distanceTableIJ(i, j int) *uint16 {
	return &(r.distanceTable[i*r.numSectors+j])
}

func (r *RejectWork) linesTooFarApart(srcLine, tgtLine *TransLine) bool {
	if r.maxDistance != uint64(0xFFFFFFFFFFFFFFFF) &&
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

func (r *RejectWork) pointTooFar(p *IntVertex, line *TransLine) bool {
	p1 := line.start
	p2 := line.end
	// TODO check if int64 is needed, or int could suffice
	c1 := int64(line.DX)*int64(p.X-p1.X) + int64(line.DY)*int64(p.Y-p1.Y)

	if c1 <= 0 {
		return !(r.mapDistance(p, p1) < r.maxDistance)
	}

	if c1 >= int64(line.H) {
		return !(r.mapDistance(p, p2) < r.maxDistance)
	}

	d := (int64(line.DX)*int64(p.Y-p1.Y) - int64(line.DY)*int64(p.X-p1.X)) / int64(line.H)
	if d < 0 {
		return false // guaranteed d < r.maxDistance, since maxDistance is non-negative
	}
	return !(uint64(d) < r.maxDistance)
}

func (r *RejectWork) mapDistance(p1, p2 *IntVertex) uint64 {
	dx := int64(p1.X - p2.X)
	dy := int64(p1.Y - p2.Y)
	return uint64(dx*dx) + uint64(dy*dy)
}

func (r *RejectWork) generateReport() bool {
	return r.generateReportForFrame(r.rmbFrame)
}

func (r *RejectWork) generateReportForFrame(rmbFrame *RMBFrame) bool {
	if rmbFrame == nil {
		return false
	}
	ret := false
	for _, cmd := range rmbFrame.Commands {
		// Unlike what RMB does, all REPORT commands in relevant frame are
		// processed, not just the last one. However, REPORT commands in parent
		// frame will be ignored if they exist in local frame
		if cmd.Type == RMB_REPORT {
			ret = true
			if int(uint16(cmd.Data[0])) != cmd.Data[0] {
				Log.Error("Distance specified for REPORT command is out of range (must be 0 <= %d <= 65535), will be ignored\n",
					cmd.Data[0])
				continue
			}
			writ := r.reportGetWriter()
			r.reportDoForDistance(writ, uint16(cmd.Data[0]))
		}
	}
	if !ret {
		// Only process parent frame if no REPORT commands were in map local
		// frame
		ret = r.generateReportForFrame(rmbFrame.Parent)
	}
	return ret
}

func (r *RejectWork) reportDoForDistance(w io.Writer, distance uint16) {
	w.Write([]byte(fmt.Sprintf("# %s All sectors with LOS distance>%d are reported\n",
		r.mapName, distance)))
	for i := 0; i < r.numSectors; i++ {
		for j := i + 1; j < r.numSectors; j++ {
			// According to manual, only _mutually_ visible sectors that
			// exceed the specified length are to be reported
			if *(r.distanceTableIJ(i, j)) > distance &&
				!isHidden(*(r.rejectTableIJ(i, j))) &&
				!isHidden(*(r.rejectTableIJ(j, i))) {
				w.Write([]byte(fmt.Sprintf("%d,%d\n",
					i, j)))
			}
		}
	}
}

// (re)creates *.rpt file if it was not created/open yet, putting
// "# VigilantBSP <version>" as a first line; if already open, then returns
// an open instance to append to
func (r *RejectWork) reportGetWriter() io.Writer {
	if r.fileControl.freport != nil {
		return r.fileControl.freport
	}
	wri, err := r.fileControl.OpenReportFile()
	if err != nil {
		Log.Panic("Couldn't create file %s: %s", r.fileControl.reportFileName,
			err.Error())
	}
	// Add a comment line specifying the version of VigilantBSP used to
	// produce the file. This is so that I might change format in the future,
	// the line might act as identificator for other tools (say, written by
	// other people) that want to parse the file for their own needs
	wri.WriteString(fmt.Sprintf("# %s %s\n", PROG_CAPIT_NAME, VERSION))
	return wri
}

// Returns whether RMB effect called LINE was applied to line #lineIdx
// If it is, such lines are treated as if they were solid instead of transient,
// also it can prevent the sector to be recognised as self-referencing for reject
// computation purposes, provided all lines suspected to produce said effect by
// the chosen method are eliminated via LINE
// NOTE functionality DISABLED for v0.74 release because I am having doubts
// about whether LINE should have this quirk for self-referencing sectors.
// NOTE remains DISABLED for v0.75 release as well
func (r *RejectWork) HasRMBEffectLINE(lineIdx uint16) bool {
	// Temporarily ignoring LINE, has effect of not implementing it
	return false
	// The below code is how it was supposed to work
	/*if r.lineEffects == nil {
		return false
	}
	return r.lineEffects[lineIdx] == LINE_EFFECT_SOLID*/
}

func (r *RejectWork) RMBLoadLineEffects() {
	r.lineEffects = make(map[uint16]uint8)
	did := r.loadLineEffectsForFrame(r.rmbFrame)
	if !did {
		r.lineEffects = nil
	}
}

func (r *RejectWork) loadLineEffectsForFrame(rmbFrame *RMBFrame) bool {
	if rmbFrame == nil {
		return false
	}
	ret := r.loadLineEffectsForFrame(rmbFrame.Parent)
	// If multiple commands specified for the same line, last option will take
	// effect. RMB may do things differently, I dunno, duplicates aren't
	// supposed to happen anyway
	for _, cmd := range rmbFrame.Commands {
		switch cmd.Type {
		case RMB_LEFT:
			{
				// FIXME can overwrite LINE_EFFECT_SOLID, but the effect itself
				// is not implemented. Also reported as not implemented by
				// rmb parser
				r.lineEffects[uint16(cmd.Data[0])] = LINE_EFFECT_LEFT
				ret = true
			}
		case RMB_RIGHT:
			{
				// FIXME can overwrite LINE_EFFECT_SOLID, but the effect itself
				// is not implemented. Also reported as not implemented by
				// rmb parser
				r.lineEffects[uint16(cmd.Data[0])] = LINE_EFFECT_RIGHT
				ret = true
			}
		case RMB_LINE:
			{
				// This should be implemented already
				r.lineEffects[uint16(cmd.Data[0])] = LINE_EFFECT_SOLID
				ret = true
			}
		}
	}
	return ret
}
