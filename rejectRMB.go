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
				// FIXME value of 65535 already means that DistanceLimits will
				// effectively not be applied
				Log.Error("RMB: LENGTH greater than 65535 is truncated to 65535 (maximum allowed sector count in limit-removing ports)\n")
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
		if fr.Commands[i].Type == RMB_LENGTH {
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
	r.distanceTable = nil // allow GC to delete distanceTable now or soon

	// INCLUDE is the 2nd highest priority option
	fr.processINCLUDEs(r)

	// EXCLUDE is the highest priority option
	fr.processEXCLUDEs(r)
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
			// Also handles second half of inverse BAND BLIND
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

// TODO find a way to reduce its size / use compression, cause this can
// allocate GBs of memory
func (r *RejectWork) CreateDistanceTable() {
	Log.Verbose(1, "Reject: calculating sector distances for RMB effects (this may allocate a lot of memory)\n")

	length := uint16(0)
	// This can be very big & overflow on 32-bit system and in general
	// require more memory than avaiable even on 64-bit systems
	r.distanceTable = make([]uint16, r.numSectors*r.numSectors)
	for i, _ := range r.distanceTable {
		r.distanceTable[i] = 65535 // maximum possible value for uint16
	}

	// TODO implement this. Hard to understand algo in Zennode
	// FUCK There is a second HUGE array there!
	// Go fuck yourself difficulty

	// So we set length to the maximum value that was encountered
	r.maxLength = length
}

func (r *RejectWork) ApplyDistanceLimits() {
	ok, maxLength := r.rmbFrame.GetLENGTHValue()
	if ok {
		// Is maximum encountered length lower than this value?
		// Cause if not, what's the point of applying the specified one
		if maxLength < r.maxLength {
			r.maxLength = maxLength
		}
	}

	if r.maxLength == 65535 { // distanceTable cell can never exceed this value
		return
	}

	for i := 0; i < r.numSectors; i++ {
		for j := i + 1; j < r.numSectors; j++ {
			if *(r.distanceTableIJ(i, j)) > maxLength {
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
	// TODO signed to unsigned cast, need verify there is no overflow
	return !(uint64(d) < r.maxDistance)
}

func (r *RejectWork) mapDistance(p1, p2 *IntVertex) uint64 {
	dx := int64(p1.X - p2.X)
	dy := int64(p1.Y - p2.Y)
	// TODO signed to unsigned cast, need verify there is no overflow
	return uint64(dx*dx + dy*dy)
}
