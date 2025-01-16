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

// rejectSymmDefs.go
package main

import (
	"io"
)

// -----------------------------------------------------------------------------
// Block of pragma directives
//
// The pragmas aren't part of Go languages blablabla... see rejectdefs.go
// Anyway, this dictates how the code for symmetric reject generation is
// generated
// -----------------------------------------------------------------------------

//
// #pragma setprefix "Symmetric"
// #pragma replace RejectExtraData with RejectSymmNoExtra
// #pragma replace_prototype *RejectWork.rejectTableIJ with *RejectWork.rejectTableIJSymm
// #pragma replace_prototype *RejectWork.getResult with *RejectWork.getResultSymm
// #pragma replace_prototype *RejectWork.prepareReject with *RejectWork.prepareRejectSymm
// #pragma replace_prototype *RejectWork.markVisibility with *RejectWork.markVisibilityOneWay
// #pragma replace_prototype *RejectWork.markVisibilitySector with *RejectWork.markVisibilitySectorOneWay
// #pragma replace_prototype *RejectWork.setupMixer with *RejectWork.setupMixerSymmNoop
// #pragma replace_prototype *RejectWork.mergeAndDestroyMixer with *RejectWork.mergeAndDestroyMixerSymmNoop
// #pragma replace_prototype *RejectWork.markVisibilityGroup with *RejectWork.markVisibilityGroupSymmNoop
// #pragma replace_prototype *RejectWork.mixerIJ with *RejectWork.mixerIJSymmNoop
// #pragma replace_prototype *RejectWork.DFSGetNeighborsAndGroupsiblings with *RejectWork.DFSGetNeighborsAndGroupsiblingsSymm
// #pragma replace_prototype *RejectWork.reportDoForDistance with *RejectWork.reportDoForDistanceSymm
// #pragma init getSymmRejectWorkIntf with morphed getRejectWorkIntf

// -----------------------------------------------------------------------------
// End block of pragma directives
// -----------------------------------------------------------------------------

type RejectSymmNoExtra struct{}

// This callback must be overriden in init section of a go source file that is
// automatically generated
var getSymmRejectWorkIntf RejectWorkIntfMaker = nil

// Here, not only dropping groups, but also dropping a call on
// rejectTableIJ(j,i)
func (r *RejectWork) markVisibilityOneWay(i, j int, visibility uint8) {
	cell1 := r.rejectTableIJ(i, j)
	if *cell1 == VIS_UNKNOWN {
		*cell1 = visibility
	}
}

func (r *RejectWork) markVisibilitySectorOneWay(i, j int, visibility uint8) {
	cell1 := r.rejectTableIJ(i, j)
	if *cell1 == VIS_UNKNOWN {
		*cell1 = visibility
	}
}

func (w *RejectWork) setupMixerSymmNoop() {

}

func (w *RejectWork) mergeAndDestroyMixerSymmNoop() {

}

func (r *RejectWork) markVisibilityGroupSymmNoop(i, j int, visibility uint8) {

}

func (r *RejectWork) mixerIJSymmNoop(i, j int) *uint8 {
	return nil
}

// The core piece of symmetric reject processing - only store data for row<=col
func (r *RejectWork) rejectTableIJSymm(i, j int) *uint8 {
	if i > j {
		i, j = j, i
	}
	return &r.rejectTable[int64(i)*(r.symmShim-int64(i))>>1+
		int64(j)]
}

func (r *RejectWork) getResultSymm() []byte {
	rejectSize := rejectLumpSize_nonUDMF(r.numSectors)
	result := make([]byte, rejectSize, rejectSize)
	//tbIdx := 0
	i := 0
	j := 0
	for k := 0; k < rejectSize; k++ {
		bits := 0
		if isHidden(*r.rejectTableIJ(i, j)) {
			bits = bits | 0x01
		}
		r.symmMoveIJ(&i, &j)
		if isHidden(*r.rejectTableIJ(i, j)) {
			bits = bits | 0x02
		}
		r.symmMoveIJ(&i, &j)
		if isHidden(*r.rejectTableIJ(i, j)) {
			bits = bits | 0x04
		}
		r.symmMoveIJ(&i, &j)
		if isHidden(*r.rejectTableIJ(i, j)) {
			bits = bits | 0x08
		}
		r.symmMoveIJ(&i, &j)
		if isHidden(*r.rejectTableIJ(i, j)) {
			bits = bits | 0x10
		}
		r.symmMoveIJ(&i, &j)
		if isHidden(*r.rejectTableIJ(i, j)) {
			bits = bits | 0x20
		}
		r.symmMoveIJ(&i, &j)
		if isHidden(*r.rejectTableIJ(i, j)) {
			bits = bits | 0x40
		}
		r.symmMoveIJ(&i, &j)
		if isHidden(*r.rejectTableIJ(i, j)) {
			bits = bits | 0x80
		}
		r.symmMoveIJ(&i, &j)
		result[k] = uint8(bits)
	}
	return result
}

func (r *RejectWork) symmMoveIJ(i, j *int) {
	(*j)++
	if *j >= r.numSectors && (*i+1) < r.numSectors {
		*j = 0
		(*i)++
	}
}

func (r *RejectWork) prepareRejectSymm() {
	// The working table size (uses bytes not bits).
	// Extra 7 bytes to simplify getResult() method
	tableSize := uint64(r.numSectors)*uint64(r.numSectors) -
		uint64(r.numSectors-1)*uint64(r.numSectors)/2 + 7
	r.rejectTable = make([]uint8, tableSize, tableSize)
	for i, _ := range r.rejectTable {
		r.rejectTable[i] = 0
	}
	r.symmShim = int64(r.numSectors)<<1 - 1
}

func (r *RejectWork) DFSGetNeighborsAndGroupsiblingsSymm(s *RejSector) []*RejSector {
	return s.neighbors
}

func (r *RejectWork) reportDoForDistanceSymm(w io.Writer, distance uint16) {
	r.printfln(w, "# %s All sectors with LOS distance>%d are reported",
		r.mapName, distance)
	for i := 0; i < r.numSectors; i++ {
		for j := i + 1; j < r.numSectors; j++ {
			// According to manual, only _mutually_ visible sectors that
			// exceed the specified length are to be reported
			// But in symmetric reject case, mutual visibility can be checked
			// in just one direction...
			if *(r.distanceTableIJ(i, j)) > distance &&
				!isHidden(*(r.rejectTableIJ(i, j))) {
				r.printfln(w, "%d,%d", i, j)
			}
		}
	}
}
