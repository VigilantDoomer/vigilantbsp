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

// rejectSymmDefs.go
package main

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
// #pragma init getSymmRejectWorkIntf with morphed getRejectWorkIntf

// -----------------------------------------------------------------------------
// End block of pragma directives
// -----------------------------------------------------------------------------

type RejectSymmNoExtra struct{}

// This callback must be overriden in init section of a go source file that is
// automatically generated
var getSymmRejectWorkIntf RejectWorkIntfMaker = nil

// This whole unit started around this - the version of markVisibility without
// support for groups (RMF effect called GROUP). This *must* be inlinable. The
// version with groups that it is replacing is not inlinable, which results in
// noticeable performance loss
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

func (r *RejectWork) rejectTableIJSymm(i, j int) *uint8 {
	if i > j {
		i, j = j, i
	}
	return &(r.rejectTable[i*r.numSectors+j-((i*(i+1))>>1)])
}

func (r *RejectWork) getResultSymm() []byte {
	rejectSize := int((r.numSectors*r.numSectors)+7) / 8
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
	tableSize := r.numSectors*r.numSectors - (r.numSectors-1)*r.numSectors/2 + 7
	r.rejectTable = make([]uint8, tableSize, tableSize)
	for i, _ := range r.rejectTable {
		r.rejectTable[i] = 0
	}
}
