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

// rejectdefs.go
package main

// -----------------------------------------------------------------------------
// Block of pragma directives
//
// The pragmas aren't part of Go languages and are not parsed by Go compiler or
// the go build command. Instead, go generate will be used to call a special
// program I wrote that will parse source code into ASTs, apply modifications to
// it, and then produce a new file (see gen/codegen.go in VigilantBSP source
// tree)
//
// The idea was first used to support Zdoom extended nodes without penalizing
// performance of generating nodes in vanilla/DeeP format (see zdefs.go). That
// case is probably easier to get into.
//
// This current file has NOTHING to do with nodes, but instead concerns the
// generation of "fast" REJECT builder, which fast variation is the one that
// will cover most used cases actually, by replacing certain functions/methods
// with easier-to-inline for Go compiler. It solves the issue of performance
// loss that occurs from implementing RMB effects such as GROUP
// -----------------------------------------------------------------------------

//
// #pragma setprefix "Fast"
// #pragma replace RejectExtraData with RejectNoExtra
// #pragma replace_prototype *RejectWork.markVisibility with *RejectWork.markVisibilityFast
// #pragma replace_prototype *RejectWork.setupMixer with *RejectWork.setupMixerNoop
// #pragma replace_prototype *RejectWork.mergeAndDestroyMixer with *RejectWork.mergeAndDestroyMixerNoop
// #pragma replace_prototype *RejectWork.markVisibilityGroup with *RejectWork.markVisibilityGroupNoop
// #pragma replace_prototype *RejectWork.mixerIJ with *RejectWork.mixerIJNoop
// #pragma replace_prototype *RejectWork.DFSGetNeighborsAndGroupsiblings with *RejectWork.DFSGetNeighborsAndGroupsiblingsFast
// #pragma init getFastRejectWorkIntf with morphed getRejectWorkIntf

// -----------------------------------------------------------------------------
// End block of pragma directives
// -----------------------------------------------------------------------------

type RejectNoExtra struct{}

type RejectWorkIntfMaker func() RejectWorkIntf

// This callback must be overriden in init section of a go source file that is
// automatically generated
var getFastRejectWorkIntf RejectWorkIntfMaker = nil

type RejectWorkIntf interface {
	main(input RejectInput, hasGroups bool, groupShareVis bool,
		groups []RejGroup) []byte
}

// This whole unit started around this - the version of markVisibility without
// support for groups (RMF effect called GROUP). This *must* be inlinable. The
// version with groups that it is replacing is not inlinable, which results in
// noticeable performance loss
func (r *RejectWork) markVisibilityFast(i, j int, visibility uint8) {
	cell1 := r.rejectTableIJ(i, j)
	if *cell1 == VIS_UNKNOWN {
		*cell1 = visibility
	}

	cell2 := r.rejectTableIJ(j, i)
	if *cell2 == VIS_UNKNOWN {
		*cell2 = visibility
	}
}

func (w *RejectWork) setupMixerNoop() {

}

func (w *RejectWork) mergeAndDestroyMixerNoop() {

}

func (r *RejectWork) markVisibilityGroupNoop(i, j int, visibility uint8) {

}

func (r *RejectWork) mixerIJNoop(i, j int) *uint8 {
	return nil
}

func (r *RejectWork) DFSGetNeighborsAndGroupsiblingsFast(s *RejSector) []*RejSector {
	return s.neighbors
}
