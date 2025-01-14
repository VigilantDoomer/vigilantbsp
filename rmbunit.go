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

// This file defines all structures shared by those parts of the code that
// need to deal with RMB: reject code and parser code.

import (
	"fmt"
)

const ( // RMB option/command types
	RMB_UNKNOWN = iota
	RMB_EXMY_MAP
	RMB_MAPXY_MAP
	RMB_BAND
	RMB_BLIND
	RMB_BLOCK
	RMB_DISTANCE
	RMB_DOOR
	RMB_EXCLUDE
	RMB_GROUP
	RMB_INCLUDE
	RMB_INVERT
	RMB_LEFT
	RMB_LENGTH
	RMB_LINE
	RMB_NODOOR
	RMB_NOMAP
	RMB_NOPROCESS
	RMB_ONE
	RMB_PERFECT
	RMB_PREPROCESS
	RMB_PROCESS
	RMB_REPORT
	RMB_RIGHT
	RMB_SAFE
	RMB_TRACE
	RMB_VORTEX

	RMB_SIMPLE_BLIND // BLIND 0/1 might be replaced by this
	RMB_SIMPLE_SAFE  // SAFE 0/1 might be replaced by this
)

const ( // RMB_FRAME_TYPE
	RMB_FRAME_GLOBAL = iota // applies to all levels
	RMB_FRAME_EXMY          // applies to a specific ExMy level
	RMB_FRAME_MAPXY         // applies to a specific MapXY level, or to UDMF level with arbitrary name
)

// RMB actually references to these as options, not commands, but from
// perspective of the reject builder these are instructions to follow
type RMBCommand struct {
	SrcLine int // index of line in source text (file) this command was parsed from
	Type    int
	Invert  bool // whether invert prefix was applied to this option
	Band    bool // whether band prefix was applied to this option
	// Banded bool
	Data [2]int   // stores integer(s) specified for this option
	List [2][]int // stores list(s) of sectors specified for this option
	//
	WadFileName []byte    // for NOPROCESS command, this can exist - the wad to get reject from
	MapLumpName []byte    // for NOPROCESS command, second optional argument is map name. Since UDMF, it does not have to be MAPXY or EXMY anymore
	Frame       *RMBFrame // which frame this belongs to. Only assigned at the end of LoadRMB
}

type RMBFrameId struct {
	Type int    // RMB_FRAME_TYPE constant
	Name string // name of map
}

// RMB allows specifying options:
// 1) to apply to all levels
// 2) to apply to specific level
// and the latter can also override the former. Thus, a two-tier structure
// based on the frame allows to track which options apply to the current level
type RMBFrame struct {
	Id       RMBFrameId
	Commands []RMBCommand
	Parent   *RMBFrame  // nil for global. Map's frame may have global frame as its parent, if one exists
	RMB      *LoadedRMB // which RMB this belongs too. Only assigned at the end of LoadRMB
}

type LoadedRMB struct {
	mapFrames   map[RMBFrameId]*RMBFrame
	globalFrame *RMBFrame
	srcFile     string
	CRLF        bool
}

// Caution, may return a non-nil frame for non-existent map, if global frame
// exists it will return that. This was done to simplify lump scheduler, it will
// request frames for maps as it encounters them
func (l *LoadedRMB) LookupRMBFrameForMap(frameId RMBFrameId) *RMBFrame {
	if l == nil {
		return nil
	}
	frame := l.mapFrames[frameId]
	if frame == nil {
		return l.globalFrame
	}
	return frame
}

// Wrapper over LookupRMBFrameForMap, this takes a name of lump that marks
// the start of level lumps. There should be no bytes beyond the null byte in
// the input marker name (ByteSliceBeforeTerm subroutine in main program takes
// care of this)
func (l *LoadedRMB) LookupRMBFrameForMapMarker(marker []byte) *RMBFrame {
	if l == nil {
		return nil
	}
	var frameId RMBFrameId
	if marker[0] == 'E' && MAP_ExMx.FindSubmatch(marker) != nil {
		frameId = RMBFrameId{
			Type: RMB_FRAME_EXMY,
			Name: string(marker),
		}
	} else {
		frameId = RMBFrameId{
			Type: RMB_FRAME_MAPXY,
			Name: string(marker),
		}
	}
	return l.LookupRMBFrameForMap(frameId)
}

func (l *RMBCommand) Error(s string, a ...interface{}) {
	fmtS := fmt.Sprintf("RMB %s%d error: %s", l.getFile(), l.SrcLine, s)
	Log.Error(fmtS, a...)
}

// returns filename with a colon appended at the end, or empty string if
// filename is empty or couldn't be determined
func (l *RMBCommand) getFile() string {
	if l.Frame == nil { // shouldn't happen
		return ""
	}
	if l.Frame.RMB == nil { // shouldn't happen
		return ""
	}
	ret := l.Frame.RMB.srcFile
	if len(ret) > 0 {
		return ret + ":"
	}
	return ret
}

func (f *RMBFrame) isEmpty() bool {
	return f == nil || (len(f.Commands) == 0 && f.Parent.isEmpty())
}

func (f *RMBFrame) Clone() *RMBFrame {
	if f == nil {
		return nil
	}
	ret := &RMBFrame{}
	*ret = *f
	ret.Parent = ret.Parent.Clone()
	ret.Commands = make([]RMBCommand, len(f.Commands))
	copy(ret.Commands, f.Commands)
	for i, _ := range ret.Commands {
		ret.Commands[i].Frame = ret
	}
	return ret
}
