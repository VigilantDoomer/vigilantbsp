// Copyright (C) 2025, VigilantDoomer
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
	"os"
)

type UDMF_Level struct {
	RejectLumpIdx      int
	wriBus             *WriteBusControl
	WroteRejectAlready bool
	le                 []LumpEntry
	f                  *os.File // input file descriptor, is not always loaded into this structure
	fc                 *FileControl
	mapName            string
}

// DoLevel is executed once per level, but UDMF_Level struct is allocated once and is
// reused for all levels, hence it must do reinitialization of all fields
func (l *UDMF_Level) DoLevel(le []LumpEntry, idx int, action *ScheduledLump,
	f *os.File, wriBus *WriteBusControl, fileControl *FileControl) {
	// reinit all
	l.le = le
	l.f = f
	l.fc = fileControl
	l.mapName = GetLumpName(le, action)
	l.WroteRejectAlready = false
	l.RejectLumpIdx = 0
	// TODO grap TEXTMAP and parse it. Parser is yet to be written
	Log.Printf("(noop) Processing UDMF level %s:\n", l.mapName)
	textmap, err := LoadTEXTMAP(f, le, action.Level[0])
	if err != nil {
		Log.Panic("Couldn't read TEXTMAP: %s\n", err.Error())
	}
	// TEXTMAP is written to output file so as not forget it
	wriBus.SendRawLump(textmap, action.DirIndex, "", "")
	// TODO implement TEXTMAP parser
	// parser should ideally be scheduled concurrently with writing to the bus
	// when TEXTMAP parser failed (or doesn't exist lol), merely copy everything
	// but, uh, the first implementation doesn't have to be this effecient
	parseFail := true
	if config.Reject == REJECT_NORMAL {
		// currently no-op, but do check RMB
		if !action.RMBOptions.isEmpty() {
			Log.Printf("Not actually building REJECT, but acknowledging that a non-empty RMB frame exists:\n")
			inherited := action.RMBOptions.Parent == nil ||
				len(action.RMBOptions.Commands) == 0
			inheritedStr := "yes"
			if !inherited {
				inheritedStr = "no"
			}
			Log.Printf("  level RMB frame is inherited from global RMB frame: " + inheritedStr + "\n")
		}
	}
	if len(action.Level) > 1 { // supposed to be, normally, unless ENDMAP is missing
		for _, subaction := range action.Level[1:] {
			idx = subaction.DirIndex
			//bname := ByteSliceBeforeTerm(le[idx].Name[:])
			copyLump := true
			if !parseFail {
				// TODO REJECT, GL Nodes
				// might set copyLump == false
			}
			if copyLump {
				tmpBuf := make([]byte, le[idx].Size, le[idx].Size)
				f.ReadAt(tmpBuf, int64(le[idx].FilePos))
				wriBus.SendRawLump(tmpBuf, idx, "", "")
			}
		}
	}

	if !parseFail {
		l.WaitForAndWriteData()
	}
}

func LoadTEXTMAP(f *os.File, le []LumpEntry, textmapAction *ScheduledLump) ([]byte, error) {
	fp := le[textmapAction.DirIndex].FilePos
	sz := le[textmapAction.DirIndex].Size
	if sz == 0 {
		return make([]byte, 0), nil
	}
	ret := make([]byte, sz)
	_, err := f.ReadAt(ret, int64(fp))
	if err != nil {
		return nil, err
	}
	return ret, nil
}

func (l *UDMF_Level) WaitForAndWriteData() {
	// TODO when have data to write (first thing to support would be building
	// reject for UDMF maps as no one else can do it... yet. Later, GL nodes.
	// Blockmap and regular nodes should not be built for UDMF.)
}
