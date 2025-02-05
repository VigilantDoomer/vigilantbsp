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
	"bytes"
	"strings"
	"testing"
)

func TestEmptyRMB(t *testing.T) {
	suc, ld := LoadRMB([]byte(""), "test_emptyrmb.rej")
	if !suc {
		t.Fatalf("Couldn't load empty rmb\n")
	}
	if ld != nil && ld.globalFrame != nil {
		t.Fatalf("global frame is not nil!\n")
	}
}

func TestCommentOnlyRMB(t *testing.T) {
	rmb := strings.Builder{}
	rmb.WriteString("# This is a valid .rej file\n")
	rmb.WriteString("# It consists of nothing but comments\n")
	suc, ld := LoadRMB([]byte(rmb.String()), "test_commentrmb.rej")
	if !suc {
		t.Fatalf("Couldn't load comment-only load rmb\n")
	}
	if ld != nil && ld.globalFrame != nil {
		t.Fatalf("global frame is not nil!\n")
	}
}

func TestEasyRMB(t *testing.T) {
	rmb := strings.Builder{}
	rmb.WriteString("# This RMB contains comments as well as effects\n")
	rmb.WriteString("\n")
	rmb.WriteString("# First, we disable processing, and take reject\n")
	rmb.WriteString("# from another file's MAP25\n")
	rmb.WriteString("NOPROCESS mahmap.wad MAP25\n")
	rmb.WriteString("# Now switch to local frame for map E1M1\n")
	rmb.WriteString("E1M1\n")
	rmb.WriteString("  INC (4) (6 7) \n")
	rmb.WriteString("  EXCLUDE 2 3 \n")
	rmb.WriteString("  # Any further effects would be associated with\n")
	rmb.WriteString("  # E1M1 level, unless another map marker is defined\n")
	suc, ld := LoadRMB([]byte(rmb.String()), "test_easyrmb.rej")
	if !suc {
		t.Fatalf("Couldn't load easy rmb\n")
	}
	if ld == nil || ld.globalFrame == nil {
		t.Fatalf("Failed to acknowledge RMB's global frame!")
	}
	if len(ld.globalFrame.Commands) != 1 {
		t.Fatalf("Wrong number of commands in global frame: expected 1 got %d\n", len(ld.globalFrame.Commands))
	}
	if ld.globalFrame.Commands[0].Type != RMB_NOPROCESS ||
		!bytes.Equal(ld.globalFrame.Commands[0].WadFileName, []byte("mahmap.wad")) ||
		ld.globalFrame.Commands[0].Data[0] != -2 ||
		ld.globalFrame.Commands[0].Data[1] != 25 {
		t.Fatalf("The command in global frame was parsed wrong file=%s Data0=%d Data1=%d \n",
			string(ld.globalFrame.Commands[0].WadFileName),
			ld.globalFrame.Commands[0].Data[0],
			ld.globalFrame.Commands[0].Data[1])
	}
	frm := ld.LookupRMBFrameForMap(RMBFrameId{
		Type: RMB_FRAME_EXMY,
		Name: "E1M1",
	})
	if frm == nil {
		t.Fatalf("Failed to produced frame for E1M1\n")
	}
	if frm.Parent == nil { // good luck ascertaining it is the same though
		t.Fatalf("The E1M1 frame doesn't link to global frame.\n")
	}
	if len(frm.Commands) != 2 {
		t.Fatalf("Wrong number of commands in E1M1 frame.\n")
	}
	if frm.Commands[0].Type != RMB_INCLUDE ||
		len(frm.Commands[0].List[0]) != 1 ||
		frm.Commands[0].List[0][0] != 4 ||
		len(frm.Commands[0].List[1]) != 2 ||
		frm.Commands[0].List[1][0] != 6 ||
		frm.Commands[0].List[1][1] != 7 {
		t.Fatalf("E1M1 frame first command is parsed wrong\n")
	}
	if frm.Commands[1].Type != RMB_EXCLUDE ||
		len(frm.Commands[1].List[0]) != 1 ||
		frm.Commands[1].List[0][0] != 2 ||
		len(frm.Commands[1].List[1]) != 1 ||
		frm.Commands[1].List[1][0] != 3 {
		t.Fatalf("E1M1 frame second command is parsed wrong\n")
	}
}

func TestReadStringArg(t *testing.T) {
	c := ParseContext{
		command: []byte("SCREW THIS"),
	}
	if string(c.ReadStringArg()) != "SCREW" || string(c.ReadStringArg()) != "THIS" {
		t.Fatalf("error\n")
	}
	c.command = []byte(`"SCREW" "THIS"`)

	if string(c.ReadStringArg()) != "SCREW" || string(c.ReadStringArg()) != "THIS" {
		t.Fatalf("error\n")
	}

	c.command = []byte(`"SCREW THIS HARD"`)

	if string(c.ReadStringArg()) != "SCREW THIS HARD" {
		t.Fatalf("error\n")
	}

	c.command = []byte(`ZERO "FREAKING CHANCE"  `)

	if string(c.ReadStringArg()) != "ZERO" || string(c.ReadStringArg()) != "FREAKING CHANCE" {
		t.Fatalf("error\n")
	}
}

func TestNoProcessRMB(t *testing.T) {
	rmb := strings.Builder{}
	rmb.WriteString("# NOPROCESS can have no arguments\n")
	rmb.WriteString("NOPROCESS\n")
	rmb.WriteString("E1M1\n")
	rmb.WriteString("# or it could have one\n")
	rmb.WriteString("NOPROCESS mahmap.wad\n")
	rmb.WriteString("E1M2\n")
	rmb.WriteString("# or two\n")
	rmb.WriteString("NOPROCESS mahothermap.wad E1M2\n")
	rmb.WriteString("MAP UDMFWTF\n")
	rmb.WriteString("# or two\n")
	rmb.WriteString(`NOPROCESS "C:\Documents and Settings\Mah  User\Desktop\the most doomy gum.wad" UDMFFTW # yeah` +
		"\n")
	rmb.WriteString(`NOPROCESS testForRemainder.wad ` +
		"\n")
	suc, ld := LoadRMB([]byte(rmb.String()), "test_noprocessrmb.rej")
	if !suc {
		t.Fatalf("Failure in NOPROCESS variations test\n")
	}
	if ld == nil || ld.globalFrame == nil {
		t.Fatalf("Failed to acknowledge RMB's global frame!\n")
	}
	frm := ld.LookupRMBFrameForMap(RMBFrameId{
		Type: RMB_FRAME_MAPXY,
		Name: "UDMFWTF",
	})
	if frm == nil {
		t.Fatalf("Failed to acknowledge frame for UDMFWTF map!\n")
	}
	if frm.Commands[0].Type != RMB_NOPROCESS || string(frm.Commands[0].WadFileName) !=
		`C:\Documents and Settings\Mah  User\Desktop\the most doomy gum.wad` ||
		string(frm.Commands[0].MapLumpName) != `UDMFFTW` {
		t.Fatalf("Wrong NOPROCESS command in UDMFWTF frame! Got: %s %s\n",
			string(frm.Commands[0].WadFileName), string(frm.Commands[0].MapLumpName))
	}
	if frm.Commands[1].Type != RMB_NOPROCESS || string(frm.Commands[1].WadFileName) !=
		"testForRemainder.wad" || frm.Commands[1].MapLumpName != nil {
		t.Fatalf("Wrong NOPROCESS command in UDMFWTF frame! Got: %s %s\n",
			string(frm.Commands[1].WadFileName), string(frm.Commands[1].MapLumpName))
	}
	// I don't bother checking the rest
}

func TestComplexRMB(t *testing.T) {
	rmb := strings.Builder{}
	rmb.WriteString("# This rmb will contain those nasty INVERT and BAND \n")
	rmb.WriteString("# prefixes\n")
	rmb.WriteString("NOMAP\n")
	rmb.WriteString("PREPROCESS 1\n")
	rmb.WriteString("length 17\n")
	rmb.WriteString("Distance 1000\n")
	rmb.WriteString("MAP01\n")
	rmb.WriteString("# I honestly didn't check if the following two conflict\n")
	rmb.WriteString("INVERT BAND BLIND 2 3 (1 2) # and comment for good measure\n")
	rmb.WriteString("INV BAND SAFE 2 4 (20 21 22)\n")
	rmb.WriteString("GROUP 10 (10 11 12)\n")
	rmb.WriteString("BLIND 0 10\n")
	rmb.WriteString("# Next marker probably wouldn't be supported by original\n")
	rmb.WriteString("MAP95 # yes I can define levels beyond vanilla Doom support\n")
	rmb.WriteString("BAND BLIND 2 4 (5 6 7)\n")
	rmb.WriteString("SAFE 1 74\n")
	rmb.WriteString("# This map will occur twice\n")
	rmb.WriteString("E2M3\n")
	rmb.WriteString("len 15\n")
	rmb.WriteString("# Next marker is also extension not present in original RMB\n")
	rmb.WriteString("# It is for UDMF map support\n")
	rmb.WriteString("MAP GOFY\n")
	rmb.WriteString("VORTEX 12 35\n")
	rmb.WriteString("VORTEX 14 11\n")
	rmb.WriteString("# Unnecessary use of long map extension, but should parse fine\n")
	rmb.WriteString("MAP E1M2\n")
	rmb.WriteString("INCLUDE 1 (3 5)\n")
	rmb.WriteString("# this map has been before, but marker was in short form\n")
	rmb.WriteString("MAP E2M3 # counting previous frame, it will have 2 commands total\n")
	rmb.WriteString("EXCLUDE 3 2\n")
	suc, ld := LoadRMB([]byte(rmb.String()), "test_complexrmb.rej")
	if !suc {
		t.Fatalf("Couldn't load complex rmb\n")
	}
	if ld == nil || ld.globalFrame == nil {
		t.Fatalf("Failed to acknowledge RMB's global frame!")
	}
	if len(ld.globalFrame.Commands) != 4 {
		t.Fatalf("Wrong number of commands in global frame: expected 4 got %d\n", len(ld.globalFrame.Commands))
	}

	if ld.globalFrame.Commands[0].Type != RMB_NOMAP {
		t.Fatalf("Rogue command instead of NOMAP in global frame")
	}

	if ld.globalFrame.Commands[1].Type != RMB_PREPROCESS ||
		ld.globalFrame.Commands[1].Data[0] != 1 {
		t.Fatalf("Parsed PREPROCESS wrong in global frame")
	}

	if ld.globalFrame.Commands[2].Type != RMB_LENGTH ||
		ld.globalFrame.Commands[2].Data[0] != 17 {
		t.Fatalf("Parsed LENGTH wrong in global frame")
	}

	if ld.globalFrame.Commands[3].Type != RMB_DISTANCE ||
		ld.globalFrame.Commands[3].Data[0] != 1000 {
		t.Fatalf("Parsed DISTANCE wrong in global frame")
	}

	// Ok, the checks below could be more precise (also check the contents
	// of the list and not just length) but it checks enough already

	var frm *RMBFrame
	// First make sure there is not a E1M1 frame... ah, wait, I made
	// LookupRMBFrameForMap return global frame in this case. Stupid me.
	frm = ld.LookupRMBFrameForMap(RMBFrameId{
		Type: RMB_FRAME_EXMY,
		Name: "E1M1",
	})
	if frm != nil && frm != ld.globalFrame {
		t.Fatalf("Got frame for non-existent map E1M1")
	}

	// Check MAP01 frame
	frm = ld.LookupRMBFrameForMap(RMBFrameId{
		Type: RMB_FRAME_MAPXY,
		Name: "MAP01",
	})
	if frm == nil {
		t.Fatalf("Failed to produced frame for MAP01\n")
	}
	if frm.Parent == nil { // good luck ascertaining it is the same though
		t.Fatalf("The MAP01 frame doesn't link to global frame.\n")
	}
	if len(frm.Commands) != 4 {
		t.Fatalf("Wrong number of commands in MAP01 frame.\n")
	}

	if frm.Commands[0].Type != RMB_BLIND || !frm.Commands[0].Invert ||
		!frm.Commands[0].Band || frm.Commands[0].Data[0] != 2 ||
		frm.Commands[0].Data[1] != 3 || len(frm.Commands[0].List[0]) != 2 {
		t.Fatalf("MAP01 frame command #0 is wrong")
	}

	if frm.Commands[1].Type != RMB_SAFE || !frm.Commands[1].Invert ||
		!frm.Commands[1].Band || frm.Commands[1].Data[0] != 2 ||
		frm.Commands[1].Data[1] != 4 || len(frm.Commands[1].List[0]) != 3 {
		t.Fatalf("MAP01 frame command #1 is wrong")
	}

	if frm.Commands[2].Type != RMB_GROUP || frm.Commands[2].Data[0] != 10 ||
		len(frm.Commands[2].List[0]) != 3 {
		t.Fatalf("MAP01 frame command #2 is wrong")
	}

	if frm.Commands[3].Type != RMB_BLIND || frm.Commands[3].Data[0] != 0 ||
		len(frm.Commands[3].List[0]) != 1 || frm.Commands[3].List[0][0] != 10 {
		t.Fatalf("MAP01 frame command #3 is wrong")
	}

	// Check MAP95 frame
	frm = ld.LookupRMBFrameForMap(RMBFrameId{
		Type: RMB_FRAME_MAPXY,
		Name: "MAP95",
	})
	if frm == nil {
		t.Fatalf("Failed to produced frame for MAP95n")
	}
	if frm.Parent == nil { // good luck ascertaining it is the same though
		t.Fatalf("The MAP95 frame doesn't link to global frame.\n")
	}
	if len(frm.Commands) != 2 {
		t.Fatalf("Wrong number of commands in MAP95 frame.\n")
	}
	if frm.Commands[0].Type != RMB_BLIND || !frm.Commands[0].Band ||
		frm.Commands[0].Data[0] != 2 || frm.Commands[0].Data[1] != 4 ||
		len(frm.Commands[0].List[0]) != 3 {
		t.Fatalf("MAP95 frame command #0 is wrong")
	}

	if frm.Commands[1].Type != RMB_SAFE || frm.Commands[1].Data[0] != 1 ||
		len(frm.Commands[1].List[0]) != 1 || frm.Commands[1].List[0][0] != 74 {
		t.Fatalf("MAP95 frame command #1 is wrong")
	}

	frm = ld.LookupRMBFrameForMap(RMBFrameId{
		Type: RMB_FRAME_MAPXY,
		Name: "GOFY",
	})
	if frm == nil {
		t.Fatalf("Failed to produce frame for map GOFY")
	}
	if frm.Parent == nil { // good luck ascertaining it is the same though
		t.Fatalf("The GOFY frame doesn't link to global frame.\n")
	}
	if len(frm.Commands) != 2 {
		t.Fatalf("Wrong number of commands in GOFY frame.\n")
	}

	frm = ld.LookupRMBFrameForMap(RMBFrameId{
		Type: RMB_FRAME_EXMY,
		Name: "E1M2",
	})
	if frm == nil {
		t.Fatalf("Failed to produce frame for map E1M2")
	}
	if frm.Parent == nil { // good luck ascertaining it is the same though
		t.Fatalf("The E1M2 frame doesn't link to global frame.\n")
	}
	if len(frm.Commands) != 1 {
		t.Fatalf("Wrong number of commands in E1M2 frame.\n")
	}

	frm = ld.LookupRMBFrameForMap(RMBFrameId{
		Type: RMB_FRAME_EXMY,
		Name: "E2M3",
	})
	if frm == nil {
		t.Fatalf("Failed to produce frame for map E2M3")
	}
	if frm.Parent == nil { // good luck ascertaining it is the same though
		t.Fatalf("The E2M3 frame doesn't link to global frame.\n")
	}
	if len(frm.Commands) != 2 {
		t.Fatalf("Wrong number of commands in E2M3 frame.\n")
	}

}

// Although RMB manual doesn't say this, example *.rej shipped with RMB
// occassionally use comma-delimited lists rather than space-delimited lists,
// and RMB program recognizes them. Comma may be followed by a space, or may
// not - original RMB program recognizes both
func TestCommaListRMB(t *testing.T) {
	rmb := strings.Builder{}
	rmb.WriteString("GROUP 10 (8, 11, 12)\n")
	rmb.WriteString("INCLUDE (10,15,13) (14, 26,16)\n")
	rmb.WriteString("EXCLUDE (20) 15\n")
	rmb.WriteString("EXCLUDE (21,22) 14\n")
	rmb.WriteString("EXCLUDE 23 (82, 33)\n")
	suc, ld := LoadRMB([]byte(rmb.String()), "test_commalistrmb.rej")
	if !suc {
		t.Fatalf("Couldn't load rmb with comma-delimited lists.\n")
	}
	if ld == nil || ld.globalFrame == nil {
		t.Fatalf("Failed to acknowledge RMB's global frame!")
	}
	if len(ld.globalFrame.Commands) != 5 {
		t.Fatalf("Wrong number of commands in global frame: expected 5 got %d\n", len(ld.globalFrame.Commands))
	}
	frm := ld.globalFrame
	if frm.Commands[0].Type != RMB_GROUP ||
		len(frm.Commands[0].List[0]) != 3 ||
		frm.Commands[0].List[0][0] != 8 ||
		frm.Commands[0].List[0][1] != 11 ||
		frm.Commands[0].List[0][2] != 12 {
		t.Fatalf("frame command #0 is wrong")
	}

	if frm.Commands[1].Type != RMB_INCLUDE ||
		len(frm.Commands[1].List[0]) != 3 ||
		len(frm.Commands[1].List[1]) != 3 ||
		frm.Commands[1].List[0][0] != 10 ||
		frm.Commands[1].List[0][1] != 15 ||
		frm.Commands[1].List[0][2] != 13 ||
		frm.Commands[1].List[1][0] != 14 ||
		frm.Commands[1].List[1][1] != 26 ||
		frm.Commands[1].List[1][2] != 16 {
		t.Fatalf("frame command #1 is wrong")
	}

	if frm.Commands[2].Type != RMB_EXCLUDE ||
		len(frm.Commands[2].List[0]) != 1 ||
		len(frm.Commands[2].List[1]) != 1 ||
		frm.Commands[2].List[0][0] != 20 ||
		frm.Commands[2].List[1][0] != 15 {
		t.Fatalf("frame command #2 is wrong")
	}

	if frm.Commands[3].Type != RMB_EXCLUDE ||
		len(frm.Commands[3].List[0]) != 2 ||
		len(frm.Commands[3].List[1]) != 1 ||
		frm.Commands[3].List[0][0] != 21 ||
		frm.Commands[3].List[0][1] != 22 ||
		frm.Commands[3].List[1][0] != 14 {
		t.Fatalf("frame command #3 is wrong")
	}

	if frm.Commands[4].Type != RMB_EXCLUDE ||
		len(frm.Commands[4].List[0]) != 1 ||
		len(frm.Commands[4].List[1]) != 2 ||
		frm.Commands[4].List[0][0] != 23 ||
		frm.Commands[4].List[1][0] != 82 ||
		frm.Commands[4].List[1][1] != 33 {
		t.Fatalf("frame command #4 is wrong")
	}

}

func TestNotInvertible(t *testing.T) {
	if NOT_INVERTIBLE != "illegal argument to INVERT, expected BAND, BLIND or SAFE\n" {
		t.Fatalf("Table to INVERTible commands changed, or message is not produced correctly. Needs update\n")
	}
}
