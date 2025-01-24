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

// -- This file is where the program entry is.
// VigilantBSP uses algorithms and ideas from various other free software
// nodebuilder programs, in addition to original research on topics such as
// multithreading, autodetection of self-referencing sectors for visibility or
// to avoid deleting them when deleting "invisible" 2-sided lines, enhancements
// to visplane reduction heuristics, etc.
// TODO document better which part was borrowed from whom. Rough breakdown:
// 1. Blockmap: Marisa Heit's code in ZDBSP + insight from Zokumbsp;
// multi-threading is mine, subset compression is also reimplemented by me
// according to Doomwiki description of the technique, byte stealing is mine
// invention, longest list placed last position idea is from Doomwiki article on
// Block Rocking Bytes
// 2. Nodes: port of BSP v5.2, heavily modified, ideas borrowed from Zennode,
// Zokumbsp and AJ-BSP, ZDBSP as well (for extended nodes); I also implemented some
// ideas proposed by Lee Killough that he considered hard at the time
// 3. Reject: port of Zennode's reject code, bugfixes from Zokumbsp, further
// optimization and self-referencing sector support is mine etc.
package main

import (
	"encoding/binary"
	"os"
	"path/filepath"
	"runtime/pprof"
	"time"
)

type LevelBounds struct {
	Xmin int16
	Ymin int16
	Xmax int16
	Ymax int16
}

func main() {
	timeStart := time.Now()

	var RMB *LoadedRMB

	// before config can be legitimately accessed, must call Configure()
	Configure()
	//Log.Verbose(2, "Parsing command line takes %s\n", time.Since(timeStart))

	var err error
	// Initialize wad file name from arguments
	if config.Profile {
		f, err := os.Create(config.ProfilePath)
		if err != nil {
			Log.Printf("Could not create CPU profile: %s", err.Error())
		} else {
			defer f.Close()
			if err := pprof.StartCPUProfile(f); err != nil {
				Log.Printf("Could not start CPU profile: %s", err.Error())
			} else {
				defer pprof.StopCPUProfile()
			}
		}
	}

	// Do we have a file?
	if config.InputFileName == "" {
		Log.Error("You must specify an input file.\n")
		os.Exit(1)
	}
	config.InputFileName, _ = filepath.Abs(config.InputFileName)
	if config.OutputFileName != "" {
		config.OutputFileName, _ = filepath.Abs(config.OutputFileName)
		// Now that we can perform --eject , this check is much warranted as
		// user might have incentive to try to overcome the limitation (--eject
		// makes output file required).
		// Note that output file colliding with input file would produce bugs anyway.
		// And apparently Linux the kernel indeed allowed to produce corrupt file
		// in this case, and opening input file with read-only access and
		// output file with exclusive read-write access doesn't prevent this.
		// So this check is really not optional
		f1, err1 := os.Stat(config.InputFileName)
		f2, err2 := os.Stat(config.OutputFileName)
		if err1 == nil && err2 == nil {
			if os.SameFile(f1, f2) {
				Log.Error("You cannot specify output file that maps to the same input file (whether via same path and name, or hardlinks, or symlinks)\n")
				os.Exit(1)
			}
		}
	}

	mainFileControl := FileControl{}
	defer mainFileControl.Shutdown()

	// If RMB needs to be read, it is done first. This allows us to abort
	// early if RMB options file exists but is malformed, or contains incorrect
	// syntax.
	if config.UseRMB {
		RMB = LoadAssociatedRMB(config.InputFileName, &mainFileControl)
		if RMB != nil {
			Log.Printf("Successfully loaded RMB file %s\n",
				mainFileControl.rmbFileName)
		}
	}

	// Try open input wad
	f, err := mainFileControl.OpenInputFile(config.InputFileName)
	if err != nil {
		Log.Error("An error has occured while trying to read %s: %s\n",
			config.InputFileName, err)
		os.Exit(1)
	}

	wh := new(WadHeader)
	le, err2 := TryReadWadDirectory(true, f, wh)
	if err2 != nil {
		Log.Error(err2.Error() + "\n")
		os.Exit(1)
	}

	// Now that we are definitely having lumps, let's organize a schedule of
	// future "copy lump"/"process level" operations
	troll := CreateTroll()
	rejectsize := make(map[int]uint32)
	wadDir := LoadWadDirectory(true, le, rejectsize, troll, config.Eject, RMB)
	ScheduleRoot, validities, lvls := wadDir.scheduleRoot, wadDir.validities,
		wadDir.lvls
	mainFileControl.inputWad = &PinnedWad{
		le:           le,
		scheduleRoot: ScheduleRoot,
		readerAt:     f,
	}

	if ScheduleRoot == nil {
		Log.Error("No lumps - terminating.\n")
		os.Exit(1)
	}

	// Now resize directory, add (or even delete) entries where needed, and place
	// them in correct order. Levels missing mandatory lumps will be removed
	// from processing and made to be just copied instead
	le = UpdateDirectoryAndSchedule(le, ScheduleRoot, validities)
	wh.LumpCount = uint32(len(le))

	lvls = FindValidLevels(ScheduleRoot)

	if lvls == 0 {
		Log.Error("Unable to find any levels I can rebuild - terminating.\n")
		os.Exit(1)
	}
	Log.Printf("Number of levels that will be rebuilt: %d\n", lvls)

	// All of our reject lumps will reuse a single pool of zeroes (lump
	// overlapping is allowed by wad format)
	rejectStart := uint32(binary.Size(wh))
	var zerosToInsert uint32
	if config.Reject == REJECT_ZEROFILLED {
		zerosToInsert = troll.Compile()
	} else {
		zerosToInsert = 0
	}
	// The reject zeroes will be placed after the header, and then directory will
	// follow.
	wh.DirectoryStart = uint32(binary.Size(wh)) + zerosToInsert
	// how many zero bytes we can obtain at the end of the header (taking
	// advantage of the fact that last field of wad's header - LumpCount - is
	// stored in LittleEndian)
	var reusableFromHeader uint32
	if zerosToInsert > 0 { // only seek savings if anything was meant to be used to begin with
		reusableFromHeader = RightTrailZeros(wh.DirectoryStart)
	} else {
		reusableFromHeader = 0
	}
	if zerosToInsert > reusableFromHeader {
		zerosToInsert = uint32(zerosToInsert - reusableFromHeader)
	} else {
		if reusableFromHeader > zerosToInsert {
			reusableFromHeader = zerosToInsert
		}
		zerosToInsert = 0
	}
	// expected to never evaluate < 0, as sizeof(header) > sizeof(header.lastfield)
	rejectStart = rejectStart - reusableFromHeader
	// After decrement, the number of right trailing zeros in little endian
	// may only increase, but we won't check it any further yet
	wh.DirectoryStart = wh.DirectoryStart - reusableFromHeader

	ZeroOffsetFirstLump := false

	if (zerosToInsert > 0) && (le[0].Size == 0) {
		// And if first entry in directory is a lump of zero size, 8 ZERO bytes may
		// become available! Remember to set offset to 0 for that lump, though
		// 2025-01-23 this means that if we could create lump at that place,
		// we could have had an error, as new lumps are created with zero size as
		// well, but they don't stay that way. Fortunately, this never happens for
		// as long as we only make lumps inside level, where they are always preceded
		// by an existing level marker
		// TODO insert a safeguard nonetheless for sanity's sake -- test if entry was
		// pre-existing
		decr := uint32(8)
		ZeroOffsetFirstLump = true
		if zerosToInsert > 8 {
			zerosToInsert = uint32(zerosToInsert - 8)
		} else {
			decr = 8 - zerosToInsert // how much to move directory back
			zerosToInsert = 0
		}
		wh.DirectoryStart = wh.DirectoryStart - decr
	}

	if config.Reject == REJECT_ZEROFILLED {
		Log.Verbose(1, "Will need to insert %d zero bytes between header and directory. Reject lumps will begin at offset %d.\n",
			zerosToInsert, rejectStart)
	}
	Log.Verbose(1, "Directory starts at offset %d.\n", wh.DirectoryStart)

	outFileName := config.OutputFileName
	if outFileName == "" {
		Log.Printf("Preparing to write %s - will create a temp file first.\n", config.InputFileName)
	} else {
		Log.Printf("Preparing to write %s...\n", outFileName)
	}
	fout, outFileName, ferr := mainFileControl.OpenOutputFile(config.OutputFileName)
	if ferr != nil {
		Log.Error("An error has occured while trying to create/modify %s: %s\n", outFileName, ferr)
		os.Exit(1)
	}

	// in the future, will probably store new wad's header separately
	err = binary.Write(fout, binary.LittleEndian, wh)
	if err != nil {
		Log.Error("An error has occured while trying to create/modify %s: %s\n", outFileName, err)
		os.Exit(1)
	}

	if zerosToInsert > 0 {
		WriteNZerosOrFail(fout, zerosToInsert, outFileName)
	}
	// skip directory also
	WriteNZerosOrFail(fout, uint32(binary.Size(le)), outFileName)
	action := ScheduleRoot
	curPos := uint32(binary.Size(wh)) + zerosToInsert + uint32(binary.Size(le))
	wriBus := StartWriteBus(fout, le, curPos)
	lvl := new(Level)          // reusable
	udmfLvl := new(UDMF_Level) // likewise
	for action != nil {
		if action.Drop {
			action = action.Next
			continue
		}
		// This part copies non-level lump (contents and directory entry) as well
		// as a marker lump
		// ! lumps that are part of a level are stored with the marker lump,
		// they are not getting copied in this part
		idx := action.DirIndex
		if le[idx].Size != 0 {
			tmpBuf := make([]byte, le[idx].Size, le[idx].Size)
			f.ReadAt(tmpBuf, int64(le[idx].FilePos))
			wriBus.SendRawLump(tmpBuf, idx, "", "")
		} else if (idx == 0) && ZeroOffsetFirstLump {
			// first lump has zero size, if we set zero offset we can steal some bytes
			// for our zero byte pool for empty zero-filled reject lumps. Do it.
			le[idx].FilePos = 0
		}

		// Now see if we are on a level marker lump and got stuff to process
		if action.Level != nil {
			// action.Level is an array of lumps belonging to the level. This
			// is where all stuff goes
			if action.LevelFormat == FORMAT_UDMF {
				udmfLvl.DoLevel(le, idx, action, f, wriBus, &mainFileControl)
			} else {
				lvl.DoLevel(le, idx, rejectsize, troll, action, rejectStart,
					f, wriBus, &mainFileControl)
			}
			wriBus.Sync() // make sure all that is to be logged is there before new level is processed
		}
		action = action.Next
	}
	wriBus.Shutdown()
	fout.Seek(int64(wh.DirectoryStart), 0)
	binary.Write(fout, binary.LittleEndian, le)
	suc := mainFileControl.Success()
	if suc {
		trFileName := outFileName
		if mainFileControl.UsingTmp() {
			// we were using tmp file, which means our real output file is same
			// as input one. So make sure user sees that we written (overwritten)
			// the desired file instead of some temp file
			trFileName = config.InputFileName
		}
		Log.Printf("%s successfully written \n ", trFileName)
	} else {
		Log.Printf("I/O error on flushing data / closing files. The data might not have been saved!\n")
	}
	if config.DumpSegsFlag {
		DebugSaveDumpedSegs(config.SegDumpFile)
	}
	if config.MemProfile {
		DumpMemoryProfile(config.MemProfilePath)
	}
	Log.Printf("Total time: %s\n", time.Since(timeStart))
}

func GetBounds(vertices []Vertex) LevelBounds {
	Xmin := int16(32767)
	Ymin := int16(32767)
	Xmax := int16(-32768)
	Ymax := int16(-32768)
	for _, v := range vertices {
		if v.XPos < Xmin {
			Xmin = v.XPos
		}
		if v.YPos < Ymin {
			Ymin = v.YPos
		}
		if v.XPos > Xmax {
			Xmax = v.XPos
		}
		if v.YPos > Ymax {
			Ymax = v.YPos
		}
	}
	return LevelBounds{
		Xmin: Xmin,
		Ymin: Ymin,
		Xmax: Xmax,
		Ymax: Ymax,
	}
}
