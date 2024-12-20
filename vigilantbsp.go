// Copyright (C) 2022-2024, VigilantDoomer
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
// Zokumbsp and AJ-BSP, possibly some ZDBSP as well; I also implemented some
// ideas from Lee Killough which he considered hard at the time
// 3. Reject: port of Zennode's reject code, bugfixes from Zokumbsp, further
// optimization and self-referencing sector support is mine etc.
package main

import (
	"bytes"
	"encoding/binary"
	"errors"
	"fmt"
	"io"
	"math"
	"os"
	"path/filepath"
	"runtime/pprof"
	"time"
)

var LUMP_SORT_ORDER = []string{"THINGS", "LINEDEFS", "SIDEDEFS", "VERTEXES", "SEGS", "SSECTORS", "NODES", "SECTORS", "REJECT", "BLOCKMAP", "BEHAVIOR"}
var LUMP_MUSTEXIST = []string{"THINGS", "LINEDEFS", "SIDEDEFS", "VERTEXES", "SECTORS"}
var LUMP_CREATE = []string{"SEGS", "SSECTORS", "NODES", "REJECT", "BLOCKMAP"}

// Controls lifetime of both input and output wads - ensures they are properly
// closed by the end of program, regardless of success and failure, and that
// a temporary file, if such was created because output file was not specified
// and we were to replace the input, either replaces that input file (on success)
// or is deleted (on failure)
type FileControl struct {
	success        bool
	tmp            bool
	fin            *os.File
	fout           *os.File
	frmb           *os.File
	freport        *os.File
	inputFileName  string
	outputFileName string
	rmbFileName    string
	reportFileName string
}

func (fc *FileControl) UsingTmp() bool {
	return fc.tmp
}

func (fc *FileControl) OpenInputFile(inputFileName string) (*os.File, error) {
	fc.inputFileName = inputFileName
	var err error
	fc.fin, err = os.Open(inputFileName)
	//fc.fin, err = os.OpenFile(inputFileName, os.O_RDONLY, 0) // wouldn't protect from opening same file as output with read-write access!
	return fc.fin, err
}

func (fc *FileControl) OpenOutputFile(outputFileName string) (*os.File, string, error) {
	fc.tmp = outputFileName == ""
	var err error
	if fc.tmp {
		// Need a temporary file
		fc.fout, err = os.CreateTemp(filepath.Dir(fc.inputFileName), "tmp")
		if err == nil {
			outputFileName = fc.fout.Name()
		}
	} else {
		fc.fout, err = os.OpenFile(outputFileName, os.O_CREATE|os.O_RDWR|os.O_TRUNC,
			os.ModeExclusive|os.ModePerm)
	}
	fc.outputFileName = outputFileName
	return fc.fout, outputFileName, err
}

func (fc *FileControl) OpenRMBOptionsFile(rmbFileName string) (*os.File, error) {
	fc.rmbFileName = rmbFileName
	var err error
	fc.frmb, err = os.Open(rmbFileName)
	if err != nil { // just in case it is not set so
		fc.frmb = nil
	}
	return fc.frmb, err
}

func (fc *FileControl) CloseRMBOptionsFile() {
	if fc.frmb == nil {
		return
	}
	err := fc.frmb.Close()
	if err != nil {
		Log.Error("Couldn't close RMB file '%s': %s\n", fc.rmbFileName, err.Error())
	}
}

func (fc *FileControl) OpenReportFile() (*os.File, error) {
	var err error
	fc.freport, err = os.OpenFile(fc.reportFileName, os.O_CREATE|os.O_RDWR|os.O_TRUNC,
		os.ModeExclusive|os.ModePerm)
	if err != nil {
		fc.freport = nil
		return nil, err
	}
	return fc.freport, nil
}

func (fc *FileControl) CloseReportFile(suc bool) bool {
	if fc.freport == nil {
		return true
	}
	if suc {
		fc.freport.Write([]byte("# Report written successfully - no entries lost\n"))
	} else {
		fc.freport.Write([]byte("# Program aborted, report might be missing entries\n"))
	}
	err := fc.freport.Close()
	if err != nil {
		Log.Error("Couldn't close report file '%s': %s\n", fc.reportFileName, err.Error())
	} else {
		if suc {
			Log.Printf("Written report file %s\n", fc.reportFileName)
		} else {
			Log.Printf("Written incomplete report file %s\n", fc.reportFileName)
		}
	}
	fc.freport = nil
	return err == nil
}

func (fc *FileControl) Success() bool {
	if fc.fin == nil || fc.fout == nil {
		Log.Panic("Sanity check failed: descriptor invalid.\n")
	}
	errFin := fc.fin.Close()
	errFout := fc.fout.Close()
	sucReport := fc.CloseReportFile(true)
	hasError := errFin != nil || errFout != nil || !sucReport
	if hasError {
		if errFin != nil {
			Log.Error("Closing input file (after wad was almost ready) returned error: %s.\n",
				errFin.Error())
		}
		if errFout != nil {
			Log.Error("Closing output file (after wad was almost ready) returned error: %s.\n",
				errFout.Error())
		}
		return false
	}
	success2 := true
	if fc.tmp {
		success2 = fc.tempFileReplacesInput()
	}
	fc.success = true // nothing to clean up on program exit anyway (original file descriptors closed)
	return success2   // but the criteria to report success to user is different - no errors should have happened
}

func (fc *FileControl) tempFileReplacesInput() bool {
	success := true
	// now former output - temp file - is where we read from,
	// where as former input is the destination file we will overwrite
	fin, errFin := os.Open(fc.outputFileName)
	if errFin != nil {
		Log.Error("Couldn't reopen the temporarily file to read from it: %s.\n",
			errFin.Error())
		return false
	}
	fout, errFout := os.OpenFile(fc.inputFileName, os.O_CREATE|os.O_RDWR|os.O_TRUNC,
		os.ModeExclusive|os.ModePerm)
	if errFout != nil {
		success = false
		Log.Error("Couldn't reopen the input file to overwrite it: %s.\n",
			errFout.Error())
	} else {
		_, err := io.Copy(fout, fin)
		if err != nil {
			success = false
			// FUCK Hope the user has backup for this
			// TODO may be make backup oneself, I dunno
			Log.Error("Error when overwriting the original file: %s.\n",
				err.Error())
		}
		fout.Close()
	}
	fin.Close()
	// Now delete the temporary file
	err := os.Remove(fc.outputFileName)
	if err != nil {
		success = false
		Log.Error("Couldn't delete temporary file after overwriting the original one: %s.\n",
			err.Error())
	}
	return success
}

// Ensures we close all files when program exits. Temporary file is getting
// deleted at this moment
func (fc *FileControl) Shutdown() {
	if fc.success {
		return
	}

	var errFrmb error
	if fc.frmb != nil {
		errFrmb = fc.frmb.Close()
	}

	var errFin error
	if fc.fin != nil {
		errFin = fc.fin.Close()
	}

	var errFout error
	if fc.fout != nil {
		errFout = fc.fout.Close()
	}

	if errFrmb != nil {
		Log.Error("Couldn't close RMB file '%s': %s\n", fc.rmbFileName, errFrmb.Error())
	}

	if errFin != nil {
		Log.Error("Couldn't close input file '%s': %s\n", fc.inputFileName, errFin.Error())
	}

	if errFout != nil {
		Log.Error("Couldn't close output file '%s': %s\n", fc.outputFileName, errFout.Error())
	}

	fc.CloseReportFile(false)

	if fc.tmp { // Aborting unsuccessful operation when a temp file has been created
		if errFout != nil {
			Log.Error("Couldn't delete temporary file '%s' because failed to close it already.\n",
				fc.outputFileName)
			return
		}
		err := os.Remove(fc.outputFileName)
		if err != nil {
			Log.Error("Got error when trying to delete a temporary file '%s': %s\n", fc.outputFileName, err.Error())
		}
	}
}

// Idea lifted from BSP nodebuilder - how to map original wad's lumps to
// something that can be iterated with the provision to either copy lump
// unchanged or process level operations
type ScheduledLump struct {
	Next        *ScheduledLump
	DirIndex    int
	LevelFormat int
	Level       []*ScheduledLump
	RMBOptions  *RMBFrame
	Drop        bool
}

type LevelValidity struct {
	scheduleEntry *ScheduledLump
	currentOrder  []int
	requisites    []int
	creatable     []int
}

type LevelBounds struct {
	Xmin int16
	Ymin int16
	Xmax int16
	Ymax int16
}

// ByteSliceBeforeTerm returns a part of the original bytes
// excluding everything that starts with zero-byte character.
// This allows string operations (such as pattern matching) to be performed
// correctly on returned value
func ByteSliceBeforeTerm(b []byte) []byte {
	i := bytes.IndexByte(b, 0)
	if i == -1 {
		return b
	} else {
		return b[:i]
	}
}

func RightTrailZeros(i uint32) uint32 {
	iView := make([]byte, 4)
	binary.LittleEndian.PutUint32(iView, i)
	if len(iView) != 4 {
		Log.Error("what %d\n", len(iView))
		panic("Sanity check failed: length of int32 in bytes is not 4")
	}
	ii := uint32(4)
	for (ii > 0) && (iView[ii-1] == 0) {
		ii--
	}
	return 4 - ii
}

func FindValidLevels(root *ScheduledLump) int {
	a := root
	res := 0
	for a != nil {
		if (a.Level != nil) && (len(a.Level) > 0) {
			res++
		}
		a = a.Next
	}
	return res
}

func WriteNZerosOrFail(a io.Writer, n uint32, fname string) {
	zb := make([]byte, n, n)
	written, err := a.Write(zb)
	if err != nil {
		Log.Error("An error has occured while trying to write %s: %s\n", fname, err)
		os.Exit(1)
	} else if uint32(written) != n {
		Log.Error("The number of written bytes %d doesn't match the number of bytes that were to be written: %d\n", written, n)
		os.Exit(1)
	}
}

func SetValiditySocket(lumpName string, reference []string, validitySub *[]int,
	val int, additive bool) {
	target := -1
	for i, refLumpName := range reference {
		if refLumpName == lumpName {
			target = i
			break
		}
	}
	if target == -1 {
		return
	}
	if additive {
		(*validitySub)[target] += val
	} else {
		(*validitySub)[target] = val
	}
}

// Returns whether a level should be rebuilt based on current configuration
// If user supplied arguments specifying precise levels that should be (not)
// rebuilt, they must have been stored in configuration in upper case, or this
// will fail to work as intended
func CanRebuildThisLevel(levelName []byte) bool {
	// Go treats nil (null) array as having zero size
	if len(config.FilterLevel) == 0 {
		return true
	}

	for _, entry := range config.FilterLevel {
		if bytes.Equal(entry, levelName) {
			if config.FilterProhibitsLevels {
				// filter excludes specific levels
				return false
			} else {
				// filter includes specific levels
				return true
			}
		}
	}

	// if filter was inclusive, return false, if it was excluding levels from
	// being rebuilt, return true
	return config.FilterProhibitsLevels
}

// So, we have an input wad directory, and must return an output wad directory
// Non-level lumps are copied intact, but level lumps need to:
// 1. Be created when missing and creation is possible
// 2. Be deleted if duplicates are existing
// 3. Be sorted in predetermined order
// If a level lump whose existence in input file is MANDATORY is absent, the
// level cannot be processed and must be converted to a collection of regular
// lumps in scheduleRoot, so that it is copied without changes and is ignored
// NOTE is implied the order of lumps as encountered in scheduleRoot and le
// is identical
func UpdateDirectoryAndSchedule(le []LumpEntry, scheduleRoot *ScheduledLump,
	validities []LevelValidity) []LumpEntry {
	// Will be updated
	nSize := len(le)
	curOut := 0 // cursor
	curLevel := 0
	leOut := make([]LumpEntry, nSize)
	scheduleEntry := scheduleRoot
	deltaCur := 0 // so as to update schedule to lump entry referencies for lumps not inside a level
	for scheduleEntry != nil {
		if scheduleEntry.Drop {
			// TODO refactor level infrastructure and/or add test coverage
			// Had enough trouble adding support for --eject, which required changes
			// not only here but elsewhere. Very non-intuitive and hard to read
			// Might consider this issue as blocking release?
			nSize--
			deltaCur--
			scheduleEntry = scheduleEntry.Next
			continue
		}
		oldMainEntryIdx := scheduleEntry.DirIndex // index valid in le
		scheduleEntry.DirIndex += deltaCur        // index valid in leOut
		nextScheduleEntry := scheduleEntry.Next
		if scheduleEntry.Level != nil {
			// The marker needs to be copied
			leOut[curOut] = le[oldMainEntryIdx]
			curOut++
			if validities[curLevel].scheduleEntry != scheduleEntry {
				Log.Error("Sanity check failure when computing the new directory.\n")
				os.Exit(1)
			}
			// Now let's conform lump order and existence to the specification
			valid := true
			lvlName := string(ByteSliceBeforeTerm(le[oldMainEntryIdx].Name[:]))
			pendingDel := 0
			for key, value := range validities[curLevel].requisites {
				if value == 0 {
					Log.Error("Level %s is not valid: missing lump %s\n", lvlName,
						LUMP_MUSTEXIST[key])
					valid = false
				} else if value > 1 {
					Log.Error("Level %s has one or more duplicate of lump %s - only one instance of this lump will remain\n",
						lvlName, LUMP_MUSTEXIST[key])
					// Some entries are not copied (ignored) to the output
					pendingDel += value - 1
				}
			}
			oldLevelLen := len(scheduleEntry.Level)
			if valid {
				nSize -= pendingDel
				deltaCur -= pendingDel
				// Level is valid. Create missing creatable lump entries,
				// but delete duplicates
				for key, value := range validities[curLevel].creatable {
					if value == 0 {
						// More space needed
						leOut = append(leOut, LumpEntry{})
						nSize++
						deltaCur++
						// For now, we insert lump at the end
						SetValiditySocket(LUMP_CREATE[key], LUMP_SORT_ORDER,
							&(validities[curLevel].currentOrder),
							-2,
							false)
						scheduleEntry.Level = append(scheduleEntry.Level,
							&ScheduledLump{
								Next:     nil,
								DirIndex: (-1) * (len(scheduleEntry.Level) - oldLevelLen + 1),
								Level:    nil,
							})
					} else if value > 1 {
						// Some entries are not copied (ignored) to the output
						nSize -= value - 1
						deltaCur -= value - 1
					}
				}

				// Now we need to add/copy entries to new directory in specific
				// order
				oldIdxsToNew := make(map[int]int)
				inss := 1
				for key, value := range validities[curLevel].currentOrder {
					if value == -2 {
						leOut[curOut] = LumpEntry{
							FilePos: 0,
							Size:    0,
						}
						copy(leOut[curOut].Name[:], []byte(LUMP_SORT_ORDER[key])[:8])
						oldIdxsToNew[-1*inss] = curOut
						curOut++
						inss++
					} else if value >= 0 {
						leOut[curOut] = le[value]
						oldIdxsToNew[value] = curOut
						curOut++
					} // else can happen - BEHAVIOR is HEXEN-specific, not needs to be present otherwise
				}
				for _, subEntry := range scheduleEntry.Level {
					subEntry.DirIndex = oldIdxsToNew[subEntry.DirIndex]
				}
			} else {
				// Level is not valid, cannot be processed, will copy verbatim.
				// To do this, a level record in schedule needs to be converted
				// to a sequence of normal (non-level) records
				for i := 0; i < len(scheduleEntry.Level); i++ {
					if i < len(scheduleEntry.Level)-1 {
						scheduleEntry.Level[i].Next = scheduleEntry.Level[i+1]
					} else {
						scheduleEntry.Level[i].Next = scheduleEntry.Next
					}
					leOut[curOut] = le[scheduleEntry.Level[i].DirIndex]
					scheduleEntry.Level[i].DirIndex += deltaCur
					curOut++
				}
				if len(scheduleEntry.Level) > 0 {
					scheduleEntry.Next = scheduleEntry.Level[0]
				}
				scheduleEntry.Level = nil
			}
			curLevel++
		} else {
			leOut[curOut] = le[oldMainEntryIdx]
			curOut++
		}
		scheduleEntry = nextScheduleEntry
	}
	leOut = leOut[:nSize]
	return leOut
}

// Loads RMB if it exists. ALLOWED to panic (and thus bring down the program)
// if:
// 1. File exists but couldn't be read, because, for example, permissions
// 2. Syntax error in RMB file encountered while parsing it
// If file doesn't exists, no panic occurs, but a message is printed to
// the output that file simply doesn't exist since this functions is only
// supposed to be called if user requested that RMB may be used whenever
// available. This may be useful indicator that there is a typo in RMB file name
// that user wanted to use alongside the file
func LoadAssociatedRMB(wadFullFileName string, fileControl *FileControl) *LoadedRMB {
	fext := filepath.Ext(config.InputFileName)
	cas := config.InputFileName
	if len(fext) > 0 {
		cas = cas[:(len(cas) - len(fext))]
	}
	rmbFullName := cas + ".rej"
	reportFullName := cas + ".rpt"

	RMBFile, err := fileControl.OpenRMBOptionsFile(rmbFullName)
	retry := false
	if err != nil {
		if errors.Is(err, os.ErrNotExist) {
			retry = true
		}
	}
	if retry {
		// Depending on OS and file system, names can be case-sensitive,
		// so we try both
		rmbFullName = cas + ".REJ"
		RMBFile, err = fileControl.OpenRMBOptionsFile(rmbFullName)
	}
	if err != nil {
		if errors.Is(err, os.ErrNotExist) {
			Log.Printf("Ignoring RMB option because there is no file in the same directory as the input wad file, which has same name as wad file but an extension of '.rej' or '.REJ'\n")
		} else {
			Log.Panic("Found RMB options file '%s' but opening it yielded an error: %s\n", rmbFullName, err.Error())
		}
		return nil
	}

	// if RMB options file exists, be ready to create reportFileName just in
	// case
	fileControl.reportFileName = reportFullName

	shortName := filepath.Base(rmbFullName)
	fileInfo, err := os.Stat(rmbFullName)
	if err != nil {
		Log.Panic("Couldn't obtain file size of '%s', aborting: %s\n", rmbFullName, err.Error())
	}
	rawSz := fileInfo.Size()
	sz := int(rawSz)
	if int64(sz) != rawSz {
		Log.Panic("RMB file is too large: %d\n", rawSz)
	}

	buf := make([]byte, sz)
	rsz, err := RMBFile.Read(buf)
	if err != nil && !errors.Is(err, io.EOF) {
		Log.Panic("Couldn't read RMB options file '%s': %s\n", rmbFullName, err.Error())
	}

	if rsz != sz {
		Log.Panic("Incomplete read of RMB options file '%s': number bytes read %d is different from byte size %d\n",
			rmbFullName, rsz, sz)
	}
	fileControl.CloseRMBOptionsFile()

	b, res := LoadRMB(buf, shortName)
	if !b {
		Log.Panic("Fatal error: RMB options file contains syntax errors.\n")
	}
	return res
}

func main() {
	timeStart := time.Now()

	var RMB *LoadedRMB

	// before config can be legitimately accessed, must call Configure()
	Configure()

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
	err = binary.Read(f, binary.LittleEndian, wh)
	if err != nil {
		Log.Printf("Couldn't read file header %s\n", err)
		os.Exit(1)
	}
	if wh.MagicSig == IWAD_MAGIC_SIG {
		Log.Printf("The input file is an IWAD\n")
	} else if wh.MagicSig == PWAD_MAGIC_SIG {
		Log.Printf("The input file is a PWAD\n")
	} else {
		Log.Error("The input file is NOT a wad.\n")
		os.Exit(1)
	}
	Log.Verbose(1, "The directory contains %d lumps and starts at %d byte offset\n",
		wh.LumpCount, wh.DirectoryStart)

	if wh.LumpCount == 0 {
		Log.Error("Unable to find any valid levels - terminating\n")
		os.Exit(1)
	}

	_, err = f.Seek(int64(wh.DirectoryStart), 0)
	if err != nil {
		Log.Error("Couldn't move to wad's directory structure (%d offset): %s\n", wh.DirectoryStart, err)
		os.Exit(1)
	}

	// Read in whole directory at once
	le := make([]LumpEntry, wh.LumpCount, wh.LumpCount)
	err = binary.Read(f, binary.LittleEndian, le)
	if err != nil {
		Log.Error("Failed to read lump info from a wad's directory: %s\n", err)
		os.Exit(1)
	}

	// Now that we are definitely having lumps, let's organize a schedule of
	// future "copy lump"/"process level" operations
	ScheduleRoot := new(ScheduledLump)
	action := ScheduleRoot
	action.Next = nil
	action.Level = nil
	action.DirIndex = -1
	lvls := 0
	PDummySector := new(Sector) // I need a variable to obtain type size
	troll := CreateTroll()
	rejectsize := make(map[int]uint32)
	validities := make([]LevelValidity, 0)
	var validity *LevelValidity
	// Now identify levels
	for i, entry := range le {
		// exclude zero byte and all that follows it from string for pattern
		// matching to work correctly
		bname := ByteSliceBeforeTerm(entry.Name[:])
		newAction := new(ScheduledLump)

		moveToNew := true             // becomes false if processing lump that belongs to a level
		newAction.Drop = config.Eject // default to removal if Eject == true
		if IsALevel(bname) {
			// Check to see if I should rebuild this level, or just copy it
			if CanRebuildThisLevel(bname) {
				// Ok, rebuild
				newAction.DirIndex = i
				newAction.Level = make([]*ScheduledLump, 0, 12)
				newAction.LevelFormat = FORMAT_DOOM
				newAction.Next = nil // for now
				validity = &LevelValidity{
					scheduleEntry: newAction,
					currentOrder:  make([]int, len(LUMP_SORT_ORDER)),
					requisites:    make([]int, len(LUMP_MUSTEXIST)),
					creatable:     make([]int, len(LUMP_CREATE)),
				}
				for j, _ := range validity.currentOrder {
					validity.currentOrder[j] = -1
				}
				for j, _ := range validity.requisites {
					validity.requisites[j] = 0
				}
				for j, _ := range validity.creatable {
					validity.creatable[j] = 0
				}
				validities = append(validities, *validity)
				validity = &validities[len(validities)-1]
				newAction.RMBOptions = RMB.LookupRMBFrameForMapMarker(bname)
				newAction.Drop = false
			} else {
				// Just copy
				Log.Verbose(1, "will not rebuild level %s\n", string(bname))
				newAction.DirIndex = i
				newAction.Level = nil
				newAction.Next = nil
			}
		} else {
			newAction.DirIndex = i
			newAction.Level = nil
			newAction.Next = nil
			if action.Level != nil {
				// we are inside a level, check if it is a lump that is supposed
				// to exist in it
				// this path is not followed if a level is not being rebuilt but
				// copied instead
				isHexenSpec := bytes.Equal([]byte("BEHAVIOR"), bname)
				isLevelSpec := isHexenSpec ||
					bytes.Equal([]byte("SEGS"), bname) ||
					bytes.Equal([]byte("SSECTORS"), bname) ||
					bytes.Equal([]byte("NODES"), bname) ||
					bytes.Equal([]byte("BLOCKMAP"), bname) ||
					bytes.Equal([]byte("SCRIPTS"), bname) || // source code for BEHAVIOR lump
					bytes.Equal([]byte("REJECT"), bname) ||
					bytes.Equal([]byte("THINGS"), bname) ||
					bytes.Equal([]byte("LINEDEFS"), bname) ||
					bytes.Equal([]byte("SIDEDEFS"), bname) ||
					bytes.Equal([]byte("VERTEXES"), bname) ||
					bytes.Equal([]byte("SECTORS"), bname)
				if isLevelSpec {
					if isHexenSpec && (action.LevelFormat == FORMAT_DOOM) {
						action.LevelFormat = FORMAT_HEXEN
					}
					action.Level = append(action.Level, newAction)
					moveToNew = false // We are collecting lumps for a level
					// Now match lumps against predefined names to determine
					// whether they are in incorrect order, missing, and whether
					// this is recoverable. This information will be processed
					// later
					sname := string(bname)
					SetValiditySocket(sname, LUMP_SORT_ORDER,
						&(validity.currentOrder), i, false)
					SetValiditySocket(sname, LUMP_MUSTEXIST,
						&(validity.requisites), 1, true)
					SetValiditySocket(sname, LUMP_CREATE,
						&(validity.creatable), 1, true)
					newAction.Drop = false
				}
			}
		}
		if moveToNew {
			action.Next = newAction
			action = newAction
		}
		if bytes.Equal(bname, []byte("SECTORS")) && !moveToNew {
			numSectors := entry.Size / uint32(binary.Size(PDummySector))
			fracBytes := float64(numSectors*numSectors) / 8.0
			numRejectSize := uint32(math.Ceil(fracBytes))
			troll.AddSize(numRejectSize)
			rejectsize[action.DirIndex] = numRejectSize
		}
	}
	if ScheduleRoot.Next != nil {
		// First meaningful record
		ScheduleRoot = ScheduleRoot.Next
	} else {
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
	action = ScheduleRoot
	curPos := uint32(binary.Size(wh)) + zerosToInsert + uint32(binary.Size(le))
	wriBus := StartWriteBus(fout, le, curPos)
	lvl := new(Level) // reusable
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
			lvl.DoLevel(le, idx, rejectsize, troll, action, rejectStart,
				f, wriBus, &mainFileControl)
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
	Log.Printf("Total time: %s\n", time.Since(timeStart))
	if config.DumpSegsFlag {
		DebugSaveDumpedSegs(config.SegDumpFile)
	}
	if config.MemProfile {
		DumpMemoryProfile(config.MemProfilePath)
	}
}

// Was investigating the math bug (turned out BSP v5.2 was wrong because of
// 32-bit integer overflow in its version of PickNode_Traditional)
func DebugSaveNodes(nodes []Node) {
	if config.NodesDebugFile == "" {
		return
	}
	fout, ferr := os.OpenFile(config.NodesDebugFile, os.O_CREATE|os.O_RDWR|os.O_TRUNC, os.ModeExclusive|os.ModePerm)
	if ferr != nil {
		Log.Printf("An error has occured while trying to create/modify %s: %s\n", config.NodesDebugFile, ferr)
		os.Exit(1)
	}
	defer fout.Close()
	j := 1
	bbox := LevelBounds{}
	for i := len(nodes) - 1; i >= 0; i-- {
		node := nodes[i]

		if node.Rbox[BB_TOP] < node.Lbox[BB_TOP] {
			bbox.Ymax = node.Lbox[BB_TOP]
		} else {
			bbox.Ymax = node.Rbox[BB_TOP]
		}

		if node.Rbox[BB_BOTTOM] < node.Lbox[BB_BOTTOM] {
			bbox.Ymin = node.Rbox[BB_BOTTOM]
		} else {
			bbox.Ymin = node.Lbox[BB_BOTTOM]
		}

		if node.Rbox[BB_LEFT] < node.Lbox[BB_LEFT] {
			bbox.Xmin = node.Rbox[BB_LEFT]
		} else {
			bbox.Xmin = node.Lbox[BB_LEFT]
		}

		if node.Rbox[BB_RIGHT] < node.Lbox[BB_RIGHT] {
			bbox.Xmax = node.Lbox[BB_RIGHT]
		} else {
			bbox.Xmax = node.Rbox[BB_RIGHT]
		}
		fout.WriteString(fmt.Sprintf(
			"Node %d (%d,%d,%d,%d) -> R(%d,%d,%d,%d) L(%d,%d,%d,%d)\n",
			j,
			bbox.Ymax, bbox.Ymin, bbox.Xmin, bbox.Xmax,
			node.Rbox[BB_TOP], node.Rbox[BB_BOTTOM], node.Rbox[BB_LEFT], node.Rbox[BB_RIGHT],
			node.Lbox[BB_TOP], node.Lbox[BB_BOTTOM], node.Lbox[BB_LEFT], node.Lbox[BB_RIGHT]))
		j++
	}
}

// This was supposed to be used to generate test data to develop a partitioning
// scheme from "split concave polygon into convex polygons" algorithms (such as
// Hertel-Mehlhorn or Keil/Snoeyink optimal partition), before I decided that it
// might be not exactly useful for SEG minimization (or minimization of anything
// for that matter) after these convex partitions need to be redone the BSP way.
func DebugSaveDumpedSegs(where string) {
	fout, ferr := os.OpenFile(where, os.O_CREATE|os.O_RDWR|os.O_TRUNC, os.ModeExclusive|os.ModePerm)
	if ferr != nil {
		Log.Printf("An error has occured while trying to create/modify %s: %s\n", where, ferr)
		os.Exit(1)
	}
	defer fout.Close()
	n, _ := fout.WriteString(Log.GetDumpedSegs())
	Log.Printf("Wrote seg dump (%d bytes) to '%s'.\n", n, where)
}

func DumpMemoryProfile(where string) {
	fout, ferr := os.OpenFile(where, os.O_CREATE|os.O_RDWR|os.O_TRUNC, os.ModeExclusive|os.ModePerm)
	if ferr != nil {
		Log.Printf("An error has occured while trying to create/modify %s: %s\n", where, ferr)
		os.Exit(1)
	}
	defer fout.Close()
	pprof.Lookup("allocs").WriteTo(fout, 0)
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

// Print with platform-specific linebreaks indicated by CRLF argument
func WriterPrintfln(w io.Writer, CRLF bool, format string, a ...interface{}) {
	if len(format) > 0 && format[len(format)-1] == '\n' {
		format = string([]byte(format)[:len(format)-1])
	}
	w.Write([]byte(appendCRLF(CRLF, fmt.Sprintf(format, a...))))
}

func appendCRLF(CRLF bool, s string) string {
	if CRLF {
		return s + "\r\n"
	} else {
		return s + "\n"
	}
}
