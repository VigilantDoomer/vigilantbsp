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
	"bytes"
	"encoding/binary"
	"errors"
	"fmt"
	"io"
	"math"
	"os"
)

var LUMP_SORT_ORDER = []string{"THINGS", "LINEDEFS", "SIDEDEFS", "VERTEXES", "SEGS", "SSECTORS", "NODES", "SECTORS", "REJECT", "BLOCKMAP", "BEHAVIOR"}
var LUMP_MUSTEXIST = []string{"THINGS", "LINEDEFS", "SIDEDEFS", "VERTEXES", "SECTORS"}
var LUMP_CREATE = []string{"SEGS", "SSECTORS", "NODES", "REJECT", "BLOCKMAP"}

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
	id          int
}

type LevelValidity struct {
	scheduleEntry *ScheduledLump
	currentOrder  []int
	requisites    []int
	creatable     []int
}

type PinnedWad struct {
	le           []LumpEntry
	scheduleRoot *ScheduledLump
	readerAt     io.ReaderAt
}

type WadDir struct {
	scheduleRoot *ScheduledLump
	validities   []LevelValidity
	lvls         int
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
	// TODO rewrite with the help of standard library
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
	if len(config.FilterLevel) == 0 { // reference to global: config
		return true
	}

	for _, entry := range config.FilterLevel {
		if bytes.Equal(entry, levelName) {
			if config.FilterProhibitsLevels { // reference to global: config
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
	return config.FilterProhibitsLevels // reference to global: config
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
	scheduleEntry := cloneSchedule(scheduleRoot)
	deltaCur := 0 // so as to update schedule to lump entry referencies for lumps not inside a level
	for scheduleEntry != nil {
		if scheduleEntry.Drop {
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
			if validities[curLevel].scheduleEntry.id != scheduleEntry.id {
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

// LoadWadDirectory parses lumps list, establishes which lumps are level lumps.
// - isInputWad == true should only be used for a single input wad, on the basis of
// which output is produced. rejectsize and troll must then be provided.
// - isInputWad == false is used for wads loaded as a resource, for example, to source
// REJECT lump for RMB option NOPROCESS. Such wads usually intended to be read but
// never written, their role is secondary. Consequently, for them rejectsize and troll
// should be nil, eject == false, RMB == nil (as it is not these wads to which RMB
// applies)
func LoadWadDirectory(isInputWad bool, le []LumpEntry, rejectsize map[int]uint32,
	troll *Troll, eject bool, RMB *LoadedRMB) *WadDir {

	ScheduleRoot := new(ScheduledLump)
	action := ScheduleRoot
	action.Next = nil
	action.Level = nil
	action.DirIndex = -1
	lvls := 0
	sectorStructSize := uint32(binary.Size(new(Sector)))

	validities := make([]LevelValidity, 0)
	var validity *LevelValidity
	idgen := 0
	// Now identify levels
	canCheckLevel := true // UDMF imposes constraints -- ENDMAP must exist
	for i, entry := range le {
		// exclude zero byte and all that follows it from string for pattern
		// matching to work correctly
		bname := ByteSliceBeforeTerm(entry.Name[:])
		isFollowedByTextmap := !config.DisableUDMF && (i < len(le)-1) &&
			bytes.Equal(ByteSliceBeforeTerm(le[i+1].Name[:]), []byte("TEXTMAP"))

		newAction := new(ScheduledLump)

		moveToNew := true      // becomes false if processing lump that belongs to a level
		newAction.Drop = eject // default to removal if Eject == true
		newAction.id = idgen
		idgen++
		udmf := false // becomes true INSIDE udmf (through canCheckLevel == false detection)
		if canCheckLevel && (isFollowedByTextmap || IsALevel(bname)) {
			// Check to see if I should rebuild this level, or just copy it
			if !isInputWad || CanRebuildThisLevel(bname) {
				// Ok, rebuild
				newAction.DirIndex = i
				newAction.Level = make([]*ScheduledLump, 0, 12)
				newAction.LevelFormat = FORMAT_DOOM
				if isFollowedByTextmap {
					newAction.LevelFormat = FORMAT_UDMF
					canCheckLevel = false // must find ENDMAP first
				}
				newAction.Next = nil // for now
				// FIXME what to do with UDMF????
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
				if isFollowedByTextmap {
					canCheckLevel = false // must find ENDMAP first
				}
			}
		} else {
			udmf = !canCheckLevel
			if udmf && bytes.Equal(bname, []byte("ENDMAP")) {
				// ENDMAP found, can look for next level from the next lump onwards
				canCheckLevel = true
			}
			if udmf && (isFollowedByTextmap || IsALevel(bname)) {
				Log.Error("Possible error in a wad: current lump seems to be a marker for a map \"%s\" yet ENDMAP of the previous UDMF level \"%s\" has not been encountered yet\n",
					string(bname), GetLumpName(le, action))
			}
			newAction.DirIndex = i
			newAction.Level = nil
			newAction.Next = nil
			if action.Level != nil {
				// we are inside a level, check if it is a lump that is supposed
				// to exist in it
				// this path is not followed if a level is not being rebuilt but
				// copied instead
				isHexenSpec := bytes.Equal([]byte("BEHAVIOR"), bname)
				isLevelSpec := udmf || isHexenSpec ||
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
		// zero-filled reject is never built for udmf, and we don't rebuild wads we
		// pinned as external (these wads are loaded for the sake of reject sourcing
		// for NOPROCESS, not for rebuilding them)
		if isInputWad && !udmf && !moveToNew && bytes.Equal(bname, []byte("SECTORS")) {
			numSectors := entry.Size / sectorStructSize
			fracBytes := float64(numSectors) * float64(numSectors) / 8.0
			numRejectSize := uint32(math.Ceil(fracBytes))
			troll.AddSize(numRejectSize)
			rejectsize[action.DirIndex] = numRejectSize
		}
	}

	if !canCheckLevel {
		lvlName := GetLumpName(le, action)
		Log.Error("The last UDMF level \"%s\" parsed was absent ENDMAP marker (reached end of wad directory without finding ENDMAP).\n",
			lvlName)
		Log.Error("It will be ignored (not considered a level)\n")
		// unroll it
		prev := action
		for _, subaction := range action.Level {
			prev.Next = subaction
			prev = subaction
		}
		action.Level = nil
		validities = validities[:len(validities)-1]
	}

	// first meaningful lump
	ScheduleRoot = ScheduleRoot.Next

	return &WadDir{
		scheduleRoot: ScheduleRoot,
		validities:   validities,
		lvls:         lvls,
	}
}

// LookupLevel returns the first level matching a specified marker
func (pw *PinnedWad) LookupLevel(lumpName string) *ScheduledLump {
	if pw.scheduleRoot == nil {
		return nil
	}
	schedule := pw.scheduleRoot
	for {
		if schedule.Level != nil && GetLumpName(pw.le, schedule) == lumpName {
			return schedule
		}

		schedule = schedule.Next
		if schedule == nil {
			return nil
		}
	}
	// return nil // go vet says this is unreachable code
}

func (pw *PinnedWad) LookupLumpInLevel(levelOwnLump *ScheduledLump,
	lumpName string) *ScheduledLump {
	if levelOwnLump.Level == nil {
		return nil
	}
	for i, _ := range levelOwnLump.Level {
		if GetLumpName(pw.le, levelOwnLump.Level[i]) == lumpName {
			return levelOwnLump.Level[i]
		}
	}
	return nil
}

func GetLumpName(le []LumpEntry, lump *ScheduledLump) string {
	return string(ByteSliceBeforeTerm(le[lump.DirIndex].Name[:]))
}

func cloneSchedule(scheduleRoot *ScheduledLump) *ScheduledLump {
	sch := scheduleRoot
	newSch := &ScheduledLump{}
	rootSch := newSch
	for sch != nil {
		*newSch = *sch
		if sch.Next != nil {
			newSch.Next = &ScheduledLump{}
			newSch = newSch.Next
		}
		sch = sch.Next
	}
	return rootSch
}

func TryReadWadDirectory(isInputFile bool, f *os.File,
	wh *WadHeader) ([]LumpEntry, error) {
	*wh = WadHeader{}
	err := binary.Read(f, binary.LittleEndian, wh)
	if err != nil {
		return nil, errors.New(fmt.Sprintf("Couldn't read file header %s\n", err.Error()))
	}
	if wh.MagicSig == IWAD_MAGIC_SIG {
		if isInputFile {
			Log.Printf("The file is an IWAD")
		}
	} else if wh.MagicSig == PWAD_MAGIC_SIG {
		if isInputFile {
			Log.Printf("The file is a PWAD")
		}
	} else {
		return nil, errors.New("The file is NOT a wad")
	}
	if isInputFile {
		Log.Verbose(1, "The directory contains %d lumps and starts at %d byte offset",
			wh.LumpCount, wh.DirectoryStart)
	}

	if wh.LumpCount == 0 {
		return nil, errors.New("Unable to find any valid levels")
	}

	_, err = f.Seek(int64(wh.DirectoryStart), 0)
	if err != nil {
		return nil, errors.New(fmt.Sprintf("Couldn't move to wad's directory structure (%d offset): %s", wh.DirectoryStart, err.Error()))
	}

	// Read in whole directory at once
	le := make([]LumpEntry, wh.LumpCount, wh.LumpCount)
	err = binary.Read(f, binary.LittleEndian, le)
	if err != nil {
		return nil, errors.New(fmt.Sprintf("Failed to read lump info from a wad's directory: %s", err.Error()))
	}
	return le, nil
}
