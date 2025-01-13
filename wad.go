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
	"io"
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
}

type LevelValidity struct {
	scheduleEntry *ScheduledLump
	currentOrder  []int
	requisites    []int
	creatable     []int
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
	scheduleEntry := scheduleRoot
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
