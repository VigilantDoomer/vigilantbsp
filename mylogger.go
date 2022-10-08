// Copyright (C) 2022, VigilantDoomer
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

// Central log (stdout/stderr) of the program
package main

import (
	"bytes"
	"fmt"
	"log"
	"os"
	"sync"
)

type MyLogger struct {
	// Writing to the same slot allows to clobber stuff so that we don't see the
	// same thing written over and over again
	slots []string
	segs  bytes.Buffer
	// Mutex is used to order writes to stdin and stderr, as well as Sync call
	mu sync.Mutex
}

func CreateLogger() *MyLogger {
	var b bytes.Buffer
	log := new(MyLogger)
	log.segs = b
	return log
}

var Log = CreateLogger()

var syslog = log.New(os.Stdout, "", 0)
var errlog = log.New(os.Stderr, "", 0)

// Your generic printf to let user see things
func (log *MyLogger) Printf(s string, a ...interface{}) {
	log.mu.Lock()
	defer log.mu.Unlock()
	syslog.Printf(s, a...)
	return
}

// As generic as printf, but writes to stderr instead of stdout
// Does NOT interrupt execution of the program
func (log *MyLogger) Error(s string, a ...interface{}) {
	log.mu.Lock()
	defer log.mu.Unlock()
	errlog.Printf(s, a...)
	return
}

// For advanced users or users that are curious, or programmers, there is
// stuff they might want to see but only when they can really bother to spend
// time reading it
func (log *MyLogger) Verbose(verbosityLevel int, s string, a ...interface{}) {
	if verbosityLevel <= config.VerbosityLevel {
		log.mu.Lock()
		defer log.mu.Unlock()
		syslog.Printf(s, a...)
		return
	}
}

// Panicking is not a good thing, but at least we can now use formatted printing
// for it
func (log *MyLogger) Panic(s string, a ...interface{}) {
	log.mu.Lock()
	defer log.mu.Unlock()
	panic(fmt.Sprintf(s, a...))
}

// Writes to the slot, clobbering whatever was there before us in that same slot
// Used when need to debug something in nodes builder but it's worthless to
// repeat if it concerns the same thing
func (log *MyLogger) Push(slotNumber int, s string, a ...interface{}) {
	log.mu.Lock()
	defer log.mu.Unlock()
	for slotNumber >= len(log.slots) {
		log.slots = append(log.slots, "")
	}
	log.slots[slotNumber] = fmt.Sprintf(s, a...)
}

// Now that slots have been written over multiple times, time to see what was
// written to begin with. If you don't call it, you might as well never write
// anything to slots (it's usually not the stuff to go into release, mind it)
func (log *MyLogger) Flush() {
	log.mu.Lock()
	defer log.mu.Unlock()
	for _, slot := range log.slots {
		syslog.Printf(slot)
	}
	log.slots = nil
}

func (log *MyLogger) DumpSegs(ts *NodeSeg) {
	if !config.DumpSegsFlag || ts == nil { // reference to global: config
		return
	}
	// Assume all come from same sector
	allSector := ts.sector
	log.segs.WriteString(fmt.Sprintf("Sector #%d:\n", allSector))
	for tmps := ts; tmps != nil; tmps = tmps.next {
		log.segs.WriteString(fmt.Sprintf(
			"  Linedef: %d Flip: %d (%d,%d) - (%d, %d)",
			tmps.Linedef, tmps.Flip, tmps.StartVertex.X, tmps.StartVertex.Y,
			tmps.EndVertex.X, tmps.EndVertex.Y))
		if tmps.sector != allSector {
			// Is not supposed to write stuff from multiple sectors. You'll have
			// to rewrite code in this function to adjust it to your use case
			log.segs.WriteString(fmt.Sprintf(" BAD! Sector = %d\n", tmps.sector))
		} else {
			log.segs.WriteString("\n")
		}
	}
}

func (log *MyLogger) GetDumpedSegs() string {
	return log.segs.String()
}

// Sync is used to wait until all messages are written to the output
func (log *MyLogger) Sync() {
	log.mu.Lock()
	log.mu.Unlock()
}
