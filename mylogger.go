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

// Logs specific to thread, or even just one task (one task is always worked on
// by a single thread and never shared with other threads until complete, but a
// single thread may work on many tasks - each of them will have their own
// logger). Their output is not forwarded to the stdout or stdin, but is instead
// buffered until (usually just one of them) is merged into main log of MyLogger
// type. Most of such logs are instead getting discarded altogether
type MiniLogger struct {
	buf   bytes.Buffer
	slots []string
	segs  bytes.Buffer
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
			"  Linedef: %d Flip: %d (%v,%v) - (%v, %v)",
			tmps.Linedef, tmps.getFlip(), tmps.StartVertex.X, tmps.StartVertex.Y,
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

func (log *MyLogger) Merge(mlog *MiniLogger, preface string) {
	if mlog == nil {
		return
	}
	log.mu.Lock()
	defer log.mu.Unlock()
	if len(preface) > 0 {
		syslog.Printf(preface)
	}
	content := mlog.buf.String()
	if len(content) > 0 {
		syslog.Printf(content)
	}
	segs := mlog.segs.String()
	if len(segs) > 0 {
		log.segs.WriteString(segs)
	}
	if len(mlog.slots) > 0 {
		log.slots = append(log.slots, mlog.slots...)
	}

}

func (mlog *MiniLogger) Printf(s string, a ...interface{}) {
	if mlog == nil {
		Log.Printf(s, a...)
		return
	}
	mlog.buf.WriteString(fmt.Sprintf(s, a...))
}

func (mlog *MiniLogger) Verbose(verbosityLevel int, s string, a ...interface{}) {
	if mlog == nil {
		Log.Verbose(verbosityLevel, s, a...)
		return
	}
	if verbosityLevel <= config.VerbosityLevel {
		mlog.buf.WriteString(fmt.Sprintf(s, a...))
	}
}

func (mlog *MiniLogger) Push(slotNumber int, s string, a ...interface{}) {
	if mlog == nil {
		Log.Push(slotNumber, s, a...)
		return
	}
	for slotNumber >= len(mlog.slots) {
		mlog.slots = append(mlog.slots, "")
	}
	mlog.slots[slotNumber] = fmt.Sprintf(s, a...)
}

func (mlog *MiniLogger) DumpSegs(ts *NodeSeg) {
	if mlog == nil {
		Log.DumpSegs(ts)
	}
	if !config.DumpSegsFlag || ts == nil { // reference to global: config
		return
	}
	// Assume all come from same sector
	allSector := ts.sector
	mlog.segs.WriteString(fmt.Sprintf("Sector #%d:\n", allSector))
	for tmps := ts; tmps != nil; tmps = tmps.next {
		mlog.segs.WriteString(fmt.Sprintf(
			"  Linedef: %d Flip: %d (%v,%v) - (%v, %v)",
			tmps.Linedef, tmps.getFlip(), tmps.StartVertex.X, tmps.StartVertex.Y,
			tmps.EndVertex.X, tmps.EndVertex.Y))
		if tmps.sector != allSector {
			// Is not supposed to write stuff from multiple sectors. You'll have
			// to rewrite code in this function to adjust it to your use case
			mlog.segs.WriteString(fmt.Sprintf(" BAD! Sector = %d\n", tmps.sector))
		} else {
			mlog.segs.WriteString("\n")
		}
	}
}

func CreateMiniLogger() *MiniLogger {
	var b bytes.Buffer
	var b2 bytes.Buffer
	mlog := new(MiniLogger)
	mlog.segs = b
	mlog.buf = b2
	return mlog
}
