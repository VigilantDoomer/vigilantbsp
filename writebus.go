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

// Bus for organized writes to destination file
package main

import (
	"os"
)

const (
	WRI_TYPE_BYTES = iota
	WRI_TYPE_INTERFACE
	WRI_TYPE_DEEPNODES
	WRI_TYPE_SYNC
)

type WriteBusRequest struct {
	tpeIndex int // typeIndex
	bData    []byte
	intfData interface{}
	deepData []DeepNode
	lumpIdx  int
	lumpName string
}

type WriteBus struct {
	fout   *os.File
	le     []LumpEntry
	curPos uint32
}

type WriteBusControl struct {
	bus      *WriteBus
	ch       chan<- WriteBusRequest
	finisher <-chan bool
	sync     <-chan bool
}

func StartWriteBus(fout *os.File, le []LumpEntry, curPos uint32) *WriteBusControl {
	bus := &WriteBus{
		fout:   fout,
		le:     le,
		curPos: curPos,
	}
	ch := make(chan WriteBusRequest)
	finisher := make(chan bool)
	sync := make(chan bool)
	go bus.WriteBusLoop(ch, finisher, sync)
	return &WriteBusControl{
		bus:      bus,
		ch:       ch,
		finisher: finisher,
		sync:     sync,
	}
}

func (b *WriteBus) WriteBusLoop(ch <-chan WriteBusRequest, chFinish chan<- bool,
	chSync chan<- bool) {
	for req := range ch {
		switch req.tpeIndex {
		case WRI_TYPE_BYTES:
			{
				WriteSliceLump(req.bData, &(b.curPos), b.fout, b.le, req.lumpIdx,
					req.lumpName)
			}
		case WRI_TYPE_INTERFACE:
			{
				ConvertAndWriteGenericLump(req.intfData, &(b.curPos), b.fout,
					b.le, req.lumpIdx, req.lumpName)
			}
		case WRI_TYPE_DEEPNODES:
			{
				ConvertAndWriteDeepNodes(req.deepData, &(b.curPos), b.fout,
					b.le, req.lumpIdx, req.lumpName)
			}
		case WRI_TYPE_SYNC:
			{
				Log.Sync()
				chSync <- true
			}
		default:
			{
				Log.Error("Unknown request at WriteBusLoop (%d)\n", req.tpeIndex)
			}
		}
	}
	chFinish <- true
}

// Asynchronous calls that instruct bus to write lump data, stored in varied
// forms before writing

// Sends lump to bus to write
// Lump data is stored in byte form, no conversion needed
func (c *WriteBusControl) SendRawLump(data []byte, lumpIdx int, s string) {
	envl := WriteBusRequest{
		tpeIndex: WRI_TYPE_BYTES,
		bData:    data,
		lumpIdx:  lumpIdx,
		lumpName: s,
	}
	c.ch <- envl
}

// Sends lump to bus to write
// Lump data is represented as a structure or an array(slice) of something
// different than bytes - conversion to respective byte order must be applied
func (c *WriteBusControl) SendGenericLump(data interface{},
	lumpIdx int, s string) {
	envl := WriteBusRequest{
		tpeIndex: WRI_TYPE_INTERFACE,
		intfData: data,
		lumpIdx:  lumpIdx,
		lumpName: s,
	}
	c.ch <- envl
}

// Sends lump to bus to write
// A DeepNodes signature needs to be prepended to data, and data needs to be
// converted to a proper byte order
func (c *WriteBusControl) SendDeepNodesLump(data []DeepNode, lumpIdx int,
	s string) {
	envl := WriteBusRequest{
		tpeIndex: WRI_TYPE_DEEPNODES,
		deepData: data,
		lumpIdx:  lumpIdx,
		lumpName: s,
	}
	c.ch <- envl
}

func (c *WriteBusControl) Shutdown() {
	close(c.ch)
	<-c.finisher
}

// This service call is needed to prevent actions pertaining to the previous
// level being logged to output after processing of new level is announced
func (c *WriteBusControl) Sync() {
	envl := WriteBusRequest{
		tpeIndex: WRI_TYPE_SYNC,
	}
	c.ch <- envl
	<-c.sync
}
