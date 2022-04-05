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
}

func StartWriteBus(fout *os.File, le []LumpEntry, curPos uint32) *WriteBusControl {
	bus := &WriteBus{
		fout:   fout,
		le:     le,
		curPos: curPos,
	}
	ch := make(chan WriteBusRequest)
	finisher := make(chan bool)
	go bus.WriteBusLoop(ch, finisher)
	return &WriteBusControl{
		bus:      bus,
		ch:       ch,
		finisher: finisher,
	}
}

func (b *WriteBus) WriteBusLoop(ch <-chan WriteBusRequest, chFinish chan<- bool) {
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
		default:
			{
				Log.Error("Unknown request at WriteBusLoop (%d)\n", req.tpeIndex)
			}
		}
	}
	chFinish <- true
}

func (c *WriteBusControl) WriteSliceLump(data []byte, lumpIdx int, s string) {
	envl := WriteBusRequest{
		tpeIndex: WRI_TYPE_BYTES,
		bData:    data,
		lumpIdx:  lumpIdx,
		lumpName: s,
	}
	c.ch <- envl
}

func (c *WriteBusControl) ConvertAndWriteGenericLump(data interface{},
	lumpIdx int, s string) {
	envl := WriteBusRequest{
		tpeIndex: WRI_TYPE_INTERFACE,
		intfData: data,
		lumpIdx:  lumpIdx,
		lumpName: s,
	}
	c.ch <- envl
}

func (c *WriteBusControl) ConvertAndWriteDeepNodes(data []DeepNode, lumpIdx int,
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
