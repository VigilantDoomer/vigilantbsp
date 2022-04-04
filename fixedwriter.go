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

// fixedwriter
package main

import (
	"encoding/binary"
	"errors"
	"fmt"
)

// Writer to fixed-size byte array/slice which can't be grown whatsoever
// Trying to write past array capacity will fail
type FixedWriterWrapper struct {
	data   []byte
	offset int // offset at which data gets writen by call to Write. Can be changed with Seek
}

func CreateFixedWriterWrapper(data []byte, offset int) *FixedWriterWrapper {
	result := new(FixedWriterWrapper)
	result.data = data
	result.offset = offset
	return result
}

// Changes offset (relative to the beginning of backing storage) at which
// next Write call will write bytes
// If requested to move beyond length but within capacity, will automatically
// increase length
func (w *FixedWriterWrapper) Seek(offset int) error {
	if offset >= cap(w.data) {
		return errors.New("out of range")
	}
	w.offset = offset
	if offset > len(w.data) {
		w.data = w.data[:offset]
	}
	return nil
}

func (w *FixedWriterWrapper) GetBytes() []byte {
	return w.data
}

// Write bytes at current offset, and move offset
// TODO code using FixedWriterWrapper should fucking check err (including
// indirect uses via binary.Write as in blockmap generation code).
func (w *FixedWriterWrapper) Write(p []byte) (n int, err error) {
	towrite := len(p)
	if towrite == 0 {
		return 0, nil
	}
	nlen := len(w.data)
	sl := w.data[w.offset:]
	if cap(sl) < towrite {
		// don't bother writing incomplete data!
		return 0, errors.New(fmt.Sprintf("Insufficient buffer size: want %d bytes have %d bytes", cap(sl), towrite))
	}
	if len(sl) < towrite {
		// we know already there is enough capacity
		// need to extend slice just enough to write data
		oldlen := len(sl)
		sl = sl[:towrite]
		nlen = nlen + len(sl) - oldlen
	}
	for i := 0; i < towrite; i++ { // TODO use copy built-in
		sl[i] = p[i]
	}

	w.data = w.data[:nlen]
	w.offset = w.offset + towrite
	return len(p), nil
}

// Writes uint16 slice with respect to endianness set by order. Purpose: avoid
// intermediary byte slice allocation in binary.Write for our poor GC-pressured
// program
func (w *FixedWriterWrapper) WriteWithOrder(order binary.ByteOrder, p []uint16) (n int, err error) {
	words := len(p)
	towrite := words << 1
	if towrite == 0 {
		return 0, nil
	}
	nlen := len(w.data)
	sl := w.data[w.offset:]
	if cap(sl) < towrite {
		// don't bother writing incomplete data!
		return 0, errors.New(fmt.Sprintf("Insufficient buffer size: want %d bytes have %d bytes", cap(sl), towrite))
	}
	if len(sl) < towrite {
		// we know already there is enough capacity
		// need to extend slice just enough to write data
		oldlen := len(sl)
		sl = sl[:towrite]
		nlen = nlen + len(sl) - oldlen
	}

	a := 0
	for i := 0; i < words; i++ {
		order.PutUint16(sl[a:], p[i])
		a += 2
	}

	w.data = w.data[:nlen]
	w.offset = w.offset + towrite
	return len(p), nil
}
