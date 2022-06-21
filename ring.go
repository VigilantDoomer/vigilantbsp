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
package main

// Implements ring buffer (a fixed size power of two queue). Not intended to
// be thread-safe or such, just when I need a fast queue.
// I encountered this article by someone actually good at programming (unlike
// me) when was searching for a smart way to do this, it is probable my
// implementation does not do it justice, though:
// https://www.snellman.net/blog/archive/2016-12-13-ring-buffers/

const MAX_RING_CAPACITY = uint32(2147483648)

// RingU16 is so called because the values stored in it are uint16 integers
// In case another buffer appears that stores something else, that could be
// named something else
// Beware: the routines perform no overflow or underflow checking for Enqueue's
// and Dequeue's. The end user is solely responsible to ascertain they don't
// dequeue an empty ring or enqueue a full ring.
type RingU16 struct {
	read     uint32
	write    uint32
	capacity uint32 // never changes after initialization
	buf      []uint16
}

// The argument capacity is how much data you expect to hold in ring buffer.
// This function will upsize it automatically to a power of two if non-power of
// two capacity is provided.
func CreateRingU16(capacity uint32) *RingU16 {
	iCap := RoundPOW2_Uint32(capacity)
	if iCap < capacity {
		Log.Panic("Integer overflow when computing ring capacity (before rounding up to power of two: %d). Specified capacity clearly exceeds the possible maximum\n",
			capacity)
	}
	if iCap > MAX_RING_CAPACITY {
		Log.Panic("Exceeds maximum ring capacity: %d (%d rounded up to power of two)\n",
			iCap, capacity)
	}
	capacity = iCap
	return &RingU16{
		read:     0,
		write:    0,
		capacity: capacity,
		buf:      make([]uint16, capacity, capacity),
	}
}

func RoundPOW2_Uint32(x uint32) uint32 {
	if x <= 2 {
		return x
	}

	x--

	for tmp := x >> 1; tmp != 0; tmp >>= 1 {
		x |= tmp
	}

	return x + 1
}

func (r *RingU16) mask(val uint32) uint32 {
	return val & (r.capacity - 1)
}

func (r *RingU16) Enqueue(item uint16) {
	r.buf[r.mask(r.write)] = item
	r.write++
}

func (r *RingU16) Dequeue() uint16 {
	res := r.buf[r.mask(r.read)]
	r.read++
	return res
}

func (r *RingU16) Empty() bool {
	return r.read == r.write
}

func (r *RingU16) Size() uint32 {
	return r.write - r.read
}

func (r *RingU16) Full() bool {
	return r.Size() == r.capacity
}

func (r *RingU16) Reset() {
	r.write = r.read
}
