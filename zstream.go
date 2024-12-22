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

// zstream.go implements stream that is optionally compressed, to reduce
// boilerplate that needs to handle both compressed and uncompressed Zdoom node
// format cases.
// With the ZStream object, only initialization one-liner will differ, the rest
// (writing and obtaining final value) will be the same regardless of whether
// compression was used
package main

import (
	"bytes"
	"compress/zlib"
)

type ZStream struct {
	raw        *bytes.Buffer
	compressor *zlib.Writer
}

func (z *ZStream) Write(p []byte) (int, error) {
	if z.compressor != nil {
		n, err := z.compressor.Write(p)
		return n, err
	}
	n, err := z.raw.Write(p)
	return n, err
}

// This is how the data that was written can actually be read
// Note that this function can only be called once!
func (z *ZStream) FinalizeAndGetBytes() ([]byte, error) {
	if z.raw == nil {
		Log.Panic("ZStream.FinalizeAndGetBytes() can be called only once\n")
	}
	if z.compressor != nil {
		err := z.compressor.Close()
		z.compressor = nil
		if err != nil {
			return nil, err
		}
	}
	btes := z.raw.Bytes()
	z.raw = nil
	return btes, nil
}

// Initializes internal buffer with header already written (if non-nil), the
// header is always non-compressed.
// All writes on returned value will occur past that header, optionally
// undergoing automatic compression (if compressed == true), or without that
// (otherwise)
// After you finished writing, you are obligated to call FinalizeAndGetBytes()
// (the returned value will also give you data you have written in its final
// byte sequence form), as in case of compression that calls Close() on the
// compressor used to perform it - and it is pretty much obligatory
func CreateZStream(header []byte, compressed bool) *ZStream {
	z := &ZStream{
		raw:        &bytes.Buffer{},
		compressor: nil,
	}
	if header != nil {
		z.raw.Write(header)
	}
	if compressed {
		z.compressor, _ = zlib.NewWriterLevel(z.raw, config.ZdoomCompression) // reference to global: config
	}
	return z
}
