// Copyright (C) 2022-2025, VigilantDoomer
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

// lumpwrite
package main

import (
	"crypto/sha256"
	"encoding/base64"
	"encoding/binary"
	"fmt"
	"os"
)

// Write byte array without conversion. It is assumed that lump was received
// as a valid byte array (also already converted to file endianness) ready to be
// written
func WriteSliceLump(data []byte, curPos *uint32, fout *os.File, le []LumpEntry,
	lumpIdx int, s string, postfix string) {
	if data != nil {
		fout.Write(data)
	}
	le[lumpIdx].FilePos = *curPos
	le[lumpIdx].Size = uint32(len(data))
	if len(s) > 0 {
		hashSuffix := ""
		if config.HashLumps {
			hashSuffix = fmt.Sprintf(" (HASH: %s)", Hash(data))
		}
		Log.Printf("Lump #%d (%s)%s has its size set to %d bytes.%s\n", lumpIdx,
			s, postfix, le[lumpIdx].Size, hashSuffix)
	}
	*curPos = *curPos + uint32(len(data))
}

// Write lump from a typed array of structures that represent game data but need
// conversion before going to file
func ConvertAndWriteGenericLump(data interface{}, curPos *uint32, fout *os.File,
	le []LumpEntry, lumpIdx int, s string, postfix string) {
	hashSuffix := ""
	binary.Write(fout, binary.LittleEndian, data)
	if config.HashLumps {
		hashSuffix = fmt.Sprintf(" (HASH: %s)", HashIntf(data))
	}
	dataLen := binary.Size(data)
	le[lumpIdx].FilePos = *curPos
	le[lumpIdx].Size = uint32(dataLen)
	Log.Printf("Lump #%d (%s)%s has its size set to %d bytes.%s\n", lumpIdx, s,
		postfix, le[lumpIdx].Size, hashSuffix)
	*curPos = *curPos + uint32(dataLen)
}

func ConvertAndWriteDeepNodes(data []DeepNode, curPos *uint32, fout *os.File,
	le []LumpEntry, lumpIdx int, s string, postfix string) {
	hashSuffix := ""
	sigCnt, _ := fout.Write(DEEPNODES_SIG[:])
	binary.Write(fout, binary.LittleEndian, data)
	if config.HashLumps {
		hashSuffix = fmt.Sprintf(" (HASH: %s)", HashIntf(data))
	}
	dataLen := binary.Size(data)
	le[lumpIdx].FilePos = *curPos
	le[lumpIdx].Size = uint32(dataLen + sigCnt)
	Log.Printf("Lump #%d (%s)%s has its size set to %d bytes.%s\n", lumpIdx, s,
		postfix, le[lumpIdx].Size, hashSuffix)
	*curPos = *curPos + uint32(dataLen+sigCnt)
}

// non-threadsafe encode of cryptographic hash, I presume
func Hash(data []byte) string {
	hsh := sha256.New()
	hsh.Write(data)
	su := hsh.Sum(nil)
	return base64.StdEncoding.EncodeToString(su)
}

// same as Hash, but for arbitrary data instead of slice of bytes
func HashIntf(data interface{}) string {
	hsh := sha256.New()
	binary.Write(hsh, binary.LittleEndian, data)
	su := hsh.Sum(nil)
	return base64.StdEncoding.EncodeToString(su)
}
