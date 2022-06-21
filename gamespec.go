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

// Wad specifications for Doom-engine family of games
// (including Heretic, Hexen, etc.)
package main

import (
	"regexp"
)

// Both brought in accordance with Prboom-Plus 2.6.1um map name ranges, except
// that E1M0x is possible (when it is probably shouldn't be) since I don't
// want to complicate these regexp's (and E9M97 is perfectly legal, for example)
var MAP_SEQUEL *regexp.Regexp = regexp.MustCompile(`^MAP[0-9][0-9]$`)
var MAP_ExMx *regexp.Regexp = regexp.MustCompile(`^E[1-9]M[0-9][0-9]?$`)

// This constant group is for internal program usage only
const (
	FORMAT_DOOM = iota
	// Not yet ready
	FORMAT_HEXEN
	//FORMAT_UDMF
)

const BLOCK_WIDTH = 128
const BLOCK_BITS = uint16(7) // replaces division by BLOCK_WIDTH with right shift

// Starting signature "xNd4\0\0\0\0" of NODES produced by DeePBSP, some ports
// do support this nodes format (PrBoom-plus v2.5.1.5 confirmed, Risen3D also
// per zdoom.org wiki)
var DEEPNODES_SIG = [8]byte{0x78, 0x4E, 0x64, 0x34, 0x00, 0x00, 0x00, 0x00}

// Starting signature "XNOD" of NODES for Zdoom extended non-GL nodes format.
// Unlike Deep NODES, this signature, together with some bytes following it, may
// accidentally occur in a valid vanilla nodes format nodes data
var ZNODES_PLAIN_SIG = [4]byte{0x58, 0x4E, 0x4F, 0x44}

// Starting signature "ZNOD" of NODES for Zdoom extended COMPRESSED non-GL
// nodes format
var ZNODES_COMPRESSED_SIG = [4]byte{0x5A, 0x4E, 0x4F, 0x44}

const IWAD_MAGIC_SIG = uint32(0x44415749) // ASCII - 'IWAD'
const PWAD_MAGIC_SIG = uint32(0x44415750) // ASCII - 'PWAD'

// Doom & Heretic thing flag constants
const TF_ROOKIE = int16(0x0001)
const TF_NORMAL = int16(0x0002)
const TF_HARD = int16(0x0004)
const TF_AMBUSH = int16(0x0008)
const TF_MULTIPLAYER_ONLY = int16(0x0010)

// Boom/MBF additions to Doom flag constants
const TF_BOOM_NOTINDEATHMATCH = int16(0x0020)
const TF_BOOM_NOTCOOP = int16(0x0040)
const TF_MFB_FRIENDLY = int16(0x0080)

// Hexen thing flag constant
const HTF_ROOKIE = int16(0x0001)
const HTF_NORMAL = int16(0x0002)
const HTF_HARD = int16(0x0004)
const HTF_AMBUSH = int16(0x0008)
const HTF_DORMANT = int16(0x0010)
const HTF_FIGHTER = int16(0x0020)
const HTF_CLERIC = int16(0x0040)
const HTF_MAGE = int16(0x0080)
const HTF_SINGLEPLAYER = int16(0x0100)
const HTF_COOP = int16(0x0200)
const HTF_DEATHMATCH = int16(0x0400)

// Strife thing flag constant (note STF_AMBUSH != TF_AMBUSH)
const STF_ROOKIE = int16(0x0001)
const STF_NORMAL = int16(0x0002)
const STF_HARD = int16(0x0004)
const STF_STANDSTILL = int16(0x0008)
const STF_NOTINSINGLEPLAYER = int16(0x0010)
const STF_AMBUSH = int16(0x0020)
const STF_FRIENDLY = int16(0x0040)
const STF_UNUSED = int16(0x0080)
const STF_TRANSLUCENT = int16(0x0100)
const STF_INVISIBLE = int16(0x0200)

// COMMON linedef flags: for Doom & derivatives
const LF_IMPASSABLE = uint16(0x0001)
const LF_BLOCK_MONSTER = uint16(0x0002)
const LF_TWOSIDED = uint16(0x0004)
const LF_UPPER_UNPEGGED = uint16(0x0008)
const LF_LOWER_UNPEGGED = uint16(0x0010)
const LF_SECRET = uint16(0x0020) // shown as 1-sided on automap
const LF_BLOCK_SOUND = uint16(0x0040)
const LF_NEVER_ON_AUTOMAP = uint16(0x0080)
const LF_ALWAYS_ON_AUTOMAP = uint16(0x0100)

// Linedef flags: Strife additions to COMMON linedef flags
const SLF_JUMPABLE = uint16(0x200)
const SLF_BLOCK_FLOATING = uint16(0x0400)
const SLF_TRANSLUCENCY1 = uint16(0x0800)
const SLF_TRANSLUCENCY2 = uint16(0x0100)

// Linedef flags: Boom additions to COMMON linedef flags
const BLF_PASSTHRU = uint16(0x200)

const SIDEDEF_NONE = uint16(0xFFFF)

const DOOM_LINEDEF_SIZE = 14   // Size of "Linedef" struct
const DOOM64_LINEDEF_SIZE = 16 // Size of "Doom64Linedef" struct
const HEXEN_LINEDEF_SIZE = 16  // Size of "HexenLinedef" struct

const DOOM_SIDEDEF_SIZE = 30 // Size of "Sidedef" struct
const DOOM_SECTOR_SIZE = 26  // Size of "Sector" struct

const HEXEN_ACTION_POLY_START = 1
const HEXEN_ACTION_POLY_EXPLICIT = 5

const PO_ANCHOR_TYPE = 3000
const PO_SPAWN_TYPE = 3001
const PO_SPAWNCRUSH_TYPE = 3002

const ZDOOM_PO_ANCHOR_TYPE = 9300
const ZDOOM_PO_SPAWN_TYPE = 9301
const ZDOOM_PO_SPAWNCRUSH_TYPE = 9302

// TODO Linedef flags: Hexen additions to COMMON linedef flags
// (There is huge mess there)

// Wad header, 12 bytes.
type WadHeader struct {
	MagicSig       uint32
	LumpCount      uint32 // vanilla treats this as signed int32
	DirectoryStart uint32 // vanilla treats this as signed int32
}

// Lump entries listed one after another comprise the directory,
// the first such lump entry is found at WadHeader.DirectoryStart offset into
// the wad file.
// Each lump entry is 16 bytes long
type LumpEntry struct {
	FilePos uint32 // vanilla treats this as signed int32
	Size    uint32 // vanilla treats this as signed int32
	Name    [8]byte
}

// This is Doom/Heretic/Strife thing. Not Hexen thing
type Thing struct {
	XPos  int16
	YPos  int16
	Angle int16
	Type  int16
	Flags int16
}

// Hexen Thing
type HexenThing struct {
	TID            int16
	XPos           int16
	YPos           int16
	StartingHeight int16
	Angle          int16
	Type           int16
	Flags          int16
	Action         uint8
	Args           [5]byte
}

type Doom64Thing struct {
	XPos  int16
	YPos  int16
	ZPos  int16
	Angle int16
	Type  int16
	Flags int16
	ID    int16
}

// Doom/Heretic linedef format
type Linedef struct {
	// Vanilla treats ALL fields as signed int16
	StartVertex uint16
	EndVertex   uint16
	Flags       uint16
	Action      uint16
	Tag         uint16
	FrontSdef   uint16 // Front Sidedef number
	BackSdef    uint16 // Back Sidedef number (0xFFFF special value for one-sided line)
}

// Doom64 linedef format: different Flags size compared to Doom (32-bit vs 16-bit)
type Linedef64 struct {
	// TODO Check whether vanilla treats ALL fields as signed
	StartVertex uint16
	EndVertex   uint16
	Flags       uint32
	Action      uint16
	Tag         uint16
	FrontSdef   uint16 // Front Sidedef number (front is to the right)
	BackSdef    uint16 // Back Sidedef number (0xFFFF special value for one-sided line)
}

// Hexen linedef format
type HexenLinedef struct {
	// Vanilla treats ALL fields as signed
	StartVertex uint16
	EndVertex   uint16
	Flags       uint16
	Action      uint8
	Arg1        uint8 // this acts as corresponding to sector tag, but sector has uint16-size tag while linedef has uint8-size tag lol
	Arg2        uint8
	Arg3        uint8
	Arg4        uint8
	Arg5        uint8
	FrontSdef   uint16
	BackSdef    uint16
}

// Sidedef format - common to all? except doom64 which is not considered for support yet
type Sidedef struct {
	XOffset int16
	YOffset int16
	UpName  [8]byte // name of upper texture
	LoName  [8]byte // name of lower texture
	MidName [8]byte // name of middle texture
	Sector  uint16  // sector number; vanilla treats this as signed int16
}

// Doom64 Sidedef format. Indices instead of texture names
type Sidedef64 struct {
	XOffset  int16
	YOffset  int16
	UpIndex  uint16 // index of upper texture, likeky signed in original
	LoIndex  uint16 // index of lower texture, likeky signed in original
	MidIndex uint16 // index of middle texture, likeky signed in original
	Sector   uint16 // sector number; vanilla treats this as signed int16
}

// A Vertex is a coordinate on the map, and can be used in both linedefs and segs
// as starting(ending) point
// Note that map editing utilities display only those vertices that were
// referenced in linedefs (which is what human user expects to see)
// As the result of building nodes and thus constructing SEGS, VERTEXES lump is
// modified to also have vertices used in SEGS introduces by splitting
type Vertex struct {
	XPos int16
	YPos int16
}

type Doom64Vertex struct {
	// These are really fixed_t: 16 bits store integral part and 16 bits store
	// fractional part
	// That is, Doom64 used fractional coordinates. The maximum map boundaries
	// are same as Doom
	XPos int32
	YPos int32
}

type Seg struct {
	// Vanilla treats ALL fields as signed int16
	StartVertex uint16
	EndVertex   uint16
	Angle       int16
	Linedef     uint16
	Flip        int16  // 0 - seg follows same direction as linedef, 1 - the opposite
	Offset      uint16 // distance along linedef to start of seg
}

// DeePBSP "standard V4" seg format
type DeepSeg struct {
	StartVertex uint32
	EndVertex   uint32
	Angle       int16
	Linedef     uint16
	Flip        int16
	Offset      uint16
}

// Each subsector has only these two fields, yes. And the segs in SEGS lump
// follow the order so that consecutive segs in FirstSeg...FirstSeq+SeqCount-1
// all belong to this subsector. So each seg is a part of one and only one subsector
type SubSector struct {
	// Vanilla treats ALL fields as signed int16
	SegCount uint16 // number of Segs in this SubSector
	FirstSeg uint16 // first Seg number
}

// DeePBSP "standard V4" subsector format
// SegCount is same size as regular subsector (DeePSea author states 64K is
// enough for one's subsector seg count), but FirstSeg is bigger to allow
// indexing into larger total number of segs
type DeepSubSector struct {
	SegCount uint16
	FirstSeg uint32
}

type Node struct {
	X      int16
	Y      int16
	Dx     int16
	Dy     int16
	Rbox   [4]int16 // right bounding box
	Lbox   [4]int16 // left bounding box
	RChild int16    // -| if sign bit = 0 then this is a subnode number
	LChild int16    // ->     else 0-14 bits are subsector number
}

// DeePBSP "standard V4" node format. Also used by Zdoom extended non-GL nodes,
// as it is the same
type DeepNode struct {
	X      int16
	Y      int16
	Dx     int16
	Dy     int16
	Rbox   [4]int16 // right bounding box
	Lbox   [4]int16 // left bounding box
	RChild int32    // -| if sign bit = 0 then this is a subnode number
	LChild int32    // ->     else 0-30 bits are subsector number
}

const BB_TOP = 0
const BB_BOTTOM = 1
const BB_LEFT = 2
const BB_RIGHT = 3

type Sector struct {
	FloorHeight int16
	CeilHeight  int16
	FloorName   [8]byte
	CeilName    [8]byte
	LightLevel  uint16
	Special     uint16
	Tag         uint16
}

type Sector64 struct {
	FloorHeight  int16
	CeilHeight   int16
	FloorIndex   uint16    // index of floor flat, likely signed in vanilla
	CeilIndex    uint16    // index of floor flat, likely signed in vanilla
	ColorIndexes [5]uint16 // Color indexes (floor, ceiling, thing, wall top, wall bottom)
	Special      uint16
	Tag          uint16
	Flags        uint16
}

// NOTE There is no type for reject - it is a stream of bits packed into bytes

// Blockmap consists of: header, followed by XBlocks*YBlocks offsets,
// followed by blocklist (arbitrary size)
type BlockMapHeader struct {
	XMin    int16
	YMin    int16
	XBlocks uint16 // vanilla treats this as signed int16
	YBlocks uint16 // vanilla treats this as signed int16
}

type ZdoomNode_VertexHeader struct {
	ReusedOriginalVertices uint32 // number of vertices reused from VERTEXES
	NumExtendedVertices    uint32 // how many vertices follow this
}

type ZdoomNode_Vertex struct {
	X int32 // fixed-point 16.16 signed int
	Y int32 // fixed-point 16.16 signed int
}

// Zdoom subsector information - nothing to define here. Just a count of
// subsectors, and each subsector defines only number of segs in current sector

// Zdoom seg information - number of segs, followed by repetition of the below
// struct. The struct is castrated - it does not include angle and offset
// information, which means some special effects (like horizon) can not be
// supported. This makes extended nodes practically inferior to deep nodes,
// despite the extra precision of vertices
type ZdoomNode_Seg struct {
	StartVertex uint32
	EndVertex   uint32
	Linedef     uint16
	Flip        int16
}

// ZdoomNode_node is not defined - it is same as DeepNode

// NOTE also no type definitions for blocks and blocklists, they are offsets
// and (each blocklist is) array of linedef indexes correspondingly

// Returns whether the string in lumpName represents Doom level marker,
// i.e. MAP02, E3M1
func IsALevel(lumpName []byte) bool {
	return MAP_SEQUEL.Match(lumpName) || MAP_ExMx.Match(lumpName)
}

func IsEmptyTexture(lumpName []byte) bool {
	return lumpName[0] == '-' && lumpName[1] == 0
}
