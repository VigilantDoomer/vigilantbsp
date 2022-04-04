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

// bblocks
// aka build blockmap
package main

import (
	"encoding/binary"
	"sort"
)

const BBLOCKS_BUCKETS_COUNT = 4096
const VANILLA_BM_OFFSET_TOO_BIG = 32768
const ANYPORT_BM_OFFSET_TOO_BIG = 65536

const (
	// idea from zokumbsp: when blocklist has both a horizontal and a vertical
	// line, it can't match any other blocklist nor even be a subset of any
	// other blocklist - the latter property is useful for an optimization in
	// GetBytesArcane method
	HAS_VERTICAL_LINE   = uint8(0x01)
	HAS_HORIZONTAL_LINE = uint8(0x02)
	BLOCKLIST_UNIQUE    = uint8(0x03)
)

var BLOCKLIST_TERM = []byte{0xFF, 0xFF}

// Type of input argument passed to CreateBlockmap
type BlockmapInput struct {
	lines           AbstractLines
	bounds          LevelBounds
	XOffset         int16
	YOffset         int16
	useZeroHeader   bool
	internalPurpose bool              // whether blockmap is for internal purposes only. Used by reject and nodes builder
	gcShield        *BlockmapGCShield // if non-nil, will reduce allocations by caching them. (NOTE Go supports calling methods on nil object.)
}

type BlockLines []uint16 // these long-lived slices must be tripping gc up... cause there can be like 262144 of them for a single blockmap

type Blockmap struct {
	blocklist        []BlockLines // [<blocknum,num_line>] -> linedef's index in LINEDEFS lump
	flags            []uint8      // taken cue of zokumbsp "this blocklist has unique linedefs"
	header           BlockMapHeader
	useZeroHeader    bool              // lump data will feature extra dummy linedef at beginning of each blocklist
	zeroLinedef      uint16            // dummy linedef doesn't need to be zero index one
	stealOneWord     bool              // write last block's blocklist first and overlap the first linedef's definition with this block's offset (zeroLinedef's index must equal block's offset)
	tooBigForVanilla bool              // vanilla interprets offsets as signed integers
	tooBigForUint16  bool              // blockmap breaks even ports using unsigned 2-byte
	couldSqueeze     bool              // the last offset could have been moved under the limit if too eager subset compression was _partially_ undone. The logic to do this is not yet implemented
	gcShield         *BlockmapGCShield // see BlockmapInput.gcShield
	XMax             int
	YMax             int
	largestOffset    int // statistics: what is the largest offset
}

// Identity is blocklist that will be actually written after the blocks
// Identical blocklist are thus merged into a single identity, where as blocklists
// that are subsets of others reference identity of greater blocklist with a non-zero offset
type BlocklistIdentity struct {
	data    BlockLines
	address int // holds offset relative to the beginning of blockmap after it was written
}

// A blocklist that is pointed by some block, but may be a subset of a larger
// blocklist referenced by some other block (in this case offset != 0)
type BlocklistChunk struct {
	identity *BlocklistIdentity
	offset   int // where in identity's data does this actual blocklist start (repurposed to store the offset in lump's data at later stages)
	hash     uint64
	hash2    uint64 // contains value equal to hash, except for lists that can't be a part of any bigger list
	tainted  bool   // not eligible to be written last - during subset merge, the same identity was used by someone with a greater offset
}

// And this is a unit of block table, the way it is represented during the
// subset compression algorithm. Chunks are shared between blocks with identical
// blocklists
type Block struct {
	chunk *BlocklistChunk
}

const (
	BMStateUnused = iota
	BMStateOld
	BMStateNew
)

type BlockmapLumpPool struct {
	pool     [3 * MAX_ADDRESSABLE_BLOCKMAP_SIZE]byte
	slots    [3]byte
	lastSlot byte
}

// Fewer allocations - smaller GC pauses
// Basically, reuse things as much as possible
type BlockmapGCShield struct {
	blocklist     []BlockLines
	hashes        []uint16
	buckets       [BBLOCKS_BUCKETS_COUNT]uint16
	blockOffsets  []uint16
	blocks        []Block
	chunkAlloc    []BlocklistChunk
	identityAlloc []BlocklistIdentity
	chunks        []*BlocklistChunk
	skiplist      []int
	flags         []uint8
	lumpPool      BlockmapLumpPool
}

func CreateBlockmap(input BlockmapInput) *Blockmap {
	// TODO handle zero number of vertices. What to do then?
	// Maybe doesn't need to be handled here? Could be handled in the caller
	// instead - no lumps can be built if the vertices (and thus linedefs and
	// sectors) are absent
	result := new(Blockmap)
	xmin := int(input.bounds.Xmin - input.XOffset)
	ymin := int(input.bounds.Ymin - input.YOffset)
	xmax := int(input.bounds.Xmax)
	ymax := int(input.bounds.Ymax)
	// Log.Printf("Map bounds: (%d, %d) - (%d, %d)\n", xmin, ymin, xmax, ymax)
	xblocks := uint16(xmax-xmin)>>BLOCK_BITS + 1
	yblocks := uint16(ymax-ymin)>>BLOCK_BITS + 1
	result.header = BlockMapHeader{
		XMin:    int16(xmin),
		YMin:    int16(ymin),
		XBlocks: xblocks,
		YBlocks: yblocks,
	}
	result.blocklist = nil
	result.useZeroHeader = input.useZeroHeader
	result.zeroLinedef = 0 // default
	result.stealOneWord = false
	result.tooBigForVanilla = false
	result.tooBigForUint16 = false
	result.couldSqueeze = false
	result.gcShield = input.gcShield
	result.flags = nil
	result.XMax = xmax
	result.YMax = ymax
	blockCount := int(xblocks) * int(yblocks) // the value can be well over 65536
	blocklist := result.gcShield.GetBlocklist(blockCount)
	if !input.internalPurpose { // only needed if we are writing a lump to speed up identifying unique blocklists
		result.flags = result.gcShield.GetFlags(blockCount)
	}

	cntLines := input.lines.Len()
	if !input.internalPurpose && input.useZeroHeader && !config.ZeroHeaderIsZero && cntLines > 0 { // use of global: config
		// offset of the last block definition (which holds offset of this block
		// blocklist) in blockmap would be:
		final_offs := 4 + blockCount - 1
		if final_offs < int(cntLines) { // TODO check whether linedef is problematic to be included in every block
			// Minor savings to cushion use of zero header in every blocklist
			// Note that result.stealOneWord may still get reverted to false
			// when lump is produced (GetBytes/GetBytesArcane and their
			// subroutines). It may happen if the blocklist of the last
			// block has to be written last because of its big size (and no other
			// blocklists have the same size), as stealing only happens when
			// blocklist of last block is written first
			result.zeroLinedef = uint16(final_offs)
			result.stealOneWord = true
		}
		// TODO compute length. If too long, will need another
	}

	// The algorithm as used here is taken from ZDBSP v1.19 (c) Marisa Heit
	// NOTE ZDBSP source code also contains a check against original BSP's output,
	// which is only compiled into program when a certain debug directive is passed
	// to compiler, it had this notice:
	// (START QUOTE)
	// "If a diagonal line passed near a block (within 2 or 4 units, I think),
	// it could be considered in the block even if it's really outside it,
	// so if things differ, see if DoomBSP was at fault."
	//     END QUOTE
	// => So, if the output of this doesn't match output by your program, this
	// *might* be the reason why.
	// I don't deny there may be bugs in my code, just check carefully and
	// make sure it was a bug in my code before reporting! -- VigilantDoomer

	// Cycle over lines ONCE  - the way zennode, zdbsp and others do
	// BSP v5.2 used cycle over blocks with inner cycle over all lines, which
	// is slower, although could allow not to hold more than a single blocklist
	// in memory
	for cid := uint16(0); cid < cntLines; cid++ {
		if input.lines.BlockmapSkipThis(cid) {
			continue
		}
		x1, y1, x2, y2 := input.lines.GetAllXY(cid)
		dx := x2 - x1
		dy := y2 - y1
		bx := (x1 - xmin) >> BLOCK_BITS
		by := (y1 - ymin) >> BLOCK_BITS
		bx2 := (x2 - xmin) >> BLOCK_BITS
		by2 := (y2 - ymin) >> BLOCK_BITS

		// pointers to blocklist of the blocks that host starting and
		// ending vertices
		wbeg := bx + by*int(xblocks)
		wend := bx2 + by2*int(xblocks)

		if wbeg == wend { // Single block
			blocklist[wbeg].Push(cid)
			if result.flags != nil {
				result.flags[wbeg] = result.flags[wbeg] | BLOCKLIST_UNIQUE
			}
		} else if by == by2 { // Horizontal line
			if bx > bx2 {
				// swap beginning and end
				PushAxis(cid, wend, wbeg, 1, blocklist)
			} else {
				PushAxis(cid, wbeg, wend, 1, blocklist)
			}
			if result.flags != nil {
				result.flags[wbeg] = result.flags[wbeg] | HAS_HORIZONTAL_LINE
			}
		} else if bx == bx2 { // Vertical line
			if by > by2 {
				// swap beginning and end
				PushAxis(cid, wend, wbeg, int(xblocks), blocklist)
			} else {
				PushAxis(cid, wbeg, wend, int(xblocks), blocklist)
			}
			if result.flags != nil {
				result.flags[wbeg] = result.flags[wbeg] | HAS_VERTICAL_LINE
			}
		} else { // Diagonal line
			// Toughest case, yeah
			xchange := Sign(dx)
			ychange := Sign(dy)
			ymove := ychange * int(xblocks)
			adx := Abs(dx)
			ady := Abs(dy)
			if adx == ady { // 45 degrees
				xb := (x1 - xmin) & (BLOCK_WIDTH - 1)
				yb := (y1 - ymin) & (BLOCK_WIDTH - 1)
				if dx < 0 {
					xb = BLOCK_WIDTH - xb
				}
				if dy < 0 {
					yb = BLOCK_WIDTH - yb
				}
				if xb < yb {
					adx--
				}
			}
			if adx >= ady { // X major
				var yadd int
				if dy < 0 {
					yadd = -1
				} else {
					yadd = BLOCK_WIDTH
				}
				for {
					stop := (Scale(by<<BLOCK_BITS+yadd-(y1-ymin), dx, dy) + (x1 - xmin)) >> BLOCK_BITS
					for bx != stop {
						blocklist[wbeg].Push(cid)
						wbeg += xchange
						bx += xchange
					}
					blocklist[wbeg].Push(cid)
					wbeg += ymove
					by += ychange
					if by == by2 {
						break
					}
				}
				for wbeg != wend {
					blocklist[wbeg].Push(cid)
					wbeg += xchange
				}
				blocklist[wbeg].Push(cid)
			} else { // Y major
				var xadd int
				if dx < 0 {
					xadd = -1
				} else {
					xadd = BLOCK_WIDTH
				}
				for {
					stop := (Scale(bx<<BLOCK_BITS+xadd-(x1-xmin), dy, dx) + (y1 - ymin)) >> BLOCK_BITS
					for by != stop {
						blocklist[wbeg].Push(cid)
						wbeg += ymove
						by += ychange
					}
					blocklist[wbeg].Push(cid)
					wbeg += xchange
					bx += xchange
					if bx == bx2 {
						break
					}
				}
				for wbeg != wend {
					blocklist[wbeg].Push(cid)
					wbeg += ymove
				}
				blocklist[wbeg].Push(cid)
			}
		}
	}

	result.blocklist = blocklist
	return result
}

// Returns bytes to store in BLOCKMAP lump
// Streaming approach - writes blocklists as it parses and hashes them
// Biggest blocklist is placed at the end of BLOCKMAP
// No subset compression. Identical blocklists are merged (traditional subset
// compression). Subset compression is implemented by different method
func (bm *Blockmap) GetBytes() []byte {
	// Ok, let's generate lump data at last
	hashed := 0
	nothashed := 0
	var i int
	totalblocks := len(bm.blocklist)
	if totalblocks == 0 {
		// empty map
		return nil
	}
	blockOffsets := bm.gcShield.GetBlockOffsets(totalblocks)
	hashes := bm.gcShield.GetHashes(totalblocks)
	buckets := bm.gcShield.GetBuckets()

	// Longest blocklist should be written last - better chance to fit all block
	// offsets within limits
	split_idx := 0 // will store the first encountered blocklist that is longest
	largest_cnt := 0
	total_size := 0 // how much 2-byte words would an uncompressed blockmap consume
	for i = 0; i < int(totalblocks); i++ {
		size_adder := len(bm.blocklist[i])
		if size_adder > largest_cnt {
			split_idx = i
			largest_cnt = size_adder
		}
		total_size = total_size + size_adder + 1 // don't forget terminal FFFF
	}

	// allocate enough for worst case blockmap size - the size of uncompressed blockmap
	lumpInitial := binary.Size(bm.header) + int(totalblocks*2)
	lumpSize := lumpInitial + total_size*2
	var zeroLinedefBytes []byte
	if bm.useZeroHeader {
		// account for the dummy linedef
		lumpSize = lumpSize + int(totalblocks*2)
		zeroLinedefBytes = make([]byte, 2)
		binary.LittleEndian.PutUint16(zeroLinedefBytes, bm.zeroLinedef)
	}

	// Reserve lumpSize as capacity, but commit smaller size initially, and let
	// length grow to consume only actually written data
	lumpWriter := CreateFixedWriterWrapper(bm.gcShield.GetLumpFromPool(lumpInitial, lumpSize), 0)
	binary.Write(lumpWriter, binary.LittleEndian, bm.header)
	// Move writing cursor past block table storing offset to blocklists
	// - so that we are ready to write blocklists themselves
	lumpWriter.Seek(lumpInitial)

	// If the last block's blocklist is written last, it can't be written first
	if bm.stealOneWord && (split_idx == int(totalblocks-1)) {
		bm.stealOneWord = false
	}
	var intBlockOffset int

	part_min := 0
	if bm.stealOneWord {
		// want save two bytes, CAN save two bytes - will write the last
		// block's blocklist first and in a special way
		part_min = -1
	}

	for part := part_min; part < 2; part++ {
		// this outer loop helps reorder writes done in the inner loop
		// largest block is written OUTSIDE both of these loops
		part_beg := -1
		part_end := -1
		switch part {
		case -1:
			{ // Write last block's blocklist first (permission granted)
				part_beg = int(totalblocks) - 1
				part_end = int(totalblocks)
			}
		case 0:
			{ // Write every block preceeding the one with largest blocklist we target to write last
				part_beg = 0
				part_end = split_idx
				// Hash largest blocklist NOW, but store it last
				// Other blocks may have this same blocklist, so use ZERO offset
				// as a marker so they all copy it and become marked as well.
				// When largest blocklist is finally written, all blocks with
				// this marker will be made to point to the actual offset
				block := bm.blocklist[split_idx]
				hash := block.Hash() % BBLOCKS_BUCKETS_COUNT
				hashes[split_idx] = buckets[hash]
				buckets[hash] = uint16(split_idx)
				blockOffsets[split_idx] = uint16(0x0000)
				nothashed++
			}
		case 1:
			{ // Write every block following the one with largest blocklist we target to write last
				part_beg = split_idx + 1
				part_end = int(totalblocks)
				if part_min == -1 {
					part_end = part_end - 1
				}
			}
		}
		// lay out as instructed
		for i = part_beg; i < part_end; i++ {
			block := bm.blocklist[i]
			hash := block.Hash() % BBLOCKS_BUCKETS_COUNT
			hashblock := buckets[hash]
			for hashblock != 0xFFFF {
				if CompareBlocklist(block, bm.blocklist[hashblock]) {
					break
				}
				hashblock = hashes[hashblock]
			}
			if hashblock != 0xFFFF {
				// this exists already
				blockOffsets[i] = blockOffsets[hashblock]
				hashed++
			} else {
				// this blocklist was unique yet
				hashes[i] = buckets[hash]
				buckets[hash] = uint16(i)
				intBlockOffset = len(lumpWriter.GetBytes()) >> 1
				if part == -1 {
					// so this is my word stealing logic
					// We picked dummy linedef so that the following can be
					// made true:
					// offset of the last block = offset of its block list =
					// dummy linedef index
					// This makes our blockmap shorter by two bytes, offsets
					// get smaller by 1
					intBlockOffset = intBlockOffset - 1
				}
				bm.LimitCheck(intBlockOffset)
				blockOffsets[i] = uint16(intBlockOffset)
				if bm.useZeroHeader {
					if part == -1 {
						// do not insert dummy linedef. It's already contained
						// in offset value
					} else {
						// write dummy linedef (not necessary #0)
						lumpWriter.Write(zeroLinedefBytes)
					}
				}
				if len(bm.blocklist[i]) > 0 {
					lumpWriter.WriteWithOrder(binary.LittleEndian, bm.blocklist[i])
				}
				lumpWriter.Write(BLOCKLIST_TERM) // write terminator
				nothashed++
			}
		}
	}
	// Write last blocklist
	lastBlocklistOffset := len(lumpWriter.GetBytes()) >> 1
	bm.LimitCheck(lastBlocklistOffset)
	blockOffsets[split_idx] = uint16(lastBlocklistOffset)
	if bm.useZeroHeader {
		// write dummy linedef (not necessary #0)
		lumpWriter.Write(zeroLinedefBytes)
	}
	if len(bm.blocklist[split_idx]) > 0 {
		binary.Write(lumpWriter, binary.LittleEndian, bm.blocklist[split_idx])
	}
	lumpWriter.Write(BLOCKLIST_TERM) // write terminator
	// now that we've written it, need to set offsets pointing to it
	for i = 0; i < int(totalblocks); i++ {
		if blockOffsets[i] == 0 { // zero was our marker
			blockOffsets[i] = uint16(lastBlocklistOffset)
		}
	}

	// now that the block table is filled, let's write it!
	lumpWriter.Seek(binary.Size(bm.header))
	lumpWriter.WriteWithOrder(binary.LittleEndian, blockOffsets)

	// done
	data := lumpWriter.GetBytes()
	//Log.Printf("Done generating BLOCKMAP size %d (%d blocks written, %d blocks saved)\n",
	//	len(bm.data), nothashed, hashed)
	return data
}

// adds object (linedef or seg or whatever, referenced by number) to the blocklist
func (bl *BlockLines) Push(object uint16) {
	(*bl) = append((*bl), object)
}

func PushAxis(object uint16, wbeg int, wend int, step int, blocklist []BlockLines) {
	for w := wbeg; w <= wend; w += step {
		blocklist[w].Push(object)
	}
}

// Decent hash to avoid bulk of blocklist comparisons (good bucket assignment)
func (bl BlockLines) Hash() uint {
	// same hash function as Zdbsp courtesy of Marisa Heit
	hash := uint(0)
	for i := 0; i < len(bl); i++ {
		hash = hash*12235 + uint(bl[i])
	}
	return hash & 0x7fffffff
}

// This hash should also be valid for subset tests (if blocklist A is subset of
// blocklist B, hash2(A) < hash2(B))
// The property of being good for bucket assignment is also desired
func (bl BlockLines) Hash2() uint64 {
	hash := uint64(0) // ... and my hash is very, very big
	for i := 0; i < len(bl); i++ {
		hash = hash + uint64(bl[i])*uint64(bl[i])
	}
	return hash
}

func CompareBlocklist(bl1 BlockLines, bl2 BlockLines) bool {
	if len(bl1) != len(bl2) {
		return false
	}
	if len(bl1) == 0 {
		return true
	}
	// NOTE zokumbsp special cases "hot paths" comparing blocklist consisting
	// of <=4 lines. I didn't do this yet
	for i := 0; i < len(bl1); i++ {
		if bl1[i] != bl2[i] {
			return false
		}
	}
	return true
}

// make uint16 region of specified size filled with "FF FF"
func MakeRegion_FF(sz int) []uint16 {
	result := make([]uint16, sz)
	for i := 0; i < sz; i++ {
		result[i] = uint16(0xFFFF)
	}
	return result
}

func Sign(x int) int {
	if x < 0 {
		return -1
	} else if x > 0 {
		return 1
	} else {
		return 0
	}
}

func Abs(x int) int {
	return x * Sign(x)
}

func Scale(a int, b int, c int) int {
	return int((int64(a) * int64(b)) / int64(c))
}

// Returns bytes to store in BLOCKMAP lump
// Implements subset compression
// Subset compression required a change of approach from streaming writer in GetBytes(),
// as well as a different hash function and more structures (and thus more memory allocations)
// For this reason, it is separate from GetBytes()
func (bm *Blockmap) GetBytesArcane() []byte {
	// Not optimized - just a straightforward way to do this
	// FIXME subset compression can increase (BAD) the greatest offset while
	// decreasing overall blockmap size:
	//                    v
	// 001,019 024,035,067 001,019,039,086,115
	// 024,035,067 039,086,115 001,019
	//                        ^
	// So some code needs to be added to BMIdentifyOrderAndSize to avoid the
	// situation when subset compression pushes the last blocklist's offset OVER
	// the limit

	// The idea is to parse the initial blocklists collection (generated by traversing
	// the lines) in CreateBlockmap into 3-tier new one:
	// - One Block for each block, containing a pointer to BlocklistChunk
	// - One BlocklistChunk for each *unique* blocklist, which may or may not be
	// a subset of a different blocklist - in turn containing a pointer to
	// BlocklistIdentity
	// - One BlocklistIdentity per the sequence of linedefs that will be stored
	// without any overlaps from any other BlocklistIdentity

	// 1. Build collection of unique blocklists represented by BlocklistChunks
	// At this stage, there is 1-to-1 mapping between BlocklistChunks and BlocklistIdentity.
	// But the number of unique BlocklistIdentity will go down during subset
	// compression stage
	totalblocks := len(bm.blocklist)
	hashes := bm.gcShield.GetHashes(totalblocks)
	buckets := bm.gcShield.GetBuckets()
	blocks := bm.gcShield.GetBlocks(totalblocks)
	chunkAlloc := bm.gcShield.GetChunkAlloc(totalblocks)
	identityAlloc := bm.gcShield.GetIdentityAlloc(totalblocks)
	chunks := bm.gcShield.GetChunks(totalblocks)
	for i, blocklist := range bm.blocklist {
		uniqueBias := bm.flags[i]&BLOCKLIST_UNIQUE != 0
		var bucket uint64
		var hashblock uint16
		var hash2 uint64
		hash := blocklist.Hash2() // this will be used in later steps
		if !uniqueBias {
			bucket = hash % BBLOCKS_BUCKETS_COUNT // and this is used during this step only
			hashblock = buckets[bucket]
			hash2 = hash
		} else {
			// This list is not just unique - it can't be a part of any bigger
			// list. This was determined at construction stage and either of the
			// following (or both) is known to be true:
			// 1. It contains both a vertical and a horizontal line
			// 2. It contains a line that is only in this block and no other
			hashblock = 0xFFFF
			hash2 = uint64(0xFFFFFFFFFFFFFFFF) // so it's bigger than anything else
		}
		for hashblock != 0xFFFF {
			if CompareBlocklist(blocklist, bm.blocklist[hashblock]) {
				break
			}
			hashblock = hashes[hashblock]
		}
		if hashblock != 0xFFFF {
			// this exists already, reuse old BlocklistChunk
			blocks[i] = blocks[hashblock]
		} else {
			if !uniqueBias { // don't store lists that are impossible to match
				hashes[i] = buckets[bucket]
				buckets[bucket] = uint16(i)
			}
			// new BlocklistChunk to represent an unique blocklist
			identityAlloc[i] = BlocklistIdentity{
				data:    bm.blocklist[i],
				address: 0,
			}
			chunkAlloc[i] = BlocklistChunk{
				identity: &(identityAlloc[i]),
				offset:   0,
				hash:     hash,
				hash2:    hash2,
				tainted:  false,
			}
			chunk := &(chunkAlloc[i])
			chunks = append(chunks, chunk)
			blocks[i] = Block{
				chunk: chunk,
			}
		}
	}
	// 2. Perform subset compression so that minimal amount of BlocklistIdentity
	// instances remains referenced
	if config.AggressiveSubsets { // reference to global: config
		// Treat subsets as if they were DUPLICATES of bigger lists - zokumbsp
		// (v1.0.11) way
		// Yes, your game engine will test MORE lines in these blocks when using
		// this option
		bm.EliminateSubsets(chunks) // depends on chunk.hash to maintain "A is subset of B => hash(a) < hash(b)"
	} else {
		// Reorder linedefs in bigger lists so that smaller lists reference
		// accurately only what is present in them.
		// This produces no extra checks for game engine but a bit meager
		// savings of blockmap's size
		// Q: What happens when dummy linedefs are enabled?
		// A: one of the linedefs of the bigger block (that is not part of
		// smaller block) will serve as dummy. This is handled during lump
		// writing stage. Dummies don't have to be zero values.
		bm.CompressSubsets(chunks) // depends on chunk.hash to maintain "A is subset of B => hash(a) < hash(b)"
	}

	// 3. Determine order in which blocks should be written and also size, in words,
	// of memory needed to write all the blocklists
	identityToWriteLast, totalIdentityWords := bm.IdentifyOrderAndSize(chunks, blocks)
	part_min := 0
	part_max := 1
	if identityToWriteLast == nil {
		// single unique identity of zero size - write only once
		part_max = 0
	}

	if bm.stealOneWord {
		part_min = -1
		if part_max == 0 { // we have a single unique identity of zero size
			part_max = -1
		}
	}

	// 4. Write out
	blockOffsets := bm.gcShield.GetBlockOffsets(totalblocks)
	// allocate enough for ... the data that will be actually written, yes!
	lumpInitial := binary.Size(bm.header) + int(totalblocks*2)
	lumpSize := lumpInitial + totalIdentityWords*2
	var zeroLinedefBytes []byte
	if bm.useZeroHeader {
		zeroLinedefBytes = make([]byte, 2)
		binary.LittleEndian.PutUint16(zeroLinedefBytes, bm.zeroLinedef)
	}
	if bm.stealOneWord {
		lumpSize -= 2
	}

	// lumpSize is the size we already know we will write
	lumpWriter := CreateFixedWriterWrapper(bm.gcShield.GetLumpFromPool(lumpInitial, lumpSize), 0)
	// write header
	binary.Write(lumpWriter, binary.LittleEndian, bm.header)
	// Move writing cursor past block table storing offset to blocklists
	// - so that we are ready to write blocklists themselves
	lumpWriter.Seek(lumpInitial)

	for part := part_min; part <= part_max; part++ {
		// Let's rock!
		if part == -1 {
			// Writing blocklist of last block first (we know it has offset == 0)
			// (also, if we are here, dummy linedefs where enabled, and byte
			// stealing to compensate was deemed possible and that is exactly
			// what are we doing here)
			identity := blocks[len(blocks)-1].chunk.identity
			// yeah, substract one from offset - will point to its own cell,
			// that's the trick that "steals" the word
			intBlockOffset := len(lumpWriter.GetBytes())>>1 - 1
			identity.address = intBlockOffset
			bm.LimitCheck(intBlockOffset)
			blockOffsets[len(blockOffsets)-1] = uint16(intBlockOffset)
			// no need to write zero header - the last block's offset in block table
			// IS the zero header (we chosen the value of zero header so)
			what := identity.data
			if len(what) > 0 {
				lumpWriter.WriteWithOrder(binary.LittleEndian, what)
			}
			lumpWriter.Write(BLOCKLIST_TERM)
			continue
		} // else

		// Normal branch handles both writing every block except with the last identity
		// as well as writing precisely every block with the last identity -
		// depending on the value of "part" iteration variable
		for i, block := range blocks {
			identity := block.chunk.identity
			if (part == 0 && identity == identityToWriteLast) ||
				(part == 1 && identity != identityToWriteLast) {
				continue
			}
			if identity.address == 0 {
				intBlockOffset := len(lumpWriter.GetBytes()) >> 1
				// bm.LimitCheck not done here, intentional - see below
				identity.address = intBlockOffset
				if bm.useZeroHeader {
					lumpWriter.Write(zeroLinedefBytes)
				}
				what := identity.data
				if len(what) > 0 {
					lumpWriter.WriteWithOrder(binary.LittleEndian, what)
				}
				lumpWriter.Write(BLOCKLIST_TERM)
			}
			// This is where bm.LimitCheck is done. We need to check the final
			// address written
			bm.LimitCheck(identity.address + block.chunk.offset)
			blockOffsets[i] = uint16(identity.address + block.chunk.offset)
		}
	}

	// now that the block table is filled, let's write it!
	lumpWriter.Seek(binary.Size(bm.header))
	lumpWriter.WriteWithOrder(binary.LittleEndian, blockOffsets)

	// done
	data := lumpWriter.GetBytes()
	return data
}

// Accurate subset merging - doesn't make game do more linedef tests than it
// already does, for either block
func (bm *Blockmap) CompressSubsets(chunks []*BlocklistChunk) {
	// First, BlocklistChunks are sorted by blocklist size in descending order
	sort.Sort(BListBySizeDesc(chunks)) // see BListBySizeDesc.Less method

	// To speed up searching for subsets, we maintain a skiplist, which hosts
	// starting offset for each unique size. This allows us to find contigous
	// block of blocklists of smaller size than the current one immediately
	skiplist := bm.gcShield.GetSkipList(len(chunks))
	sz := 0 // if chunks start with zero size, they are all of zero size, and skiplist not needed anyway
	for cpos, chunk := range chunks {
		l := len(chunk.identity.data)
		if sz != l {
			skiplist = append(skiplist, cpos)
			sz = l
		}
	}

	// Make sure not to include blocklist of zero size (the one that contains
	// no lines
	// This is done so we don't accidently taint large blocklist that would
	// get placed at the end of blockmap (we want to fit all block offsets
	// within the limit)
	// There can be only zero chunk btw, and it is going to be the last then
	cutoffSkip := len(skiplist) - 1
	cutoffChunkIndex := len(chunks)
	nullSet := false
	if len(skiplist) >= 2 && len(chunks[len(chunks)-1].identity.data) == 0 {
		nullSet = true
		cutoffSkip--
		cutoffChunkIndex--
	}

	//var atmp BlockLines
	for i := 0; i < cutoffSkip; i++ {
		cpos := skiplist[i]
		// For each blocklist of current size, perform search of subsets
		subsetCandidatesStart := skiplist[i+1]
		for j := cpos; j < subsetCandidatesStart; j++ {
			bigOne := chunks[j]
			// atmp = nil
			for k := subsetCandidatesStart; k < cutoffChunkIndex; k++ {
				smallOne := chunks[k]
				if smallOne.offset != 0 {
					// already assigned to some other blocklist
					continue
				}
				// hash function was chosen so that
				// every subset has a smaller hash
				if bigOne.hash <= smallOne.hash2 {
					continue
				}
				smallOneData := smallOne.identity.data
				bigOneData := bigOne.identity.data
				if BMIsSubset(smallOneData, bigOneData, bigOne.offset) {
					BMReorder(smallOneData, bigOneData, bigOne.offset)
					smallOne.identity = bigOne.identity
					smallOne.offset = len(bigOneData) - len(smallOneData)
					bigOne.tainted = true
					break
				}
			}
		}
	}

	if nullSet { // special treatment
		// Merge null blocklist as a subset of arbitrary blocklist of smallest non-zero
		// size
		i := skiplist[len(skiplist)-2]
		bigOne := chunks[i]
		smallOne := chunks[len(chunks)-1]
		smallOne.identity = bigOne.identity
		smallOne.offset = len(bigOne.identity.data)
		bigOne.tainted = true
	}
}

// Zokumbsp's way of merging subsets. Subsets are eliminated altogether, replaced
// with a reference to the very beginning of big blocklist.
func (bm *Blockmap) EliminateSubsets(chunks []*BlocklistChunk) {
	// First, BlocklistChunks are sorted by blocklist size in descending order
	sort.Sort(BListBySizeDesc(chunks)) // see BListBySizeDesc.Less method

	// To speed up searching for subsets, we maintain a skiplist, which hosts
	// starting offset for each unique size. This allows us to find contiguous
	// block of blocklists of smaller size than the current one immediately
	skiplist := bm.gcShield.GetSkipList(len(chunks))
	sz := 0 // if chunks start with zero size, they are all of zero size, and skiplist not needed anyway
	for cpos, chunk := range chunks {
		l := len(chunk.identity.data)
		if sz != l {
			skiplist = append(skiplist, cpos)
			sz = l
		}
	}

	// Here no special treatment of zero-sized list is needed
	cutoffSkip := len(skiplist) - 1
	lenChunks := len(chunks)

	//var atmp BlockLines
	for i := 0; i < cutoffSkip; i++ {
		cpos := skiplist[i]
		// For each blocklist of current size, perform search of subsets
		subsetCandidatesStart := skiplist[i+1]
		for j := cpos; j < subsetCandidatesStart; j++ {
			bigOne := chunks[j]
			//atmp = nil
			for k := subsetCandidatesStart; k < lenChunks; k++ {
				smallOne := chunks[k]
				if smallOne.offset != 0 {
					// already assigned to some other blocklist
					continue
				}
				// hash function was chosen so that
				// every subset has a smaller hash
				if bigOne.hash <= smallOne.hash2 {
					continue
				}
				smallOneData := smallOne.identity.data
				bigOneData := bigOne.identity.data
				if BMIsSubsetSimple(smallOneData, bigOneData) {
					smallOne.identity = bigOne.identity
					smallOne.offset = -1 // just a marker to replace with 0 later
					// bigOne.tainted = true // no tainting here, offset is (will be) zero
					// No break here - we can still merge (subsume) things!
				}
			}
		}
	}

	// Fix offsets to be zero not -1
	for i := 0; i < lenChunks; i++ {
		chunks[i].offset = 0 // unconditionally to make things easier for CPU
	}
}

// LimitCheck checks that offset is within max vanilla or port limit, and sets
// respective flags within Blockmap structure
// For this to work, offset MUST have been calculated as int32 or wider (if you
// computed it as int16 to begin with, check would be useless)
func (bm *Blockmap) LimitCheck(intBlockOffset int) {
	if intBlockOffset > bm.largestOffset {
		bm.largestOffset = intBlockOffset
	}
	if intBlockOffset >= VANILLA_BM_OFFSET_TOO_BIG {
		bm.tooBigForVanilla = true
	}
	if intBlockOffset >= ANYPORT_BM_OFFSET_TOO_BIG {
		// Generated blockmap won't be valid, might have as well
		// stopped now
		bm.tooBigForUint16 = true
	}
}

// IdentifyOrderAndSize returns {last identity to be written, number of words
// for blocklist part of BLOCKMAP lump}
// Return values remain valid if no further manipulations are done on blocklists,
// that is subset compression, identical list merge, etc. must be done BEFORE
// this call
func (bm *Blockmap) IdentifyOrderAndSize(chunks []*BlocklistChunk, blocks []Block) (*BlocklistIdentity, int) {
	// TODO choosing last identity is tricky if BLOCKMAP "exceeds" the limit
	// It may become needed to unpack some subsets so that BLOCKMAP gets *slightly*
	// bigger but the STARTING offset referencing the rightmost chunk (rightmost
	// part of identity to be written last after adjustments) fits in.
	var identityToWriteLast *BlocklistIdentity
	var chunkToWriteLast *BlocklistChunk
	sz := 0
	totalIdentityWords := 0
	for _, chunk := range chunks {
		if !chunk.tainted && (len(chunk.identity.data)-chunk.offset) > sz {
			sz = len(chunk.identity.data) - chunk.offset
			identityToWriteLast = chunk.identity
			chunkToWriteLast = chunk
			if bm.stealOneWord && (identityToWriteLast == blocks[len(blocks)-1].chunk.identity) {
				// Decrease by just one so that another, distinct identity of the
				// same size may overtake it. Note that it won't be the same
				// identity, because chunk without tainted flag is the smallest
				// subset of this identity
				sz--
			}
		}
		if chunk.offset == 0 {
			totalIdentityWords += len(chunk.identity.data) + 1 // reserve word for FF FF marker
			if bm.useZeroHeader {
				totalIdentityWords += 1 // reserve for dummy linedef, too
			}
		}
	}

	// if the last block is written last, it can't be written first
	// it can't also be written first if it is a subset of another blocklist
	if bm.stealOneWord && ((identityToWriteLast == blocks[len(blocks)-1].chunk.identity) ||
		blocks[len(blocks)-1].chunk.offset != 0) {
		bm.stealOneWord = false
	}

	// Let's check if we compressed too much and have our last subset in tricky
	// position
	if config.AggressiveSubsets { // reference to global: config
		// ! But if was aggressive, nothing could be done at all for sure,
		// as no subsets remain - they were merged into full lists
		return identityToWriteLast, totalIdentityWords
	}
	// For accurate analysis, remember about byte stealing
	bmSize := binary.Size(bm.header) + len(blocks)*2 + totalIdentityWords*2
	if bm.stealOneWord {
		bmSize -= 2
	}
	if bmSize > (VANILLA_BM_OFFSET_TOO_BIG << 1) {
		// Note that blockmap's SIZE is only an issue if there is OFFSET to blocklist
		// that goes beyond it. If the greatest offset is within the limit, it is
		// fine even if the end of that blocklist is beyond it.
		tryLimit := (VANILLA_BM_OFFSET_TOO_BIG << 1)
		if bmSize > (ANYPORT_BM_OFFSET_TOO_BIG << 1) {
			tryLimit = (ANYPORT_BM_OFFSET_TOO_BIG << 1)
		}
		L := len(identityToWriteLast.data)
		if (bmSize-L+1 <= tryLimit) &&
			(bmSize-(L-chunkToWriteLast.offset)+1 > tryLimit) {
			// What happened is that the large blocklist written last fits in,
			// but some other blocklist that is merged as it is subset is not
			// And by de-merging it maybe the blockmap would get a bit bigger
			// but it might be possible (or not - depending on specific situation;
			// more tests are needed to ascertain) to get the offsets under limit
			// TODO so fix it here when possible
			bm.couldSqueeze = true // currently only tell user we might try this in newer versions
		}
	}
	return identityToWriteLast, totalIdentityWords
}

type BListBySizeDesc []*BlocklistChunk

func (x BListBySizeDesc) Len() int { return len(x) }
func (x BListBySizeDesc) Less(i, j int) bool {
	// Sort by hash2 improves things even when flags aren't used btw
	a := len(x[i].identity.data)
	b := len(x[j].identity.data)
	return (a > b) || ((a == b) && (x[i].hash2 > x[j].hash2)) // by size DESC, by hash2 DESC
}
func (x BListBySizeDesc) Swap(i, j int) { x[i], x[j] = x[j], x[i] }

func BMIsSubset(sOne BlockLines, bOne BlockLines, bOneOffset int) bool {
	// Assumes len(bOne[bOneOffset:]) > len(sOne)
	// Assumes that linedef in both input blocklist are ordered in ascending order,
	// and that no linedef occurs twice
	bPos := bOneOffset
	L1 := len(sOne)
	L2 := len(bOne)
	for i := 0; i < L1; i++ {
		found := false
		for j := bPos; j < L2; j++ {
			if bOne[j] > sOne[i] {
				return false
			} else if bOne[j] == sOne[i] {
				bPos = j + 1
				found = true
				break
			}
		}
		if !found {
			return false
		}

	}
	return true
}

func BMIsSubsetSimple(sOne BlockLines, bOne BlockLines) bool {
	// Assumes len(bOne) > len(sOne)
	// Assumes that linedef in both input blocklist are ordered in ascending order,
	// and that no linedef occurs twice
	// NOTE this one is simplified version of BMIsSubset, when no offsets are
	// ever used because subsets are eliminated altogether
	L1 := len(sOne)
	L2 := len(bOne)
	for i := 0; i < L1; i++ {
		found := false
		for j := 0; j < L2; j++ {
			if bOne[j] == sOne[i] {
				found = true
				break
			} else if bOne[j] > sOne[i] {
				return false
			}
		}
		if !found {
			return false
		}

	}
	return true
}

func BMReorder(sOne BlockLines, bOne BlockLines, bOneOffset int) {
	// Assumes that BMIsSubset returned true for this and doesn't do any checks
	// Assumes that linedef in both input blocklist are ordered in ascending order,
	// and that no linedef occurs twice
	// Assumes both lists have non-zero size
	// Fuck GC, giving me hard times
	bPos := 0
	L1 := len(sOne)
	tmp := (bOne)[bOneOffset:]
	L2 := len(tmp)
	lastPos := L2 - 1
	// Shift everything that is in sOne to the end of bOne as it is found
	for i := 0; i < L1; i++ {
		j := 0
		for j = bPos; j < L2; j++ {
			if tmp[j] == (sOne)[i] { // should always happen somewhere
				break
			}
		}
		a := tmp[j]
		L2 = L2 - 1
		// shift everything following the found value to the left
		for k := j; k < L2; k++ {
			tmp[k] = tmp[k+1]
		}
		// shift everything at the end to the left, as the found value has to
		// be placed rightmost
		for k := L2; k < lastPos; k++ {
			tmp[k] = tmp[k+1]
		}
		// put the last found value at rightmost position
		tmp[lastPos] = a
		bPos = j
	}
}

func (gcs *BlockmapGCShield) GetBlocklist(size int) []BlockLines {
	if gcs == nil {
		blocklist := make([]BlockLines, size)
		for ibl := 0; ibl < size; ibl++ {
			blocklist[ibl] = make([]uint16, 0, 8)
		}
		return blocklist
	} else {
		for ibl := 0; ibl < size; ibl++ {
			gcs.blocklist[ibl] = gcs.blocklist[ibl][:0]
		}
	}
	return gcs.blocklist[:size]
}

func (gcs *BlockmapGCShield) GetHashes(size int) []uint16 {
	if gcs == nil {
		return MakeRegion_FF(size)
	} else {
		gcs.hashes = gcs.hashes[:size]
		gcs.hashes[0] = 0xFFFF
		for j := 1; j < size; j = j << 1 {
			copy(gcs.hashes[j:], gcs.hashes[:j])
		}
	}
	return gcs.hashes
}

func (gcs *BlockmapGCShield) GetBuckets() []uint16 {
	if gcs == nil {
		return MakeRegion_FF(BBLOCKS_BUCKETS_COUNT)
	} else {
		gcs.buckets[0] = 0xFFFF
		for j := 1; j < BBLOCKS_BUCKETS_COUNT; j = j << 1 {
			copy(gcs.buckets[j:], gcs.buckets[:j])
		}
	}
	return gcs.buckets[0:]
}

func BlockmapCreateGCShield(size int, arcane bool) *BlockmapGCShield {
	a := &BlockmapGCShield{
		blocklist:    make([]BlockLines, size),
		hashes:       make([]uint16, size),
		blockOffsets: make([]uint16, size),
		flags:        make([]uint8, size),
	}
	for i := 0; i < size; i++ {
		a.blocklist[i] = make([]uint16, 0, 8)
	}
	if arcane {
		a.blocks = make([]Block, size)
		a.chunkAlloc = make([]BlocklistChunk, size)
		a.identityAlloc = make([]BlocklistIdentity, size)
		a.chunks = make([]*BlocklistChunk, 0, size)
		a.skiplist = make([]int, 0, size)
	}
	return a
}

func (gcs *BlockmapGCShield) GetBlockOffsets(size int) []uint16 {
	if gcs == nil {
		blockOffsets := make([]uint16, size)
		return blockOffsets
	}
	// No need to clear - each array entry always overwritten
	return gcs.blockOffsets[:size]
}

func (gcs *BlockmapGCShield) GetBlocks(size int) []Block {
	if gcs == nil {
		return make([]Block, size)
	}
	// No need to clear - each array entry always overwritten
	return gcs.blocks[:size]
}

func (gcs *BlockmapGCShield) GetChunkAlloc(size int) []BlocklistChunk {
	if gcs == nil {
		return make([]BlocklistChunk, size)
	}
	// No need to clear - each array entry always overwritten
	return gcs.chunkAlloc[:size]
}

func (gcs *BlockmapGCShield) GetIdentityAlloc(size int) []BlocklistIdentity {
	if gcs == nil {
		return make([]BlocklistIdentity, size)
	}
	// No need to clear - each array entry always overwritten
	return gcs.identityAlloc[:size]
}

func (gcs *BlockmapGCShield) GetChunks(size int) []*BlocklistChunk {
	if gcs == nil {
		return make([]*BlocklistChunk, 0, size)
	}
	return gcs.chunks[:0]
}

func (gcs *BlockmapGCShield) GetSkipList(size int) []int {
	if gcs == nil {
		return make([]int, 0, size)
	}
	return gcs.skiplist[:0]
}

func (gcs *BlockmapGCShield) GetFlags(size int) []uint8 {
	var flags []uint8
	if gcs == nil {
		flags = make([]uint8, size)
	} else {
		flags = gcs.flags[:size]
	}
	flags[0] = uint8(0)
	for j := 1; j < size; j = j << 1 {
		copy(flags[j:], flags[:j]) // fill big array with zeros fast trick
	}
	return flags
}

func (gcs *BlockmapGCShield) getUnusedSlot() byte {
	if gcs == nil { // shouldn't be called at all in this case
		return 0
	}
	for i := byte(0); i < 3; i++ {
		if gcs.lumpPool.slots[i] == BMStateUnused {
			return i
		}
	}
	panic("No unused slots in lump pool.")
	return 100 // out of range of course (shouldn't happen)
}

// Called when lump data from current slot is selected as best blockmap, so
// it remains or can (concurrency) remain pinned by BlockmapHive for 2
// generations (until RotateLumpPool gets called 2 more times), after which
// it becomes safe to recycle again for writing new blockmap lump
func (gcs *BlockmapGCShield) RotateLumpPool() {
	if gcs == nil { // shouldn't be called at all in this case
		return
	}
	for i := 0; i < 3; i++ {
		switch gcs.lumpPool.slots[i] {
		case BMStateNew:
			{ // This one might still be pinned by BlockmapHive
				gcs.lumpPool.slots[i] = BMStateOld
			}
		case BMStateOld:
			{ // This one is surely gone from BlockmapHive by now and can be
				//reused to make new blockmaps
				gcs.lumpPool.slots[i] = BMStateUnused
			}
		}
	}
	gcs.lumpPool.slots[gcs.lumpPool.lastSlot] = BMStateNew
}

func (gcs *BlockmapGCShield) GetLumpFromPool(expectedSize, expectedCapacity int) []byte {
	if gcs == nil {
		return make([]byte, expectedSize, expectedCapacity)
	}
	if expectedCapacity > MAX_ADDRESSABLE_BLOCKMAP_SIZE {
		// Such blockmap is too large for any port anyway
		expectedCapacity = MAX_ADDRESSABLE_BLOCKMAP_SIZE
	}
	unusedSlot := gcs.getUnusedSlot()
	start := int(unusedSlot) * MAX_ADDRESSABLE_BLOCKMAP_SIZE
	gcs.lumpPool.lastSlot = unusedSlot
	return gcs.lumpPool.pool[start : start+expectedSize : start+expectedCapacity]
}
