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

import (
	"context"
	"reflect"
	"runtime"
	"time"
)

// If a single offset is to be tried, BlockmapGenerator goroutine does the work
// itself in a single thread.
// If multiple offsets are to be tried, BlockmapGenerator calls BlockmanQueen,
// which setups "bees" (BlockmapBee goroutines) that will run on their own
// threads, and calls BlockmapHive which is where bees will report their
// results. (Honey = blockmap)

// The limit is only applied if user didn't specify the number of threads
// explicitly
// There is high memory consumption in BlockmapLumpPool, just keep it in mind
// if you are increasing this.
const MAX_BEES = 16

// A blockmap where the greatest offset value for the start of some block's
// blocklist is FFFF (65535), which has dummy linedef, followed by every of
// 65536 possible linedefs, then by FF FF terminal marker
const MAX_ADDRESSABLE_BLOCKMAP_SIZE = 65535*2 + 2 + 65536*2 + 2

const MIN_MAP_COORD = -32768

type BlockmapBeeInput struct {
	offsetYMax   int16
	offsetXMax   int16
	offsetXStep  int16
	offsetYStep  int16
	replyTo      chan<- BlockmapBeeOutput
	triedOffsets chan<- TriedOffsets
	bailout      uint16
	sieveMode    bool
}

type BlockmapBeeOutput struct {
	data     []byte
	blockmap *Blockmap
	XOffset  int16
	YOffset  int16
	aborted  bool // skipped by heuristic
}

type SieveItemNoBool struct {
	XOffset int16
	YOffset int16
}

type SieveItem struct {
	XOffset int16
	YOffset int16
	Marked  bool
}

type TriedOffsets struct {
	data []SieveItemNoBool
}

type OffsetSieve struct {
	values []SieveItem
}

// This is goroutine used to produce BLOCKMAP lump data and give it back
// to the thing that run it
func BlockmapGenerator(input BlockmapInput, where chan<- []byte,
	levelFormat int, sectors []Sector, sidedefs []Sidedef) {
	var offsetXMax int16 // not inclusive, i.e. use < rather than <=
	var offsetYMax int16 // not inclusive, i.e. use < rather than <=
	var offsetStep int16
	start := time.Now()

	if config.RemoveNonCollideable { // reference to global: config
		input.linesToIgnore = RemoveLinesFromBlockmap(input.linesToIgnore,
			input.lines, levelFormat, sectors, sidedefs)
	}

	// Some lines may be excluded from blockmap, their vertices don't count
	// And neither should any vertices added from building nodes on a map that
	// was later edited
	input.bounds = RecomputeBounds(input.lines, input.linesToIgnore)
	Log.Printf("Blockmap: collidable part of map goes from (%d,%d) to (%d,%d). These values are before offsets would be applied.\n",
		input.bounds.Ymax, input.bounds.Xmin, input.bounds.Ymin, input.bounds.Xmax)

	// Depending on what the bounds are, some offsets or even all non-zero
	// offsets might be unavailable. Take that into account!
	XCutoff, YCutoff := CutoffOffset(input.bounds)
	mode := ChooseModeWithCutoff(config.BlockmapOffsetMode, // reference to global: config
		&offsetXMax, &offsetYMax, &offsetStep, XCutoff, YCutoff)
	// Now offsetXMax, offsetYMax, offsetStep were set for multi-blockmap modes!

	switch mode {
	case BM_OFFSET_THIRTYSIX:
		{ // Builds 36 blockmaps - (40,40) is the final offset for blockmap to be generated
			BlockmapQueen(offsetXMax, offsetYMax, offsetStep, input, where,
				0, start)
		}
	case BM_OFFSET_BRUTEFORCE:
		{ // Careful what you wish for (Best of all 65536 offset combinations)
			// blockmaps are built for every value in 0-127 range inclusive
			// alongside both axises
			BlockmapQueen(offsetXMax, offsetYMax, offsetStep, input, where,
				0, start)
		}
	case BM_OFFSET_HEURISTIC:
		{ // Heuristic method to reduce from 65536 offsets
			bailout := uint16(0)
			xblocks := uint16(input.bounds.Xmax-input.bounds.Xmin)>>BLOCK_BITS + 1
			yblocks := uint16(input.bounds.Ymax-input.bounds.Ymin)>>BLOCK_BITS + 1
			ratio := float32(xblocks) / float32(yblocks)
			if (ratio < 1.15) && (ratio > 0.85) {
				// when maps are close to quadratic, we try all offsets (credit: zokumbsp)
				// that means, we are running as in BM_OFFSET_BRUTEFORCE mode
				Log.Printf("Will try all 65536 offsets for blockmap - map is close to quadratic.\n")
				bailout = 0
			} else {
				bailout = (xblocks + 1) * (yblocks + 1)
			}
			BlockmapQueen(offsetXMax, offsetYMax, offsetStep, input, where,
				bailout, start)
		}
	default:
		{ // BM_OFFSET_FIXED goes here
			// Single offset
			// It is computed using one thread of course
			// No helper goroutines or subroutines are needed
			if input.XOffset > XCutoff || input.YOffset > YCutoff {
				Log.Printf("Offset x,y=%d,%d would move map out of bounds, resetting blockmap offset to 0,0. (Maximum possible offset would be %d,%d)\n",
					input.XOffset, input.YOffset, XCutoff, YCutoff)
				input.XOffset = 0
				input.YOffset = 0
			}
			bm := CreateBlockmap(input)
			var data []byte
			if config.SubsetCompressBlockmap {
				data = bm.GetBytesArcane()
			} else {
				data = bm.GetBytes()
			}
			StatBlockmap(bm)
			DeleteIfTooBig(bm, &data)
			Log.Printf("Blockmap took %s\n", time.Since(start))
			where <- data
		}
	}
}

// A regular procedure called from BlockmapGenerator goroutine
// Sets up a hive of bees to split the "build multiple blockmaps" workload
// roughly even among the concurrent goroutines (called bees)
func BlockmapQueen(offsetXMax int16, offsetYMax int16, offsetStep int16,
	input BlockmapInput, where chan<- []byte, bailout uint16, start time.Time) {

	var bees []chan BlockmapBeeOutput
	beeCount := config.BlockmapThreads // global config

	if beeCount == 0 { // auto mode
		beeCount = int16(runtime.NumCPU())
		if beeCount > MAX_BEES { // the limit is only applied in auto mode
			beeCount = MAX_BEES
		}
	}
	if beeCount < 1 { // just in case something goes wrong
		beeCount = 1
	}

	var ctx context.Context
	var cancel context.CancelFunc
	if config.BlockmapSearchAbortion != BM_OFFSET_NOABORT { // global config
		ctx, cancel = context.WithCancel(context.Background())
	}

	Log.Printf("Blockmap generator will use %d CPUs\n", beeCount)

	deterministic := config.Deterministic // reference to global: config
	var sieve *OffsetSieve
	var offsetAggregators []chan TriedOffsets
	if deterministic && beeCount > 1 && ctx != nil {
		// sieve is used to track offsets for which blockmaps were not produced
		// (because threads outpace each other) before the process was
		// interrupted
		sieve = BMCreateOffsetSieve(offsetXMax, offsetYMax, offsetStep)
		offsetAggregators = make([]chan TriedOffsets, beeCount)
	}

	// So that no bee try the same offsets as any other one, each bee starts
	// on a different X offset with Y = 0, and then increase it by a offsetBeeStep
	// to jump over all offsets that would be tried by other bees. Exceeding
	// offsetXMax is taken care off in BlockmapBee - offsetY changes when that
	// happens
	offsetBeeStep := offsetStep * beeCount
	bees = make([]chan BlockmapBeeOutput, beeCount)
	var i int16
	for i = 0; i < beeCount; i++ {
		bees[i] = make(chan BlockmapBeeOutput)
		newInput := input
		newInput.XOffset = offsetStep * i
		newInput.YOffset = 0
		beeConfig := BlockmapBeeInput{
			offsetYMax:  offsetYMax,
			offsetXMax:  offsetXMax,
			offsetXStep: offsetBeeStep,
			offsetYStep: offsetStep,
			replyTo:     bees[i],
			bailout:     bailout,
			sieveMode:   sieve != nil,
		}
		if beeConfig.sieveMode {
			offsetAggregators[i] = make(chan TriedOffsets)
			beeConfig.triedOffsets = offsetAggregators[i]
		}
		go BlockmapBee(ctx, newInput, beeConfig)
	}
	res := BlockmapHive(ctx, cancel, bees, sieve, input, offsetAggregators,
		bailout)
	Log.Printf("Blockmap took %s\n", time.Since(start))
	where <- res
}

// BlockmapHive collects the smallest BLOCKMAP off bees and returns it
// to BlockmapQueen. Also a regular function called from BlockmapQueen, waiting
// synchronously for bees to complete their work.
func BlockmapHive(ctx context.Context, cancel context.CancelFunc,
	bees []chan BlockmapBeeOutput, sieve *OffsetSieve, input BlockmapInput,
	offsetAggregators []chan TriedOffsets, bailout uint16) []byte {
	beeCount := len(bees)
	NA := reflect.Value{}
	var best BlockmapBeeOutput
	var bestXOffset, bestYOffset int16
	everRun := false
	cancelled := false
	// Because the number of channels to listen on is determined in run-time, we
	// have to resort to reflect package (rather than use "select" keyword that
	// only works for predetermined number and type of cases) to make a "select"
	// inside loop
	branches := make([]reflect.SelectCase, beeCount, beeCount)
	for i := 0; i < beeCount; i++ {
		branches[i] = reflect.SelectCase{
			Dir:  reflect.SelectRecv,
			Chan: reflect.ValueOf(bees[i]),
			Send: NA,
		}
	}
	// 'chase' indicates whether to continue getting data even when further
	// mining was cancelled, instead of ignoring it (on best effort basis, since
	// threads will cancel themselves soon and thus blockmap results for certain
	// offsets may still be absent)
	chase := sieve != nil
	for len(branches) > 0 {
		chi, recv, recvOk := reflect.Select(branches)
		if !recvOk { // channel closed
			branches = BMHive_DeleteBranch(branches, chi)
			continue
		}
		if !cancelled || chase {
			work := (recv.Interface()).(BlockmapBeeOutput) // got best effort of bee #chi
			if cancelled && chase {
				Log.Verbose(1, "Blockmap generator: cancelled because reached endgoal but still getting (%d,%d) to ensure determinism\n", work.XOffset, work.YOffset)
			}
			if !work.aborted { // aborted happens in heuristic mode when determinism, multiple threads and endgoal (good enough blockmap) are present
				// But is it best among all bees' work?
				if !everRun { // this bee is the first to report, so store result as best so far
					everRun = true
					best = work
					bestXOffset = work.XOffset
					bestYOffset = work.YOffset
				} else {
					// check against previous best
					if sieve == nil {
						if IsBlockmapBetter(work.data, work.blockmap, len(best.data), best.blockmap) {
							// Previous best value is now subject to garbage collection
							best = work
							bestXOffset = work.XOffset
							bestYOffset = work.YOffset
						}
					} else {
						// We have to provide deterministic result in
						// multi-threaded mode with an endgoal
						// Since we have an endgoal, we need not the best, but
						// we need the EARLIEST (determinism) good enough
						// possible, as would be encountered if we were trying
						// them sequentially
						if (!IsBlockmapGoodEnough(best.blockmap) &&
							IsBlockmapBetter(work.data, work.blockmap, len(best.data), best.blockmap)) ||
							(OffsetPreceedes(work.XOffset, work.YOffset, bestXOffset, bestYOffset) &&
								IsBlockmapGoodEnough(work.blockmap)) {
							// Previous best value is now subject to garbage collection
							best = work
							bestXOffset = work.XOffset
							bestYOffset = work.YOffset
						}
					}
				}

				if ctx != nil {
					if IsBlockmapGoodEnough(best.blockmap) {
						if !cancelled {
							Log.Printf("Found blockmap that is within limits, aborting futher search.\n")
							cancel()
							cancelled = true
						}
						// Now all bees shutdown, and must reach by closing their channel,
						// which results in clean exit from the loop once all channels
						// are closed (as usual)
					}
				}
			}
		}

	} // loop is exited when all branches are deleted (all channels closed)

	// Now collect all tried offsets, IF needed
	BMHive_AggregateAll(offsetAggregators, sieve, cancelled)

	// Now see if we still need to check some untried offsets
	cntHoles := 0
	if cancelled {
		cntHoles = sieve.CountHoles(bestXOffset, bestYOffset)
	} // else there can't possibly be holes - no good enough blockmap for endgoal, we tried all offsets
	if cntHoles > 0 {
		// We have an offset that produces good enough blockmap with regards to
		// endgoal, but we have no data for some of the earlier offsets that
		// might also produce good enough blockmap. Since user requested
		// determinism, we must check all those offsets too, and we must always
		// use the earliest which gives good enough blockmap, even if it is worse
		// than the best we've found
		best = BlockmapHoleChaser(&best, sieve, bestXOffset, bestYOffset,
			input, bailout, cntHoles)
		bestXOffset = best.XOffset
		bestYOffset = best.YOffset
	} else if sieve != nil {
		Log.Printf("Blockmap generator: I happened to not need backtracking when looking for good enough blockmap under determinism in multi-threaded mode.")
	}

	Log.Printf("Blockmap offset: (%d,%d)\n", bestXOffset, bestYOffset)
	StatBlockmap(best.blockmap)
	DeleteIfTooBig(best.blockmap, &(best.data))
	return best.data
}

func OffsetPreceedes(newX, newY, oldX, oldY int16) bool {
	return newY < oldY || (newY == oldY && newX < oldX)
}

func BMHive_AggregateAll(offsetsAggregators []chan TriedOffsets,
	sieve *OffsetSieve, cancelled bool) {
	if offsetsAggregators == nil {
		return
	}
	if sieve == nil {
		Log.Panic("Inconsistency error (programmer error): offsetsAggregators is not nil but sieve is nil.")
	}
	beeCount := len(offsetsAggregators)
	NA := reflect.Value{}
	branches := make([]reflect.SelectCase, beeCount, beeCount)
	for i := 0; i < beeCount; i++ {
		branches[i] = reflect.SelectCase{
			Dir:  reflect.SelectRecv,
			Chan: reflect.ValueOf(offsetsAggregators[i]),
			Send: NA,
		}
	}
	for len(branches) > 0 {
		// We must retrieve all answers regardless of whether we need them
		// - this unblocks the channel and let's those Bee goroutines terminate
		chi, recv, recvOk := reflect.Select(branches)
		if !recvOk { // channel closed
			branches = BMHive_DeleteBranch(branches, chi)
			continue
		}
		if cancelled {
			// Processing offsets is only meaningful when process was
			// "cancelled" - we found good enough blockmap. If no blockmap is
			// good enough, then we know that all offsets were tried already
			offsets := (recv.Interface()).(TriedOffsets)
			for _, item := range offsets.data {
				sieve.Mark(item.XOffset, item.YOffset)
			}
		}
	}
}

func BMHive_DeleteBranch(branches []reflect.SelectCase, chi int) []reflect.SelectCase {
	for i := chi; i < (len(branches) - 1); i++ {
		branches[i] = branches[i+1]
	}
	return branches[:(len(branches) - 1)]
}

// each BlockmapBee is run on its own goroutine and hopefully thread,
// communicating with BlockmapHive
func BlockmapBee(ctx context.Context, bmConfig BlockmapInput, beeConfig BlockmapBeeInput) {
	var lenOld int
	var oldBm *Blockmap
	everRun := false
	var data []byte
	var offsetsTried []SieveItemNoBool
	if beeConfig.sieveMode {
		offsetsTried = make([]SieveItemNoBool, 0)
	}

	gcShield := BlockmapCreateGCShield(BMBeeMaxSize(bmConfig.bounds),
		config.SubsetCompressBlockmap) // reference to global variable "config"
	bmConfig.gcShield = gcShield

	// depending on how many bee goroutines were started, starting XOffset
	// might not even be within the limit
	for bmConfig.XOffset >= beeConfig.offsetXMax {
		bmConfig.XOffset -= beeConfig.offsetXMax
		bmConfig.YOffset += beeConfig.offsetYStep
	}
	for bmConfig.YOffset < beeConfig.offsetYMax {
		bailedOut := false
		if ctx != nil {
			// Provision to cancel when blockmap is judged good enough by Hive
			select {
			case <-ctx.Done():
				{
					// we were cancelled, we're done and must signal it
					BlockmapBeeFinalize(beeConfig, offsetsTried)
					return
				}
			default:
				{
					// free pass (makes select statement non-blocking) - we are
					// still doing our job (will proceed to execute code below)
				}
			}
		}
		// Log.Printf("Computing blockmap at (%d,%d) offset\n", bmConfig.XOffset, bmConfig.YOffset)
		if beeConfig.bailout > 0 {
			// Check block count against heuristics to early skip when testing 65536 offsets
			xmin := int(bmConfig.bounds.Xmin - bmConfig.XOffset)
			ymin := int(bmConfig.bounds.Ymin - bmConfig.YOffset)
			xmax := int(bmConfig.bounds.Xmax)
			ymax := int(bmConfig.bounds.Ymax)
			xblocks := uint16(xmax-xmin)>>BLOCK_BITS + 1
			yblocks := uint16(ymax-ymin)>>BLOCK_BITS + 1
			if beeConfig.bailout == (xblocks * yblocks) {
				bailedOut = true
				Log.Verbose(2, "Heuristics: offset (%d,%d) produces extra column and row\n", bmConfig.XOffset, bmConfig.YOffset)
			}
		}
		if beeConfig.sieveMode {
			// even if bailed out, mark this as done
			offsetsTried = append(offsetsTried, SieveItemNoBool{
				XOffset: bmConfig.XOffset,
				YOffset: bmConfig.YOffset,
			})
		}
		if !bailedOut {
			bm := CreateBlockmap(bmConfig)
			if config.SubsetCompressBlockmap { // another reference to global variable "config"
				data = bm.GetBytesArcane()
			} else {
				data = bm.GetBytes()
			}
			bmConfig.gcShield = bm.gcShield
			if !everRun || ((!beeConfig.sieveMode &&
				IsBlockmapBetter(data, bm, lenOld, oldBm)) ||
				(beeConfig.sieveMode && !IsBlockmapGoodEnough(oldBm) &&
					(IsBlockmapGoodEnough(bm) || IsBlockmapBetter(data, bm, lenOld, oldBm)))) {
				// Found first / better blockmap (or first good enough blockmap
				// in lieu of better, if deterministic and endgoal parameters
				// were used)
				everRun = true
				lenOld = len(data)
				bm.blocklist = nil
				oldBm = bm
				// only rotating when data is uploaded to Hive, otherwise can
				// use the same slot for generating another blockmap data
				bmConfig.gcShield.RotateLumpPool()

				if ctx != nil && !beeConfig.sieveMode {
					// user specified that cancellation is possible
					// but beware that in sieveMode we still need to send
					// computed result, as we already listed it
					select { // here we wait either for Hive to read, or to cancel us
					case beeConfig.replyTo <- BlockmapBeeOutput{ // send it to Hive
						data:     data,
						blockmap: bm,
						XOffset:  bmConfig.XOffset,
						YOffset:  bmConfig.YOffset}:
						{

						}
					case <-ctx.Done():
						{ // we were cancelled, we're done and must signal it
							BlockmapBeeFinalize(beeConfig, offsetsTried)
							return
						}
					}
				} else {
					beeConfig.replyTo <- BlockmapBeeOutput{ // send it to Hive
						data:     data,
						blockmap: bm,
						XOffset:  bmConfig.XOffset,
						YOffset:  bmConfig.YOffset}
				}

			}
			// now forget it, go mine next blockmap
			data = nil
		}
		keep := true
		oldYOffset := bmConfig.YOffset
		for keep { // run once if NOT bailing out, skip to the next row otherwise
			// TODO (low priority) improve performance - skip row smarter when bailing out xD
			bmConfig.XOffset += beeConfig.offsetXStep
			for bmConfig.XOffset >= beeConfig.offsetXMax {
				bmConfig.XOffset -= beeConfig.offsetXMax
				bmConfig.YOffset += beeConfig.offsetYStep
			}
			keep = bailedOut && bmConfig.YOffset == oldYOffset
		}
	}
	// We're done (here because we finished our part of work)
	BlockmapBeeFinalize(beeConfig, offsetsTried)
}

func BlockmapBeeFinalize(beeConfig BlockmapBeeInput, triedOffsets []SieveItemNoBool) {
	close(beeConfig.replyTo) // allow Hive to exit loop
	if beeConfig.sieveMode {
		// Ah, need to supply all the offsets we tried
		beeConfig.triedOffsets <- TriedOffsets{
			data: triedOffsets,
		}
		close(beeConfig.triedOffsets) // Hive will wait on this too (but at later point) in this mode
	}
}

func BMBeeMaxSize(b LevelBounds) int {
	xmin := int(b.Xmin)
	ymin := int(b.Ymin)
	xmax := int(b.Xmax)
	ymax := int(b.Ymax)
	xblocks := uint16(xmax-xmin)>>BLOCK_BITS + 1
	yblocks := uint16(ymax-ymin)>>BLOCK_BITS + 1
	return int((xblocks + 1) * (yblocks + 1))
}

func StatBlockmap(bm *Blockmap) {
	mayNag := false
	Log.Printf("Blockmap: largest block offset value: %d", bm.largestOffset)
	if bm.useZeroHeader {
		Log.Verbose(1, "Blockmap: dummy linedef %d, word stealing was used: %t\n",
			bm.zeroLinedef, bm.stealOneWord)
	} else {
		Log.Verbose(1, "Blockmap: didn't use dummy linedef\n")
	}
	if (bm.gcShield != nil) && (bm.largestOffset >= MAX_ADDRESSABLE_BLOCKMAP_SIZE) {
		Log.Printf("Blockmap lump required a buffer so large it got truncated.\n")
	}
	if bm.tooBigForUint16 {
		Log.Printf("Blockmap too large - contains offsets out of range for ANY port.\n")
		mayNag = true
	} else if bm.tooBigForVanilla {
		Log.Printf("Blockmap won't work in vanilla - requires port that treat offsets as unsigned.\n")
		mayNag = true
	}
	if mayNag && bm.couldSqueeze {
		Log.Printf("It seems it might be possible to put offsets within the limit by partially reversing subset compression for the largest blocklist. This logic, however, is not implemented in this version of program. (You may try to turn aggressive subset compression ON in meanwhile.)\n")
	}
}

func DeleteIfTooBig(bm *Blockmap, data *[]byte) {
	if bm.tooBigForUint16 {
		*data = nil
		Log.Error("Couldn't generate a valid blockmap (blockmap lump would contain invalid offsets because of its size). Will write 0-sized blockmap lump into output file instead.")
	}
}

func IsBlockmapBetter(newData []byte, newBM *Blockmap, lenOld int, oldBM *Blockmap) bool {
	// Fact: slightly bigger blockmap that has all of blocklist starting OFFSETS
	// WITHIN the limit is BETTER than smaller blockmap with some offsets out
	// of range!!!
	// In other words: it is better to have a blockmap that runs in your port
	// than the one that doesn't, and whether it does depends on what the offsets
	// are (the lump size itself is bound by 32-bit integer, where as offset
	// are stored as 16-bit integer. Vanilla treats both as signed, other ports
	// might treat it as unsigned instead, raising the limits but not
	// eliminating them)
	if oldBM.tooBigForUint16 && !newBM.tooBigForUint16 {
		return true
	}
	if oldBM.tooBigForVanilla && !newBM.tooBigForVanilla {
		return true
	}
	if newBM.tooBigForUint16 && !oldBM.tooBigForUint16 {
		// Don't downgrade!
		return false
	}
	if newBM.tooBigForVanilla && !oldBM.tooBigForVanilla {
		// Don't downgrade!
		return false
	}
	// Only when the ability to fit limits didn't change does length count
	if len(newData) < lenOld {
		return true
	} else if config.Deterministic && (len(newData) == lenOld) { // reference to global variable config
		// User requested: make any runs of the program on same input file
		// produce the same output. So order things to match what a
		// single-threaded crunch would produce.
		// Assume convention: earlier offset to be tried is preferred for
		// blockmaps of the same quality and size
		if newBM.header.YMin > oldBM.header.YMin { // smaller Y offset was used
			return true
		} else if (newBM.header.YMin == oldBM.header.YMin) &&
			(newBM.header.XMin > oldBM.header.XMin) { // smaller X offset was used for the same Y offset
			return true
		}
		return false
	}
	return false
}

func IsBlockmapGoodEnough(bm *Blockmap) bool {
	switch config.BlockmapSearchAbortion {
	case BM_OFFSET_FITS_VANILLA:
		{
			return !bm.tooBigForVanilla
		}
	case BM_OFFSET_FITS_ANYPORT:
		{
			return !bm.tooBigForUint16
		}
	default:
		{ // shouldn't actually reach here
			return true
		}
	}
}

// RecomputeBounds returns level boundaries produced from only those vertices
// which belong to the linedefs that will be included in blockmap. Anything
// removed from blockmap would not participate in bounds estimation.
// This functionality is only used for generating blockmap lump. On the contrary,
// blockmaps produced for internal purposes use a different logic for what gets
// included in blockmap, so these boundaries would not apply or potentially
// even be valid to them.
// Also, this operation is only executed once even if multiple blockmaps with
// different offsets are generated
func RecomputeBounds(lines AbstractLines, linesToIgnore []bool) LevelBounds {
	Xmax := -2147483648
	Ymax := -2147483648
	Xmin := 2147483647
	Ymin := 2147483647
	linesCount := lines.Len()
	for i := uint16(0); i < linesCount; i++ {
		if linesToIgnore != nil && linesToIgnore[i] {
			continue
		}
		if lines.BlockmapSkipThis(i) {
			continue
		}
		x1, y1, x2, y2 := lines.GetAllXY(i)

		if x1 < Xmin {
			Xmin = x1
		}
		if x1 > Xmax {
			Xmax = x1
		}
		if y1 < Ymin {
			Ymin = y1
		}
		if y1 > Ymax {
			Ymax = y1
		}

		if x2 < Xmin {
			Xmin = x2
		}
		if x2 > Xmax {
			Xmax = x2
		}
		if y2 < Ymin {
			Ymin = y2
		}
		if y2 > Ymax {
			Ymax = y2
		}
	}
	return LevelBounds{
		Xmin: int16(Xmin),
		Ymin: int16(Ymin),
		Xmax: int16(Xmax),
		Ymax: int16(Ymax),
	}
}

// Returns max possible offset, inclusive
func CutoffOffset(bounds LevelBounds) (int16, int16) {
	maxXOffset := 127 // inclusive
	maxYOffset := 127 // inclusive
	if int(bounds.Xmin)-maxXOffset < MIN_MAP_COORD {
		maxXOffset = int(bounds.Xmin) - MIN_MAP_COORD
	}
	if int(bounds.Ymin)-maxYOffset < MIN_MAP_COORD {
		maxYOffset = int(bounds.Ymin) - MIN_MAP_COORD
	}
	return int16(maxXOffset), int16(maxYOffset)
}

// Here offsets are not inclusive
func ChooseModeWithCutoff(mode int, offsetXMax, offsetYMax, offsetStep *int16,
	cutOffX, cutOffY int16) int {
	// TODO print message if the cutoff is applied and the number of
	// offsets to try is reduced
	switch mode {
	case BM_OFFSET_THIRTYSIX:
		{ // Builds 36 blockmaps - (40,40) is the final offset for blockmap to be generated
			*offsetXMax = 48
			*offsetYMax = 48
			*offsetStep = 8
		}
	case BM_OFFSET_BRUTEFORCE:
		{ // Careful what you wish for (Best of all 65536 offset combinations)
			// blockmaps are built for every value in 0-127 range inclusive
			// alongside both axises
			*offsetXMax = 128
			*offsetYMax = 128
			*offsetStep = 1
		}
	case BM_OFFSET_HEURISTIC:
		{ // Heuristic method to reduce from 65536 offsets
			*offsetXMax = 128
			*offsetYMax = 128
			*offsetStep = 1
		}
	default:
		{
			// cutoff not applied here, will be checked by whoever called us
			return BM_OFFSET_FIXED
		}
	}
	didCut := false
	if *offsetXMax > cutOffX+1 {
		*offsetXMax = cutOffX + 1
		didCut = true
	}
	if *offsetYMax > cutOffY+1 {
		*offsetYMax = cutOffY + 1
		didCut = true
	}
	if didCut {
		Log.Printf("Map bounds are such they would overflow with certain offsets. Limiting max X offset to %d, max Y offset to %d.\n",
			*offsetXMax-1, *offsetYMax-1)
	}
	// Currently the cutoff should work, but very suboptimally in some cases...
	return mode
}

func BMCreateOffsetSieve(offsetXMax, offsetYMax, offsetStep int16) *OffsetSieve {
	a := &OffsetSieve{
		values: make([]SieveItem, 0),
	}
	offsetX := int16(0)
	offsetY := int16(0)
	hasOffsets := true // at least (0,0) will be there sure
	for hasOffsets {
		a.values = append(a.values, SieveItem{
			XOffset: offsetX,
			YOffset: offsetY,
			Marked:  false,
		})
		offsetX += offsetStep
		for offsetX >= offsetXMax {
			offsetX -= offsetXMax
			offsetY += offsetStep
		}
		hasOffsets = offsetY < offsetYMax
	}
	return a
}

// Marks offset pair as visited (blockmap or heuristic was evaluated for this)
// to avoid computing blockmap a second time
// SLOOOOW. Need to represent sieve in a different way, with O(1) lookups not O(n)
func (s *OffsetSieve) Mark(XOffset, YOffset int16) {
	if s == nil {
		return
	}
	marked := false
	for i, item := range s.values {
		if item.XOffset == XOffset && item.YOffset == YOffset {
			if item.Marked {
				Log.Panic("This blockmap offset (%d,%d) was already registered within OffsetSieve (programmer error, shouldn't happen).\n",
					XOffset, YOffset)
			}
			s.values[i].Marked = true
			marked = true
		}
	}
	if !marked {
		Log.Panic("This blockmap offset  (%d,%d) is not present in OffsetSieve (programmer error, shouldn't happen).\n",
			XOffset, YOffset)
	}
}

func (s *OffsetSieve) CountHoles(LastXOffset, LastYOffset int16) int {
	if s == nil {
		return 0
	}
	cnt := 0
	for _, item := range s.values {
		if item.XOffset == LastXOffset && item.YOffset == LastYOffset {
			return cnt
		}
		if !item.Marked {
			cnt++
		}
	}
	Log.Panic("Point that should not be reachable has been reached (programmer error).")
	return cnt
}

// BlockmapHoleChaser iterates through all untried offsets in sieve till
// (LastXOffset, LastYOffset) non-inclusive, computes blockmap in sequential
// mode and returns it
// NOTE this is implemented as a single thread and would not necessary benefit
// from multi-threading. In fact, multi-threading could be detrimental if number
// of holes is very low, which is what indeed happens on maps I ran it on
func BlockmapHoleChaser(oldBest *BlockmapBeeOutput, sieve *OffsetSieve,
	LastXOffset, LastYOffset int16, input BlockmapInput, bailout uint16,
	cntHoles int) BlockmapBeeOutput {
	Log.Printf("Blockmap generator: backtracking to produce blockmap for %d skipped blockmap offsets to ensure determinism.\n",
		cntHoles)
	best := oldBest
	if !IsBlockmapGoodEnough(best.blockmap) {
		// BlockmapHoleChaser should not have been called
		Log.Panic("Assertion failure: backtracking after exhaustive search through all tried offsets already revealed that no blockmap satisfies the endgoal limits.")
	}
	gcShield := BlockmapCreateGCShield(BMBeeMaxSize(input.bounds),
		config.SubsetCompressBlockmap) // reference to global variable "config"
	input.gcShield = gcShield
	for _, item := range sieve.values {
		if item.XOffset == LastXOffset && item.YOffset == LastYOffset {
			Log.Printf("Backtracking didn't find better earlier blockmap offset, sticking to (%d,%d)\n", LastXOffset, LastYOffset)
			return *best
		}
		if !item.Marked {
			// needs to compute
			input.XOffset = item.XOffset
			input.YOffset = item.YOffset
			bailedOut := false
			if bailout > 0 {
				// Check block count against heuristics to early skip when testing 65536 offsets
				xmin := int(input.bounds.Xmin - input.XOffset)
				ymin := int(input.bounds.Ymin - input.YOffset)
				xmax := int(input.bounds.Xmax)
				ymax := int(input.bounds.Ymax)
				xblocks := uint16(xmax-xmin)>>BLOCK_BITS + 1
				yblocks := uint16(ymax-ymin)>>BLOCK_BITS + 1
				if bailout == (xblocks * yblocks) {
					bailedOut = true
					Log.Verbose(2, "Heuristics: offset (%d,%d) produces extra column and row\n", input.XOffset, input.YOffset)
				}
			}
			if bailedOut {
				continue
			}
			var data []byte
			bm := CreateBlockmap(input)
			if config.SubsetCompressBlockmap { // another reference to global variable "config"
				data = bm.GetBytesArcane()
			} else {
				data = bm.GetBytes()
			}
			input.gcShield = bm.gcShield
			if IsBlockmapGoodEnough(bm) {
				best = &BlockmapBeeOutput{
					data:     data,
					blockmap: bm,
					XOffset:  input.XOffset,
					YOffset:  input.YOffset,
					aborted:  false,
				}
				Log.Printf("Backtracking revealed better earlier blockmap offset at (%d,%d) instead of (%d,%d)\n",
					input.XOffset, input.YOffset, LastXOffset, LastYOffset)
				return *best
			}
		}
	}
	Log.Panic("The evaluated item was not present in the sieve (programmer error)\n")
	return *best
}

func RemoveLinesFromBlockmap(linesToIgnore []bool, lines AbstractLines,
	levelFormat int, sectors []Sector, sidedefs []Sidedef) []bool {
	cntLines := lines.Len()
	oldEfficiency := 0
	if linesToIgnore == nil {
		linesToIgnore = make([]bool, cntLines)
	} else {
		// need copy! the original is shared with sector and reject builder
		newLinesToIgnore := make([]bool, cntLines)
		for i, v := range linesToIgnore {
			if v {
				oldEfficiency++
			}
			newLinesToIgnore[i] = v
		}
		linesToIgnore = newLinesToIgnore
	}
	// ZokumBSP special switch. Disables removing non-collideable lines between
	// distinct sectors if any linedefs are capable of acting on multiple
	// sectors
	multiSectorSpecial := false
	if levelFormat == FORMAT_HEXEN {
		// In Hexen, specials can be executed via ACS and not necessarily
		// linedefs, thus the possibility of multi sector special existing can
		// not be ruled out
		multiSectorSpecial = true
	}
	if !multiSectorSpecial {
		multiSectorSpecial = hasMultiSectorSpecial(lines, cntLines)
	}

	// TODO Zokumbsp has one more removal feature (which seems to be active
	// always, rather than only when "non-collideable lines" removal is
	// enabled). It's description:
	// "Remove boundary walls in sectors that have 0 height, not tagged, not a door."
	// Guess we'll have to implement it also. But, it relies on exhaustive
	// definition of "door" and probably doesn't work well with other games
	// and/or advanced ports...

	cull := new(Culler)
	cull.SetMode(CREATE_BLOCKMAP, sidedefs)
	cull.SetAbstractLines(lines)
	cull.EnablePerimeterSink(true)

	for cid := uint16(0); cid < cntLines; cid++ {
		if linesToIgnore[cid] { // ignore dummy lines for fast scrollers
			continue
		}
		culled := cull.AddLine(cid)
		if culled {
			// Potentially non-collideable line with same sector on both sides
			linesToIgnore[cid] = true // might be changed later
		} else if !multiSectorSpecial {
			// In this branch, we are looking for potentially non-collideable
			// lines with different sectors on both sides
			// ! Branch is prevented from executions if map has specials that
			// act on multiple sectors (either through linedef, or map is in
			// Hexen format and sh*t can happen)
			firstSdef := lines.GetSidedefIndex(cid, true)
			secondSdef := lines.GetSidedefIndex(cid, false)
			investigate := FirstStageUncollideable(lines, sidedefs, cid,
				firstSdef, secondSdef)
			if !investigate || sidedefs[firstSdef].Sector ==
				sidedefs[secondSdef].Sector {
				continue
			}
			// we have two sided linedef, without any action, between two
			// different sectors
			fsector := sidedefs[firstSdef].Sector
			bsector := sidedefs[secondSdef].Sector
			// if floor and ceiling are the same, etc... then this linedef
			// should be safe to be considered non-collideable
			if sectors[fsector].FloorHeight == sectors[bsector].FloorHeight &&
				sectors[fsector].CeilHeight == sectors[bsector].CeilHeight &&
				sectors[fsector].Tag == 0 && sectors[bsector].Tag == 0 &&
				// timed doors sector specials
				sectors[fsector].Special != 10 && sectors[bsector].Special != 10 &&
				sectors[fsector].Special != 14 && sectors[bsector].Special != 14 {
				linesToIgnore[cid] = true
			}
		}
	}
	// Now let's see if we destroyed self-referencing sector effects with
	// our overly eager assumptions
	cull.Analyze()
	// inner lines may still be non-collideable, but perimeters of
	// self-referencing sectors have to be collideable
	sectorPerimeters := cull.GetPerimeters()
	for _, perimeters := range sectorPerimeters {
		for _, perimeter := range perimeters {
			for _, line := range perimeter {
				linesToIgnore[line] = false
			}
		}
	}
	// but if we failed analyzing some perimeter(s), we must assume that all
	// 2-sided same sector lines (within that sector where failure occured)
	// are collideable. Here is where all the screw-ups will land.
	// If all perimeters computed successfully (which is a desirable
	// scenario), this loop will have no data to iterate over
	for cull.SpewBack() {
		i := cull.GetLine()
		linesToIgnore[i] = false
	}

	efficiency := 0
	for _, v := range linesToIgnore {
		if v {
			efficiency++
		}
	}
	efficiency = efficiency - oldEfficiency
	Log.Printf("Blockmap: removed %d non-collideable lines.\n", efficiency)

	return linesToIgnore
}

// Returns true if there exist linedef with a type which affects or can
// affect multiple sectors (stairs, donuts)
func hasMultiSectorSpecial(lines AbstractLines, cntLines uint16) bool {
	for cid := uint16(0); cid < cntLines; cid++ {
		tpe := lines.GetAction(cid)
		// FIXME was copied from ZokumBSP. Sure as hell not sufficient,
		// Heretic, Hexen ffs! possibly other Boom types, too, and never forget
		// advanced engines
		// Zokum: we look for raising stairs and donuts
		switch tpe {
		case 9, 8, 127, 100, 7:
			{
				return true
			}
			// Heretic types - stairs:
		case 106, 107:
			{
				return true
			}
			// TODO Strife support
			// Boom types - stairs
		case 258, 256, 259, 257:
			{
				return true
			}
			// Boom types - donuts
		case 146, 155, 191:
			{
				return true
			}
		}
		// Generalized Boom types - stairs
		if tpe >= 0x3000 && tpe <= 0x33FF {
			return true
		}
		// There are no generalized donuts, thank goodness
	}
	return false

}
