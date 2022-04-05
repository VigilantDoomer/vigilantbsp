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
	offsetYMax  int16
	offsetXMax  int16
	offsetXStep int16
	offsetYStep int16
	replyTo     chan<- BlockmapBeeOutput
	bailout     uint16
}

type BlockmapBeeOutput struct {
	data     []byte
	blockmap *Blockmap
}

// This is goroutine used to produce BLOCKMAP lump data and give it back
// to the thing that run it
func BlockmapGenerator(input BlockmapInput, where chan<- []byte) {
	var offsetXMax int16 // not inclusive, i.e. use < rather than <=
	var offsetYMax int16 // not inclusive, i.e. use < rather than <=
	var offsetStep int16
	start := time.Now()

	// Some lines may be excluded from blockmap, their vertices don't count
	// And neither should any vertices added from building nodes on a map that
	// was later edited
	input.bounds = RecomputeBounds(input.lines)
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
		if config.Deterministic { // global config
			// Currently, can't guarantee determinism when the search stops at first
			// good blockmap unless only one thread is used for the search
			// TODO might be possible to achieve determinism without this
			// degradation?
			Log.Printf("Forcing single-threaded mode for trying blockmap offsets - this is the only way I can be deterministic while allowing blockmap offset search to stop as soon as 'max offset satisfies limits' condition is reached.")
			beeCount = 1
		}
	}

	Log.Printf("Blockmap generator will use %d CPUs\n", beeCount)

	// So that no bee try the same offsets as any other one, each bee starts
	// on a different X offset with Y = 0, and then increase it by a offsetBeeStep
	// to jump over all offsets that would be tried by other bees. Exceeding
	// offsetXMax is taken care off in BlockmapBee - offsetY changes when that
	// happens
	offsetBeeStep := offsetStep * beeCount
	bees = make([]chan BlockmapBeeOutput, beeCount, beeCount)
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
		}
		go BlockmapBee(ctx, newInput, beeConfig)
	}
	res := BlockmapHive(ctx, cancel, bees)
	Log.Printf("Blockmap took %s\n", time.Since(start))
	where <- res
}

// BlockmapHive collects the smallest BLOCKMAP off bees and returns it
// to BlockmapQueen. Also a regular function called from BlockmapQueen, waiting
// synchronously for bees to complete their work.
func BlockmapHive(ctx context.Context, cancel context.CancelFunc,
	bees []chan BlockmapBeeOutput) []byte {
	beeCount := len(bees)
	NA := reflect.Value{}
	var best BlockmapBeeOutput
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
	for len(branches) > 0 {
		chi, recv, recvOk := reflect.Select(branches)
		if !recvOk { // channel closed
			branches = BMHive_DeleteBranch(branches, chi)
			continue
		}
		if !cancelled {
			work := (recv.Interface()).(BlockmapBeeOutput) // got best effort of bee #chi
			// But is it best among all bees' work?
			if !everRun { // this bee is the first to report, so store result as best so far
				everRun = true
				best = work
			} else {
				// check against previous best
				if IsBlockmapBetter(work.data, work.blockmap, len(best.data), best.blockmap) {
					// Previous best value is now subject to garbage collection
					best = work
				}
			}

			if ctx != nil {
				if IsBlockmapGoodEnough(best.blockmap) {
					Log.Printf("Found blockmap that is within limits, aborting futher search.\n")
					cancel()
					cancelled = true
					// Now all bees shutdown, and must reach by closing their channel,
					// which results in clean exit from the loop once all channels
					// are closed (as usual)
				}
			}
		}

	} // loop is exited when all branches are deleted (all channels closed)
	StatBlockmap(best.blockmap)
	DeleteIfTooBig(best.blockmap, &(best.data))
	return best.data
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
					close(beeConfig.replyTo) // allow Hive to exit loop
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
		if !bailedOut {
			bm := CreateBlockmap(bmConfig)
			if config.SubsetCompressBlockmap { // another reference to global variable "config"
				data = bm.GetBytesArcane()
			} else {
				data = bm.GetBytes()
			}
			bmConfig.gcShield = bm.gcShield
			if !everRun || IsBlockmapBetter(data, bm, lenOld, oldBm) {
				// Found first / better blockmap
				everRun = true
				lenOld = len(data)
				bm.blocklist = nil
				oldBm = bm
				// only rotating when data is uploaded to Hive, otherwise can
				// use the same slot for generating another blockmap data
				bmConfig.gcShield.RotateLumpPool()
				if ctx != nil { // user specified that cancellation is possible
					select { // here we wait either for Hive to read, or to cancel us
					case beeConfig.replyTo <- BlockmapBeeOutput{ // send it to Hive
						data:     data,
						blockmap: bm}:
						{

						}
					case <-ctx.Done():
						{ // we were cancelled, we're done and must signal it
							close(beeConfig.replyTo) // allow Hive to exit loop
							return
						}
					}
				} else {
					beeConfig.replyTo <- BlockmapBeeOutput{ // send it to Hive
						data:     data,
						blockmap: bm}
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
	close(beeConfig.replyTo)
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
func RecomputeBounds(lines AbstractLines) LevelBounds {
	Xmax := -2147483648
	Ymax := -2147483648
	Xmin := 2147483647
	Ymin := 2147483647
	linesCount := lines.Len()
	for i := uint16(0); i < linesCount-1; i++ {
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
