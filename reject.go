// Copyright (C) 2022-2023, VigilantDoomer
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

// reject
package main

import (
	"sort"
	"time"
)

// NOTE Programmers beware: zennode's original reject code was already not
// exactly easy to understand (although I'm deeply indebted to its existence),
// and the code you are seeing here originated from translating that code from
// C++ to Go. Afterwards, a row of optimization and debugging followed, as
// things that are good in C++ may have bad performance in Go + implementing
// support for self-referencing sectors is not exactly walk in the park.
// NOTE The output is unlikely to be identical to zennode/zokumbsp's output (not
// to say those two even differ between themselves). It has to do with the fact
// that reject computation depends on blockmap computation, and the algorithm
// used is different, as it was derived from ZDBSP's code not zennodes' one.
// NOTE Unlike what happens in RMB program, GROUP in VigilantBSP is effectively
// a no-op unless other RMB options are present with which it must cooperate
// then. Certain RMB options (example: DISTANCE, LINE) modify reject building
// part rather directly, though, instead of being applied before/after main
//  course, as the result these will force VigilantBSP to use "slow path"
// instead of "fast path". But some other RMB options can still be combined with
// GROUP in a "fast path"

const VIS_UNKNOWN uint8 = 0x00
const VIS_VISIBLE uint8 = 0x01 // At least 1 valid LOS found
const VIS_HIDDEN uint8 = 0x02  // No actual LOS exists
/* Stuff from zennode dropped for actually making things worse and introducing
bugs when DISTANCE and INCLUDE were used
const VIS_RMB_VISIBLE uint8 = 0x04 // Special - ignores VIS_RMB_HIDDEN
const VIS_RMB_HIDDEN uint8 = 0x08  // Special - force sector to be hidden
const VIS_RMB_MASK uint8 = 0x0C    // Special - RMB option present*/

const UNLIMITED_DISTANCE uint64 = 0xFFFFFFFFFFFFFFFF

// Group of consts that decides what happens if self-referencing pedantic
// mode fails. This is important when RMB contains certain effects that might
// require 100% robustness in identifying sector position in relation to other
// sectors
const (
	PEDANTIC_FAIL_NOTMATTER = iota // no RMB effects are going to be ruined by this
	PEDANTIC_FAIL_REPORT           // will be screwed, no encounters yet
	PEDANTIC_FAIL_REPORTED         // screwed and encountered already
)

// Replaced with an empty struct in fast path
type RejectExtraData struct {
	// mixer intercepts VIS_HIDDEN directed at sectors which are part of some
	// groups (produced by RMB effect GROUP) when DFS tries to aggressively hide
	// sectors. This allows positive visibility to propagate between sectors in
	// the same group during DFS. Mixer is merged after DFS is complete, by
	// those VIS_HIDDEN values from mixer allowed to take over only VIS_UNKNOWN
	// values, but not VIS_VISIBLE values in reject table
	// Using mixer can and will result in significant slowdown, so even groups
	// by default don't use it. Need both GROUP and DISTANCE effect to be
	// present to justify the use of this, as GROUP must affect how distance
	// in map units is counted.
	mixer []byte
}

// RejectWork stands for reject work data
type RejectWork struct {
	// the stuff that goes into "extra" field disappears in "fast path"
	extra       RejectExtraData
	numSectors  int
	rejectTable []byte
	input       *RejectInput
	// the bulk of working data begins
	solidLines       []SolidLine
	transLines       []TransLine
	indexToSolid     []*SolidLine
	lineVisDone      []uint8 // bitarray, replaces Zennode's lineVisTable that held redundant information
	sectors          []RejSector
	slyLinesInSector map[uint16]bool // line with equal sector references on both sides. May indicate self-referencing sector
	maxDistance      uint64
	testLines        []*SolidLine
	polyPoints       []*IntVertex
	reSectors        []*RejSector
	lineMap          []*TransLine
	blockmap         *Blockmap
	loRow            int
	hiRow            int
	//blockity         *BlockityLines // was used to avoid 3D arrays and blockmap copying, but then I've found an even faster way
	blockMapBounds []BlockMapBounds
	src, tgt       TransLine        // memory optimization: used in testLinePair, which is not reentrant. Allocate once, reuse every time.
	p1, p2, p3, p4 IntVertex        // memory optimization: used in rejectLOS.go -> initializeWorld, another non-reentrant function
	seenLines      [65536 / 8]uint8 // replaces checkLine bool array, here we use unsigned bytes to store them compactly
	// groups
	hasGroups bool       // whether actual group functionality is on
	groups    []RejGroup // length should ALWAYS equal number of sectors
	// groupShareVis == true means groups need - faithful to RMB documentation -
	// confer visibility between groupsiblings to produce reject consistent with
	// RMB options.
	// groupShareVis == true implies groups == true, but implication does not
	// hold in reverse
	groupShareVis bool
	// rejectDFS.go junk
	graphTable GraphTable
	// misc
	linesToIgnore []bool // fast Doom scrollers dummy lines, etc.
	// rejectRMB.go junk
	rmbFrame          *RMBFrame
	RejectSelfRefMode int      // may differ from config's one because of RMB processing needs
	PedanticFailMode  int      // what to do if failed to be pedantic and reverted to "always visible" hack
	distanceTable     []uint16 // "distance in sector units" flattened i*j array
	maxLength         uint16   // initialized in CreateDistanceTable, may be overridden in ApplyDistanceTable
	fileControl       *FileControl
	mapName           string
	lineEffects       map[uint16]uint8
	specialSolids     []uint16   // all linedefs that need to be treated as solid even though they aren't
	drLine            *TransLine // memory optimization: used in non-reentrant divideRegion
}

type RejectInput struct {
	lines         AbstractLines
	bounds        LevelBounds
	sectors       []Sector
	sidedefs      []Sidedef
	bcontrol      chan BconRequest
	bgenerator    chan BgenRequest
	rejectChan    chan<- []byte
	linesToIgnore []bool
	rmbFrame      *RMBFrame
	fileControl   *FileControl
	mapName       string
}

type IntVertex struct {
	X int
	Y int
}

type SolidLine struct {
	index  uint16
	start  *IntVertex
	end    *IntVertex
	ignore bool
}

type TransLine struct {
	index          uint16
	start          *IntVertex
	end            *IntVertex
	frontSector    uint16
	backSector     uint16
	DY, DX, H      int
	lo, hi         float64
	loPoint        *IntVertex
	hiPoint        *IntVertex
	indexInLineVis int
}

type RejSector struct {
	index        int
	lines        []*TransLine
	neighbors    []*RejSector
	numLines     int
	numNeighbors int
	//isNeighbor   map[int]bool
	// graph stuff
	metric           int
	baseGraph, graph *RejGraph
	graphParent      *RejSector
	isArticulation   bool
	indexDFS         int
	loDFS            int
	hiDFS            int
}

type RejGraph struct {
	numSectors int
	sectors    []*RejSector
}

type GraphTable struct {
	numGraphs   int
	graphs      []RejGraph
	sectorStart []*RejSector
	sectorPool  []*RejSector
}

type Neighboring struct {
	smallIndex int
	bigIndex   int
}

// A group is initially created for every sector and includes that sector
// In fact, it continues to include it even after that sector joins some other
// group (in this case, the group with the same index as sector is changing
// attributes the following way:
// 1. legal becomes false;
// 2. parent lists the index of group that sector has joined)
type RejGroup struct {
	// legal is true in either of two conditions:
	// 1. the sector hasn't joined a group, so this group actually references to
	// a lone sector
	// 2. this is the sector under which grouping occurs (the number of sectors
	// is >= 2)
	// legal is false if sector belongs to a group that has different index than
	// the sector
	legal   bool
	sectors []int
	// parent - if legal == false, the index which group it belongs to.
	// if legal == true, equal to its own index
	parent int
	// can be nil, which means neighbors weren't computed. Else expected to be
	// up to date (derived from sectors neighbors). Neighbors of groups are
	// always legal groups, and group index is index in RejectWork.groups
	neighbors []int
}

// goroutine
func RejectGenerator(input RejectInput) {
	start := time.Now()

	if !input.rmbFrame.isEmpty() {
		Log.Printf("Reject: RMB options present.\n")
	} else if config.UseRMB {
		Log.Printf("Reject: no RMB options applicable to current map.\n")
	}

	// This might replace/delete commands, or alter their arguments. If it does
	// so, it returns a new structure (pointer not shared with the original
	// then), except that it still points to original LoadedRMB, which in turn
	// makes no reference to the new frame(s) (parent also gets cloned, if
	// exists).
	input.rmbFrame = input.rmbFrame.Optimize()

	// If true, must use more complicated / more difficult to optimize (for
	// compiler) algorithms
	needSlowPath := false
	// If true, a certain premise stated in RMB documentation DOES make a
	// difference here and so must be ensured: if any sector in the group is
	// visible (or sees some other sector), then all sectors in the same group
	// must also be visible (or see that same sector)
	groupShareVis := false

	// Load RMB sector groups
	hasGroups, groups := input.rmbFrame.LoadGroups(len(input.sectors))
	if hasGroups && input.rmbFrame.HasOptionTrickyForGroups() {
		Log.Verbose(1, "Reject: GROUP option is present together with some of trickier options. I won't be able to take shortcuts.\n")
		needSlowPath = true
		groupShareVis = true
	}

	// Select interface depending on whether certain effects are used
	var intf RejectWorkIntf
	if needSlowPath {
		// This can handle anything, but slower
		Log.Verbose(1, "Reject: using heavy interface (without shortcuts).\n")
		intf = getRejectWorkIntf()
	} else {
		// Fast performance that covers 99% of use cases. Certain things here
		// are inlined by Go compiler (see rejectdefs.go, markVisibilityFast)
		Log.Verbose(1, "Reject: using default interface (with shortcuts).\n")
		intf = getFastRejectWorkIntf()
	}
	data := intf.main(input, hasGroups, groupShareVis, groups)

	Log.Printf("Reject took %s\n", time.Since(start))
	input.rejectChan <- data
}

func (r *RejectWork) main(input RejectInput, hasGroups bool, groupShareVis bool,
	groups []RejGroup) []byte {
	*r = RejectWork{
		numSectors:    len(input.sectors),
		input:         &input,
		linesToIgnore: input.linesToIgnore,
		rmbFrame:      input.rmbFrame,
		// RejectSelfRefMode DEFAULTS to config value, but may be overridden
		RejectSelfRefMode: config.RejectSelfRefMode, // global: config
		// another value that may be overriden later
		PedanticFailMode: PEDANTIC_FAIL_NOTMATTER,
		fileControl:      input.fileControl,
		mapName:          input.mapName,
		hasGroups:        hasGroups,
		groups:           groups,
		groupShareVis:    groupShareVis,
	}

	// Check if any RMB options make use of "distance in SECTOR COUNT" values
	// This will require scrutiny in case of existing self-referencing sectors
	needDistances := input.rmbFrame.NeedDistances()
	if needDistances {
		r.PedanticFailMode = PEDANTIC_FAIL_REPORT
		if r.RejectSelfRefMode != REJ_SELFREF_PEDANTIC {
			r.RejectSelfRefMode = REJ_SELFREF_PEDANTIC
			Log.Printf("Forcing pedantic mode for self-referencing sectors because RMB options make use of length aka 'distance in sector units'\n")
		}
	}

	if input.rmbFrame.HasLineEffects() {
		r.PedanticFailMode = PEDANTIC_FAIL_REPORT
		if r.RejectSelfRefMode != REJ_SELFREF_PEDANTIC {
			r.RejectSelfRefMode = REJ_SELFREF_PEDANTIC
			Log.Printf("Forcing pedantic mode for self-referencing sectors because RMB options that block sight across a line are present\n")
		}
	}

	// Now check if there is DISTANCE option in RMB. It specifies distance
	// in map units, not sectors
	maxDistOk, maxDist := input.rmbFrame.GetDISTANCEValue()
	if maxDistOk {
		r.PedanticFailMode = PEDANTIC_FAIL_REPORT
		r.maxDistance = maxDist
		if r.RejectSelfRefMode != REJ_SELFREF_PEDANTIC {
			// Yes, here too, because other options could either get us sectors
			// that are not self-referencing, or bypass this option by making
			// them always visible. Can't do this, must obey DISTANCE
			r.RejectSelfRefMode = REJ_SELFREF_PEDANTIC
			Log.Printf("Forcing pedantic mode for self-referencing sectors because DISTANCE is present in RMB options")
		}
	} else {
		r.maxDistance = UNLIMITED_DISTANCE
	}

	r.prepareReject()
	if r.setupLines() {
		r.ScheduleSolidBlockmap()
		// Blockmap will be needed a little bit later, do other tasks meanwhile
		r.createSectorInfo()
		if r.hasGroups {
			r.computeGroupNeighbors()
		}
		r.finishLineSetup()
		r.eliminateTrivialCases()

		if needDistances {
			// Another source of possible major memory allocation
			r.CreateDistanceTable()
			r.ApplyDistanceLimits()
		}

		r.testLines = make([]*SolidLine, len(r.solidLines)+1)      // Vigilant: needs + 1 to avoid out-of-bounds write in rejectLOS.go!findInterveningLines
		r.polyPoints = make([]*IntVertex, 2*(len(r.solidLines)+2)) // this is going to be reused many times to hold both lower and upper polygons (rejectLOS.go!InitializeWorld, etc)
		r.reSectors = make([]*RejSector, len(r.sectors))
		for i := 0; i < len(r.sectors); i++ {
			r.reSectors[i] = &(r.sectors[i])
		}

		if config.UseGraphsForLOS {
			// --- Graphs code starts here
			mixerSetup := false
			if r.groupShareVis {
				r.setupMixer()
				mixerSetup = true
			}
			r.InitializeGraphs(r.numSectors)
			// Try to order lines to maximize our chances of culling child sectors (zennode)
			// VigilantDoomer: we use a different sort function in graphs branch
			// so that function in bruteforce branch is not slowed by checking
			// stuff that never gets initialized for it
			sort.Sort(reSectors_SorterWithGraphs(r.reSectors))
			// Here, lineMap is used to localize lineVisibility access with
			// respect to sectors. As I plan to compress lineVisibility array
			// in-memory when it is too big, I expect localization to reduce the
			// number of compress/decompress operations -- VigilantDoomer
			r.lineMap = make([]*TransLine, len(r.transLines))
			SetupLineMap(r.lineMap, r.reSectors, len(r.sectors))
			// Get blockmap, it will be used in call tree initiated by testLinePair
			// method
			r.RetrieveSolidBlockmap()
			r.prepareBlockmapForLOS() // arrangements for hard porn in rejectLOS.go

			for i := 0; i < r.numSectors; i++ {
				//UpdateProgress ( 1, 100.0 * ( double ) i / ( double ) noSectors );
				r.ProcessSector(r.reSectors[i])
			}
			if mixerSetup {
				r.mergeAndDestroyMixer()
			}
			// --- Graphs code ends here
		} else {
			// --- Bruteforce code starts here

			// Try to order lines to maximize our chances of culling child sectors (zennode)
			sort.Sort(reSectorsType(r.reSectors))
			r.lineMap = make([]*TransLine, len(r.transLines))
			lineMapSize := SetupLineMap(r.lineMap, r.reSectors, len(r.sectors))

			// Get blockmap, it will be used in call tree initiated by testLinePair
			// method
			r.RetrieveSolidBlockmap()
			r.prepareBlockmapForLOS() // arrangements for hard porn in rejectLOS.go

			// Check all lines against each other
			for i := 0; i < lineMapSize; i++ {
				srcLine := r.lineMap[i]
				for j := lineMapSize - 1; j > i; j-- {
					tgtLine := r.lineMap[j]
					if r.testLinePair(srcLine, tgtLine) {
						r.markPairVisible(srcLine, tgtLine)
					}
				}
			}
			// --- Bruteforce code ends here
		}

		r.DoneWithSolidBlockmap()
		//
	} else {
		r.NoNeedSolidBlockmap()
		Log.Printf("Reject builder: not a single two-sided linedef between two distinct sectors was found. You will have an empty, zero-filled REJECT.")
	}

	// Apply special RMB rules (now that all physical LOS calculations are done)
	r.rmbFrame.ProcessOptionsRMB(r)

	// Printf debugging (bruh)
	/*
		for i := 0; i < r.numSectors; i++ {
			for j := i + 1; j < r.numSectors; j++ {
				Log.Printf("Sector %d <-> %d : %t\n", i, j, !isHidden(*r.rejectTableIJ(i, j)))
			}
		}*/

	return r.getResult()
}

// This may do one of the following two things:
// a) Request external generator to create blockmap containing only 1-sided
// lines
// b) or, if certain RMB special effects are present, solid-only blockmap
// is created via other means. This happens when RMB effects dictate some
// linedefs that are not 1-sided to be treated like they were solid, or
// otherwise included in "can block view" computations
func (r *RejectWork) ScheduleSolidBlockmap() {
	if r.specialSolids == nil {
		// This should guarantee the specialSolids array is populated with
		// relevant data - as setupLines ought to have done
		Log.Panic("Programmer error: setupLines must be called before ScheduleSolidBlockmap()")
	}
	if len(r.specialSolids) > 0 {
		r.createSolidBlockmapNow()
		// tell the control goroutine we don't need its work ( because
		// we did all the work ourselves)
		r.NoNeedSolidBlockmap()
	} else {
		// No artificial solids generated, can use 1-sided lines blockmap that
		// other parts of VigilantBSP can also share
		r.input.bcontrol <- BconRequest{
			Sender:  SOLIDBLOCKS_REJECT,
			Message: BCON_NEED_SOLID_BLOCKMAP,
		}
	}
}

// May block
func (r *RejectWork) RetrieveSolidBlockmap() {
	if len(r.specialSolids) > 0 { // see ScheduleSolidBlockmap
		return
	}
	bmResponse := make(chan *Blockmap)
	r.input.bgenerator <- BgenRequest{
		Action:  BGEN_RETRIEVE_BLOCKMAP,
		ReplyTo: bmResponse,
	}
	r.blockmap = <-bmResponse
}

// May block
func (r *RejectWork) DoneWithSolidBlockmap() {
	if len(r.specialSolids) > 0 { // see ScheduleSolidBlockmap
		return
	}
	r.input.bcontrol <- BconRequest{
		Sender:  SOLIDBLOCKS_REJECT,
		Message: BCON_DONE_WITH_SOLID_BLOCKMAP,
	}
}

func (r *RejectWork) NoNeedSolidBlockmap() {
	r.input.bcontrol <- BconRequest{
		Sender:  SOLIDBLOCKS_REJECT,
		Message: BCON_NONEED_SOLID_BLOCKMAP,
	}
}

// TODO In the future, blockmap creation may happen in its own thread, so that
// we can compute distanceTable in parallel
func (r *RejectWork) createSolidBlockmapNow() {
	lines := r.input.lines.GetSolidVersion()
	bmi := BlockmapInput{
		bounds:          r.input.bounds,
		XOffset:         0,
		YOffset:         0,
		useZeroHeader:   false,
		internalPurpose: true,
		gcShield:        nil,
		linesToIgnore:   r.input.linesToIgnore,
	}
	for _, lidx := range r.specialSolids {
		lines.TreatAsSolid(lidx)
	}
	bmi.lines = lines
	r.blockmap = CreateBlockmap(bmi)
}

func isHidden(vis uint8) bool {
	// simpler, than in Zennode... because Zennode did it wrong.
	// Zennode used bits to track RMB visibility, so that visibility set
	// through RMB by INCLUDE option could not apply to sectors hidden by
	// normal visibility computations. But, it didn't account for the fact that
	// "normal" visibility computations take DISTANCE rmb effect, if specified,
	// into account nonetheless (due to technical design), and that - both per
	// documentation (INCLUDE has second highest priority) and per how RMB
	// program behaves - INCLUDE must be able to force visibility back on sector
	// hidden due to DISTANCE effect.
	// And the only use for tracking "it came from RMB" bits in Zennode was just
	// to limit INCLUDE, as Zennode erroneously believed its non-RMB parts of
	// algorithm to be correct (despite not caring about self-referencing
	// sectors), after removal of this bug-introducing limitation, those bits
	// have no use
	return vis&VIS_VISIBLE != VIS_VISIBLE
}

func (r *RejectWork) prepareReject() {
	// The working table size (uses bytes not bits).
	// Extra 7 bytes to simplify getResult() method
	tableSize := r.numSectors*r.numSectors + 7
	r.rejectTable = make([]uint8, tableSize, tableSize)
	for i, _ := range r.rejectTable {
		r.rejectTable[i] = 0
	}
}

func (r *RejectWork) getResult() []byte {
	rejectSize := int((r.numSectors*r.numSectors)+7) / 8
	result := make([]byte, rejectSize, rejectSize)
	tbIdx := 0
	for i := 0; i < rejectSize; i++ {
		bits := 0
		if isHidden(r.rejectTable[tbIdx]) {
			bits = bits | 0x01
		}
		tbIdx++
		if isHidden(r.rejectTable[tbIdx]) {
			bits = bits | 0x02
		}
		tbIdx++
		if isHidden(r.rejectTable[tbIdx]) {
			bits = bits | 0x04
		}
		tbIdx++
		if isHidden(r.rejectTable[tbIdx]) {
			bits = bits | 0x08
		}
		tbIdx++
		if isHidden(r.rejectTable[tbIdx]) {
			bits = bits | 0x10
		}
		tbIdx++
		if isHidden(r.rejectTable[tbIdx]) {
			bits = bits | 0x20
		}
		tbIdx++
		if isHidden(r.rejectTable[tbIdx]) {
			bits = bits | 0x40
		}
		tbIdx++
		if isHidden(r.rejectTable[tbIdx]) {
			bits = bits | 0x80
		}
		tbIdx++
		result[i] = uint8(bits)
	}
	return result
}

func (r *RejectWork) setupLines() bool {
	if r.NoProcess_TryLoad() {
		return false
	}
	numLines := r.input.lines.Len()
	r.RMBLoadLineEffects()
	r.specialSolids = make([]uint16, 0)
	// REMARK lineProcessed array - was never used in zennode. Was just created,
	// then/ deleted, never written to or read from.
	r.solidLines = make([]SolidLine, numLines)
	r.transLines = make([]TransLine, numLines)
	r.drLine = new(TransLine)
	r.indexToSolid = make([]*SolidLine, numLines) // maps index of a line in input.lines to its record in solidLines array, if one exists
	r.slyLinesInSector = make(map[uint16]bool)
	for i := uint16(0); i < numLines; i++ {
		r.indexToSolid[i] = nil
	}
	vertices := make([]IntVertex, int(numLines)*2) // cast to int important!
	numSolidLines := 0
	numTransLines := 0
	var cull *Culler
	if r.RejectSelfRefMode != REJ_SELFREF_TRIVIAL {
		cull = new(Culler)
		cull.SetMode(CREATE_REJECT, r.input.sidedefs)
		cull.SetAbstractLines(r.input.lines)
		cull.EnablePerimeterSink(r.RejectSelfRefMode == REJ_SELFREF_PEDANTIC)
	}
	for i := uint16(0); i < numLines; i++ {
		// Skip dummy lines from fast/remote scroller effect implementation
		if r.linesToIgnore != nil && r.linesToIgnore[i] {
			continue
		}
		x1, y1, x2, y2 := r.input.lines.GetAllXY(i)
		vertices[int(i)<<1] = IntVertex{X: x1, Y: y1}
		vertices[int(i)<<1+1] = IntVertex{X: x2, Y: y2}
		if x1 == x2 && y1 == y2 {
			continue
		}
		culled := cull.AddLine(i)
		twoSided := uint16(r.input.lines.GetFlags(i))&LF_TWOSIDED == LF_TWOSIDED
		if twoSided && !r.HasRMBEffectLINE(i) {
			fSide := r.input.lines.GetSidedefIndex(i, true)
			bSide := r.input.lines.GetSidedefIndex(i, false)
			if fSide == SIDEDEF_NONE || bSide == SIDEDEF_NONE {
				continue
			}
			fSector := r.input.sidedefs[fSide].Sector
			bSector := r.input.sidedefs[bSide].Sector
			if fSector == bSector || culled { // could be (or not be) a border of self-referencing (part of) sector
				if r.RejectSelfRefMode == REJ_SELFREF_TRIVIAL {
					// In trivial mode, we mark this straight away. Otherwise,
					// we *might* mark this later OR (for pedantic mode) create
					// a transient line by finding which sector surrounds the
					// self-referencing one
					r.slyLinesInSector[fSector] = true
				}
				Log.Verbose(2, "Reject: sector %d has line %d which references it on both sides.\n",
					fSector, i)
				continue // can't put this in transient lines while both sides are referencing one sector
			}
			r.transLines[numTransLines] = TransLine{
				index:          i,
				start:          &vertices[int(i)<<1],
				end:            &vertices[int(i)<<1+1],
				frontSector:    fSector,
				backSector:     bSector,
				DX:             x2 - x1,
				DY:             y2 - y1,
				H:              (x2-x1)*(x2-x1) + (y2-y1)*(y2-y1),
				indexInLineVis: int(numTransLines),
			}
			numTransLines++
		} else {
			// This can be either 1-sided line, OR a 2-sided line that has
			// LINE specified for it as an RMB option
			if twoSided {
				r.specialSolids = append(r.specialSolids, i)
			}
			r.solidLines[numSolidLines] = SolidLine{
				index:  i,
				start:  &vertices[int(i)<<1],
				end:    &vertices[int(i)<<1+1],
				ignore: false,
			}
			r.indexToSolid[i] = &r.solidLines[numSolidLines]
			numSolidLines++
		}
	}
	cull.Analyze()
	if r.RejectSelfRefMode == REJ_SELFREF_PEDANTIC {
		// In pedantic mode, we are generating transient line from all
		// self-referencing effect lines in perimeter by finding the outer
		// sector to which one of these lines' sides can be assigned to. This
		// is done by tracing a line through a map and finding the closest
		// proper line it intersects that is outside the line segment.
		// Blockmap is used to speed up the computation
		// NOTE it's possible for sectors to have multiple borders and thus
		// multiple neighbors
		var it *BlockityLines
		var blXMin, blXMax, blYMin, blYMax int
		bm := CreateBlockmap(BlockmapInput{
			lines:           r.input.lines.GetAuxVersion(),
			bounds:          r.input.bounds,
			XOffset:         0,
			YOffset:         0,
			useZeroHeader:   false,
			internalPurpose: true,
			linesToIgnore:   r.linesToIgnore,
		})
		it = GetBlockityLines(bm)
		lineTraces := make(map[[2]IntOrientedVertex]IntCollinearOrientedVertices)
		blXMin = int(bm.header.XMin)
		blXMax = bm.XMax
		blYMin = int(bm.header.YMin)
		blYMax = bm.YMax

		sectorPerimeters := cull.GetPerimeters()
		for sector, perimeters := range sectorPerimeters {
			for _, perimeter := range perimeters {
				whichSector := r.traceSelfRefLines(&numTransLines,
					&numSolidLines, sector, perimeter, it,
					lineTraces, blXMin, blXMax, blYMin, blYMax, vertices)
				if whichSector != -1 {
					Log.Verbose(1, "Self-referencing sector %d has sector %d as its neighbor.\n",
						sector, whichSector)
				}
			}
		}
	}

	// This is used for all other reject treatment modes, and also as fallback
	// in pedantic mode when perimeter computation failed for any reason.
	// It just tells which sectors contain self-referencing lines and need to
	// be made always visible because of that.
	for cull.SpewBack() {
		i := cull.GetLine()
		if r.HasRMBEffectLINE(i) {
			// this line doesn't allow sight across it, so if all borders of
			// a sector are this, it can't be hacked to be visible to all even
			// if self-referencing.
			// But LINE should trigger "pedantic computation", so generally if
			// we are here, pedantic has apparently failed
			if r.RejectSelfRefMode == REJ_SELFREF_PEDANTIC {
				fSide := r.input.lines.GetSidedefIndex(i, true)
				fSector := r.input.sidedefs[fSide].Sector
				Log.Verbose(1, "Reject: line %d from sector %d is affected by line effect, but I failed to classify sector as self-referencing or not.\nIf EVERY 2-sided line that references this sector on both sides is like this, this sector will be hidden from all, but if at least one such line is not marked, it will be hacked to be visible to all.\n",
					i, fSector)
			}
			continue
		}
		fSide := r.input.lines.GetSidedefIndex(i, true)
		fSector := r.input.sidedefs[fSide].Sector
		_, ok := r.slyLinesInSector[fSector]
		r.slyLinesInSector[fSector] = true
		if !ok { // marking sector for the first time
			if r.RejectSelfRefMode == REJ_SELFREF_PEDANTIC {
				// FIXME and if we had an RMB option depending on length etc.
				// we have screwed up, must tell the user!!!
				Log.Verbose(1, "Reject: sector %d seems to be self-referencing, I failed to be pedantic about which lines make up the border though: will resort to a hack to make it always visible.\n", fSector)
				if r.PedanticFailMode == PEDANTIC_FAIL_REPORT {
					r.PedanticFailMode = PEDANTIC_FAIL_REPORTED
				}
			} else {
				Log.Verbose(1, "Reject: sector %d is self-referencing.\n",
					fSector)
			}
		}
	}

	// set correct length
	r.solidLines = r.solidLines[:numSolidLines]
	r.transLines = r.transLines[:numTransLines]
	return numTransLines > 0
}

// FIXME there remain errors in this function. Traces one of the sectors in
// first map of Hexen as self-referencing because it picks wrong side of the
// line by mistake. Looks like there are things to improve...
func (r *RejectWork) traceSelfRefLines(numTransLines *int, numSolidLines *int,
	sector uint16, perimeter []uint16, it *BlockityLines,
	lineTraces map[[2]IntOrientedVertex]IntCollinearOrientedVertices,
	blXMin, blXMax, blYMin, blYMax int, vertices []IntVertex) int {
	// Ok, let's see if we failed miserably for this sector on some other
	// perimeter (couldn't trace) and so already using the fallback to
	// sly lines in sector marker treatment
	if r.slyLinesInSector[sector] {
		return -1
	}
	// All good, proceed. Here is the plan:
	// 1. We have a blockmap (passed to us via BlockityLines object)
	// 2. We trace every "self-referencing effect" line like it was a partition
	// line throughout the whole blockmap, until the following sequence
	// succeeds:
	// - identify all intersection points between this line and whole
	// map lines (with the use of blockmap)
	// - find nearest intersection point (produced by a solid line or
	// a proper 2-sided line) outside the line segment, use sector
	// from that (from the side that faces towards the "partition line"
	// segment of course)
	// - if it was a success, the rest of lines should result in the same
	// sector reference, so exit the loop
	// 3. Now we go through each self-referencing line from the perimeter (which
	// perimeter is ALWAYS clockwise-ordered sequence of lines), and assign
	// the calculated sector from stage 2 to the sidedef that points outside the
	// self-referencing sector
	sectorFound := false
	whichSector := uint16(0)
	tracedSelf := false
	lineEffect := false
	for _, i := range perimeter {
		// First, we make sure this line is 2-sided and reference same sector
		// on both sides
		fSide := r.input.lines.GetSidedefIndex(i, true)
		bSide := r.input.lines.GetSidedefIndex(i, false)
		if fSide == SIDEDEF_NONE || bSide == SIDEDEF_NONE {
			continue
		}
		if r.input.sidedefs[fSide].Sector != sector || r.input.sidedefs[bSide].Sector != sector {
			continue
		}
		if r.HasRMBEffectLINE(i) {
			lineEffect = true
		}
		// Ok, it is/does
		X1, Y1, X2, Y2 := r.input.lines.GetAllXY(i)
		srLine := NodeSeg{
			StartVertex: &NodeVertex{
				X: Number(X1),
				Y: Number(Y1),
			},
			EndVertex: &NodeVertex{
				X: Number(X2),
				Y: Number(Y2),
			},
			Angle:   0, // unused
			Linedef: i,
			flags:   0,
			Offset:  0,
			perp:    0,
			sector:  sector,
			alias:   0,
			len:     0,
			partner: nil,
		}
		srLine.psx = Number(X1)
		srLine.psy = Number(Y1)
		srLine.pex = Number(X2)
		srLine.pey = Number(Y2)
		srLine.pdx = Number(X2 - X1)
		srLine.pdy = Number(Y2 - Y1)
		partSegCoords := srLine.toIntVertexPairC()
		var c IntersectionContext
		c.psx = partSegCoords.StartVertex.X
		c.psy = partSegCoords.StartVertex.Y
		c.pex = partSegCoords.EndVertex.X
		c.pey = partSegCoords.EndVertex.Y
		c.pdx = c.pex - c.psx
		c.pdy = c.pey - c.psy
		ov1, ov2 := IntPartitionInBoundary(&srLine, &c, blXMax, blYMax, blXMin,
			blYMin, partSegCoords)
		if ov1 == nil || ov2 == nil {
			Log.Verbose(2, "Reject: PartitionInBoundary failed for line %d.",
				i)
			// Try next line
			continue
		}
		traceKey := [2]IntOrientedVertex{*ov1, *ov2}
		trace, ok := lineTraces[traceKey]
		if !ok {
			trace = IntCollinearOrientedVertices(make([]IntOrientedVertex, 1))
			trace[0] = *ov1
			it.SetContext(int(ov1.v.X), int(ov1.v.Y), int(ov2.v.X), int(ov2.v.Y))
			for it.NextLine() {
				aline := it.GetLine()
				lsx, lsy, lex, ley := r.input.lines.GetAllXY(aline)
				c.lsx = Number(lsx)
				c.lsy = Number(lsy)
				c.lex = Number(lex)
				c.ley = Number(ley)
				if c.lsx == c.lex && c.lsy == c.ley { // skip zero-length lines
					continue
				}
				pt1, pt2 := c.intGetIntersectionOrIndicence()
				if pt1 != nil {
					if pt2 != nil {
						// Incidence of no use to us
					} else { // pt2 == nil
						// Intersection
						left := IntIsClockwiseTriangle(&NodeVertex{X: c.lsx, Y: c.lsy},
							&NodeVertex{X: c.lex, Y: c.ley}, ov1.v)
						pt1.idx = uint32(aline)
						ov := IntOrientedVertex{
							v:    pt1,
							left: left,
						}
						trace = append(trace, ov)
					}
				}
			}
			trace = append(trace, *ov2)
			sort.Stable(trace)
			if trace[0] != *ov1 || trace[len(trace)-1] != *ov2 {
				Log.Verbose(2, "Reject: failed to produce a trace %t %t (%d, %d) != (%d, %d).\n",
					trace[0] != *ov1, trace[len(trace)-1] != *ov2,
					trace[len(trace)-1].v.X, trace[len(trace)-1].v.Y,
					ov2.v.X, ov2.v.Y)
				trace = nil
			}
			// Cache it!
			lineTraces[traceKey] = trace
		}
		if trace == nil {
			// Try next line
			continue
		}

		leftStart := -1
		for i := 0; i < len(trace); i++ {
			if IntVertexPairCOrdering(trace[i].v, partSegCoords.StartVertex, true) {
				leftStart = i
			} else {
				break
			}
		}
		rightStart := len(trace)
		for i := len(trace) - 1; i >= 0; i-- {
			if IntVertexPairCOrdering(partSegCoords.EndVertex, trace[i].v, true) {
				rightStart = i
			} else {
				break
			}
		}
		if leftStart == -1 || rightStart == len(trace) || leftStart >= rightStart {
			// Try next line
			continue
		}
		// Pick any point (let's take the first one) of currently traced line
		// segment to evaluate which side this point is from line segment
		// intersecting the trace
		nv3 := &NodeVertex{
			X: Number(X1),
			Y: Number(Y1),
		}
		lineThatWasTraced := i
		for i := leftStart; i >= 0; i++ {
			aline := uint16(trace[i].v.idx)
			// Multiple self-referencing sectors can coexist inside one sector
			// that surrounds them. So, make sure line didn't come from another
			// self-referencing sector. We will hit a normal sector transient
			// line or a solid line sooner or later
			// TODO investigate whether nested self-referencing sectors
			// are possible. The surrounding sector for self-referencing sectors
			// inside another self-referencing sector wouldn't be correctly
			// identified then with the current code and needs to be fixed.
			if uint16(r.input.lines.GetFlags(aline))&LF_TWOSIDED == LF_TWOSIDED {
				fSide := r.input.lines.GetSidedefIndex(aline, true)
				bSide := r.input.lines.GetSidedefIndex(aline, false)
				if fSide != SIDEDEF_NONE || bSide != SIDEDEF_NONE {
					fSector := r.input.sidedefs[fSide].Sector
					bSector := r.input.sidedefs[bSide].Sector
					if fSector == bSector {
						continue
					}
				}
			}
			// Ok, we are hitting a good line it seems
			TX1, TY1, TX2, TY2 := r.input.lines.GetAllXY(aline)
			nv1 := &NodeVertex{
				X: Number(TX1),
				Y: Number(TY1),
			}
			nv2 := &NodeVertex{
				X: Number(TX2),
				Y: Number(TY2),
			}
			// FIXME there may still be an error here somewhere. Not to say that
			// apparently changes in selfref.go are warranted. Something to
			// rule out polygon of 2-sided lines nested in regular sector's true
			// perimeter becoming a "self-referencing" perimeter
			// Now calculate which side of line the point is. Whether line goes
			// AB or BA, there shouldn't be a difference of where the point is
			// relative to it. The calculation, however, depends on us starting
			// triangle by walking the line in specific direction so that
			// nv1'<nv2', which may or may not match the vector direction of
			// line segment
			if (nv1.X > nv2.X) || (nv1.X == nv2.X && nv1.Y > nv2.Y) {
				// Swap the points
				nv1, nv2 = nv2, nv1
			} else if nv1.X < nv2.X && nv1.Y > nv2.Y { // recent addition!
				// Also swap? This seems to fix Hexen map01 (no sectors shall
				// be self-referencing), but I'm afraid of breaking something
				// else. Without this, 210 is becoming self-referencing with
				// 207 assigned to the interior 2-sided lines
				nv1, nv2 = nv2, nv1
			}
			isLeft := !IntIsClockwiseTriangle(nv1, nv2, nv3)
			Log.Verbose(3, "Reject: sector %d traced line %d hit line %d, left = %t, traceLeft = %t.\n",
				sector, lineThatWasTraced, aline, isLeft, trace[i].left)
			// Compare where the point is to where the intersecting line looks
			if isLeft == trace[i].left { // line is looking towards the point
				// front side is where our sector is
				fSide := r.input.lines.GetSidedefIndex(aline, true)
				if fSide == SIDEDEF_NONE {
					Log.Verbose(2, "Reject: sector %d traced line %d hit line %d, expected to yield sector from the latter's front side but there was no front sidedef.\n",
						sector, lineThatWasTraced, aline)
					break
				}
				fSector := r.input.sidedefs[fSide].Sector
				whichSector = fSector
			} else { // line is looking away from the point
				// back side is where our sector is
				bSide := r.input.lines.GetSidedefIndex(aline, false)
				if bSide == SIDEDEF_NONE {
					Log.Verbose(2, "Reject: sector %d traced line %d hit line %d, expected to yield sector from the latter's back side but there was no back sidedef.\n",
						sector, lineThatWasTraced, aline)
					break
				}
				bSector := r.input.sidedefs[bSide].Sector
				whichSector = bSector
			}
			if whichSector != sector {
				sectorFound = true
				Log.Verbose(2, "Reject: sector %d traced line %d hit line %d and yielded sector %d from the latter.\n",
					sector, lineThatWasTraced, aline, whichSector)
				break
			} else { // new case catched in Hexen map01
				tracedSelf = true
				break
			}
		}
		if sectorFound {
			break
		}
	}
	if !sectorFound && tracedSelf { // new case catched in Hexen map01
		// This is not self-referencing sector, but just some lines forming
		// a polygon set to the same sector they are surrounded by
		Log.Verbose(2, "The entire perimeter of subgraph of sector %d traced to be surrounded by the same sector. Verdict: this part is not self-referencing.\n",
			sector)
		return -1
	}
	if !sectorFound {
		if lineEffect {
			// Might appear several times, if sector has several perimeters,
			// all with this effect
			Log.Verbose(1, "Reject: sector %d contains lines that have RMB effect LINE applied to them.\n",
				sector)
		} else {
			r.slyLinesInSector[sector] = true
			Log.Verbose(1, "Reject: sector %d is self-referencing, but I failed to trace any of its lines to an outside sector. I will have to resort to 'make it visible to all' hack.\n",
				sector)
		}
		return -1
	}
	// Now iterate through perimeter again and create transient lines where
	// inner sector is self-referencing and outer one is whichSector
	// Actually can create solid lines - if they are affected by certain RMB
	// options (think: LINE)
	for ii, i := range perimeter {
		// First, we make sure this line is 2-sided and reference same sector
		// on both sides
		fSide := r.input.lines.GetSidedefIndex(i, true)
		bSide := r.input.lines.GetSidedefIndex(i, false)
		if fSide == SIDEDEF_NONE || bSide == SIDEDEF_NONE {
			continue
		}
		if r.input.sidedefs[fSide].Sector != sector || r.input.sidedefs[bSide].Sector != sector {
			continue
		}
		if r.HasRMBEffectLINE(i) {
			// RMB effect makes it solid instead of transient
			// Creation of solid should have already been handled (setupLines,
			// solid branch), so only thing to do here is not to add a transient
			// line
			Log.Verbose(2, "Reject: line %d is border of self-referencing sector %d, however RMB option LINE is applied to it, so I will treat it as solid not transient.\n",
				i, sector)
			continue
		}
		// Now must analyze two consecutive lines (current one and next one) in
		// the perimeter to see whether current one's direction matches the
		// clockwise perimeter order (front side references inner,
		// self-referencing sector, back side the outer one and is the one to
		// be reassigned) or not (vice versa)
		iiPlusOne := ii + 1
		if iiPlusOne == len(perimeter) {
			iiPlusOne = 0
		}
		// How to: Two lines share one vertex, let's name it B. The vertex
		// of current line that is not B will be called A, and the vertex of
		// next line that is not B will be called C.
		// It the current line is ordered AB, it matches the clockwise ordering,
		// else (if it is BA) it doesn't.
		X1, Y1, X2, Y2 := r.input.lines.GetAllXY(i)
		NX1, NY1, NX2, NY2 := r.input.lines.GetAllXY(perimeter[iiPlusOne])
		BIsWhere := 2 // imply AB
		if (X1 == NX1 && Y1 == NY1) || (X1 == NX2 && Y1 == NY2) {
			BIsWhere = 1 // it's BA instead
		}
		// Perform the sector assignment
		newFrontSector := sector
		newBackSector := sector
		if BIsWhere == 2 { // AB
			newBackSector = whichSector
			Log.Verbose(2, "Reject: line %d (original sector %d) back sector will be reassigned to %d when representing it as transient line for reject computations.\n",
				i, sector, whichSector)
		} else { // BA
			newFrontSector = whichSector
			Log.Verbose(2, "Reject: line %d (original sector %d) front sector will be reassigned to %d when representing it as transient line for reject computations.\n",
				i, sector, whichSector)
		}

		r.transLines[*numTransLines] = TransLine{
			index:          i,
			start:          &vertices[int(i)<<1],
			end:            &vertices[int(i)<<1+1],
			frontSector:    newFrontSector,
			backSector:     newBackSector,
			DX:             X2 - X1,
			DY:             Y2 - Y1,
			H:              (X2-X1)*(X2-X1) + (Y2-Y1)*(Y2-Y1),
			indexInLineVis: *numTransLines,
		}
		(*numTransLines)++

	}

	return int(whichSector)
}

// Refactored away from setupLines so that it can be called after
// createSectorInfo(), so that garbage collection can delete the huge map
// allocated in createSectorInfo() (and no longer used after it returns)
// to fit the lineVisDone array we'll be creating here
func (r *RejectWork) finishLineSetup() {
	numTransLines := len(r.transLines)
	// How is value derived:
	// 1. We need to store a triangle, rather than whole matrix, and without
	// the dominant diagonal, of numTransLines*numTransLines matrix
	// 2. The number of cells in such triangle is equivalent to a number of cells
	// in triangle WITH the diagonal of (numTransLines-1)*(numTransLines-1)
	// matrix
	// 3. The number of cells in triangle with the major diagonal of matrix N*N
	// is N*(N+1)/2, where N = (numTransLines-1) hence cell count is
	// (numTransLines-1)*numTransLines/2 in our case
	// 4. Each cell is a bit rather than a byte, number of bits is then
	// (numCells + 7) / 8 where / is integer division
	//
	// uint64 is required before the final division to avoid overflow, but the
	// final result can be represented using type that is greater or equal to
	// int32
	lineVisSize := int((uint64(numTransLines-1)*uint64(numTransLines)/2 + 7) / 8)
	r.lineVisDone = make([]uint8, lineVisSize)
	for i, _ := range r.lineVisDone {
		r.lineVisDone[i] = 0
	}

	if r.PedanticFailMode == PEDANTIC_FAIL_REPORTED {
		Log.Error("RMB's accuracy is going to be decreased, because I failed to be pedantic about self-referencing sector effects.\n")
	}

}

func (r *RejectWork) createSectorInfo() {
	numSectors := r.numSectors

	r.sectors = make([]RejSector, numSectors)
	for i := 0; i < numSectors; i++ {
		r.sectors[i] = RejSector{
			index:        i,
			numNeighbors: 0,
			numLines:     0,
			// initialize anything else that needs to start from blank state
		}
	}

	isNeighbor := make(map[Neighboring]bool)
	numTransLines := len(r.transLines)
	// Count the number of lines for each sector first
	for i := 0; i < numTransLines; i++ {
		line := &r.transLines[i]
		r.sectors[line.frontSector].numLines++
		r.sectors[line.backSector].numLines++
	}

	// Allocate contiguous arrays (efficient allocation). Each transient line
	// belongs to two sectors
	sectorLines := make([]*TransLine, uint64(numTransLines)*2)
	neighborList := make([]*RejSector, uint64(numTransLines)*2)

	lines := sectorLines
	neighbors := neighborList

	// Set up the line & neighbor array for each sector
	for i := 0; i < numSectors; i++ {
		// cut the slice part designated for this sector
		r.sectors[i].lines = lines[:r.sectors[i].numLines]
		r.sectors[i].neighbors = neighbors[:r.sectors[i].numLines]
		// skip the cut part
		lines = lines[r.sectors[i].numLines:]
		neighbors = neighbors[r.sectors[i].numLines:]
		r.sectors[i].numLines = 0
	}

	// Fill in line information & mark off neighbors
	for i := 0; i < numTransLines; i++ {
		line := &r.transLines[i]
		sec1 := &r.sectors[line.frontSector]
		sec2 := &r.sectors[line.backSector]
		sec1.lines[sec1.numLines] = line
		sec1.numLines++
		sec2.lines[sec2.numLines] = line
		sec2.numLines++
		r.makeNeighbors(sec1, sec2, isNeighbor)
	}
}

func (r *RejectWork) makeNeighbors(sec1, sec2 *RejSector, isNeighbor map[Neighboring]bool) {
	nei := Neighboring{
		smallIndex: sec1.index,
		bigIndex:   sec2.index,
	}
	if nei.smallIndex > nei.bigIndex {
		nei.smallIndex, nei.bigIndex = nei.bigIndex, nei.smallIndex
	}
	_, ok := isNeighbor[nei]
	if ok {
		return
	}
	isNeighbor[nei] = true
	sec1.neighbors[sec1.numNeighbors] = sec2
	sec1.numNeighbors++
	sec2.neighbors[sec2.numNeighbors] = sec1
	sec2.numNeighbors++
}

// marks two sectors visible. Never acknowledges group functionality
func (r *RejectWork) markVisibilitySector(i, j int, visibility uint8) {
	cell1 := r.rejectTableIJ(i, j)
	if *cell1 == VIS_UNKNOWN {
		*cell1 = visibility
	}

	cell2 := r.rejectTableIJ(j, i)
	if *cell2 == VIS_UNKNOWN {
		*cell2 = visibility
	}
}

// "Smart" markVisibility for use in main reject path. rejectRMB not allowed
// to use this, because rejectRMB needs to account for groups even in fast path,
// and fast path replaces this with a groupless version
// NOTE this function is replaced with a faster one in FastRejectWork variant,
// which is exactly like markVisibilitySector and doesn't take groups into
// account at all
func (r *RejectWork) markVisibility(i, j int, visibility uint8) {
	// needs to check for groupShareVis, rather than for hasGroups, still - to
	// account for the future where "slow path" might be used not only for
	// "groups+some trickier effects" condition
	if !r.groupShareVis || (r.sectorIsNotGroup(i) && r.sectorIsNotGroup(j)) {
		cell1 := r.rejectTableIJ(i, j)
		if *cell1 == VIS_UNKNOWN {
			*cell1 = visibility
		}

		cell2 := r.rejectTableIJ(j, i)
		if *cell2 == VIS_UNKNOWN {
			*cell2 = visibility
		}
	} else {
		r.markVisibilityGroup(i, j, visibility)
	}
}

func (r *RejectWork) sectorIsNotGroup(i int) bool {
	return r.groups[i].legal && len(r.groups[i].sectors) == 1
}

// This is a bit complicated: locates groups which sectors i and j are part of,
// and then makes those two groups visible to each other (every sector in first
// group can see every sector in second group, and vice versa)
func (r *RejectWork) markVisibilityGroup(i, j int, visibility uint8) {
	groupI := r.groups[r.groups[i].parent].sectors
	groupJ := r.groups[r.groups[j].parent].sectors
	if visibility == VIS_HIDDEN && r.extra.mixer != nil {
		// Intercepted by mixer. Some other sector might still conduit
		// visibility

		for _, s1 := range groupI {
			for _, s2 := range groupJ {
				r.mixerSetVisibility(s1, s2, visibility)
				r.mixerSetVisibility(s2, s1, visibility)
			}
		}
		return
	}
	for _, s1 := range groupI {
		for _, s2 := range groupJ {
			r.markVisibilitySector(s1, s2, visibility)
			r.markVisibilitySector(s2, s1, visibility)
		}
	}
}

func (r *RejectWork) markPairVisible(srcLine, tgtLine *TransLine) {
	// there is a direct LOS between the two lines - mark all affected sectors (zennode)
	// that is, mark both sectors of one line as visible from both of another and vice versa
	r.markVisibility(int(srcLine.frontSector), int(tgtLine.frontSector), VIS_VISIBLE)
	r.markVisibility(int(srcLine.backSector), int(tgtLine.frontSector), VIS_VISIBLE)
	r.markVisibility(int(srcLine.frontSector), int(tgtLine.backSector), VIS_VISIBLE)
	r.markVisibility(int(srcLine.backSector), int(tgtLine.backSector), VIS_VISIBLE)
}

func (r *RejectWork) mixerSetVisibility(i, j int, visibility uint8) {
	cell1 := r.mixerIJ(i, j)
	if *cell1 == VIS_UNKNOWN {
		*cell1 = visibility
	}

	cell2 := r.mixerIJ(j, i)
	if *cell2 == VIS_UNKNOWN {
		*cell2 = visibility
	}
}

func (r *RejectWork) rejectTableIJ(i, j int) *uint8 {
	return &(r.rejectTable[i*r.numSectors+j])
}

func (r *RejectWork) mixerIJ(i, j int) *uint8 {
	return &(r.extra.mixer[i*r.numSectors+j])
}

func (r *RejectWork) eliminateTrivialCases() {
	if config.DebugNoSlyForReject { // reference to global: config
		// debug. Revert to zennode's behavior of not counting lines which
		// refer to the same sector on both sides as transient. (empty map
		// substituted for the real map of which sectors contain such lines)
		r.slyLinesInSector = make(map[uint16]bool)
	}

	if r.groupShareVis {
		// GROUP is being combined with some of the trickier RMB options
		// (like DISTANCE)
		// So we have to make sectors within same group see each other, can't
		// bail out of it
		for _, g := range r.groups {
			if !g.legal || len(g.sectors) == 1 {
				// parse each group only once, and don't bother with groups
				// consisting of one sector
				continue
			}
			for _, s1 := range g.sectors {
				for _, s2 := range g.sectors {
					*(r.rejectTableIJ(s1, s2)) = VIS_VISIBLE
				}
			}
		}
	}

	for i := 0; i < r.numSectors; i++ {
		// each sector can see itself
		*(r.rejectTableIJ(i, i)) = VIS_VISIBLE

		// zennode thinks: all sectors with no seethrough lines can't see
		// (be seen by) others
		// VigilantDoomer: self-referencing sectors must not be hidden. This is
		// the way they are kept visible in modes other than pedantic, and as
		// a fallback in pedantic mode
		if r.sectors[i].numLines == 0 {
			sly, _ := r.slyLinesInSector[uint16(i)]
			if !sly {
				for j := 0; j < r.numSectors; j++ {
					r.markVisibilitySector(i, j, VIS_HIDDEN)
				}
			} else {
				// If any non-solid lines with both references set to same
				// sector exist, assume this could be self-referencing sector
				// and keep it visible to ALL
				Log.Verbose(1, "REJECT: HACK Sector %d marked as visible to all (type: 1).\n", i)
				for j := 0; j < r.numSectors; j++ {
					r.markVisibilitySector(i, j, VIS_VISIBLE)
				}
			}
		} else {
			// This acknowledges that one of remotely located parts of sector
			// can be self-referencing - made of only sly lines - the other may
			// be a normal one. Thus anything with suspicious lines is marked
			// as visible to all (and seeing all) sectors
			sly, _ := r.slyLinesInSector[uint16(i)]
			if sly {
				Log.Verbose(1, "REJECT: HACK Sector %d marked as visible to all (type: 2).\n", i)
				for j := 0; j < r.numSectors; j++ {
					r.markVisibilitySector(i, j, VIS_VISIBLE)
				}
			}
		}

		// each sector can see it's immediate neighbors
		// VigilantDoomer: in pedantic mode, self-referencing sectors too can
		// see neighboring sectors through this code, because their neighbors
		// are identified in traceSelfRefLines method
		sec := &(r.sectors[i])
		for j := 0; j < sec.numNeighbors; j++ {
			nei := sec.neighbors[j]
			if nei.index > sec.index {
				r.markVisibilitySector(sec.index, nei.index, VIS_VISIBLE)
			}
		}
	}
}

// Returns true if all sectors in group which the given sector belongs to
// have no transient lines, and no self-referencing effects are suspected in
// either. If at least one sector is suspected to be self-referencing or
// has transient lines, returns false
func (r *RejectWork) entireGroupHiddenNoSly(sector int) bool {
	if r.sectors[sector].numLines != 0 {
		return false
	}

	for _, sec := range r.groups[r.groups[sector].parent].sectors {
		if r.sectors[sec].numLines != 0 {
			return false
		}
		if r.slyLinesInSector[uint16(sec)] {
			return false
		}
	}
	return true
}

func reSectorsCompare(x reSectorsType, i, j int) int {
	sec1 := x[i]
	sec2 := x[j]

	// Favor the sector with the most neighbors
	if sec1.numNeighbors != sec2.numNeighbors {
		return sec2.numNeighbors - sec1.numNeighbors
	}

	// Favor the sector with the most visible lines
	if sec1.numLines != sec2.numLines {
		return sec2.numLines - sec1.numLines
	}

	// It's a tie - use the sector index - lower index favored
	return sec1.index - sec2.index
}

func (r *RejectWork) testLinePair(srcLine, tgtLine *TransLine) bool {
	done := r.getLineVisibilityDone(srcLine, tgtLine)
	if done || r.dontBother(srcLine, tgtLine) {
		return false
	}

	if r.linesTooFarApart(srcLine, tgtLine) {
		r.setLineVisibilityDone(srcLine, tgtLine)
		return false
	}

	isVisible := false
	r.src = *srcLine
	r.tgt = *tgtLine
	bisect := false
	if adjustLinePair(&(r.src), &(r.tgt), &bisect) {
		if bisect {
			isVisible = r.divideRegion(&(r.src), &(r.tgt))
		} else {
			isVisible = r.checkLOS(&(r.src), &(r.tgt))
		}
	}

	r.setLineVisibilityDone(srcLine, tgtLine)
	return isVisible
}

func getVisTableOffsetPrefab(srcLine, tgtLine *TransLine, numTransLines int) uint64 {
	var row uint64
	var col uint64
	if srcLine.indexInLineVis < tgtLine.indexInLineVis {
		row = uint64(srcLine.indexInLineVis)
		col = uint64(tgtLine.indexInLineVis)
	} else {
		row = uint64(tgtLine.indexInLineVis)
		col = uint64(srcLine.indexInLineVis)
	}

	return row*(uint64(numTransLines)<<1-1-row)>>1 + (col - row - 1)
}

func (r *RejectWork) getLineVisibilityDone(srcLine, tgtLine *TransLine) bool {
	if srcLine == tgtLine {
		return true
	}

	offset := getVisTableOffsetPrefab(srcLine, tgtLine, len(r.transLines))
	data := r.lineVisDone[offset>>3]
	bit := uint8(1 << (offset % 8))

	return data&bit == bit
}

func (r *RejectWork) setLineVisibilityDone(srcLine, tgtLine *TransLine) {

	if srcLine == tgtLine {
		return
	}

	offset := getVisTableOffsetPrefab(srcLine, tgtLine, len(r.transLines))
	data := r.lineVisDone[offset>>3]
	bit := uint8(1 << (offset % 8))

	r.lineVisDone[offset>>3] = data | bit
}

func (r *RejectWork) dontBother(srcLine, tgtLine *TransLine) bool {

	if (*r.rejectTableIJ(int(srcLine.frontSector), int(tgtLine.frontSector)) != VIS_UNKNOWN) &&
		(*r.rejectTableIJ(int(srcLine.frontSector), int(tgtLine.backSector)) != VIS_UNKNOWN) &&
		(*r.rejectTableIJ(int(srcLine.backSector), int(tgtLine.frontSector)) != VIS_UNKNOWN) &&
		(*r.rejectTableIJ(int(srcLine.backSector), int(tgtLine.backSector)) != VIS_UNKNOWN) {
		return true
	}

	return false
}

// adjustLinePair adjusts the two lines so that:
//   1) If one line bisects the other:
//      a) The bisecting line is tgt
//      b) The point farthest from src is made both start & end
//   2) tgt is on the left side of src
//   3) src and tgt go in 'opposite' directions
//
func adjustLinePair(src, tgt *TransLine, bisects *bool) bool {

	// Rotate & Translate so that src lies along the +X asix
	y1 := src.DX*(tgt.start.Y-src.start.Y) - src.DY*(tgt.start.X-src.start.X)
	y2 := src.DX*(tgt.end.Y-src.start.Y) - src.DY*(tgt.end.X-src.start.X)

	// The two lines are co-linear and should be ignored
	if (y1 == 0) && (y2 == 0) {
		return false
	}

	// Make sure that src doesn't bi-sect tgt
	if ((y1 > 0) && (y2 < 0)) || ((y1 < 0) && (y2 > 0)) {
		// Swap src & tgt then recalculate the endpoints
		src, tgt = tgt, src

		y1 = src.DX*(tgt.start.Y-src.start.Y) - src.DY*(tgt.start.X-src.start.X)
		y2 = src.DX*(tgt.end.Y-src.start.Y) - src.DY*(tgt.end.X-src.start.X)
		// See if these two lines actually intersect
		if ((y1 > 0) && (y2 < 0)) || ((y1 < 0) && (y2 > 0)) {
			// TODO this should be logged to stderr rather than stdout
			// or maybe not. I don't see what's so big a problem about it.
			// Demoted to verbose level=1 -- VigilantDoomer
			Log.Verbose(1, "ERROR: Two lines (%d & %d) intersect\n", src.index, tgt.index)
			return false
		}
	}

	// Make sure that tgt will end up on the correct (left) side
	if (y1 <= 0) && (y2 <= 0) {
		// Flip src
		src.start, src.end = src.end, src.start
		// Adjust values y1 and y2 end reflect new src
		src.DX = -src.DX
		src.DY = -src.DY
		y1 = -y1
		y2 = -y2
	}

	// See if the lines are parallel
	if y2 == y1 {
		x1 := src.DX*(tgt.start.X-src.start.X) + src.DY*(tgt.start.Y-src.start.Y)
		x2 := src.DX*(tgt.end.X-src.start.X) + src.DY*(tgt.end.Y-src.start.Y)
		if x1 < x2 {
			tgt.start, tgt.end = tgt.end, tgt.start
			tgt.DX = -tgt.DX
			tgt.DY = -tgt.DY
		}
		return true
	}

	// Now look at src from tgt's point of view
	x1 := tgt.DX*(src.start.Y-tgt.start.Y) - tgt.DY*(src.start.X-tgt.start.X)
	x2 := tgt.DX*(src.end.Y-tgt.start.Y) - tgt.DY*(src.end.X-tgt.start.X)

	// See if a line along tgt intersects src
	if ((x1 < 0) && (x2 > 0)) || ((x1 > 0) && (x2 < 0)) {
		*bisects = true
		// Make sure tgt points away from src
		if y1 > y2 {
			tgt.start, tgt.end = tgt.end, tgt.start
			tgt.DX = -tgt.DX
			tgt.DY = -tgt.DY
		}
	} else if (x1 <= 0) && (x2 <= 0) {
		tgt.start, tgt.end = tgt.end, tgt.start
		tgt.DX = -tgt.DX
		tgt.DY = -tgt.DY
	}

	return true
}

func (r *RejectWork) checkLOS(src, tgt *TransLine) bool {
	// all functions called from here were placed in rejectLOS.go
	var myWorld WorldInfo
	r.initializeWorld(&myWorld, src, tgt)
	r.markBlockMap(&myWorld)
	if r.findInterveningLines(&(myWorld.solidSet)) {
		// If src & tgt touch, look for a quick way out - no lines passing through the common point
		var common *IntVertex
		common = src.end
		linesTouch := false
		if common == tgt.start {
			linesTouch = true
		} else {
			common = src.start
			if common == tgt.end {
				linesTouch = true
			}
		}
		if linesTouch {
			more := false
			for i := myWorld.solidSet.loIndex; i <= myWorld.solidSet.hiIndex; i++ {
				line := myWorld.solidSet.lines[i]
				if (*line.start == *common) || (*line.end == *common) {
					more = true
				}
			}
			// The two lines touch and there are no lines blocking them
			if !more {
				return true
			}
		}
		// Do a more refined check to see if there are any lines
		switch TrimLines(myWorld.src, myWorld.tgt, &myWorld.solidSet) {

		case -1:
			{ // A single line completely blocks the view
				return false
			}

		case 0:
			{ // No intervening lines left - end check

			}

		default:
			{
				// Do an even more refined check
				if !FindPolyLines(&myWorld) {
					return false
				}
				// Now see if there are any obstacles that may block the LOS
				if FindObstacles(&myWorld) {
					return false
				}
			}
		}
	}
	return true
}

func (r *RejectWork) divideRegion(src, tgt *TransLine) bool {
	// Find the point of intersection on src
	num := tgt.DX*(src.start.Y-tgt.start.Y) - tgt.DY*(src.start.X-tgt.start.X)
	det := src.DX*tgt.DY - src.DY*tgt.DX
	t := float64(num) / float64(det)

	crossPoint := IntVertex{
		X: int(int64(src.start.X) + int64(t*float64(src.DX))),
		Y: int(int64(src.start.Y) + int64(t*float64(src.DY))),
	}

	// See if we ran into an integer truncation problem (shortcut if we did)
	if (crossPoint == *(src.start)) || (crossPoint == *(src.end)) {
		Log.Verbose(3, "Reject: integer truncation detected in divideRegion for lines %d & %d\n", src.index, tgt.index)
		return r.checkLOS(src, tgt)
	}

	// drLine is a reusable field that is allocated once
	*(r.drLine) = *src
	r.drLine.end = &crossPoint

	isVisible := r.checkLOS(r.drLine, tgt)

	if !isVisible {
		r.drLine.start = &crossPoint
		r.drLine.end = src.end
		tgt.start, tgt.end = tgt.end, tgt.start
		tgt.DX = -tgt.DX
		tgt.DY = -tgt.DY
		isVisible = r.checkLOS(r.drLine, tgt)
	}

	return isVisible
}

type reSectorsType []*RejSector

func (x reSectorsType) Len() int { return len(x) }
func (x reSectorsType) Less(i, j int) bool {
	return reSectorsCompare(x, i, j) < 0
}
func (x reSectorsType) Swap(i, j int) { x[i], x[j] = x[j], x[i] }

func SetupLineMap(lineMap []*TransLine, reSectors []*RejSector, numSectors int) int {
	inMap := make(map[uint16]bool)
	maxIndex := 0
	for i := 0; i < numSectors; i++ {
		for j := 0; j < reSectors[i].numLines; j++ {
			line := reSectors[i].lines[j]
			val, _ := inMap[(*line).index]
			if !val {
				inMap[line.index] = true
				lineMap[maxIndex] = line
				line.indexInLineVis = maxIndex // reorder so that lineVisibility accesses are closer to consecutive
				maxIndex++
			}
		}
	}

	return maxIndex
}

// Overwrites visibility. Both i and j can refer to a group, or a sector,
// even if that sector is part of some other group. In the latter case,
// the effect would be applied only to that specific sector. If i or j refer to
// the index under which grouping has occured, the effect will be applied to
// whole group and not just the sector with the same index
func (r *RejectWork) forceVisibility(i, j int, visibility uint8) {
	if !r.hasGroups {
		*(r.rejectTableIJ(i, j)) = visibility
		return
	}
	groupI := r.groups[i].sectors
	groupJ := r.groups[j].sectors
	for _, i2 := range groupI {
		for _, j2 := range groupJ {
			*(r.rejectTableIJ(i2, j2)) = visibility
		}
	}
}

// Has same quirks as forceVisibility, but doesn't override visibility, but
// instead toggles a corresponding bit set on
func (r *RejectWork) orMaskVisibility(i, j int, visibility uint8) {
	if !r.hasGroups {
		*(r.rejectTableIJ(i, j)) |= visibility
		return
	}
	groupI := r.groups[i].sectors
	groupJ := r.groups[j].sectors
	for _, i2 := range groupI {
		for _, j2 := range groupJ {
			*(r.rejectTableIJ(i2, j2)) |= visibility
		}
	}
}

func (r *RejectWork) computeGroupNeighbors() {
	if r.groups[0].neighbors != nil {
		Log.Error("computeGroupNeighbors called after group neighbors were already computed. (Programmer error)\n")
		return
	}
	for i, _ := range r.groups {
		r.groups[i].neighbors = make([]int, 0)
		dupChecker := make(map[int]bool)
		for _, si := range r.groups[i].sectors {
			for _, nei := range r.sectors[si].neighbors {
				if nei == nil {
					break
				}
				gi := r.groups[nei.index].parent
				if gi != i && !dupChecker[gi] {
					r.groups[i].neighbors = append(r.groups[i].neighbors, gi)
					dupChecker[gi] = true
				}
			}
		}
	}
}

// If NOPROCESS directive is present in RMB and is not overridden, this will
// attempt to use existing reject, if it is compatible.
// Returns false in any of the following conditions:
// 1. NOPROCESS option, or maybe RMB options file itself, is not present
// 2. NOPROCESS option is present but is overridden, so normal reject building
// ought to happen
// 3. NOPROCESS option is present, not overridden, but attempting to load
// existing reject lump fails or it was not compatible (different number of
// sectors)
func (r *RejectWork) NoProcess_TryLoad() bool {
	if !(r.rmbFrame.IsNOPROCESSInEffect()) {
		return false
	}
	return false
}
