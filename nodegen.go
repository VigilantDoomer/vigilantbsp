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
	"bytes"
	"encoding/binary"
	"math"
	"strconv"
	"time"
)

// Nodes Generator, ooh yes

// TODO investigate Zennode's partitioning of _convex_ multi-sector nodes
// (something current IsItConvex does not even identify) into subsectors. Could
// it be worth it?
// TODO work more on superblocks: consider sector lists; utilizing existing
// superblocks instead of creating new from scratch in actual partitioning
// process (would require merging next and nextInSuperBlock pointers); research
// aj-bsp superblock-related code more. I theoritize that reusing existing
// superblocks when whole superblocks go to left/right side, would make sector
// lists especially viable - I expect major speed improvements in
// visplaneKillough and visplaneVigilant modes
// TODO "wide trees" algorithm, possible using multiple threads + partitioning
// single sector doesn't affect visplanes, so this part can be done with
// convexity.go and without wide treatment
// TODO potentially mergeable sectors in "advanced visplane reduction" - since
// visplanes are built separately for ceilings and floors, maybe sector
// equivalencies also need to be split into ceiling and floor equivalencies.
// This proposition will increase algorithm's running time and complexity,
// however, I also wonder how often it will actually make a difference and how
// often a positive one. It actually may be very difficult to estimate,
// something like this should be more of a secondary score. Current experiments,
// however, didn't lead to improvements on maps I tested - the code for them
// was moved into separate branch
// TODO ZokumBSP (maybe Zennode too) sorts segs in case splits upset "seg
// ordering", as some effects (which ones?) allegedly depend on the order of
// segs in subsectors matching the numerical order of linedefs. Currently, in
// VigilantBSP there is a definitive source of disruption, in case "cull
// invisible segs" is used: when segs that could have been invisible determined
// to be NOT so, they are inserted at the END of the seg array, write before
// a single node is created. This might trigger that condition of wrong order.
// If those effects ZokumBSP alludes to exist, at the very least "cull segs"
// option should result in "unculled" segs to be inserted at their "original"
// spots into initial segs list - where their creation was first skipped

const SSECTOR_NORMAL_MASK = 0x8000
const SSECTOR_DEEP_MASK = 0x80000000

// Passed to the nodes builder goroutine
type NodesInput struct {
	lines             WriteableLines
	solidLines        AbstractLines // when a solid-only blockmap is needed to detect void space for more accurate node pick quality weighting
	sectors           []Sector
	sidedefs          []Sidedef
	bcontrol          chan BconRequest
	bgenerator        chan BgenRequest
	nodesChan         chan<- NodesResult
	pickNodeUser      int
	diagonalPenalty   int
	pickNodeFactor    int
	minorIsBetterUser int
	linesToIgnore     []bool // dummy linedefs such as for scrolling
	depthArtifacts    bool
	nodeType          int
}

type Number int

type WideNumber int64

// PickNodeFunc is a signature of all PickNode* function variants (see
// picknode.go)
// Yes, I consciously use a function reference stored in structs in lieu of
// interface - interfaces are "fat", function references should work faster
type PickNodeFunc func(*NodesWork, *NodeSeg, *NodeBounds, *Superblock) *NodeSeg

// Some node partitioning algos implement separate CreateNode function for the
// the case of partitioning non-convex node with only one sector. Thus this
// CreateNodeSSFunc exists.
// SS stands for "single sector". In case you worried.
type CreateNodeSSFunc func(*NodesWork, *NodeSeg, *NodeBounds, *Superblock) *NodeInProcess

// Which secondary metric to use
type MinorIsBetterFunc func(current, prev MinorCosts) bool

// Returned from nodes builder goroutine through a channel
type NodesResult struct {
	subsectors     []SubSector
	segs           []Seg
	nodes          []Node
	deepSubsectors []DeepSubSector
	deepSegs       []DeepSeg
	deepNodes      []DeepNode
	rawNodes       []byte // for Zdoom extended/compressed nodes - all data sans the signature
}

type NodesTotals struct {
	numNodes               int
	numSSectors            int
	numSegs                uint32
	maxSegCountInSubsector uint32
	segSplits              int
}

type NodesWork struct {
	lines           WriteableLines
	sides           []Sidedef
	sectors         []Sector
	allSegs         []*NodeSeg // internal format for calculations
	subsectors      []SubSector
	segs            []Seg // game format for actual SEGS lump
	nodeX           int
	nodeY           int
	nodeDx          int
	nodeDy          int
	totals          *NodesTotals
	vertices        []NodeVertex
	nodes           []Node
	nreverse        uint32
	segAliasObj     *SegAliasHolder
	pickNode        PickNodeFunc
	createNodeSS    CreateNodeSSFunc // SS stands for "single sector". In case you worried.
	sectorHits      []uint8          // used by PickNode_Visplane* variants, keeps track of sectors on both sides
	incidental      []VertexPairC    // used by PickNode_visplaneVigilant, stores list segs collinear with partition line being evaluated to compute the length without overlap
	solidMap        *Blockmap
	nonVoidCache    map[int]NonVoidPerAlias
	blockity        *BlockityLines
	deepNodes       []DeepNode // also used for Zdoom (extended/compressed) nodes while in production
	deepSubsectors  []DeepSubSector
	deepSegs        []DeepSeg
	SsectorMask     uint32
	blocksHit       []BlocksHit
	diagonalPenalty int
	pickNodeFactor  int
	pickNodeUser    int
	minorIsBetter   MinorIsBetterFunc
	multipart       bool // whether multiple partition with same primary costs are considered (for depth evaluation)
	depthArtifacts  bool
	nodeType        int
	// Stuff related to zdoom nodes, with exception to deepNodes, which is above
	zdoomVertexHeader *ZdoomNode_VertexHeader
	zdoomVertices     []ZdoomNode_Vertex
	zdoomSubsectors   []uint32
	zdoomSegs         []ZdoomNode_Seg
}

type NodeVertex struct {
	X   Number
	Y   Number
	idx uint32
}

type NodeBounds struct {
	Xmin int
	Ymin int
	Xmax int
	Ymax int
}

type NodeSeg struct {
	StartVertex        *NodeVertex
	EndVertex          *NodeVertex
	Angle              int16
	Linedef            uint16
	Flip               int16  // 0 - seg follows same direction as linedef, 1 - the opposite
	Offset             uint16 // distance along linedef to start of seg
	next               *NodeSeg
	partner            *NodeSeg
	psx, psy, pex, pey Number // start, end coordinates
	pdx, pdy, perp     Number // used in intersection calculations
	len                Number
	sector             uint16
	secEquiv           uint16 // only initialized if sectorEquivalencies are computed
	alias              int
	block              *Superblock
	nextInSuper        *NodeSeg // not yet ready to port everything to superblock use, need conventional "next" to be separate field
}

type NodeInProcess struct {
	X            int16
	Y            int16
	Dx           int16
	Dy           int16
	Rbox         [4]int16 // right bounding box
	Lbox         [4]int16 // left bounding box
	RChild       uint32   // This may contain either int16 Node.RChild or int32 DeepNode.RChild value
	LChild       uint32   // likewise
	nextL, nextR *NodeInProcess
}

// DoLinesIntersect and ComputeIntersection use this
type IntersectionContext struct {
	psx, psy, pex, pey Number // start, end of partition coordinates
	pdx, pdy           Number // used in intersection calculations
	lex, lsx, ley, lsy Number // - same for checking line
}

const ( // BAMAction "enum"
	BAM_NOSPECIAL = iota
	BAM_REPLACE
	BAM_ADDITIVE
)

type BAMEffect struct {
	Action int   // BAMAction "enum"
	Value  int16 // value related to action
}

const (
	CONVEX_SUBSECTOR = iota
	NONCONVEX_ONESECTOR
	NONCONVEX_MULTISECTOR
)

// goroutine, replies on input.nodesChan
func NodesGenerator(input *NodesInput) {
	start := time.Now()
	// let other goroutines working on something else work on the shared read-only
	// copy of vertices (and linedefs), we will be having our own copy to write to
	input.lines.UnshareData()
	// remove the remnants of any previous nodebuilder work
	input.lines.PruneUnusedVertices()
	// perform detection of polyobjects (sectors containing polyobjects must not
	// be split if possible)
	input.lines.DetectPolyobjects()

	allSegs, intVertices := createSegs(input.lines, input.sidedefs,
		input.linesToIgnore, input.nodeType == NODETYPE_ZDOOM_EXTENDED ||
			input.nodeType == NODETYPE_ZDOOM_COMPRESSED)
	if len(allSegs) == 0 {
		Log.Error("Failed to create any SEGs (BAD). Quitting (%s)\n.", time.Since(start))
		// First respond to solid blocks control goroutine that we won't serve
		// any requests to them
		input.bcontrol <- BconRequest{
			Sender:  SOLIDBLOCKS_NODES,
			Message: BCON_NONEED_SOLID_BLOCKMAP,
		}
		// And to this channel we reply that all is fucked
		input.nodesChan <- NodesResult{} // all fields are nil, no lump data generated
		return
	}

	ssectorMask := uint32(SSECTOR_NORMAL_MASK)
	// Deep, Zdoom extended and Zdoom compressed use the same convention
	if input.nodeType != NODETYPE_VANILLA {
		ssectorMask = SSECTOR_DEEP_MASK
	}

	// See if we need a blockmap of solid-only lines
	requestedSolid := input.pickNodeUser == PICKNODE_VISPLANE_ADV
	if requestedSolid {
		// Vigilant way of evaluating a node line length without void parts
		// depends on it available
		input.bcontrol <- BconRequest{
			Sender:  SOLIDBLOCKS_NODES,
			Message: BCON_NEED_SOLID_BLOCKMAP,
		}
	} else {
		// Other methods don't need this computation
		input.bcontrol <- BconRequest{
			Sender:  SOLIDBLOCKS_NODES,
			Message: BCON_NONEED_SOLID_BLOCKMAP,
		}
	}

	rootSeg := allSegs[0]
	// And now we'll have bounds based on the initial SEGs we built, yay
	rootBox := FindLimits(rootSeg)
	Log.Printf("Initial number of segs is %d.\n", len(allSegs))
	Log.Printf("Nodes: rendered part of map goes from (%d,%d) to (%d,%d)\n",
		rootBox.Ymax, rootBox.Xmin, rootBox.Ymin, rootBox.Xmax)

	var solidMap *Blockmap
	if requestedSolid {
		blockmapGetter := make(chan *Blockmap)
		input.bgenerator <- BgenRequest{
			Action:  BGEN_RETRIEVE_BLOCKMAP,
			ReplyTo: blockmapGetter,
		}
		solidMap = <-blockmapGetter
	}

	sectorEquiv, whichLen := input.computeSectorEquivalence()
	if sectorEquiv == nil {
		whichLen = len(input.sectors)
	} else {
		input.applySectorEquivalence(allSegs, sectorEquiv)
	}

	workData := NodesWork{
		lines:   input.lines,
		sides:   input.sidedefs,
		sectors: input.sectors,
		allSegs: allSegs,
		totals: &NodesTotals{
			numNodes:               0,
			numSSectors:            0,
			numSegs:                0,
			maxSegCountInSubsector: 0,
			segSplits:              0,
		},
		vertices:        intVertices,
		segAliasObj:     new(SegAliasHolder),
		pickNode:        PickNodeFuncFromOption(input.pickNodeUser),
		createNodeSS:    CreateNodeSSFromOption(input.pickNodeUser),
		sectorHits:      make([]byte, whichLen),
		incidental:      make([]VertexPairC, 0),
		solidMap:        solidMap,
		nonVoidCache:    make(map[int]NonVoidPerAlias),
		SsectorMask:     ssectorMask,
		blocksHit:       make([]BlocksHit, 0),
		diagonalPenalty: input.diagonalPenalty,
		pickNodeFactor:  input.pickNodeFactor,
		pickNodeUser:    input.pickNodeUser,
		minorIsBetter:   MinorCmpFuncFromOption(input.minorIsBetterUser),
		multipart:       input.minorIsBetterUser == MINOR_CMP_DEPTH,
		depthArtifacts:  input.depthArtifacts,
		nodeType:        input.nodeType,
	}
	if input.nodeType == NODETYPE_ZDOOM_EXTENDED ||
		input.nodeType == NODETYPE_ZDOOM_COMPRESSED {
		workData.zdoomVertexHeader = &ZdoomNode_VertexHeader{
			ReusedOriginalVertices: uint32(input.lines.Len()),
		}
	}
	workData.segAliasObj.Init()
	initialSuper := workData.doInitialSuperblocks(rootBox)

	// The main act
	rootNode := CreateNode(&workData, rootSeg, rootBox, initialSuper) // recursively create nodes
	// Ok, so now we are printing stats, checking limits, and reverting the
	// tree to produce the final data

	// Debugging helper. Sits here just in case some programmer decides to use
	// Log.Push() to debug things related to nodes (this is where they actually
	// get printed)
	Log.Flush()

	Log.Printf("Created %d subsectors, %d nodes. Got %d segs. Split segs %d times.\n",
		workData.totals.numSSectors, workData.totals.numNodes,
		workData.totals.numSegs, workData.totals.segSplits)

	if requestedSolid {
		// If requested solid blockmap before, tell them we are done with it
		// - the CreateNode() ran its course
		input.bcontrol <- BconRequest{
			Sender:  SOLIDBLOCKS_NODES,
			Message: BCON_DONE_WITH_SOLID_BLOCKMAP,
		}
	}
	hLeft := 0
	hRight := 0
	if rootNode.nextL != nil {
		hLeft = HeightOfNodes(rootNode.nextL) + 1
	}
	if rootNode.nextR != nil {
		hRight = HeightOfNodes(rootNode.nextR) + 1
	}
	Log.Printf("Height of left and right subtrees = (%d,%d)\n", hLeft, hRight)
	if input.nodeType == NODETYPE_VANILLA {
		workData.reverseNodes(rootNode)
	} else {
		// Same node structure for Deep, Extended and Compressed non-GL nodes
		workData.reverseDeepNodes(rootNode)
	}
	Log.Printf("Max seg count in subsector: %d\n", workData.totals.maxSegCountInSubsector)

	// Now we can actually UNDO all our hard work if limits were exceeded
	if uint32(workData.totals.numSSectors)&workData.SsectorMask == workData.SsectorMask {
		Log.Error("Number of subsectors is too large (%d) for this format. Lumps NODES, SEGS, SSECTORS will be emptied.\n",
			workData.totals.numSSectors)
		workData.emptyNodesLumps()
	} else if workData.tooManySegs() {
		Log.Error("Number of segs is too large (%d) for this format. Lumps NODES, SEGS, SSECTORS will be emptied.\n",
			len(workData.segs))
		workData.emptyNodesLumps()
	}
	// TODO for vanilla limits (when in not deep nodes mode of course) - try to
	// move segs so that FirstSeg is <= 32767

	if input.nodeType == NODETYPE_DEEP {
		input.nodesChan <- NodesResult{
			deepNodes:      workData.deepNodes,
			deepSegs:       workData.deepSegs,
			deepSubsectors: workData.deepSubsectors,
		}
	} else if input.nodeType == NODETYPE_VANILLA {
		input.nodesChan <- NodesResult{
			nodes:      workData.nodes,
			segs:       workData.segs,
			subsectors: workData.subsectors,
		}
	} else { // NODETYPE_EXTENDED / NODETYPE_COMPRESSED
		if workData.deepNodes == nil {
			// Was emptied for whatever reason (number of subsectors too
			// large even for this format?! see check somewhere above)
			input.nodesChan <- NodesResult{}
		} else {
			// Ok, so we need to write whole bytestream. And we will allocate
			// it all in memory for now, for simplicity
			input.nodesChan <- NodesResult{
				rawNodes: workData.getZdoomNodesBytes(),
			}
		}
	}
	Log.Printf("Nodes took %s\n", time.Since(start))
}

func RoundToPrecision(n float64) Number {
	if n < 0 {
		return Number(-1 * int(.5-n))
	} else {
		return Number(int(.5 + n))
	}
}

func (n Number) Abs() Number {
	if n < Number(0) {
		return -n
	} else if n > Number(0) {
		return n
	} else {
		return 0
	}
}

func (n Number) Ceil() int {
	return int(n)
}

// On Number, this truncates x. The value of n is ignored (is used only
// to determine type of truncation). See also zdefs.go!ZNumber.Trunc
func (n Number) Trunc(x float64) float64 {
	return float64(int64(x))
}

func PickNodeFuncFromOption(userOption int) PickNodeFunc {
	switch userOption {
	case PICKNODE_TRADITIONAL:
		{
			return PickNode_traditional
		}
	case PICKNODE_VISPLANE:
		{
			return PickNode_visplaneKillough
		}
	case PICKNODE_VISPLANE_ADV:
		{
			return PickNode_visplaneVigilant
		}
	case PICKNODE_MAELSTROM:
		{
			return PickNode_maelstrom
		}
	default:
		{
			panic("Invalid argument")
		}
	}
}

func CreateNodeSSFromOption(userOption int) CreateNodeSSFunc {
	if userOption == PICKNODE_VISPLANE_ADV {
		return CreateNodeForSingleSector
	}
	return CreateNode
}

func MinorCmpFuncFromOption(userOption int) MinorIsBetterFunc {
	switch userOption {
	case MINOR_CMP_NOOP:
		{
			return minorIsBetter_Dummy
		}
	case MINOR_CMP_SEGS:
		{
			return minorIsBetter_Segs
		}
	case MINOR_CMP_SECTORS:
		{
			return minorIsBetter_Sectors
		}
	case MINOR_CMP_BALANCE:
		{
			return minorIsBetter_Balanced
		}
	case MINOR_CMP_DEPTH:
		{
			// the decisive minor comparison is done elsewhere
			return minorIsBetter_Always
		}
	default:
		{
			panic("Invalid argument")
		}
	}
}

func (w *NodesWork) emptyNodesLumps() {
	w.deepNodes = nil
	w.deepSegs = nil
	w.deepSubsectors = nil
	w.nodes = nil
	w.segs = nil
	w.subsectors = nil
}

func (w *NodesWork) tooManySegs() bool {
	// Currently only normal nodes are checked, not deep nodes or extended nodes
	// (w.segs expected to be nil for both of those)
	if w.segs == nil { // assume no overflow is possible for advanced node formats
		return false
	}
	l := len(w.subsectors)
	if l == 0 { // wtf number of subsectors should never be zero
		return true
	}
	// Now, easy: FirstSeg and SegCount are both uint16. If their sum is not
	// equal to the total number of segs, it is obvious we had uint16 overflow
	// (truncation)
	return len(w.segs) > (int(w.subsectors[l-1].FirstSeg) +
		int(w.subsectors[l-1].SegCount))
}

// createSegs iterates over linedefs to create initial segs
// In some circumstances, it won't create a seg:
// 1. Linedef is zero length
// 2. A sidedef has a lower texture with BSPNOSEG name, seg will not be created
// for this side (but may be created for other side)
// 3. Linedef was tagged with "no render" tag (998 for Doom)
// 4. Removing "invisible" lines was allowed in configuration, and the
// line has same sector on both sides and doesn't seem to be part of
// self-referencing sector effect nor was tagged as precious, has no textures
// either (handled by culler)
// 5. Fast/remote scroller effect dummy lines (linesToIgnore[i] == true)
func createSegs(lines WriteableLines, sidedefs []Sidedef,
	linesToIgnore []bool, extNode bool) ([]*NodeSeg, []NodeVertex) {
	res := make([]*NodeSeg, 0, 65536)
	var rootCs, lastCs *NodeSeg
	// This is responsible for handling for which lines we don't create segs,
	// if nil then no segs will be skipped besides the explicitly marked ones
	var cull *Culler
	if config.CullInvisibleSegs != CULL_SEGS_DONT { // reference to global: config
		// Ok, so we will skip some linedefs instead of creating segs for them
		cull = new(Culler)
		// What algo is used to determine what gets skipped (and what gets
		// processed later, when having evidence that it was skipped wrong)
		if config.CullInvisibleSegs == CULL_SEGS_SLOPPY { // reference to global: config
			cull.SetMode(CREATE_SEGS_SLOPPY, sidedefs)
		} else {
			cull.SetMode(CREATE_SEGS, sidedefs)
		}
		cull.SetWriteableLines(lines)
	}
	// This array MUST match the vertices array that lines used, that is
	// corresponding indexes MUST represent the same vertex, even if in
	// different ways
	myVertices := make([]NodeVertex, lines.GetVerticesCount())
	l := lines.Len()
	for i := uint16(0); i < l; i++ {
		if linesToIgnore != nil && linesToIgnore[i] {
			continue
		}
		x1, y1, x2, y2 := lines.GetAllXY(i)
		vidx1, vidx2 := lines.GetLinedefVertices(i)
		storeNodeVertex(myVertices, x1, y1, uint32(vidx1))
		storeNodeVertex(myVertices, x2, y2, uint32(vidx2))
		if x1 == x2 && y1 == y2 {
			continue
		}
		// For nil culler, this is always false. Note that we need to let
		// culler process explicit "do-not-render" lines for correct analysis
		// (and culler needs to duplicate this check to ensure he doesn't spew
		// them back)
		culled := cull.AddLine(i)
		if lines.IsDoNotRender(i) {
			Log.Verbose(1, "Linedef %d will not be rendered because it was either tagged so (tag 998) or assigned an action with numeric code 1086.\n",
				i)
			continue
		}

		if culled {
			continue // skip creating segs for this line - at least, for now
		} else {
			// Creating segs for this line is not a question - do it
			firstSdef := lines.GetSidedefIndex(i, true)
			secondSdef := lines.GetSidedefIndex(i, false)
			addSegsPerLine(myVertices, i, lines, vidx1, vidx2, firstSdef, secondSdef,
				&rootCs, &lastCs, sidedefs, &res, extNode)
		}

	}
	// If culler is not nil, will identify self-referencing sectors and their
	// lines that need segs creation but were skipped. For nil culler these
	// were not skipped anyway
	cull.Analyze()

	// Now that analysis determined some lines were skipped in error, we create
	// segs for those lines here
	for cull.SpewBack() {
		line := cull.GetLine()
		vidx1, vidx2 := lines.GetLinedefVertices(line)
		firstSdef := lines.GetSidedefIndex(line, true)
		secondSdef := lines.GetSidedefIndex(line, false)
		if cull.GetMode() == CREATE_SEGS_SLOPPY {
			// Sloppy gives false positives often enough, and has a chance
			// of false negative (the latter would happen in a rather
			// non-conventional setup).
			Log.Verbose(1, "Linedef %d will be rendered after all, I think sector %d may be self-referencing.\n",
				line, sidedefs[firstSdef].Sector)
		} else {
			Log.Verbose(1, "Linedef %d will be rendered after all - used to create self-referencing effect for sector %d.\n",
				line, sidedefs[firstSdef].Sector)
		}
		// We need to add segs for this line after all.
		addSegsPerLine(myVertices, line, lines, vidx1, vidx2, firstSdef, secondSdef,
			&rootCs, &lastCs, sidedefs, &res, extNode)
	}

	return res, myVertices
}

func addSegsPerLine(myVertices []NodeVertex, i uint16, lines WriteableLines, vidx1, vidx2 int,
	firstSdef, secondSdef uint16, rootCs **NodeSeg, lastCs **NodeSeg, sidedefs []Sidedef,
	res *[]*NodeSeg, extNode bool) {
	var lcs, rcs *NodeSeg
	horizon := lines.IsHorizonEffect(i)
	action := lines.GetAction(i)
	bamEffect := lines.GetBAMEffect(i) // support for linedef actions = 1080...1083
	// FIXME what if sidedef index is invalid? Well, so far it looks like we
	// don't perform validity check for ANY references (say, from lines to
	// vertices)
	if firstSdef != SIDEDEF_NONE {
		if !bytes.Equal(sidedefs[firstSdef].LoName[:], []byte("BSPNOSEG")) {
			// Action = 1085 is do not render front side (zokumbsp)
			if action == 1085 {
				Log.Verbose(1, "Will not create seg for front sidedef of linedef %d - instructed so by action's numeric code set to 1085 on linedef.\n",
					i)
			} else {
				lcs = addSeg(myVertices, i, vidx1, vidx2, rootCs,
					&(sidedefs[firstSdef]), lastCs, horizon, bamEffect)
				lcs.Flip = 0
				*res = append(*res, lcs)
			}
		} else {
			// Implementing BSPNOSEG (zokumbsp specification)
			Log.Verbose(1, "Will not create seg for FRONT sidedef of linedef %d (because BSPNOSEG was used as lower texture)\n", i)
		}
	} else {
		Log.Printf("Warning: linedef %d doesn't have front sidedef\n", i)
	}
	if secondSdef != SIDEDEF_NONE {
		if !bytes.Equal(sidedefs[firstSdef].LoName[:], []byte("BSPNOSEG")) {
			// Action = 1084 is do not render back side (zokumbsp)
			if action == 1084 {
				Log.Verbose(1, "Will not create seg for BACK sidedef of linedef %d - instructed so by action's numeric code set to 1084 on linedef.\n",
					i)
			} else {
				rcs = addSeg(myVertices, i, vidx2, vidx1, rootCs,
					&(sidedefs[secondSdef]), lastCs, horizon, bamEffect)
				rcs.Flip = 1
				*res = append(*res, rcs)
			}
		} else {
			// Implementing BSPNOSEG (zokumbsp specification)
			Log.Verbose(1, "Will not create seg for back sidedef of linedef %d (because BSPNOSEG was used as lower texture)\n", i)
		}
	} else if uint16(lines.GetFlags(i))&LF_TWOSIDED != 0 {
		Log.Printf("Warning: linedef %d is marked as 2-sided but doesn't have back sidedef\n", i)
	}
	if extNode && (horizon || bamEffect.Action != BAM_NOSPECIAL) {
		// Internally, angle is still computed with all the effects. But with
		// or without the effect, angle is not stored in Zdoom nodes format
		// because that format does not provide any field for it
		Log.Printf("Warning: linedef %d has BAM or horizon effect specified, but it cannot be supported in Zdoom nodes format and so will be ignored.\n",
			i)
	}
	if lcs != nil && rcs != nil {
		// Partner segs as used here are optimization technique to speed up
		// PickNode_XXX subroutines, and have nothing to do with GL meaning of
		// the term. We build regular software nodes here.
		// When a seg gets split, its partner seg also gets split, but partners
		// will "break up" if they go to different subtrees (left, right).
		// In so far as partnership is preserved (same subtree), partners are
		// always encountered next to each other
		lcs.partner = rcs
		rcs.partner = lcs
	}
}

func storeNodeVertex(vs []NodeVertex, x, y int, idx uint32) {
	vs[idx] = NodeVertex{
		X:   Number(x),
		Y:   Number(y),
		idx: idx,
	}
}

func addSeg(vs []NodeVertex, i uint16, vidx1, vidx2 int, rootCs **NodeSeg,
	sdef *Sidedef, lastCs **NodeSeg, horizon bool, bamEffect BAMEffect) *NodeSeg {
	s := new(NodeSeg)
	if *lastCs == nil {
		*rootCs = s
		*lastCs = s
	} else {
		(*lastCs).next = s
		*lastCs = s
	}
	s.next = nil
	s.StartVertex = &(vs[vidx1])
	s.EndVertex = &(vs[vidx2])
	s.sector = sdef.Sector
	s.partner = nil
	s.pex = s.EndVertex.X
	s.psx = s.StartVertex.X
	s.pdx = s.pex - s.psx
	s.pey = s.EndVertex.Y
	s.psy = s.StartVertex.Y
	s.pdy = s.pey - s.psy
	s.perp = s.pdx*s.psy - s.psx*s.pdy
	s.Linedef = i
	flen := math.Sqrt(float64(s.pdx)*float64(s.pdx) + float64(s.pdy)*float64(s.pdy))
	s.len = Number(flen)
	s.Offset = 0
	if bamEffect.Action == BAM_REPLACE { // zokumbsp actions 1081, 1083
		// Completely replaces value, computation not needed
		s.Angle = bamEffect.Value
	} else {
		// Default computation of angle
		s.Angle = int16(computeAngle(s.pdx, s.pdy))
		// Check if need to add to this value
		if bamEffect.Action == BAM_ADDITIVE { // zokumbsp actions 1080, 1082
			s.Angle += bamEffect.Value
		}
	}
	// BSP v5.2 horizon effect was specified for linedef
	if horizon {
		s.Angle += int16(float64(sdef.XOffset) * float64(65536.0/360.0))
	}
	return s
}

func computeAngle(dx, dy Number) int {
	w := math.Atan2(float64(dy), float64(dx)) * float64(65536.0/(math.Pi*2))

	if w < 0 {
		w = 65536.0 + w
	}

	return int(w)
}

// FindLimits scans all SEGs in a list to find a minimal bounding box within
// which they all fit. It does this by stepping through the segs and
// comparing the vertices at both ends (taken from BSP nodebuilder)
func FindLimits(ts *NodeSeg) *NodeBounds {
	var r NodeBounds
	// using 32-bit signed min/max values as initial, even though most engines
	// don't have coords this big
	r.Xmax = -2147483648
	r.Ymax = -2147483648
	r.Xmin = 2147483647
	r.Ymin = 2147483647
	for true {
		fv := ts.StartVertex
		tv := ts.EndVertex

		if fv.X < Number(r.Xmin) {
			r.Xmin = int(fv.X)
		}
		if fv.X > Number(r.Xmax) {
			r.Xmax = fv.X.Ceil()
		}
		if fv.Y < Number(r.Ymin) {
			r.Ymin = int(fv.Y)
		}
		if fv.Y > Number(r.Ymax) {
			r.Ymax = fv.Y.Ceil()
		}

		if tv.X < Number(r.Xmin) {
			r.Xmin = int(tv.X)
		}
		if tv.X > Number(r.Xmax) {
			r.Xmax = tv.X.Ceil()
		}
		if tv.Y < Number(r.Ymin) {
			r.Ymin = int(tv.Y)
		}
		if tv.Y > Number(r.Ymax) {
			r.Ymax = tv.Y.Ceil()
		}

		if ts.next == nil {
			break
		}
		ts = ts.next
	}
	return &r
}

// splitDist computes offset: distance along linedef to start of seg
// takes flip bit into account
func splitDist(lines AbstractLines, seg *NodeSeg) int {
	var dx, dy float64

	if seg.Flip == 0 {
		// from start vertex of linedef
		x1, y1, _, _ := lines.GetAllXY(seg.Linedef)
		fx1 := Number(x1)
		fy1 := Number(y1)
		dx = float64(fx1 - seg.StartVertex.X)
		dy = float64(fy1 - seg.StartVertex.Y)
	} else { // seg.Flip == 1
		// from end vertex of linedef
		_, _, x2, y2 := lines.GetAllXY(seg.Linedef)
		fx2 := Number(x2)
		fy2 := Number(y2)
		dx = float64(fx2 - seg.StartVertex.X)
		dy = float64(fy2 - seg.StartVertex.Y)
	}

	if dx == 0 && dy == 0 {
		// TODO stderr
		Log.Printf("Trouble in splitDist %d!%d %f,%f\n", seg.Linedef, seg.Flip, dx, dy)
	}
	t := math.Sqrt((dx * dx) + (dy * dy))
	return int(t)
}

// computeIntersection calculates the point of intersection of two lines.
// ps?->pe? & ls?->le?
// returns xcoord int, ycoord int
func (c *IntersectionContext) computeIntersection() (Number, Number) {
	dx := c.pex - c.psx
	dy := c.pey - c.psy
	dx2 := c.lex - c.lsx
	dy2 := c.ley - c.lsy

	if dx == 0 && dy == 0 {
		// TODO stderr
		Log.Printf("Trouble in computeIntersection dx,dy\n")
	}
	if dx2 == 0 && dy2 == 0 {
		// TODO stderr
		Log.Printf("Trouble in computeIntersection dx2,dy2\n")
	}
	// Truncation was introduced because this is how BSP v5.2 computed this
	// On regular/Deep nodes, I keep it, but it is not performed for extended
	// nodes. Testing should be performed to see if there is reason to keep it
	// for regular version
	// UPD: made conversion more resistant to numerical overflow. This might
	// have performance impact, especially on 32-bit computers.
	l2 := dx2.Trunc(math.Sqrt(float64(dx2)*float64(dx2) + float64(dy2)*float64(dy2)))

	a := float64(dx)        // no normalization of a,b necessary,
	b := float64(dy)        // since division by d in formula for w
	a2 := float64(dx2) / l2 // cancels it out.
	b2 := float64(dy2) / l2
	d := b*a2 - a*b2
	if d != 0 {
		w := ((a * float64(c.lsy-c.psy)) + (b * float64(c.psx-c.lsx))) / d

		a = float64(c.lsx) + (a2 * w)
		b = float64(c.lsy) + (b2 * w)
		return RoundToPrecision(a), RoundToPrecision(b)
	} else {
		// outx = c.lsx, outy = c.lsy
		return c.lsx, c.lsy
	}
}

// psx,psy,pex,pey = partition line coordinates
// lsx,lsy,lex,ley = checking line coordinates
// doLinesIntersect returns 'val' which has 3 bits assigned to the the start and 3
// to the end. These allow a decent evaluation of the lines state.
// bit 0,1,2 = checking lines starting point and bits 4,5,6 = end point
// these bits mean 	0,4 = point is on the same line
// 						1,5 = point is to the left of the line
//						2,6 = point is to the right of the line
// There are some failsafes in here, these mainly check for small errors in the
// side checker.
// VigilantDoomer: "small errors in side checker" - you see, PickNode_* doesn't
// evaluate where segs go precisely as it is done here (*might* have been done
// on purpose, for the sake of performance, by Lee Killough). Something to keep
// in mind going forward, as doing things the same way would enable me to borrow
// superblocks idea from AJ-BSP (the speed-up from that I expect would be
// greater than performance loss from doing things more accurately)
func (c *IntersectionContext) doLinesIntersect() uint8 {
	dx2 := c.psx - c.lsx // Checking line -> partition
	dy2 := c.psy - c.lsy
	dx3 := c.psx - c.lex
	dy3 := c.psy - c.ley

	a := c.pdy*dx2 - c.pdx*dy2
	b := c.pdy*dx3 - c.pdx*dy3
	if (a^b) < 0 && (a != 0) && (b != 0) { // Line is split, just check that

		x, y := c.computeIntersection()
		dx2 = c.lsx - x // Find distance from line start
		dy2 = c.lsy - y // to split point
		if dx2 == 0 && dy2 == 0 {
			a = 0
		} else {
			l := WideNumber(dx2)*WideNumber(dx2) + WideNumber(dy2)*WideNumber(dy2)
			if l < WideNumber(4) {
				// If either ends of the split
				// are smaller than 2 pixs then
				// assume this starts on part line
				a = 0
			}
		}
		dx3 = c.lex - x // Find distance from line end
		dy3 = c.ley - y // to split point
		if dx3 == 0 && dy3 == 0 {
			b = 0
		} else {
			l := WideNumber(dx3)*WideNumber(dx3) + WideNumber(dy3)*WideNumber(dy3)
			if l < WideNumber(4) { // same as start of line
				b = 0
			}
		}
	}

	var val uint8

	if a == 0 {
		val = val | 16 // start is on middle
	} else if a < 0 {
		val = val | 32 // start is on left side
	} else {
		val = val | 64 // start is on right side
	}

	if b == 0 {
		val = val | 1 // end is on middle
	} else if b < 0 {
		val = val | 2 // end is on left side
	} else {
		val = val | 4 // end is on right side
	}

	return val
}

// IsItConvex returns whether the list of segs:
// 1. CONVEX_SUBSECTOR: is convex and fit to be put into a subsector, no
// futher partitioning is needed nor desired
// 2. NONCONVEX_ONESECTOR: is non-convex, but has only one sector so perhaps
// can use a special algorithm to do optimal partition of it
// 3. NONCONVEX_MULTISECTOR: generic non-convex soup which we have to recurse
// into with CreateNode calls
func (w *NodesWork) isItConvex(ts *NodeSeg) int {
	nonConvexityMode := NONCONVEX_ONESECTOR
	// All ssectors must come from same sector unless it's marked
	// "special" with sector tag >= 900. Original idea, Lee Killough

	var sector uint16
	if ts.Flip != 0 {
		sector = w.sides[w.lines.GetSidedefIndex(ts.Linedef, false)].Sector
	} else {
		sector = w.sides[w.lines.GetSidedefIndex(ts.Linedef, true)].Sector
	}
	if w.sectors[sector].Tag < 900 {
		line := ts.next
		for line != nil {
			var csector uint16
			if line.Flip != 0 {
				csector = w.sides[w.lines.GetSidedefIndex(line.Linedef, false)].Sector
			} else {
				csector = w.sides[w.lines.GetSidedefIndex(line.Linedef, true)].Sector
			}
			if csector != sector {
				if w.sectors[csector].Tag < 900 {
					return NONCONVEX_MULTISECTOR // MUST SPLIT
				} else {
					// Special partitioning mode for non-convex single sector
					// subtrees doesn't support hacks like this
					nonConvexityMode = NONCONVEX_MULTISECTOR
				}
			}
			line = line.next
		}
	} else {
		// Special partitioning mode for non-convex single sector
		// subtrees doesn't support hacks like this
		nonConvexityMode = NONCONVEX_MULTISECTOR
	}

	// all of the segs must be on the same side all the other segs
	var c IntersectionContext
	for line := ts; line != nil; line = line.next {
		c.psx = line.StartVertex.X
		c.psy = line.StartVertex.Y
		c.pex = line.EndVertex.X
		c.pey = line.EndVertex.Y
		c.pdx = c.psx - c.pex // Partition line DX,DY
		c.pdy = c.psy - c.pey
		for check := ts; check != nil; check = check.next {
			if line != check {
				c.lsx = check.StartVertex.X
				c.lsy = check.StartVertex.Y
				c.lex = check.EndVertex.X
				c.ley = check.EndVertex.Y
				val := c.doLinesIntersect()
				if val&34 != 0 {
					if nonConvexityMode == NONCONVEX_ONESECTOR {
						Log.DumpSegs(ts)
						// It was implemented. Not a breakthrough as I hoped,
						// but it was implemented
						//Log.Verbose(3, "Non-convex subtree has one sector only (%d). Consider implementing a special case to partition it in as few convex subsectors as possible.\n",
						//	sector)
					}
					return nonConvexityMode // MUST SPLIT
				}
			}
		}
	}

	// no need to split the list: these Segs can be put in a SSector
	return CONVEX_SUBSECTOR
}

// Adds a subsector. This version handles only deep and vanilla nodes format.
// For prototype of Zdoom counterpart, see zdefs.go!ZCreateSSector_Proto, it is
// that function from which ZExt_CreateSSector is generated, not this one
func (w *NodesWork) CreateSSector(tmps *NodeSeg) uint32 {
	// TODO since vanilla can't use deep nodes, at least make segs relocation
	// for its seg limit (max seg index = 32767)
	// TODO check that stuff is within limits. Currently node lumps are emptied
	// AFTER the whole process completes if limit for current format was
	// exceeded. Might try to detect condition earlier?
	// TODO rewrite seg lump to have stuff withing limits (vanilla; limit-raising/"limit-removing")
	// (relocate firstsegs if needed)
	// => w.subsectors[]
	// => w.segs[]
	var subsectorIdx uint32
	var oldNumSegs uint32
	if w.nodeType == NODETYPE_DEEP {
		subsectorIdx = uint32(len(w.deepSubsectors))
		w.deepSubsectors = append(w.deepSubsectors, DeepSubSector{})
		oldNumSegs = uint32(len(w.deepSegs))
	} else { // vanilla
		subsectorIdx = uint32(len(w.subsectors))
		w.subsectors = append(w.subsectors, SubSector{})
		oldNumSegs = uint32(len(w.segs))
	}

	w.totals.numSSectors++
	var currentCount uint32
	if w.nodeType == NODETYPE_DEEP {
		w.deepSubsectors[subsectorIdx].FirstSeg = oldNumSegs
		for ; tmps != nil; tmps = tmps.next {
			w.deepSegs = append(w.deepSegs, DeepSeg{
				StartVertex: uint32(tmps.StartVertex.idx),
				EndVertex:   uint32(tmps.EndVertex.idx),
				Angle:       tmps.Angle,
				Linedef:     tmps.Linedef,
				Flip:        tmps.Flip,
				Offset:      tmps.Offset,
			})
		}
		currentCount = uint32(len(w.deepSegs)) - oldNumSegs
	} else { // vanilla
		w.subsectors[subsectorIdx].FirstSeg = uint16(oldNumSegs) // MIGHT overflow - try changing layout if that happens
		for ; tmps != nil; tmps = tmps.next {
			w.segs = append(w.segs, Seg{
				StartVertex: uint16(tmps.StartVertex.idx),
				EndVertex:   uint16(tmps.EndVertex.idx),
				Angle:       tmps.Angle,
				Linedef:     tmps.Linedef,
				Flip:        tmps.Flip,
				Offset:      tmps.Offset,
			})
		}
		currentCount = uint32(len(w.segs)) - oldNumSegs
	}
	if currentCount > w.totals.maxSegCountInSubsector {
		w.totals.maxSegCountInSubsector = currentCount
	}
	w.totals.numSegs += currentCount
	if w.nodeType == NODETYPE_DEEP {
		w.deepSubsectors[subsectorIdx].SegCount = uint16(currentCount)
	} else {
		w.subsectors[subsectorIdx].SegCount = uint16(currentCount)
	}
	return uint32(w.totals.numSSectors - 1)
}

// Adds a new vertex at specified position (or might found an existing one and
// return it).
func (w *NodesWork) AddVertex(x, y Number) *NodeVertex {
	idx := len(w.vertices)
	if w.lines.AddVertex(int(x), int(y)) != idx {
		panic("Inconsistent state in NodesWork.AddVertex")
	}
	w.vertices = append(w.vertices, NodeVertex{
		X:   x,
		Y:   y,
		idx: uint32(idx),
	})
	return &(w.vertices[idx])
}

// Split a list of segs (ts) into two using the method described at bottom of
// file, this was taken from OBJECTS.C in the DEU5beta source. Well, the actual
// splitting was refactored to DivideSegsActual (called at the end of this
// method), because a twin named "DivideSegsForSingleSector" exists in
// convexity.go
func (w *NodesWork) DivideSegs(ts *NodeSeg, rs **NodeSeg, ls **NodeSeg, bbox *NodeBounds,
	super *Superblock, rightsSuper, leftsSuper **Superblock) {
	// Pick best node to use
	best := w.pickNode(w, ts, bbox, super)

	if best == nil {
		// To programmers: write PickNode so it never happens
		panic("Couldn't pick nodeline!")
	}

	w.nodeX = int(best.StartVertex.X)
	w.nodeY = int(best.StartVertex.Y)
	w.nodeDx = best.EndVertex.X.Ceil() - w.nodeX
	w.nodeDy = best.EndVertex.Y.Ceil() - w.nodeY

	// Partition line coords
	c := &IntersectionContext{
		psx: best.StartVertex.X,
		psy: best.StartVertex.Y,
		pex: best.EndVertex.X,
		pey: best.EndVertex.Y,
	}
	c.pdx = c.psx - c.pex
	c.pdy = c.psy - c.pey
	w.DivideSegsActual(ts, rs, ls, bbox, best, c, super, rightsSuper, leftsSuper)
}

// This does the actual splitting job (partition already picked) using the
// method described at bottom of file, this was taken from OBJECTS.C in the
// DEU5beta source.
func (w *NodesWork) DivideSegsActual(ts *NodeSeg, rs **NodeSeg, ls **NodeSeg,
	bbox *NodeBounds, best *NodeSeg, c *IntersectionContext, super *Superblock,
	rightsSuper, leftsSuper **Superblock) {
	// When we get to here, best is a pointer to the partition seg.
	// Using this partition line, we must split any lines that are intersected
	// into a left and right half, flagging them to be put their respective sides
	// Ok, now we have the best line to use as a partitioning line, we must
	// split all of the segs into two lists (rightside & leftside).
	var rights, lefts, strights, stlefts *NodeSeg // start empty

	*rightsSuper = NewSuperBlock()
	*leftsSuper = NewSuperBlock()
	(*rightsSuper).SetBounds(bbox)
	(*leftsSuper).SetBounds(bbox)
	(*rightsSuper).InitSectorsIfNeeded(super)
	(*leftsSuper).InitSectorsIfNeeded(super)

	asector := -1 // track how many sectors there are

	// Split segs (if needed) and sort all segs we have into left and right subtree
	tmpsNext := ts.next
	for tmps := ts; tmps != nil; tmps = tmpsNext {
		if asector == -1 {
			asector = int(tmps.sector)
		} else if asector != int(tmps.sector) {
			asector = -2
		}
		// progress()
		// Yes up to four segs may happen at once, if splitting a seg that has
		// a partner
		addToRs := [2]*NodeSeg{nil, nil}
		addToLs := [2]*NodeSeg{nil, nil}
		if tmps != best && tmps != best.partner {
			// Process seg AND its partner (if one exists). Must keep partnership valid
			w.CategoriseAndMaybeDivideSeg(addToRs[:], addToLs[:], c, &tmps)
		} else { // tmps == best || tmps == best.partner
			// Kept this consistent with how BSP would do things, and the way
			// PickNode sorts colinear partner segs -- VigilantDoomer
			addToRs[0] = best
			addToLs[0] = best.partner
			if best.partner != nil {
				// sorry, and goodbye - going to different subtrees
				tmps.partner.partner = nil
				tmps.partner = nil
				if tmps == best { // best came before best.partner, next loop iteration must skip both
					tmps = tmps.next
				}
			}
		}
		tmpsNext = tmps.next

		addSegToSide(addToRs[:], &rights, &strights)
		addSegToSide(addToLs[:], &lefts, &stlefts)
	}

	if strights == nil {
		// VigilantDoomer - this branch must never happen (seg from which
		// partition is derived is hardcored to go right)
		panic("Should have had partition at the right side (hardcoded to go there).")
	}

	if stlefts == nil { // This one definitely happens often enough
		// And the reason was identified, indeed. PickNode* and
		// doLinesIntersect() identify left-sideness a bit differently,
		// apparently as result of aggressive optimization in PickNode* done by
		// Lee Killough (and inherited in vigilantBSP through BSP v5.2 code).
		// Currently, I settled for incorporating a small punishing charge  in
		// my PickNode_visplaneVigilant to be assigned when partition with no
		// left side according to doLinesIntersect would be picked. This is one
		// of the last cost charges to be calculated so it doesn't impact
		// performance, but still prevents some extreme cases with too many
		// sectors all clustered at one side.
		if asector > 0 {
			Log.Verbose(1, "No left side, moving partition into left side (single sector #%d)\n",
				asector)
		} else {
			if config.VerbosityLevel >= 3 { // reference to global: config
				// Ok, let's actually see the entire sector list
				sectorsHit := make(map[uint16]bool)
				for tmps := ts; tmps != nil; tmps = tmps.next {
					sectorsHit[tmps.sector] = true
				}
				s := ""
				for sector := range sectorsHit {
					s += "," + strconv.Itoa(int(sector))
				}
				Log.Verbose(1, "No left side, moving partition into left side (sectors %s)\n", s)
			} else {
				Log.Verbose(1, "No left side, moving partition into left side (multiple sectors in this node)\n")
			}
		}
		lefts = best
		stlefts = best
		var prev *NodeSeg
		prev = nil
		for tmps := strights; tmps != nil; tmps = tmps.next {
			if tmps == best {
				if prev != nil {
					prev.next = tmps.next
				} else {
					strights = tmps.next
				}
			}
			prev = tmps
		}
		// VigilantDoomer: the next line of code was present in BSP v5.2 already
		stlefts.next = nil
		prev.next = nil // Make sure end of list = NULL
		// VigilantDoomer since stlefts now points to one seg: partition line,
		// stlefts.partner == nil (newBest.partner == nil) always. I didn't
		// include the check here, but it should be this way - ensure split/sort
		// segs loop does it
	}

	if rights.next != nil {
		rights.next = nil
	}

	if lefts.next != nil {
		lefts.next = nil
	}

	for tmps := strights; tmps != nil; tmps = tmps.next {
		(*rightsSuper).AddSegToSuper(tmps)
	}

	for tmps := stlefts; tmps != nil; tmps = tmps.next {
		(*leftsSuper).AddSegToSuper(tmps)
	}

	*rs = strights
	*ls = stlefts
}

// This doesn't have to recreate segs anymore - progress!
func addSegToSide(segToAdd []*NodeSeg, thisside, stThisSide **NodeSeg) {
	if segToAdd[0] != nil {
		// add to specified side
		segToAdd[0].next = nil
		if *thisside == nil {
			*thisside = segToAdd[0]
			*stThisSide = segToAdd[0]

		} else {
			(*thisside).next = segToAdd[0]
			*thisside = segToAdd[0]
		}
		// add second seg if there was one (in which case it is a partner)
		if segToAdd[1] != nil {
			segToAdd[1].next = nil
			(*thisside).next = segToAdd[1]
			*thisside = segToAdd[1]
		}
	}
}

// CategoriseAndMaybeDivideOneSeg decides whether seg needs to be split (and
// splits it if so), and also which side this seg/the ones resulting from
// split goes/go. If seg has a partner, it categorises two segs at once (and
// if one of them gets split, the other gets split too)
// Returns true if seg was split
func (w *NodesWork) CategoriseAndMaybeDivideSeg(addToRs []*NodeSeg,
	addToLs []*NodeSeg, c *IntersectionContext, tmps **NodeSeg) bool {
	// Assumptions atmps.partner == nil || atmps.partner == atmps.next
	// This is also the function that does the bulk of maintaining this
	// assumption for future
	atmps := *tmps
	if atmps.partner != nil && atmps.partner != atmps.next {
		panic("Partnership was not kept up to date!")
	}
	c.lsx = atmps.StartVertex.X
	c.lsy = atmps.StartVertex.Y
	c.lex = atmps.EndVertex.X
	c.ley = atmps.EndVertex.Y
	val := c.doLinesIntersect()
	if ((val&2 != 0) && (val&64 != 0)) || ((val&4 != 0) && (val&32 != 0)) {
		// Partition intersects this seg in tmps parameter, so it needs to be split
		x, y := c.computeIntersection()
		newVertex := w.AddVertex(x, y)
		news := new(NodeSeg)
		atmps.alias = 0 // clear alias, as angle may change after split
		*news = *atmps
		atmps.next = news
		news.StartVertex = newVertex
		atmps.EndVertex = newVertex
		news.Offset = uint16(splitDist(w.lines, news))
		w.totals.segSplits++
		if atmps.partner != nil {
			w.totals.segSplits++
			atmps.partner.alias = 0 // clear alias, as angle may change after split
			// Split partner too
			newVertex2 := newVertex
			news2 := new(NodeSeg)
			*news2 = *(atmps.partner)
			news2.StartVertex = newVertex2
			atmps.partner.EndVertex = newVertex2
			news2.Offset = uint16(splitDist(w.lines, news2))
			// Now that basic parameters are set, let's deal with partnership
			// If you find these hard to understand, draw these things on paper
			oldpartner := atmps.partner
			atmps.partner = news2
			news2.partner = atmps
			oldpartner.partner = news
			news.partner = oldpartner
			oldpartner.next = news2
			news.next = oldpartner
			// Move cursor to the last part of partner seg, so both segs are
			// not evaluated again, and the next one will be the original next
			// of partner seg before it was split
			*tmps = news2
		} else {
			// Move cursor to the part that comes last. Next will be the seg
			// that was not yet passed
			*tmps = news
		}

		// Update length and angle of segs (BSP v5.2 never did this)
		// Length is used in internal calculations, but angle is used by
		// game engine. Both must be updated
		w.recomputeSegs(atmps, news)
		if atmps.partner != nil {
			w.recomputeSegs(news.partner, atmps.partner)
		}

		if val&32 != 0 {
			addToLs[0] = atmps
			addToLs[1] = atmps.partner // news2 if non-nil
		}
		if val&64 != 0 {
			addToRs[0] = atmps
			addToRs[1] = atmps.partner // news2 if non-nil
		}
		if val&2 != 0 {
			addToLs[0] = news
			addToLs[1] = news.partner
		}
		if val&4 != 0 {
			addToRs[0] = news
			addToRs[1] = news.partner
		}
		return true
	} else {
		partnersBreakUp := false
		// Not split, which side ?
		if val&34 != 0 {
			// to the left
			addToLs[0] = atmps
			addToLs[1] = atmps.partner
		}
		if val&68 != 0 {
			// to the right
			addToRs[0] = atmps
			addToRs[1] = atmps.partner
		}
		if (val&1 != 0) && (val&16 != 0) {
			// On same line
			/* 06/01/97 Lee Killough: this fixes a bug ever since 1.2x,
			   probably 1.0, of BSP: when partitioning a parallel seg,
			   you must take its vertices' orientation into account, NOT the
			   flip bits, to determine which side of the partitioning line a
			   parallel seg should go on. If you simply flip the linedef in
			   question, you will be flipping both its vertices and sidedefs,
			   and the flip bits as well, even though the basic geometry has
			   not changed. Thus you need to use the vertices' orientation
			   (whether the seg is in the same direction or not, regardless
			   of its original linedef's being flipped or not), into account.

			   Originally, some segs were partitioned backwards, and if
			   it happened that there were different sectors on either
			   side of the seg being partitioned, it could leave holes
			   in space, causing either invisible barriers or disappearing
			   Things, because the ssector would be associated with the
			   wrong sector.

			   The old logic of tmps->flip != best->flip seems to rest on
			   the assumption that if two segs are parallel, they came
			   from the same linedef. This is clearly not always true.   */

			/*  if (tmps.flip != best.flip)   old logic -- wrong!!! */

			/* We know the segs are parallel or nearly so, so take their
			   dot product to determine their relative orientation. */

			if ((c.lsx-c.lex)*c.pdx + (c.lsy-c.ley)*c.pdy) < 0 {
				addToLs[0] = atmps
				addToRs[0] = atmps.partner
				partnersBreakUp = true
			} else {
				addToRs[0] = atmps
				addToLs[0] = atmps.partner
				partnersBreakUp = true
			}
		}
		if atmps.partner != nil {
			*tmps = atmps.partner // .partner the same as .next in this instance
			if partnersBreakUp {
				// sorry, and goodbye - going to different subtrees
				atmps.partner.partner = nil
				atmps.partner = nil
			}
		}
		return false
	}
}

// After you split seg into two, a lot of properties actually need to be
// recomputed. (It's amazing bsp v5.2 managed to go without this.) The man that
// brought this to public attention is zokum, but I saw recomputation logic in
// AJ-BSP as well.
func (w *NodesWork) recomputeSegs(originSeg, newSeg *NodeSeg) {
	// originSeg is the one that existed before split on this iteration,
	// newSeg is the one that was created because of the split on this iteration
	originNewLen := int(newSeg.Offset) - int(originSeg.Offset)
	originSeg.len = Number(originNewLen)
	newSeg.len = newSeg.len - Number(originNewLen)

	oldAngle := originSeg.Angle
	w.recomputeOneSeg(originSeg)
	w.recomputeOneSeg(newSeg)
	if oldAngle != originSeg.Angle || oldAngle != newSeg.Angle {
		// Happens often enough btw
		Log.Verbose(3, "Angle changed after splitting seg:  %d -> (%d ; %d)",
			oldAngle, originSeg.Angle, newSeg.Angle)
	}
}

func (w *NodesWork) recomputeOneSeg(s *NodeSeg) {
	s.pex = s.EndVertex.X
	s.psx = s.StartVertex.X
	s.pdx = s.pex - s.psx
	s.pey = s.EndVertex.Y
	s.psy = s.StartVertex.Y
	s.pdy = s.pey - s.psy
	s.perp = s.pdx*s.psy - s.psx*s.pdy
	bamEffect := w.lines.GetBAMEffect(s.Linedef)
	if bamEffect.Action == BAM_REPLACE { // zokumbsp actions 1081, 1083
		s.Angle = bamEffect.Value
	} else {
		// Default computation of angle
		s.Angle = int16(computeAngle(s.pdx, s.pdy))
		if bamEffect.Action == BAM_ADDITIVE { // zokumbsp actions 1080, 1082
			s.Angle += bamEffect.Value
		}
	}
	horizon := w.lines.IsHorizonEffect(s.Linedef)
	if horizon {
		// I check flip bit just in case. Actually I made horizon effect apply
		// to only one-sided lines, but if someone changes that, the angle
		// shall be reflected correctly nonetheless
		isFront := s.Flip == 0
		sdef := w.lines.GetSidedefIndex(s.Linedef, isFront)
		s.Angle += int16(float64(w.sides[sdef].XOffset) * float64(65536.0/360.0))
	}
}

func CreateNode(w *NodesWork, ts *NodeSeg, bbox *NodeBounds, super *Superblock) *NodeInProcess {
	res := new(NodeInProcess)
	var rights *NodeSeg
	var lefts *NodeSeg
	var rightsSuper *Superblock
	var leftsSuper *Superblock
	// Divide node in two
	w.totals.numNodes++
	w.DivideSegs(ts, &rights, &lefts, bbox, super, &rightsSuper, &leftsSuper)
	res.X = int16(w.nodeX)
	res.Y = int16(w.nodeY)
	res.Dx = int16(w.nodeDx)
	res.Dy = int16(w.nodeDy)

	// These will form the left box
	leftBox := FindLimits(lefts)
	res.Lbox[BB_TOP] = int16(leftBox.Ymax)
	res.Lbox[BB_BOTTOM] = int16(leftBox.Ymin)
	res.Lbox[BB_LEFT] = int16(leftBox.Xmin)
	res.Lbox[BB_RIGHT] = int16(leftBox.Xmax)
	state := w.isItConvex(lefts)
	if state == CONVEX_SUBSECTOR {
		res.nextL = nil
		res.LChild = w.CreateSSector(lefts) | w.SsectorMask
	} else if state == NONCONVEX_ONESECTOR {
		res.nextL = w.createNodeSS(w, lefts, leftBox, leftsSuper)
		res.LChild = 0
	} else {
		res.nextL = CreateNode(w, lefts, leftBox, leftsSuper)
		res.LChild = 0
	}

	// These will form the right box
	rightBox := FindLimits(rights)
	res.Rbox[BB_TOP] = int16(rightBox.Ymax)
	res.Rbox[BB_BOTTOM] = int16(rightBox.Ymin)
	res.Rbox[BB_LEFT] = int16(rightBox.Xmin)
	res.Rbox[BB_RIGHT] = int16(rightBox.Xmax)
	state = w.isItConvex(rights)
	if state == CONVEX_SUBSECTOR {
		res.nextR = nil
		res.RChild = w.CreateSSector(rights) | w.SsectorMask
	} else if state == NONCONVEX_ONESECTOR {
		res.nextR = w.createNodeSS(w, rights, rightBox, rightsSuper)
		res.RChild = 0
	} else {
		res.nextR = CreateNode(w, rights, rightBox, rightsSuper)
		res.RChild = 0
	}

	return res
}

func SegCount(seg *NodeSeg) int { // printf debugging
	i := 0
	for ; seg != nil; seg = seg.next {
		i++
	}
	return i
}

func HeightOfNodes(node *NodeInProcess) int {
	lHeight := 1
	rHeight := 1
	if node.nextL != nil {
		lHeight = HeightOfNodes(node.nextL) + 1
	}
	if node.nextR != nil {
		rHeight = HeightOfNodes(node.nextR) + 1
	}
	if lHeight < rHeight {
		return rHeight
	}
	return lHeight
}

// Node reversal for standard nodes
func (w *NodesWork) reverseNodes(node *NodeInProcess) uint32 {
	if w.nodes == nil {
		w.nodes = make([]Node, w.totals.numNodes)
		w.nreverse = 0
	}
	if node.nextR != nil {
		node.RChild = w.reverseNodes(node.nextR)
	}
	if node.nextL != nil {
		node.LChild = w.reverseNodes(node.nextL)
	}

	w.nodes[w.nreverse] = Node{
		X:      node.X,
		Y:      node.Y,
		Dx:     node.Dx,
		Dy:     node.Dy,
		Rbox:   node.Rbox,
		Lbox:   node.Lbox,
		LChild: int16(node.LChild),
		RChild: int16(node.RChild),
	}

	w.nreverse++
	return w.nreverse - 1
}

// Node reversal for deep nodes
func (w *NodesWork) reverseDeepNodes(node *NodeInProcess) uint32 {
	if w.deepNodes == nil {
		w.deepNodes = make([]DeepNode, w.totals.numNodes)
		w.nreverse = 0
	}
	if node.nextR != nil {
		node.RChild = w.reverseDeepNodes(node.nextR)
	}
	if node.nextL != nil {
		node.LChild = w.reverseDeepNodes(node.nextL)
	}

	w.deepNodes[w.nreverse] = DeepNode{
		X:      node.X,
		Y:      node.Y,
		Dx:     node.Dx,
		Dy:     node.Dy,
		Rbox:   node.Rbox,
		Lbox:   node.Lbox,
		LChild: int32(node.LChild),
		RChild: int32(node.RChild),
	}

	w.nreverse++
	return w.nreverse - 1
}

// computeSectorEquivalence()
// Returns an array that maps sector indices to the least index of respective
// equivalent sectors, such that:
// equivalence[sector_index] = equivalent_sector_index
// where sector_index and equivalent_sector_index sometimes match.
// Sector equivalence is when two sectors has compatible properties such that,
// depending on how nodes where generated and granted the possiblity to do so,
// can produce compatible visplanes to be merged by game engine at run-time,
// thus decreasing visplanes and possibly preventing overflow
// The existence of visplane merging was verified and documented by Lee Killough
// upon Doom source release, but he never adjusted logic in BSP nodebuilder to
// account for equivalence. --VigilantDoomer
func (ni *NodesInput) computeSectorEquivalence() ([]uint16, int) {
	if ni.pickNodeUser != PICKNODE_VISPLANE_ADV { // not used by other node picking algos
		return nil, 0
	}
	equiv := make([]uint16, len(ni.sectors))
	hmap := make(map[Sector]uint16)
	uniq := 0
	for i, sector := range ni.sectors {
		// Don't consider sectors that have specials or are assigned tags
		// as being equivalent to any other sectors, even to sectors with same
		// properties. These properties might change out of sync (blinking lights
		// that blink in order, floors lowering etc.). But we will assume that
		// zero tagged things normally don't demerge against each other (unless
		// they are doors opened with DR tags, but ok, I'll neglect that for now)
		if sector.Special == 0 && sector.Tag == 0 {
			ASector := sector
			// Names shorter than 8 symbols can have garbage after the zero
			// byte, and refuse to be recognised as identical if garbage bytes
			// differ in otherwise identical names
			sanitizeName(ASector.CeilName[:8])
			sanitizeName(ASector.FloorName[:8])
			// Go can handle hashing variable of fixed size structure type
			v, ok := hmap[ASector]
			if ok {
				equiv[i] = v
			} else { // first instance of this
				hmap[ASector] = uint16(uniq)
				equiv[i] = uint16(uniq)
				uniq++
			}
		} else {
			// This branch was absent up to and including to v0.69a,
			// which was an error (all non-mergeable sectors where "merged"
			// into sector 0)
			equiv[i] = uint16(uniq)
			uniq++
		}
	}
	return equiv, uniq
}

func sanitizeName(a []byte) []byte {
	var i int
	for i = 0; i < len(a); i++ {
		if a[i] == 0 {
			break
		}
	}
	if i < len(a) {
		for j := i; j < len(a); j++ {
			a[j] = 0
		}
	}
	return a
}

// Store the number of equivalent sector directly in NodeSeg for fast access
// in pickNode's hot loop
func (ni *NodesInput) applySectorEquivalence(allSegs []*NodeSeg, sectorEquiv []uint16) {
	for i, _ := range allSegs {
		allSegs[i].secEquiv = sectorEquiv[allSegs[i].sector]
	}
}

// Create superblocks from initial list of segs before the start of partitioning
func (w *NodesWork) doInitialSuperblocks(rootBox *NodeBounds) *Superblock {
	ret := NewSuperBlock()
	ret.SetBounds(rootBox)
	if w.pickNodeUser == PICKNODE_VISPLANE {
		ret.sectors = make([]uint16, 0)
		ret.secMap = make(map[uint16]bool)
	} else if w.pickNodeUser == PICKNODE_VISPLANE_ADV {
		ret.secEquivs = make([]uint16, 0)
		ret.secMap = make(map[uint16]bool)
	}
	for _, seg := range w.allSegs {
		ret.AddSegToSuper(seg)
	}
	return ret
}

func (w *NodesWork) getZdoomNodesBytes() []byte {
	w.zdoomVertexHeader.NumExtendedVertices = uint32(len(w.vertices) -
		int(w.zdoomVertexHeader.ReusedOriginalVertices))
	w.zdoomVertices = make([]ZdoomNode_Vertex,
		w.zdoomVertexHeader.NumExtendedVertices)
	for i, srcv := range w.vertices[w.zdoomVertexHeader.ReusedOriginalVertices:] {
		w.zdoomVertices[i].X = srcv.X.ToFixed16Dot16()
		w.zdoomVertices[i].Y = srcv.Y.ToFixed16Dot16()
	}
	var writ *ZStream
	if w.nodeType == NODETYPE_ZDOOM_COMPRESSED {
		writ = CreateZStream(ZNODES_COMPRESSED_SIG[:], true)
	} else {
		writ = CreateZStream(ZNODES_PLAIN_SIG[:], false)
	}
	// NOTE always LittleEndian per Zdoom specs
	binary.Write(writ, binary.LittleEndian, &w.zdoomVertexHeader)
	binary.Write(writ, binary.LittleEndian, w.zdoomVertices)
	binary.Write(writ, binary.LittleEndian, uint32(len(w.zdoomSubsectors)))
	binary.Write(writ, binary.LittleEndian, w.zdoomSubsectors)
	binary.Write(writ, binary.LittleEndian, uint32(len(w.zdoomSegs)))
	binary.Write(writ, binary.LittleEndian, w.zdoomSegs)
	binary.Write(writ, binary.LittleEndian, uint32(len(w.deepNodes)))
	binary.Write(writ, binary.LittleEndian, w.deepNodes)
	ret, err := writ.FinalizeAndGetBytes()
	if err != nil {
		Log.Panic("IO error at writing Zdoom nodes stream: %s\n", err.Error())
	}
	return ret
}

const FIXED16DOT16_MULTIPLIER = 65536.0

// Redundant declaration on Number, but will be relevant on ZNumber
func (n Number) ToFixed16Dot16() int32 {
	return int32(float64(n) * FIXED16DOT16_MULTIPLIER)
}

/*---------------------------------------------------------------------------*

	This message has been taken, complete, from OBJECTS.C in DEU5beta source.
	It outlines the method used here to pick the nodelines.

	IF YOU ARE WRITING A DOOM EDITOR, PLEASE READ THIS:

   I spent a lot of time writing the Nodes builder.  There are some bugs in
   it, but most of the code is OK.  If you steal any ideas from this program,
   put a prominent message in your own editor to make it CLEAR that some
   original ideas were taken from DEU.  Thanks.

   While everyone was talking about LineDefs, I had the idea of taking only
   the Segs into account, and creating the Segs directly from the SideDefs.
   Also, dividing the list of Segs in two after each call to CreateNodes makes
   the algorithm faster.  I use several other tricks, such as looking at the
   two ends of a Seg to see on which side of the nodeline it lies or if it
   should be split in two.  I took me a lot of time and efforts to do this.

   I give this algorithm to whoever wants to use it, but with this condition:
   if your program uses some of the ideas from DEU or the whole algorithm, you
   MUST tell it to the user.  And if you post a message with all or parts of
   this algorithm in it, please post this notice also.  I don't want to speak
   legalese; I hope that you understand me...  I kindly give the sources of my
   program to you: please be kind with me...

   If you need more information about this, here is my E-mail address:
   Raphael.Quinet@eed.ericsson.se (Rapha‰l Quinet).

   Short description of the algorithm:
     1 - Create one Seg for each SideDef: pick each LineDef in turn.  If it
	 has a "first" SideDef, then create a normal Seg.  If it has a
	 "second" SideDef, then create a flipped Seg.
     2 - Call CreateNodes with the current list of Segs.  The list of Segs is
	 the only argument to CreateNodes.
     3 - Save the Nodes, Segs and SSectors to disk.  Start with the leaves of
	 the Nodes tree and continue up to the root (last Node).

   CreateNodes does the following:
     1 - Pick a nodeline amongst the Segs (minimize the number of splits and
	 keep the tree as balanced as possible).
     2 - Move all Segs on the right of the nodeline in a list (segs1) and do
	 the same for all Segs on the left of the nodeline (in segs2).
     3 - If the first list (segs1) contains references to more than one
	 Sector or if the angle between two adjacent Segs is greater than
	 180ш, then call CreateNodes with this (smaller) list.  Else, create
	 a SubSector with all these Segs.
     4 - Do the same for the second list (segs2).
     5 - Return the new node (its two children are already OK).

   Each time CreateSSector is called, the Segs are put in a global list.
   When there is no more Seg in CreateNodes' list, then they are all in the
   global list and ready to be saved to disk.

*---------------------------------------------------------------------------*/
