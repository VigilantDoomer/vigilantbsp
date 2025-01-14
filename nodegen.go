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
package main

import (
	"bytes"
	"fmt"
	"log"
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
// segs in subsectors matching the numerical order of linedefs. Prior to
// VigilantBSP v0.75, there was a definitive source of disruption - when "cull
// invisible segs" is used: when segs that could have been invisible determined
// to be NOT so, they are inserted at the END of the seg array, right before
// a single node is created. In VigilantBSP 0.75, this disruption no longer
// occurs in this case (again, this change leads to a different BSP generated
// for "cull invisible segs" modes compared to previous versions, because when
// multiple segs score the same in partition selection - which occurs frequently
// - the earliest seg wins). It remains to be seen if sorting segs is still
// necessary when forming a subsector

const SSECTOR_NORMAL_MASK = uint32(0x8000)
const SSECTOR_DEEP_MASK = uint32(0x80000000)

const DETAILED_LEVEL_THRESHOLD = 500

// Passed to the nodes builder goroutine
type NodesInput struct {
	lines              WriteableLines
	sectors            []Sector
	sidedefs           []Sidedef
	bcontrol           chan BconRequest
	bgenerator         chan BgenRequest
	nodesChan          chan<- NodesResult
	pickNodeUser       int
	diagonalPenalty    int
	pickNodeFactor     int
	minorIsBetterUser  int
	linesToIgnore      []bool // dummy linedefs such as for scrolling
	depthArtifacts     bool
	nodeType           int
	multiTreeMode      int
	specialRootMode    int
	detailFriendliness int
	cacheSideness      bool
	stkNode            bool
	width              int
	solidMap           *Blockmap // rarely filled field (passthrough from another nodes generator, typically nonZ -> Z)
	linedefForMTP      *uint16   // another rare passthrough
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
type StkCreateNodeSSFunc func(*NodesWork, *NodeSeg, *NodeBounds, *Superblock, *StkQueue) *NodeInProcess

// For stknode.go, so that StkCreateNode BFS part can also execute special
// non-convex single-sector partitioner
type SingleSectorDivisorFunc func(w *NodesWork, ts *NodeSeg, rs **NodeSeg,
	ls **NodeSeg, bbox *NodeBounds, super *Superblock, rightsSuper,
	leftsSuper **Superblock, partsegs *[]PartSeg)

// Which secondary metric to use
type MinorIsBetterFunc func(current, prev MinorCosts) bool

type DoLinesIntersectFunc func(c *IntersectionContext) uint8

// Returned from nodes builder goroutine through a channel
type NodesResult struct {
	subsectors     []SubSector
	segs           []Seg
	nodes          []Node
	deepSubsectors []DeepSubSector
	deepSegs       []DeepSeg
	deepNodes      []DeepNode
	rawNodes       []byte // for Zdoom extended/compressed nodes - all data sans the signature
	compressed     bool
}

type NodesTotals struct {
	numNodes               int
	numSSectors            int
	numSegs                uint32
	maxSegCountInSubsector uint32
	segSplits              int
	preciousSplit          int
}

// NodesWork encapsulates data used for nodebuilding thread. When multi-tree is
// built, each tree uses a copy of original NodesWork, with some fields copied
// deep, but some are copied SHALLOW (and must thus never be written to once
// multi-tree starts)! Be cautious to support this functionality when adding
// new fields by modifying NodesWork method GetInitialStateClone()
type NodesWork struct {
	// mlog - if any multi-trees are used, the verbose output will be retained
	// only for the chosen tree, and displayed only after all trees are built
	// This allows to use verbosity properly with multi-tree modes
	// In single-tree mode, mlog should be nil, and calls executed on that
	// (Go supports execution on nil objects) will be forwarded to global Log
	// variable (central VigilantBSP logger)
	mlog            *MiniLogger
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
	createNodeSS    CreateNodeSSFunc    // SS stands for "single sector". In case you worried.
	stkCreateNodeSS StkCreateNodeSSFunc // to be replaced by use of stkDivisorSS

	stkDivisorSS SingleSectorDivisorFunc // hard multi-tree uses this rather than createNodeSS

	sectorHits       []uint8          // used by PickNode_Visplane* variants, keeps track of sectors on both sides
	incidental       []IntVertexPairC // used by PickNode_visplaneVigilant, stores list segs collinear with partition line being evaluated to compute the length without overlap
	solidMap         *Blockmap
	nonVoidCache     map[int]NonVoidPerAlias
	dgVertexMap      *VertexMap // vertex map for computing void parts
	blockity         *BlockityLines
	deepNodes        []DeepNode // also used for Zdoom (extended/compressed) nodes while in production
	deepSubsectors   []DeepSubSector
	deepSegs         []DeepSeg
	SsectorMask      uint32
	blocksHit        []BlocksHit
	diagonalPenalty  int
	pickNodeFactor   int
	pickNodeUser     int
	minorIsBetter    MinorIsBetterFunc
	doLinesIntersect DoLinesIntersectFunc
	// multipart now has two functions:
	// -  whether multiple partition with same PRIMARY costs are considered FOR DEPTH EVALUATION, not for multi-tree
	//    Only for advanced visplane reduction. Zennode-like applies config.EffectiveSecondary directly
	//    So must be set to false for anyone else, except for next case
	// -  for multitree_plain, to implement --roots logic. Zennode-like will use multipart for this
	//    thus temporary set to true in multitree_plain for roots picking only, than reverted back, and all trees proceed without it
	// If you accidently set this to true during normal operation, you may break Zennode-like partitioner.
	multipart        bool
	parts            []*NodeSeg // this one is for HARD multi-tree - the list of partition candidates with same secondary costs as well
	width            int        // and this is the width cap for HARD multi-tree, can be -1 (unlimited)
	depthArtifacts   bool
	nodeType         int
	vertexCache      map[SimpleVertex]int // for nodes without extra precision
	vertexExists     int
	sidenessCache    *SidenessCache
	zenScores        []DepthScoreBundle
	qallocSupers     *Superblock // quick alloc supers - also AJ-BSP ideato decrease allocations
	upgradableToDeep bool        // whether, when building vanilla nodes, can upgrade to DeeP format on overflow
	// vertexSink is used to track all AddVertex requests by returned indices
	// (even when vertex was not added). stknode installs this for
	// node_rearrange
	vertexSink []int

	stkExtra map[*NodeInProcess]StkNodeExtraData // TODO make a better implementation: map with a pointer key is not really a good thing
	// Stuff related to zdoom nodes, with exception to deepNodes, which is above
	zdoomVertexHeader *ZdoomNode_VertexHeader
	zdoomVertices     []ZdoomNode_Vertex
	zdoomSubsectors   []uint32
	zdoomSegs         []ZdoomNode_Seg
	vertexMap         *VertexMap
}

type NodeVertex struct {
	X   Number
	Y   Number
	idx uint32
}

type SimpleVertex struct {
	X int
	Y int
}

type NodeBounds struct {
	Xmin int
	Ymin int
	Xmax int
	Ymax int
}

// April 2023 reordered struct for faster access to very hot fields (flattened
// coordinates for use in intersection calculations). Still have problems with
// CPU cache misses (apparently) on visplane-aware partitioners when accessing
// these
type NodeSeg struct {
	psx, psy, pex, pey Number // start, end coordinates
	pdx, pdy, perp     Number // used in intersection calculations
	len                Number
	nextInSuper        *NodeSeg // not yet ready to port everything to superblock use, need conventional "next" to be separate field
	next               *NodeSeg
	partner            *NodeSeg
	alias              int
	sector             uint16
	secEquiv           uint16 // only initialized if sectorEquivalencies are computed
	flags              uint8  // flip and flags for internal use go here
	sidenessIdx        int    // uses to linearize access to sidenessCache (since the traversal happens through superblocks and not in seg creation order)
	block              *Superblock
	StartVertex        *NodeVertex
	EndVertex          *NodeVertex
	Angle              int16
	Linedef            uint16
	Offset             uint16 // distance along linedef to start of seg
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

const FRACBITS = 16

// FloatVertex is a vertex with floating point coordinates and associated id
type FloatVertex struct {
	Id int
	X  float64
	Y  float64
}

const (
	// SEG_FLAG_FLIP - seg's direction is opposite to its linedef direction.
	// Used to be separate field in NodeSeg
	SEG_FLAG_FLIP = uint8(0x01)
	// SEG_FLAG_NOCACHE - doLinesIntersect result for check=<this seg> is not
	// stored in sideness cache
	// (used by Zenlike partitioner under certain conditions)
	// Usual reason seg is not cached is that it has been split, but programmer
	// might introduce other reasons. Since sideness cache takes a lot of memory,
	// it might be beneficial (hypothesis) to omit segs which are rarely evaluated
	// directly (thanks to quick sideness analysis of superblocks), IF this is
	// shown to benefit performance
	SEG_FLAG_NOCACHE = uint8(0x02)
	// SEG_FLAG_GLOBAL_FLIP - also used for cache purposes, since cache only
	// stores value per alias but not the direction of partition line, all
	// values in cache are assumed to be stored against a certain direction,
	// with partition segs following opposite direction marked with this flag.
	// This flag has no correlation with SEG_FLAG_FLIP - the two must never be
	// confused!
	SEG_FLAG_GLOBAL_FLIP = uint8(0x04)
	// SEG_FLAG_PRECIOUS - seg belongs to precious linedef, and (in case
	// preciousness comes from being a border of polyobject containing sector),
	// is on the side where it matters (sector referenced by seg contains
	// polyobject)
	SEG_FLAG_PRECIOUS = uint8(0x08)
	//SEG_FLAG_COALESCE - seg belongs to a sector tagged >= 900, and as per
	// Killough, they can be mixed with other sectors in one subsector. Used to
	// implement transparent door effect. Flag is used to avoid lookup into
	// NodesWork.sectors structure
	SEG_FLAG_COALESCE = uint8(0x10) // hex 10, dec 16
)

const (
	CONVEX_SUBSECTOR = iota
	NONCONVEX_ONESECTOR
	NONCONVEX_MULTISECTOR
)

// goroutine, replies on input.nodesChan
func NodesGenerator(input *NodesInput) {
	start := time.Now()

	// Logic for concurrent running of Zdoom generator and non-Zdoom generator,
	// kinda contrieved. Non-zdoom generator is calling Zdoom generator later and
	// is thus responsible for setting things up, while Zdoom generator should realize
	// things were set up for him in this case. But Zdoom generator can also run
	// directly from level, when this trickery is not in effect
	oldNodeType := input.nodeType
	cloneEarly := false
	alreadyCloned := false
	var linesForZdoom WriteableLines
	if input.nodeType == NODETYPE_VANILLA_RELAXED {
		input.nodeType = NODETYPE_VANILLA
		if input.multiTreeMode == MULTITREE_NOTUSED {
			Log.Printf("Relaxed vanilla nodes target (-nc=V) does not produce any effect in non-multi-trees modes.\n")
		}
	}
	if input.stkNode && input.multiTreeMode == MULTITREE_NOTUSED &&
		(input.nodeType == NODETYPE_VANILLA_OR_ZEXTENDED ||
			input.nodeType == NODETYPE_VANILLA_OR_ZCOMPRESSED) {
		// stknode doesn't support vanilla_or_zdoom format mode -- yet.
		// ok, disabled node_rearrange for multi-tree, so vanilla_or_zdoom can work
		// properly with (future) hard multi-tree
		// TODO implement support for stknode to build vanilla but fallback on zdoom format
		input.stkNode = false
		if !isZDoomNodes() { // so that message is printed only once
			Log.Error("Building in --stknode mode is not supported for \"vanilla with Zdoom nodes fallback\", disabling stknode.\n")
		}
	}
	if config.Ableist && // reference to global: config
		input.multiTreeMode != MULTITREE_NOTUSED && (input.nodeType == NODETYPE_VANILLA_OR_ZEXTENDED ||
		input.nodeType == NODETYPE_VANILLA_OR_ZCOMPRESSED) {
		// TODO time tallied incorrectly now, as timer will restart
		input.nodeType = promoteNodeType(input.nodeType)
		oldNodeType = input.nodeType // intentional override
		if !isZDoomNodes() {
			Log.Printf("Ableist mode and multi-tree with fallback to Zdoom nodes: switching to Zdoom nodes without trying vanilla.\n")
			ZNodesGenerator(input)
			return
		}
	}

	if input.nodeType == NODETYPE_VANILLA_OR_ZEXTENDED ||
		input.nodeType == NODETYPE_VANILLA_OR_ZCOMPRESSED {
		if isZDoomNodes() {
			alreadyCloned = true // we must have been launched from non-Zdoom generator instead of level
			input.nodeType = promoteNodeType(input.nodeType)
		} else {
			input.nodeType = NODETYPE_VANILLA
			cloneEarly = true // we are responsible for launching Zdoom generator
		}
	}

	if !alreadyCloned {
		// alreadyCloned == true if this is Zdoom generator that is called from
		// non-Zdoom generator, rather than directly from level

		// let other goroutines working on something else work on the shared read-only
		// copy of vertices (and linedefs), we will be having our own copy to write to
		input.lines.UnshareData()
		// remove the remnants of any previous nodebuilder work
		input.lines.PruneUnusedVertices()
		// perform detection of polyobjects (sectors containing polyobjects must not
		// be split if possible)
		input.lines.DetectPolyobjects()
		if cloneEarly {
			linesForZdoom = input.lines.Clone()
		}
	}

	upgradableToDeep := false
	if input.nodeType == NODETYPE_VANILLA_OR_DEEP {
		input.nodeType = NODETYPE_VANILLA
		upgradableToDeep = true
	}

	allSegs, intVertices := createSegs(input.lines, input.sidedefs,
		input.linesToIgnore, input.nodeType == NODETYPE_ZDOOM_EXTENDED ||
			input.nodeType == NODETYPE_ZDOOM_COMPRESSED, input.sectors)
	if len(allSegs) == 0 {
		Log.Error("Failed to create any SEGs (BAD). Quitting (%s)\n.", time.Since(start))
		// First respond to solid blocks control goroutine that we won't serve
		// any requests to them
		if !alreadyCloned {
			input.bcontrol <- BconRequest{
				Sender:  SOLIDBLOCKS_NODES,
				Message: BCON_NONEED_SOLID_BLOCKMAP,
			}
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
	requestedSolid := input.pickNodeUser == PICKNODE_VISPLANE_ADV &&
		input.solidMap == nil
	if !alreadyCloned {
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
	}

	rootSeg := allSegs[0]
	// And now we'll have bounds based on the initial SEGs we built, yay
	rootBox := FindLimits(rootSeg)
	Log.Printf("Initial number of segs is %d.\n", len(allSegs))
	Log.Printf("Nodes: rendered part of map goes from (%d,%d) to (%d,%d)\n",
		rootBox.Ymax, rootBox.Xmin, rootBox.Ymin, rootBox.Xmax)

	doLinesIntersect := doLinesIntersectStandard
	if input.nodeType == NODETYPE_ZDOOM_COMPRESSED ||
		input.nodeType == NODETYPE_ZDOOM_EXTENDED {
		// Zdoom extended nodes keep "standard" because override (zdefs.go) is
		// placed on it not the other func
	} else {
		heur := false
		switch input.detailFriendliness {
		case NODE_DETAIL_ALWAYS:
			doLinesIntersect = doLinesIntersectDetail
		case NODE_DETAIL_SUPPRESS:
			doLinesIntersect = doLinesIntersectStandard
		case NODE_DETAIL_HEURISTIC:
			heur = true
		default: // should be NODE_DETAIL_AUTO
			heur = input.nodeType == NODETYPE_VANILLA
			if !heur { // deep nodes default to detailed version
				doLinesIntersect = doLinesIntersectDetail
			}
		}
		if heur {
			// use heuristic to see if level is detailed, if yes, use
			// detail-friendly version (but that generates more segs and nodes)
			detail := IsTooDetailed(input.lines, input.linesToIgnore, rootBox)
			if detail {
				doLinesIntersect = doLinesIntersectDetail
			}
		}

	}

	// ZNodesGenerator gets non-nil soludMap only if called from NodesGenerator.
	// Nil under all other conditions
	solidMap := input.solidMap
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
		Log.Verbose(1, "Number of sector equivalencies vs number of sectors: %d/%d\n",
			whichLen, len(input.sectors))
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
			preciousSplit:          0,
		},
		vertices:        intVertices,
		segAliasObj:     new(SegAliasHolder),
		pickNode:        PickNodeFuncFromOption(input.pickNodeUser),
		createNodeSS:    CreateNodeSSFromOption(input.pickNodeUser),
		stkCreateNodeSS: StkCreateNodeSSFromOption(input.pickNodeUser),
		stkDivisorSS:    StkSingleSectorDivisorFromOption(input.pickNodeUser),
		sectorHits:      make([]byte, whichLen),
		incidental:      make([]IntVertexPairC, 0),
		solidMap:        solidMap,
		nonVoidCache:    make(map[int]NonVoidPerAlias),
		SsectorMask:     ssectorMask,
		blocksHit:       make([]BlocksHit, 0),
		diagonalPenalty: input.diagonalPenalty,
		pickNodeFactor:  input.pickNodeFactor,
		pickNodeUser:    input.pickNodeUser,
		minorIsBetter:   MinorCmpFuncFromOption(input.minorIsBetterUser),
		multipart: input.minorIsBetterUser == MINOR_CMP_DEPTH &&
			input.pickNodeUser == PICKNODE_VISPLANE_ADV, // 2025 multipart received a secondary purpose
		depthArtifacts:   input.depthArtifacts,
		nodeType:         input.nodeType, // this *MAY* change during nodebuilding process or after
		doLinesIntersect: doLinesIntersect,
		sidenessCache: &SidenessCache{
			maxKnownAlias: 0,
		},
		width:            input.width,
		upgradableToDeep: upgradableToDeep,
	}
	if input.nodeType == NODETYPE_ZDOOM_EXTENDED ||
		input.nodeType == NODETYPE_ZDOOM_COMPRESSED {
		workData.zdoomVertexHeader = &ZdoomNode_VertexHeader{
			ReusedOriginalVertices: uint32(input.lines.GetVerticesCount()),
		}
		workData.vertexMap = CreateVertexMap(&workData, rootBox.Xmin, rootBox.Ymin,
			rootBox.Xmax, rootBox.Ymax)
		PopulateVertexMap(workData.vertexMap, allSegs)
	} else {
		workData.vertexCache = make(map[SimpleVertex]int)
		PopulateVertexCache(workData.vertexCache, allSegs)

	}

	// TODO refactor the preparations for node_rearrange logic
	var pristineVertexMap *VertexMap
	var pristineVertexCache map[SimpleVertex]int
	if input.stkNode {
		if workData.vertexCache != nil {
			pristineVertexCache = make(map[SimpleVertex]int)
			for k, v := range workData.vertexCache {
				pristineVertexCache[k] = v
			}
		}

		if workData.vertexMap != nil {
			pristineVertexMap = workData.vertexMap.Clone()
			pristineVertexMap.w = &workData
		}
	}

	if workData.solidMap != nil {
		// Need to initialize dgVertexMap - the new line tracing algo for
		// advanced visplane reduction partitioner uses it, and it is distinct
		// from the regular vertex map (which gets used for extended nodes) in
		// many aspects.
		lineBox := GetLineBounds(workData.lines)
		dgVertexMap := CreateVertexMap(&workData, lineBox.Xmin, lineBox.Ymin,
			lineBox.Xmax, lineBox.Ymax)
		PopulateVertexMapFromLines(dgVertexMap, workData.lines)
		workData.dgVertexMap = dgVertexMap
	}
	if input.pickNodeUser == PICKNODE_ZENLIKE {
		// Uses lots of memory, especially if cloned for each multi-tree task.
		// So now is only initialized if actually used by partitioner
		workData.zenScores = make([]DepthScoreBundle, 0, len(allSegs))
	}
	workData.segAliasObj.Init()
	initialSuper := workData.doInitialSuperblocks(rootBox, false)
	if input.cacheSideness {
		workData.buildSidenessCache(rootSeg, initialSuper)
	}

	// The main act
	var rootNode *NodeInProcess
	treeCount := 0
	if input.multiTreeMode == MULTITREE_NOTUSED { // This is a most commonly used single-tree mode
		if input.stkNode {
			// breadth-first node creation (for debug purposes, debug parameter
			// --stknode, this variant is normally used for HARD multi-tree
			// only and is only available for single-tree to make it easier
			// compare results in deterministic mode and ensure they are
			// identical)
			rootNode = StkTestEntryPoint(&workData, rootSeg, rootBox, initialSuper)
		} else {
			if cloneEarly {
				// vanilla_or_zdoom* nodes format -- contrived operation here
				// keep in mind that Zdoom generator branch won't reach here, this
				// is dispatch that runs in non-Zdoom generator
				passthrough := &MultiformatPassthrough{
					input:         input,
					solidMap:      solidMap,
					linesForZdoom: linesForZdoom,
					start:         start,
				}
				rootNode = VanillaOrZdoomFormat_Create(&workData, rootSeg, rootBox,
					initialSuper, oldNodeType, passthrough)
				if rootNode == nil {
					return // message was sent from Zdoom generator spawned from dispatch
				}
			} else {
				// classic recursive node creation (the default)
				rootNode = CreateNode(&workData, rootSeg, rootBox, initialSuper)
			}
		}
	} else if input.multiTreeMode == MULTITREE_ROOT_ONLY {
		// Create multiple trees - bruteforce ROOT node among all (one-sided,
		// two-sided, or every - depending on input.specialRootMode), the REST
		// nodes are picked as NORMAL. So multiple trees are generated (in parallel
		// with a limited number of worker threads) and the "best" is chosen
		// This is NOT the way Zokumbsp does multi-tree, but rather a more
		// simple feature requested by user nicknamed jerko

		if workData.sidenessCache.maxKnownAlias == 0 {
			// Damn cache, if it was not built, needs still be locked. If was build,
			// should be locked already
			// This is really just used as defense against programmers who might
			// not have yet learned the fact that cache is built before
			// multitree and is shared between multitree threads
			workData.sidenessCache.readOnly = true
		}

		var foreign *MTPForeignInput
		if input.linedefForMTP != nil {
			// plain multi-tree will try only this one specified linedef as root
			// Intended use is building Zdoom nodes after plain multi-tree proper
			// failed to find tree that fits vanilla nodes format, even unsigned,
			// and the mode was "prefer vanilla, build Zdoom on overflow"
			if foreign != nil {
				Log.Panic("Invalid MTPForeign combination (programmer error)\n")
			}
			foreign = &MTPForeignInput{
				Action:  MTP_FOREIGN_THIS_ONE_LINEDEF,
				LineIdx: *input.linedefForMTP,
			}
		}
		if workData.nodeType == NODETYPE_VANILLA &&
			(oldNodeType == NODETYPE_VANILLA_OR_ZEXTENDED ||
				oldNodeType == NODETYPE_VANILLA_OR_ZCOMPRESSED) {
			// extra parameters to handle the fallback to Zdoom nodes format
			if foreign != nil {
				Log.Panic("Invalid MTPForeign combination (programmer error)\n")
			}
			foreign = &MTPForeignInput{
				Action:        MTP_FOREIGN_EXTRADATA,
				linesForZdoom: linesForZdoom,
				start:         start,
				input:         input,
				solidMap:      solidMap,
			}
		}

		rootNode, treeCount = MTPSentinel_MakeBestBSPTree(&workData, rootBox,
			initialSuper, input.specialRootMode, oldNodeType, foreign)
		if rootNode == nil {
			return // message was sent from Zdoom generator spawned from dispatch
		}
	} else {
		Log.Panic("Multi-tree variant not implemented.\n")
	}
	// NOTE rootBox, rootSeg, initialSuper must NOT be used past this point!

	// Ok, so now we are printing stats, checking limits, and reverting the
	// tree to produce the final data

	// Merge in (and print most stuff buffered) from the workData log (associated
	// with respective tree)
	Log.Merge(workData.mlog, "Merging buffered output (if any) from the chosen tree.\n")
	workData.mlog = nil

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

	Log.Printf("Splits reused already created vertices to avoid duplicates: %d times\n", workData.vertexExists)

	if workData.upgradableToDeep && workData.nodeType == NODETYPE_VANILLA &&
		workData.tooManySegsCantFix(true) {
		workData.upgradeToDeep()
	}

	if input.stkNode && input.multiTreeMode == MULTITREE_NOTUSED {
		// If stknode mode was enabled through debug parameter, do perform
		// rearrangement, so that can compare resulting wad with non-stknode build
		// for identity and confirm validity that way. This, of course, implies both
		// stknode and non-stknode builds of the wad are done with deterministism
		// (-d parameter).
		// But, won't do this for multi-tree anymore, because:
		// 1) if hard multi-tree gets implemented (work is underway as of January 2025),
		// it will be in single-threaded mode, and multi-threaded mode will still
		// require a lot of work
		// 2) once supported, can still revert to single-threaded build whenever
		// user asks for determinism, just like it used to be with blockmap,
		// multiple offsets and a specified endgoal, until I have implemented
		// backtracking in later version
		// 3) for single-threaded, is unnecessary, and creates additional point of
		// failure at the end of length process of crunching millions of trees
		// 4) I have another idea, it is a spoiler but it will get around the lack
		// of multi-threading for initial brainstormed implementation of hard
		// multi-tree
		if workData.stkExtra == nil {
			// if rearrangement is used with multi-tree again, this catches changes
			// programmer forgot to implement, such as NOT calling StkFree anymore,
			// which deletes this data exactly
			Log.Printf("Impossible to do rearrangement -- workData.stkExtra not kept (programmer error)\n")
			Log.Printf("Rearrangement step skipped, program will proceed fine\n")
		} else {
			if workData.nodeType == NODETYPE_VANILLA {
				// NOTE MUST NOT rearrange segs via tooManySegsCantFix(_false_) prior
				// to this call !!! Assumes segs for subsectors were laid out
				// sequentially (recomputes FirstSeg based on SegCount as there
				// might have been integer overflow in FirstSeg)
				// NOTE call to tooManySegsCantFix(_true_) is ok, because it doesn't
				// modify stuff. Just don't mess with seg arrangement
				workData.RearrangeBSPVanilla(rootNode, pristineVertexCache,
					pristineVertexMap)
			} else if workData.nodeType == NODETYPE_DEEP {
				workData.RearrangeBSPDeep(rootNode, pristineVertexCache,
					pristineVertexMap)
			} else { // NODETYPE_EXTENDED / NODETYPE_COMPRESSED
				workData.RearrangeBSPExtended(rootNode, pristineVertexCache,
					pristineVertexMap)
			}
		}
	}

	Log.Printf("Height of left and right subtrees = (%d,%d)\n", hLeft, hRight)
	if workData.nodeType == NODETYPE_VANILLA {
		workData.reverseNodes(rootNode)
	} else {
		// Same node structure for Deep, Extended and Compressed non-GL nodes
		workData.reverseDeepNodes(rootNode)
	}
	Log.Printf("Max seg count in subsector: %d\n", workData.totals.maxSegCountInSubsector)
	if workData.totals.preciousSplit > 0 {
		// TODO Hexen contains some awkwardly designed polyobjects, placed in
		// non-convex sectors, sectors joined with others (map10) although
		// not remotely etc. Until polyobject containing part is determined more
		// precisely, this report (and the total itself) is not much use.
		// NOTE I intend to use this field (totals.preciousSplit) for multi-tree
		// so that it would select trees where polyobjs were not damaged. But
		// currently the number paints worse picture than there actually is: in
		// a lot of cases with non-zero number polyobjs are functioning as
		// intended
		// It seems that rather than being computed in-progress, the splits need
		// to be counted after the tree is done, via determining the number of
		// subsectors within the sector, and plus determining whether the sector
		// was originally convex, and if not, the minimal number of convex
		// polygons it could have been split into (not entirely sure, mind you...
		// I doubt we can read the mind of the user to know which exactly
		// subsection of sector should have been polyobject performing in if
		// sector is non-convex. Who knows if the user may have made a blunder
		// and it was not supposed to be working anyway?)
		//Log.Printf("Bad splits (split precious lines or lines of polyobj containing sector): %d\n",
		//	workData.totals.preciousSplit)
	}

	if workData.upgradableToDeep {
		if workData.nodeType == NODETYPE_DEEP {
			Log.Printf("I have switched to DeeP nodes format to avoid overflow.\n")
		} else {
			Log.Printf("I have kept nodes in vanilla format.\n")
		}
	}

	// Now we can actually UNDO all our hard work if limits were exceeded
	if uint32(workData.totals.numSSectors)&workData.SsectorMask == workData.SsectorMask {
		if !cloneEarly {
			Log.Error("Number of subsectors is too large (%d) for this format. Lumps NODES, SEGS, SSECTORS will be emptied.\n",
				workData.totals.numSSectors)
		}
		workData.emptyNodesLumps()
	} else if workData.tooManySegsCantFix(false) {
		if !cloneEarly {
			Log.Error("Number of segs is too large (%d) for this format. Lumps NODES, SEGS, SSECTORS will be emptied.\n",
				len(workData.segs))
		}
		workData.emptyNodesLumps()
	}

	if input.cacheSideness {
		Log.Printf("[nodes] Sideness cache was generated for %d INITIAL aliases (out of %d total aliases) for %d (out of %d) linedefs \n",
			workData.sidenessCache.maxKnownAlias,
			workData.segAliasObj.maxAlias, workData.sidenessCache.colCount,
			input.lines.Len())
	}

	if workData.nodeType == NODETYPE_DEEP {
		input.nodesChan <- NodesResult{
			deepNodes:      workData.deepNodes,
			deepSegs:       workData.deepSegs,
			deepSubsectors: workData.deepSubsectors,
		}
	} else if workData.nodeType == NODETYPE_VANILLA {
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
				rawNodes:   workData.getZdoomNodesBytes(),
				compressed: workData.nodeType == NODETYPE_ZDOOM_COMPRESSED,
			}
		}
	}
	if !alreadyCloned {
		if treeCount == 0 {
			Log.Printf("Nodes took %s\n", time.Since(start))
		} else {
			dur := time.Since(start)
			avg := time.Duration(int64(dur) / int64(treeCount))
			Log.Printf("Nodes took %s (avg %s per tree)\n", dur, avg)
		}
	}
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

func (n Number) Floor() int {
	return int(n)
}

func (n Number) Floor16() int16 {
	return int16(n)
}

// On Number, this truncates x. The value of n is ignored (is used only
// to determine type of truncation). See also zdefs.go!ZNumber.Trunc
func (n Number) Trunc(x float64) float64 {
	return float64(int64(x))
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
			r.Xmin = fv.X.Floor()
		}
		if fv.X > Number(r.Xmax) {
			r.Xmax = fv.X.Ceil()
		}
		if fv.Y < Number(r.Ymin) {
			r.Ymin = fv.Y.Floor()
		}
		if fv.Y > Number(r.Ymax) {
			r.Ymax = fv.Y.Ceil()
		}

		if tv.X < Number(r.Xmin) {
			r.Xmin = tv.X.Floor()
		}
		if tv.X > Number(r.Xmax) {
			r.Xmax = tv.X.Ceil()
		}
		if tv.Y < Number(r.Ymin) {
			r.Ymin = tv.Y.Floor()
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

func NodeBoundsToLevelBounds(n *NodeBounds) *LevelBounds {
	return &LevelBounds{
		Xmin: int16(n.Xmin),
		Ymin: int16(n.Ymin),
		Xmax: int16(n.Xmax),
		Ymax: int16(n.Ymax),
	}
}

func UnionNodeBounds(b1, b2 *NodeBounds) *NodeBounds {
	bu := &NodeBounds{
		Xmin: b1.Xmin,
		Xmax: b1.Xmax,
		Ymin: b1.Ymin,
		Ymax: b1.Ymax,
	}
	if b2.Xmin < bu.Xmin {
		bu.Xmin = b2.Xmin
	}
	if b2.Xmax > bu.Xmax {
		bu.Xmax = b2.Xmax
	}
	if b2.Ymin < bu.Ymin {
		bu.Ymin = b2.Ymin
	}
	if b2.Ymax > bu.Ymax {
		bu.Ymax = b2.Ymax
	}
	return bu
}

func GetLineBounds(lines AbstractLines) *NodeBounds {
	var r NodeBounds
	// using 32-bit signed min/max values as initial, even though most engines
	// don't have coords this big
	r.Xmax = -2147483648
	r.Ymax = -2147483648
	r.Xmin = 2147483647
	r.Ymin = 2147483647
	l := lines.Len()
	for i := uint16(0); i < l; i++ {
		x1, x2, y1, y2 := lines.GetAllXY(i)

		if x1 < r.Xmin {
			r.Xmin = x1
		}
		if x1 > r.Xmax {
			r.Xmax = x1
		}
		if y1 < r.Ymin {
			r.Ymin = y1
		}
		if y1 > r.Ymax {
			r.Ymax = y1
		}

		if x2 < r.Xmin {
			r.Xmin = x2
		}
		if x2 > r.Xmax {
			r.Xmax = x2
		}
		if y2 < r.Ymin {
			r.Ymin = y2
		}
		if y2 > r.Ymax {
			r.Ymax = y2
		}
	}
	return &r
}

// splitDist computes offset: distance along linedef to start of seg
// takes flip bit into account
func splitDist(lines AbstractLines, seg *NodeSeg) int {
	var dx, dy float64

	if seg.getFlip() == 0 {
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
		Log.Error("Trouble in splitDist %d!%d %f,%f\n", seg.Linedef, seg.getFlip(), dx, dy)
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
		Log.Error("Trouble in computeIntersection dx,dy\n")
	}
	if dx2 == 0 && dy2 == 0 {
		Log.Error("Trouble in computeIntersection dx2,dy2\n")
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
//
//	1,5 = point is to the left of the line
//	2,6 = point is to the right of the line
//
// There are some failsafes in here, these mainly check for small errors in the
// side checker.
// VigilantDoomer: "small errors in side checker" - you see, PickNode_* doesn't
// evaluate where segs go precisely as it is done here (*might* have been done
// on purpose, for the sake of performance, by Lee Killough). Something to keep
// in mind going forward, as doing things the same way would enable me to borrow
// superblocks idea from AJ-BSP (the speed-up from that I expect would be
// greater than performance loss from doing things more accurately)
func doLinesIntersectStandard(c *IntersectionContext) uint8 {
	dx2 := c.psx - c.lsx // Checking line -> partition
	dy2 := c.psy - c.lsy
	dx3 := c.psx - c.lex
	dy3 := c.psy - c.ley

	a := c.pdy*dx2 - c.pdx*dy2
	b := c.pdy*dx3 - c.pdx*dy3
	if DiffSign(a, b) && (a != 0) && (b != 0) { // Line is split, just check that

		x, y := c.computeIntersection()
		dx2 = c.lsx - x // Find distance from line start
		dy2 = c.lsy - y // to split point
		if dx2 == 0 && dy2 == 0 {
			a = 0
		} else {
			// NOTE this margin may lead to errors on complex, very detailed
			// levels (or areas), so:
			// 1. For non-extended nodes, there is alternate function
			// doLinesIntersectDetail
			// 2. For extended nodes, I had to completely replace the check
			// with more expensive but accurate one from ZDBSP
			// -- VigilantDoomer
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

// doLinesIntersect for detailed maps
func doLinesIntersectDetail(c *IntersectionContext) uint8 {
	dx2 := c.psx - c.lsx // Checking line -> partition
	dy2 := c.psy - c.lsy
	dx3 := c.psx - c.lex
	dy3 := c.psy - c.ley

	a := c.pdy*dx2 - c.pdx*dy2
	b := c.pdy*dx3 - c.pdx*dy3
	if DiffSign(a, b) && (a != 0) && (b != 0) { // Line is split, just check that

		x, y := c.computeIntersection()
		dx2 = c.lsx - x // Find distance from line start
		dy2 = c.lsy - y // to split point
		if dx2 == 0 && dy2 == 0 {
			a = 0
		} else {
			// Margin decreased to account for maps such as in Water Spirit, etc.
			// -- VigilantDoomer
			l := WideNumber(dx2)*WideNumber(dx2) + WideNumber(dy2)*WideNumber(dy2)
			if l < WideNumber(2) { // Killough used 4 not 2
				// If either ends of the split
				// are smaller than 1 (was 2) pixs then
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
			if l < WideNumber(2) { // same as start of line
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

func DiffSign(a, b Number) bool {
	return (a ^ b) < 0
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
	// This is used to implement transparent doors, for example
	// April 2023: when sector tag >= 900, seg is marked with SEG_FLAG_COALESCE

	sector := ts.sector
	if ts.flags&SEG_FLAG_COALESCE == 0 {
		line := ts.next
		for line != nil {
			csector := line.sector
			if csector != sector {
				if ts.flags&SEG_FLAG_COALESCE == 0 {
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
				val := w.doLinesIntersect(&c)
				if val&34 != 0 {
					if nonConvexityMode == NONCONVEX_ONESECTOR {
						w.mlog.DumpSegs(ts)
					}
					return nonConvexityMode // MUST SPLIT
				}
			}
		}
	}

	// TODO seg-count-in-subsector limit for vanilla and DeeP formats is 32767
	// (signed) / 65535 (unsigned). This is BAD. And because we have not exited
	// proc earlier, it likely means this is otherwise convex, and we are not
	// getting much luck splitting it by using only segs as partition candidates
	// So, need two things:
	// 1. An example map with obnoxious setup - convex sector with more than
	// 32767 linedefs (?) bordering it. Will have to use autogeneration
	// 2. Implementation of that stuff from Zennode about splitting convex
	// multi-sectored (or whatever) by arbitrary partitions from vertices, not
	// segs/linedefs

	// no need to split the list: these Segs can be put in a SSector
	return CONVEX_SUBSECTOR
}

// Zennode (the nodebuilder, not the algo inspired by it) does special
// optimization for convex multi-sectors. But what are they in first place
func (w *NodesWork) IsConvexMultiSector(ts *NodeSeg) bool {
	multisector := false
	sector := ts.sector
	for check := ts; check != nil; check = check.next {
		if check.sector != sector {
			multisector = true
			break
		}
	}
	if !multisector { // had only single sector
		return false
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
				val := w.doLinesIntersect(&c)
				if val&34 != 0 { // non-convex
					return false
				}
			}
		}
	}
	return true // convex MULTI-sector
}

// Adds a subsector. This version handles only deep and vanilla nodes format.
// For prototype of Zdoom counterpart, see zdefs.go!ZCreateSSector_Proto, it is
// that function from which ZExt_CreateSSector is generated, not this one
func (w *NodesWork) CreateSSector(tmps *NodeSeg) uint32 {
	// TODO check that stuff is within limits. Currently node lumps are emptied
	// AFTER the whole process completes if limit for current format was
	// exceeded. Might try to detect condition earlier?

	w.totals.numSSectors++
	if w.upgradableToDeep && w.nodeType == NODETYPE_VANILLA &&
		(uint32(w.totals.numSSectors)&SSECTOR_NORMAL_MASK) != 0 {
		// switch to DeeP nodes
		w.mlog.Printf("Subsector limit reached for vanilla nodes format. Switching to DeeP nodes format.\n")
		w.upgradeToDeep()
	}

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

	var currentCount uint32
	if w.nodeType == NODETYPE_DEEP {
		w.deepSubsectors[subsectorIdx].FirstSeg = oldNumSegs
		for ; tmps != nil; tmps = tmps.next {
			tmps.block = nil
			tmps.nextInSuper = nil
			w.deepSegs = append(w.deepSegs, DeepSeg{
				StartVertex: uint32(tmps.StartVertex.idx),
				EndVertex:   uint32(tmps.EndVertex.idx),
				Angle:       tmps.Angle,
				Linedef:     tmps.Linedef,
				Flip:        tmps.getFlip(),
				Offset:      tmps.Offset,
			})
		}
		currentCount = uint32(len(w.deepSegs)) - oldNumSegs
	} else { // vanilla
		w.subsectors[subsectorIdx].FirstSeg = uint16(oldNumSegs) // MIGHT overflow - try changing layout if that happens
		for ; tmps != nil; tmps = tmps.next {
			tmps.block = nil
			tmps.nextInSuper = nil
			w.segs = append(w.segs, Seg{
				StartVertex: uint16(tmps.StartVertex.idx),
				EndVertex:   uint16(tmps.EndVertex.idx),
				Angle:       tmps.Angle,
				Linedef:     tmps.Linedef,
				Flip:        tmps.getFlip(),
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
	rec := SimpleVertex{int(x), int(y)}
	idx, exists := w.vertexCache[rec]
	if exists { // happens rather rarely, and not at all on some maps
		w.vertexExists++
		if w.vertexSink != nil {
			w.vertexSink = append(w.vertexSink, idx)
		}
		//w.mlog.Printf("Vertex that existed: %d\n", idx)
		return &(w.vertices[idx])
	}
	idx = len(w.vertices)
	if w.lines.AddVertex(int(x), int(y)) != idx {
		Log.Panic("Inconsistent state in NodesWork.AddVertex\n")
	}
	w.vertices = append(w.vertices, NodeVertex{
		X:   x,
		Y:   y,
		idx: uint32(idx),
	})
	w.vertexCache[rec] = idx
	if w.upgradableToDeep && w.nodeType == NODETYPE_VANILLA &&
		len(w.vertices) > 65536 {
		// switch to DeeP nodes
		w.mlog.Printf("Vertices limit reached for vanilla nodes format. Switching to DeeP nodes format.\n")
		w.upgradeToDeep()
	}
	if w.vertexSink != nil {
		w.vertexSink = append(w.vertexSink, idx)
	}
	return &(w.vertices[idx])
}

func (w *NodesWork) SetNodeCoords(part *NodeSeg, bbox *NodeBounds,
	c *IntersectionContext) {
	// In node format without extra precision of segs, part may no longer be
	// alongside the original linedef, so picking linedef coords instead of
	// seg coords (like some nodebuilders do who also do GL nodes output) is not
	// a good idea.
	w.nodeX = int(part.StartVertex.X)
	w.nodeY = int(part.StartVertex.Y)
	w.nodeDx = part.EndVertex.X.Ceil() - w.nodeX
	w.nodeDy = part.EndVertex.Y.Ceil() - w.nodeY
	// Check if there is overflow
	if w.nodeDx <= 32767 && w.nodeDy <= 32767 && w.nodeDx >= -32768 &&
		w.nodeDy >= -32768 {
		// No overflow - good
		return
	}

	// If we are still here, we have an atypically long line.

	// Scale down via integer division, if can do _with zero remainders_
	dd := GCD(Abs(w.nodeDx), Abs(w.nodeDy))
	if dd != 1 {
		w.nodeDx = w.nodeDx / dd
		w.nodeDy = w.nodeDy / dd
		if dd > 2 {
			// Distance from partition line to other segs - if it is large - can
			// cause numerical overflows and visual glitches in run-time even in
			// advanced ports (PrBoom-plus etc). In particular, without this
			// step there would be glitches in PrBoom-Plus when standing near
			// either end of original linedef. Thus I try to keep it small by
			// placing segment at the line's center
			// This mitigation can't do anything for vanilla, though - vanilla
			// has significant visual glitches with ANY long line in view (not
			// just partition lines), which can not be helped
			d2 := dd / 2
			w.nodeX += d2 * w.nodeDx
			w.nodeY += d2 * w.nodeDy
		}
	}

	if w.nodeDx <= 32767 && w.nodeDy <= 32767 && w.nodeDx >= -32768 &&
		w.nodeDy >= -32768 {
		w.mlog.Verbose(1, "Prevented partition line coords overflow (from segment of linedef %d).\n",
			part.Linedef)
		return
	}

	// Could do nothing
	// TODO could be acceptable enough if I scale down with rounding? Not sure -
	// the angle would be different and that should be probably considered when
	// sorting/splitting segs to each side?

	w.mlog.Verbose(1, "Overflow: partition line DX=%d, DY=%d (from segment of linedef %d) can not be represented correctly due to (-32768,32767) signed int16 range limit in format. Parts of map will not be rendered correctly in any port.\n",
		w.nodeDx, w.nodeDy, part.Linedef)
}

// GCD returns greatest common divisor for POSITIVE integers
func GCD(a, b int) int {
	for b != 0 {
		tmp := b
		b = a % b
		a = tmp
	}
	return a
}

// Hard multi-tree needs a stable way to identify segs to use as partitions
// Segs are normally divided many times, and are unique to their threads
// Where as PartSeg is an identifier that would work across threads
func (w *NodesWork) GetPartSegs(ts *NodeSeg, best *NodeSeg, receiver *[]PartSeg) {
	wasNil := false
	if len(w.parts) == 0 {
		if w.parts == nil { // just in case
			w.parts = make([]*NodeSeg, 0)
			wasNil = true
		}
		w.parts = append(w.parts, best)
	}
	if *receiver == nil {
		*receiver = make([]PartSeg, 0)
	}

	limit := w.width
	if limit < 0 { // unlimited
		limit = len(w.parts)
	} else if limit > len(w.parts) {
		// limit is greater, but the way I am going to use the value I need to
		// lower it to actual length to avoid go panicking
		limit = len(w.parts)
	}

	for _, v := range w.parts[:limit] {
		partseg := getPartSeg(ts, v)
		*receiver = append(*receiver, partseg)
	}
	fold := false
	for _, v := range *receiver {
		if v.Occurence == -1 {
			fold = true
			break
		}
	}
	if fold {
		// receiver contained unrecognised segs - replace with just one
		// unrecognised marker (signaling "wtf" condition)
		// TODO might actually be a normal condition sometime later when
		// not all partitions are from segs, but currently very suspicious as
		// all partitioners currently only build partitions from segs
		w.mlog.Printf("Best partition didn't come from seg\n")
		*receiver = (*receiver)[:0]
		*receiver = append(*receiver, PartSeg{
			Linedef:   0,
			Occurence: -1,
		})
	}
	if wasNil {
		w.parts = nil
	} else {
		w.parts = w.parts[:0]
	}
}

func getPartSeg(ts *NodeSeg, target *NodeSeg) PartSeg {
	ldef := target.Linedef
	occurence := 0
	for tmps := ts; tmps != nil; tmps = tmps.next {
		if tmps.Linedef == ldef {
			occurence++
		}
		if tmps == target {
			return PartSeg{
				Linedef:   ldef,
				Occurence: occurence,
			}
		}
	}
	return PartSeg{
		Linedef:   0,
		Occurence: -1, // marker that was not found
	}
}

// Split a list of segs (ts) into two using the method described at bottom of
// file, this was taken from OBJECTS.C in the DEU5beta source. Well, the actual
// splitting was refactored to DivideSegsActual (called at the end of this
// method), because a twin named "DivideSegsForSingleSector" exists in
// convexity.go
func (w *NodesWork) DivideSegs(ts *NodeSeg, rs **NodeSeg, ls **NodeSeg, bbox *NodeBounds,
	super *Superblock, rightsSuper, leftsSuper **Superblock, partsegs *[]PartSeg) {
	// Pick best node to use
	best := w.pickNode(w, ts, bbox, super)

	if best == nil {
		// To programmers: write PickNode so it never happens
		Log.Panic("Couldn't pick nodeline!\n")
	}

	if partsegs != nil {
		w.GetPartSegs(ts, best, partsegs)
	}

	c := &IntersectionContext{
		psx: best.StartVertex.X,
		psy: best.StartVertex.Y,
		pex: best.EndVertex.X,
		pey: best.EndVertex.Y,
	}
	c.pdx = c.psx - c.pex
	c.pdy = c.psy - c.pey

	// Node line coords
	w.SetNodeCoords(best, bbox, c)

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

	w.dismissChildrenToSuperblockPool(super)
	*rightsSuper = w.getNewSuperblock(super)
	*leftsSuper = w.getNewSuperblock(super)
	(*rightsSuper).SetBounds(bbox)
	(*leftsSuper).SetBounds(bbox)
	w.returnSuperblockToPool(super)

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
				// 20230307 Now that I have experimented with algorithms that
				// could indeed lead to satisfying tmps == best.partner before
				// tmps == best, I know that tmps = tmps.next assignment must
				// be done unconditionally
				tmps = tmps.next
				// let's see why
				// if tmps == best is what occurs, the layout is
				// [..., best == tmps, best.next == best.partner ...]
				// so we do both at once and then skip
				// if tmps == best.partner is what occurs, we have
				// [..., smth == tmps, smth.next == smth.partner == best, ...]
				// once again, standing on tmps we do both and must skip both
			}
		}
		tmpsNext = tmps.next

		addSegToSide(addToRs[:], &rights, &strights)
		addSegToSide(addToLs[:], &lefts, &stlefts)
	}

	if strights == nil {
		// VigilantDoomer - this branch must never happen (seg from which
		// partition is derived is hardcored to go right)
		Log.Panic("Should have had partition at the right side (hardcoded to go there).\n")
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
			w.mlog.Verbose(1, "No left side, moving partition into left side (single sector #%d)\n",
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
				if s != "" { // should be always true
					s = s[1:] // remove "," at the beginning (",3,4,5" => "3,4,5")
				}
				w.mlog.Verbose(1, "No left side, moving partition into left side (sectors %s)\n", s)
			} else {
				w.mlog.Verbose(1, "No left side, moving partition into left side (multiple sectors in this node)\n")
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

// CategoriseAndMaybeDivideSeg decides whether seg needs to be split (and
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
		Log.Panic("Partnership was not kept up to date!\n")
	}
	c.lsx = atmps.StartVertex.X
	c.lsy = atmps.StartVertex.Y
	c.lex = atmps.EndVertex.X
	c.ley = atmps.EndVertex.Y
	val := w.doLinesIntersect(c)
	if ((val&2 != 0) && (val&64 != 0)) || ((val&4 != 0) && (val&32 != 0)) {
		// Partition intersects this seg in tmps parameter, so it needs to be split
		if atmps.flags&SEG_FLAG_PRECIOUS != 0 {
			// undesirable split MIGHT have occured. Hexen.wad actually has lot
			// of setups that are non-convex sectors, etc. so this doesn't
			// necessarily mean things were broken
			w.totals.preciousSplit++
		}
		x, y := c.computeIntersection()
		newVertex := w.AddVertex(x, y)
		news := new(NodeSeg)
		atmps.alias = 0 // clear alias, as angle may change after split
		if w.sidenessCache.maxKnownAlias > 0 {
			if getGlobalFlip(atmps) {
				atmps.flags |= SEG_FLAG_GLOBAL_FLIP
			} else {
				atmps.flags &= ^uint8(SEG_FLAG_GLOBAL_FLIP)
			}
		}
		atmps.flags |= SEG_FLAG_NOCACHE // not in cache because split
		*news = *atmps
		atmps.next = news
		news.StartVertex = newVertex
		atmps.EndVertex = newVertex
		news.Offset = uint16(splitDist(w.lines, news))
		w.totals.segSplits++
		if atmps.partner != nil {
			if atmps.partner.flags&SEG_FLAG_PRECIOUS != 0 {
				w.totals.preciousSplit++
			}
			w.totals.segSplits++
			atmps.partner.alias = 0 // clear alias, as angle may change after split
			if w.sidenessCache.maxKnownAlias > 0 {
				if getGlobalFlip(atmps.partner) {
					atmps.partner.flags |= SEG_FLAG_GLOBAL_FLIP
				} else {
					atmps.partner.flags &= ^uint8(SEG_FLAG_GLOBAL_FLIP)
				}
			}
			atmps.partner.flags |= SEG_FLAG_NOCACHE // not in cache because split
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

	// NOTE length computation here not good enough for extended nodes, due to
	// Offset being integer. Instead for extended nodes, will recompute length
	// in recomputeOneSeg
	originNewLen := int(newSeg.Offset) - int(originSeg.Offset)
	originSeg.len = Number(originNewLen)
	newSeg.len = newSeg.len - Number(originNewLen)

	oldAngle := originSeg.Angle
	w.recomputeOneSeg(originSeg)
	w.recomputeOneSeg(newSeg)
	if oldAngle != originSeg.Angle || oldAngle != newSeg.Angle {
		// Happens often enough btw
		w.mlog.Verbose(3, "Angle changed after splitting seg:  %d -> (%d ; %d)\n",
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
	w.updateSegLenBetter(s) // anchor to inject precise len computation for extended nodes
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
		isFront := s.getFlip() == 0
		sdef := w.lines.GetSidedefIndex(s.Linedef, isFront)
		s.Angle += int16(float64(w.sides[sdef].XOffset) * float64(65536.0/360.0))
	}
}

// Placeholder that will be overridden for extended nodes when code generator
// is used
func (w *NodesWork) updateSegLenBetter(s *NodeSeg) {
	// does nothing for normal nodes. For normal nodes, seg length is already
	// computed "good enough" using integer offset, but for extended nodes,
	// this won't fly and so you'll find an override defined in zdefs.go for
	// code generator to parse and generate znodegen.go
}

func CreateNode(w *NodesWork, ts *NodeSeg, bbox *NodeBounds, super *Superblock) *NodeInProcess {
	res := new(NodeInProcess)
	var rights *NodeSeg
	var lefts *NodeSeg
	var rightsSuper *Superblock
	var leftsSuper *Superblock
	// Divide node in two
	w.totals.numNodes++
	w.DivideSegs(ts, &rights, &lefts, bbox, super, &rightsSuper, &leftsSuper, nil)
	// NOTE after DivideSegs return, super may no longer be valid
	super = nil
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
		res.LChild = w.CreateSSector(lefts) | SSECTOR_DEEP_MASK
		w.returnSuperblockToPool(leftsSuper)
	} else if state == NONCONVEX_ONESECTOR {
		res.nextL = w.createNodeSS(w, lefts, leftBox, leftsSuper)
		res.LChild = 0
	} else {
		if config.VerbosityLevel >= 1 && w.IsConvexMultiSector(lefts) { // reference to global: config
			w.dumpMultiSectorSegs(lefts)
		}
		res.nextL = CreateNode(w, lefts, leftBox, leftsSuper)
		res.LChild = 0
	}
	leftsSuper = nil

	// These will form the right box
	rightBox := FindLimits(rights)
	res.Rbox[BB_TOP] = int16(rightBox.Ymax)
	res.Rbox[BB_BOTTOM] = int16(rightBox.Ymin)
	res.Rbox[BB_LEFT] = int16(rightBox.Xmin)
	res.Rbox[BB_RIGHT] = int16(rightBox.Xmax)
	state = w.isItConvex(rights)
	if state == CONVEX_SUBSECTOR {
		res.nextR = nil
		res.RChild = w.CreateSSector(rights) | SSECTOR_DEEP_MASK
		w.returnSuperblockToPool(rightsSuper)
	} else if state == NONCONVEX_ONESECTOR {
		res.nextR = w.createNodeSS(w, rights, rightBox, rightsSuper)
		res.RChild = 0
	} else {
		if config.VerbosityLevel >= 1 && w.IsConvexMultiSector(rights) { // reference to global: config
			w.dumpMultiSectorSegs(rights)
		}
		res.nextR = CreateNode(w, rights, rightBox, rightsSuper)
		res.RChild = 0
	}
	rightsSuper = nil

	//CheckNodeBounds(bbox, leftBox, rightBox)

	return res
}

func (w *NodesWork) dumpMultiSectorSegs(ts *NodeSeg) {
	if config.VerbosityLevel < 1 { // reference to global: config
		return
	}
	var buf bytes.Buffer
	buf.WriteString("Multiple sectors detected that form something CONVEX!\n")
	for check := ts; check != nil; check = check.next {
		buf.WriteString(fmt.Sprintf(
			"  Sector: %v Linedef: %v Flip: %v (%v,%v) - (%v, %v)\n",
			check.sector, check.Linedef, check.getFlip(),
			check.psx, check.psy, check.pex, check.pey))
	}
	w.mlog.Verbose(1, buf.String())
}

const FIXED16DOT16_MULTIPLIER = 65536.0

// Redundant declaration on Number, but will be relevant on ZNumber
func (n Number) ToFixed16Dot16() int32 {
	return int32(float64(n) * FIXED16DOT16_MULTIPLIER)
}

func (s *NodeSeg) getFlip() int16 {
	if s.flags&SEG_FLAG_FLIP != 0 {
		return 1
	}
	return 0
}

// first seg is not stored for Zdoom extended/compressed nodes - compute
func getFirstSegExtended(ssubsectors []uint32, ssector uint32) uint32 {
	cum := uint32(0)
	for i := uint32(0); i < ssector; i++ {
		cum += ssubsectors[i]
	}
	return cum
}

// suppressErrorDueToExtraLogImport is never called
// znodegen.go gets "log" import added to it as the result of
// code generation, but nothing uses it, and unused imports cause build
// to fail, as go is very strict about it. So this stupid function will be
// copied to znodegen and use up the import
func suppressErrorDueToExtraLogImport(ts *NodeSeg) {
	// Notice it is lower case "log" that I never use in actually called code
	log.Panic("Function that was not supposed to be called was called.\n")
}

// upgradeToDeep is called when limits were exceeded while building vanilla
// nodes. User gave consent to switch to DeeP nodes format to overflow
// This can be called either during or after the nodebuilding process
func (w *NodesWork) upgradeToDeep() {
	if w.deepSegs != nil {
		Log.Panic("upgradeToDeep called twice on the same NodesWork structure (programmer error)\n")
	}
	w.deepSegs = make([]DeepSeg, len(w.segs))
	for i := range w.segs {
		w.deepSegs[i].Angle = w.segs[i].Angle
		w.deepSegs[i].EndVertex = uint32(w.segs[i].EndVertex)
		w.deepSegs[i].StartVertex = uint32(w.segs[i].StartVertex)
		w.deepSegs[i].Flip = w.segs[i].Flip
		w.deepSegs[i].Linedef = w.segs[i].Linedef
		w.deepSegs[i].Offset = w.segs[i].Offset
	}
	w.deepSubsectors = make([]DeepSubSector, len(w.subsectors))
	firstSeg := uint32(0)
	for i := range w.subsectors {
		// w.subsectors[i].FirstSeg is uint16 and can't be relied upon
		w.deepSubsectors[i].FirstSeg = firstSeg
		w.deepSubsectors[i].SegCount = w.subsectors[i].SegCount
		firstSeg += uint32(w.subsectors[i].SegCount)
	}
	w.segs = nil
	w.subsectors = nil
	w.SsectorMask = SSECTOR_DEEP_MASK
	w.nodeType = NODETYPE_DEEP
}

func isZDoomNodes() bool {
	return false
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
