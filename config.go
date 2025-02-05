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
	"os"
)

const PROG_CAPIT_NAME = "VigilantBSP"
const VERSION = "0.92"

/*
-b Rebuild BLOCKMAP.
	o= Offset configuration
		0 0,0 offset BLOCKMAP (zennode preset)
		1 8,8 offset BLOCKMAP (BSP v5.2 preset)
		2 Best of 36 offset combinations (default)
		3 Heuristic method to reduce from 65536 offsets
		4 Best of all 65536 offset combinations
		x,y Specify specific offsets
	r Remove non-collideable lines from blockmap (default: disabled)
		This option is experimental and is likely to break
		non-vanilla or non-Doom maps. Enable at your own risk
	s Subset compress BLOCKMAP.
	    this parameter may be optionally followed by question mark*
	a Aggressive subset compression. If enabled, overrides s.
	    this parameter may be optionally followed by question mark*
	z= Zero/dummy header configuration
		0 No dummy header
		1 Use dummy header, but not necessary zero (default)
		2 Use linedef #0 as dummy header
	t= Number of cores to use when trying multiple offsets
		(defaults to number of cores available)
	e= Endgoal for multiple offsets
		0 Smallest blockmap size (default)
		1 Blockmap fits vanilla limits is enough
		2 Blockmap works in limit-removing ports is enough
  * if parameter is followed by question mark, it is tried when a working blockmap can't be produced otherwise.

-d Deterministic output (default: disabled)

-n Rebuild NODES.
	a= Partition selection algorithm.
		0 Seg balancing only
		1 Visplane reduction per Lee Killough
		2 Advanced visplane reduction
		3 Maelstrom - fastest build speed
		4 Zennode/Zokumbsp-like (tree balance and depth)
	p= Priority for partition selection (if applicable).
		0 Split minimization (default for AVR)
		1 Depth reduction
		2 Balanced tree (default for Zennode-like)
	m= Multi-tree mode (experimental, SLOW).
		0 Don't use, build single tree (default)
		1 Try every one-sided linedef as a possible root partition
		2 Try every two-sided linedef as a possible root partition
		3 Try every linedef as a possible root partition
	t= Number of threads to use for multi-tree
		(defaults to number of cores available)
	i= Cull (don't create segs from) invisible linedefs.
		0 Don't cull (default)
		1 Cull, use faulty check to preserve self-referencing sectors.
		2 Cull, robust preservation of self-referencing sectors.
	c= NODES format compatibility
		v Vanilla format which all software ports recognise
		d Deep nodes format
		x Zdoom extended nodes format
		z Zdoom compressed nodes format
			or zN where N is a single digit between 1 and 9
		V Vanilla+ (remove multi-tree bias against unsigned trees)
			identical to "v" when not in multi-tree mode
		D Prefer vanilla, switch to Deep nodes on overflow
		X Prefer vanilla, switch to Zdoom extended nodes on overflow
		Z Prefer vanilla, switch to Zdoom compressed nodes on overflow
		  or ZN where N is a single digit between 1 and 9
	f= Tuning factor (seg split cost, etc.)
		17 - default seg split cost
	d= Penalty factor for _diagonal_ lines
		34 - default for Hexen format levels, for others 0 (disabled)
		Explicitly specifying non-zero value will enable it for all levels,
		while explicitly specifying 0 will disable it even for Hexen levels

-r Rebuild REJECT resource.
	z Insert zero-filled REJECT resource
	g Use graphs to reduce LOS calculations (default: enabled)
	s= If 2-sided lines have same sector on both sides
		0 Mark such sectors as always visible (default)
		1 Mark visible only when self-referencing sector effects are detected
		2 Be pedantic about self-referencing sector visibility
		3 Assume self-referencing sectors don't exist (not recommended)
		s+ will temporarily be interpreted as s=1
	m Process RMB option file (.rej)

-m (example -m:map01+map03) Rebuild only specific maps
	if it is instead written -!m:map01+map03, the meaning is inverse.

-v Add verbosity to text output. Use multiple times for increased verbosity.

*/

const (
	BM_OFFSET_FIXED = iota
	BM_OFFSET_THIRTYSIX
	BM_OFFSET_BRUTEFORCE
	BM_OFFSET_HEURISTIC
)

const (
	REJECT_ZEROFILLED = iota
	REJECT_NORMAL
	REJECT_DONTTOUCH
)

const (
	PICKNODE_TRADITIONAL = iota
	PICKNODE_VISPLANE
	PICKNODE_VISPLANE_ADV
	PICKNODE_MAELSTROM
	PICKNODE_ZENLIKE
)

const (
	REJ_SELFREF_TRIVIAL       = iota // all sectors with 2-sided lines pointing to the same sector are marked always visible
	REJ_SELFREF_CHECK                // all sectors with self-referencing effect are marked always visible
	REJ_SELFREF_PEDANTIC             // visibility of sectors with self-referencing effect is determined as robustly as visibility of normal sectors
	REJ_SELFREF_IGNORE_ALWAYS        // ignore (do not check for) self-referencing sector effects, even in presence of RMB
)

const (
	CULL_SEGS_DONT = iota // don't cull segs
	CULL_SEGS_SLOPPY
	CULL_SEGS_PROPER
)

const ( // when to abort search for yet better blockmap
	BM_OFFSET_NOABORT      = iota // No abortion, search for the smallest blockmap possible
	BM_OFFSET_FITS_VANILLA        // abort search as soon as blockmap that works in vanilla is found
	BM_OFFSET_FITS_ANYPORT        // abort search as soon as blockmap that works in any port is found
)

const (
	PENALIZE_DIAGONALITY_HEXEN = iota
	PENALIZE_DIAGONALITY_ALWAYS
	PENALIZE_DIAGONALITY_NEVER
)

const (
	MINOR_CMP_NOOP = iota
	MINOR_CMP_SEGS
	MINOR_CMP_SECTORS
	MINOR_CMP_BALANCE
	MINOR_CMP_DEPTH
)

const (
	NODETYPE_VANILLA = iota
	NODETYPE_DEEP
	NODETYPE_ZDOOM_EXTENDED
	NODETYPE_ZDOOM_COMPRESSED
	NODETYPE_VANILLA_OR_DEEP
	NODETYPE_VANILLA_OR_ZEXTENDED
	NODETYPE_VANILLA_OR_ZCOMPRESSED
	NODETYPE_VANILLA_RELAXED
)

const (
	MULTITREE_NOTUSED = iota
	MULTITREE_ROOT_ONLY
	MULTITREE_HARD
)

// below constants must be chosen so that
// MROOT_ONESIDED & MROOT_TWOSIDED = 0
// MROOT_ONESIDED | MROOT_TWOSIDED = MROOT_EVERY
const (
	MROOT_NONE     = 0x00
	MROOT_ONESIDED = 0x01
	MROOT_TWOSIDED = 0x02
	MROOT_EVERY    = 0x03
)

// which version of doLinesIntersect to use
const (
	NODE_DETAIL_AUTO = iota
	NODE_DETAIL_SUPPRESS
	NODE_DETAIL_ALWAYS
	NODE_DETAIL_HEURISTIC
)

const (
	CACHE_SIDENESS_AUTO = iota
	CACHE_SIDENESS_NEVER
	CACHE_SIDENESS_ALWAYS
)

const (
	SECONDARY_PRIORITY_AUTO = iota
	SECONDARY_PRIORITY_SEGS
	SECONDARY_PRIORITY_SUBSECTORS
	SECONDARY_PRIORITY_BALANCE
)

const ( // bitmask
	BM_TRYCOND_NONE                          = 0x00
	BM_TRYCOND_PROPER_SUBSET_COMPRESSION     = 0x01
	BM_TRYCOND_AGGRESSIVE_SUBSET_ELIMINATION = 0x02
)

type ProgramConfig struct {
	InputFileName          string
	OutputFileName         string
	BlockmapXOffset        int16
	BlockmapYOffset        int16
	BlockmapOffsetMode     int
	BlockmapThreads        int16
	UseZeroHeader          bool
	ZeroHeaderIsZero       bool
	SubsetCompressBlockmap bool
	Reject                 int
	Profile                bool
	ProfilePath            string
	MemProfile             bool
	MemProfilePath         string
	NodesDebugFile         string
	Deterministic          bool
	AggressiveSubsets      bool // mimic the way zokumbsp "compresses" subsets - no reordering, just replace reference with that to a bigger blocklist
	VerbosityLevel         int
	// Function references can not be compared in Go (even for equality).
	// Thus PickNodeUser is an actual config option user might modify,
	// but PickNode and CreateNodeSS aren't
	PickNodeUser int // <- This is the actual config option
	// Derivative of secondary metric for picking a partition
	MinorCmpUser int // Used to be actual config option, replaced by SecondaryPriority
	//
	BlockmapSearchAbortion int    // when trying multiple offsets, finish the search for a good blockmap as soon as it fits the limit
	UseGraphsForLOS        bool   // use graphs for LOS calculations (build reject faster)
	DumpSegsFlag           bool   // seg debugging
	SegDumpFile            string // where do dumped segs go?
	CullInvisibleSegs      int    // do not create segs for linedefs that will be invisible anyway
	PenalizeSectorSplits   bool   // Another options which may or may not help with visplanes. Vigilant visplane algorithm only
	RejectSelfRefMode      int    // what measures are taken for self-referencing sector support in reject
	NodeType               int    // there are various BSP tree formats beside vanilla (Deep, Zdoom extended or compressed nodes). Prboom-Plus v2.5.1.5 supports these fine, except for compressed - try PrBoom-plus v2.6.2 for those
	PersistThroughInsanity bool   // when computing partition node length in PickNode_visplaneVigilant, ignore "sanity check failed" if it occurs and construct non-void intervals anyway
	// Rebuilding options allow to disable rebuilding some parts of level.
	// For reject, the "disable rebuilding" is specified through Reject field
	// instead via REJECT=REJECT_DONTTOUCH
	RebuildNodes    bool
	RebuildBlockmap bool
	PickNodeFactor  int
	// Diagonal penalty and option to enable/disable it conditionally or always
	DiagonalPenalty     int  // the value itself
	PenalizeDiagonality int  // whether and when the penalty is applied
	DepthArtifacts      bool // whether to enable Zennode/ZokumBSP factual deviations from algorithm described in their docs

	// FilterLevel is a list of level names to rebuild. All levels not in this
	// list are to be copied along, not rebuilt (unless FilterProhibitsLevels
	// is also set, which inverts the meaning). If list is nil or has zero size,
	// there is no filter: all levels have to be rebuilt.
	FilterLevel [][]byte
	// FilterProhibitsLevels signals whether the meaning of FilterLevel is
	// inverted and levels in FilterLevel are the ones NOT getting rebuilt
	FilterProhibitsLevels bool
	UseRMB                bool
	// Multi-tree mode
	MultiTreeMode   int
	SpecialRootMode int // used when MultiTreeMode = MULTITREE_ROOT_ONLY
	NodeThreads     int16
	//
	StraightNodes          bool // write nodes without traditional reversal
	RemoveNonCollideable   bool // dangerous option. Tries to reason about which lines are not necessary to be included in blockmap and removes then. Prone to break advanced maps and maps not for Doom
	NodeDetailFriendliness int  // whether to use detailed version of doLinesIntersect when building non-extended nodes
	//
	CacheSideness      int  // whether to use cache for WhichSide (for algorithms that use it)
	SecondaryPriority  int  // (since 0.82a) Secondary metric for picking a partition
	EffectiveSecondary int  // derivative value of SecondaryPriority that never has AUTO value
	StkNode            bool // whether to use stknode in conjuction MULTITREE_NOTUSED (multi-trees ignore this parameter)
	TreeWidth          int  // width of hard tree
	TreeReach          int  // how far does hard tree branch
	SpeedTree          bool // accelerate multitree at the cost of memory usage
	SpeedTreeExplicit  bool // whether the user explicitly passed --speedtree on/off
	Eject              bool // whether to only put rebuilt maps into output file and no other lumps
	ZdoomCompression   int  // zlib compression level for compressed Zdoom nodes
	Ableist            bool // whether to pick absolute best tree in multi-tree even if it is not compatible with vanilla but vanilla is required/preferred
	Roots              int  // how many (max) best roots to pick for plain multi-tree, default 0, which means try every linedef that satisfies SpecialRootMode
	RootsReadPending   bool // if need to set roots from width
	//
	BlockmapTryConditionally int // options to try conditionally when blockmap doesn't fit with non-conditional options
	//
	NoSymm      bool // disable symm reject interface
	DisableUDMF bool // don't recognize udmf maps
	HashLumps   bool // whether to calculate and print lump hash to standard output for level lumps
}

// PickNode values: PickNode_traditional, PickNode_visplaneKillough, PickNode_visplaneVigilant

var config *ProgramConfig // global variable that will be accessed from other threads too

func init() {
	// Initialize with defaults
	config = &(ProgramConfig{
		BlockmapXOffset:    0,
		BlockmapYOffset:    0,
		BlockmapOffsetMode: BM_OFFSET_THIRTYSIX,
		// BlockmapThreads is the number of bees to run when trying multiple
		// offsets. 0 means "auto = the number of cores, but no more than 16".
		// If set to specific value, that value is used, and can be greater than
		// 16.
		BlockmapThreads:          0,
		UseZeroHeader:            true,
		ZeroHeaderIsZero:         false,
		SubsetCompressBlockmap:   false,
		DumpSegsFlag:             false,
		SegDumpFile:              "",
		Profile:                  false,
		ProfilePath:              "",
		MemProfile:               false,
		MemProfilePath:           "",
		NodesDebugFile:           "",
		Reject:                   REJECT_NORMAL,
		Deterministic:            false,
		AggressiveSubsets:        false,
		VerbosityLevel:           0,
		PickNodeUser:             PICKNODE_TRADITIONAL,
		BlockmapSearchAbortion:   BM_OFFSET_NOABORT,
		UseGraphsForLOS:          true,
		CullInvisibleSegs:        CULL_SEGS_DONT,
		PenalizeSectorSplits:     true,
		RejectSelfRefMode:        REJ_SELFREF_TRIVIAL,
		NodeType:                 NODETYPE_VANILLA,
		PersistThroughInsanity:   true,
		RebuildNodes:             true,
		RebuildBlockmap:          true,
		PickNodeFactor:           PICKNODE_FACTOR,
		DiagonalPenalty:          DIAGONAL_PENALTY,
		PenalizeDiagonality:      PENALIZE_DIAGONALITY_HEXEN,
		MinorCmpUser:             MINOR_CMP_BALANCE,
		DepthArtifacts:           true,
		FilterLevel:              nil,
		UseRMB:                   false,
		MultiTreeMode:            MULTITREE_NOTUSED,
		SpecialRootMode:          MROOT_ONESIDED,
		NodeThreads:              0,
		StraightNodes:            false,
		RemoveNonCollideable:     false,
		NodeDetailFriendliness:   NODE_DETAIL_AUTO,
		CacheSideness:            CACHE_SIDENESS_AUTO,
		SecondaryPriority:        SECONDARY_PRIORITY_AUTO,
		EffectiveSecondary:       SECONDARY_PRIORITY_BALANCE, // will be overridden in config.FromCommandLine
		StkNode:                  false,
		TreeWidth:                0,  // auto
		TreeReach:                10, // default depth of branching for hard multi-tree
		FilterProhibitsLevels:    false,
		SpeedTree:                false, // overridden dependent on other parameters if SpeedTreeExplicit == false
		SpeedTreeExplicit:        false,
		Eject:                    false,
		ZdoomCompression:         6,
		Ableist:                  false,
		BlockmapTryConditionally: BM_TRYCOND_NONE,
		NoSymm:                   false,
		DisableUDMF:              true, //false, // not ready yet
		HashLumps:                false,
	})
}

// Moved away from init, used to confuse the "go test"!
func Configure() {
	Log.Printf("VigilantBSP ver %s\n", VERSION)
	Log.Printf("Copyright (c)   2022-2025 VigilantDoomer\n")
	Log.Printf("This program is built upon ideas first implemented in DEU by Raphael Quinet, \n")
	Log.Printf("BSP v5.2 by Colin Reed, Lee Killough and other contributors to BSP (program),\n")
	Log.Printf("ZDBSP by Marisa Heit, Zennode by Marc Rousseau,\n")
	Log.Printf("Zokumbsp by Kim Roar Foldøy Hauge, AJ-BSP by Andrew Apted, et al,\n")
	Log.Printf("and is distributed under the terms of GNU General Public License v2.\n")
	Log.Printf("\n")
	// Proceed to parse command line
	if !(config.FromCommandLine()) {
		Log.Printf("\n")
		//PrintHelp()
		os.Exit(1)
	}

	// If input file name was not passed, print help
	if config.InputFileName == "" {
		PrintHelp()
		os.Exit(0)
	}
}

func PrintHelp() {
	Log.Printf("Usage: vigilantbsp {-options} filename.wad {-o output.wad}\n")
	Log.Printf("\n")
	//Log.Printf("-x+ turn on option -x- turn off option * = default")
	Log.Printf("-x+ turn on option -x- turn off option")
	Log.Printf("\n")
	Log.Printf("-b Rebuild BLOCKMAP.\n")
	Log.Printf("	o= Offset configuration\n")
	Log.Printf("		0 0,0 offset BLOCKMAP (Zennode preset)\n")
	Log.Printf("		1 8,8 offset BLOCKMAP (BSP v5.2 preset)\n")
	Log.Printf("		2 Best of 36 offset combinations (default)\n")
	Log.Printf("		3 Heuristic method to reduce from 65536 offsets\n")
	Log.Printf("		4 Best of all 65536 offset combinations\n")
	Log.Printf("		x,y Specify specific offsets\n")
	Log.Printf("	r Remove non-collideable lines from blockmap (default: disabled)\n")
	Log.Printf("		This option is experimental and is likely to break\n")
	Log.Printf("		non-vanilla or non-Doom maps. Enable at your own risk\n")
	Log.Printf("	s Subset compress BLOCKMAP.\n")
	Log.Printf("		this parameter may be optionally followed by a question mark*\n")
	Log.Printf("	a Aggressive subset compression. If enabled, overrides s.\n")
	Log.Printf("		this parameter may be optionally followed by a question mark*\n")
	Log.Printf("	z= Zero/dummy header configuration\n")
	Log.Printf("		0 No dummy header\n")
	Log.Printf("		1 Use dummy header, but not necessary zero (default)\n")
	Log.Printf("		2 Use linedef #0 as dummy header\n")
	Log.Printf("	t= Number of cores to use when trying multiple offsets\n")
	Log.Printf("		(defaults to number of cores available)\n")
	Log.Printf("	e= Endgoal for multiple offsets\n")
	Log.Printf("		0 Smallest blockmap size (default)\n")
	Log.Printf("		1 Blockmap fits vanilla limits is enough\n")
	Log.Printf("		2 Blockmap works in limit-removing ports is enough\n")
	Log.Printf("  * if parameter is followed by question mark, it is tried when a working blockmap can't be produced otherwise.\n")
	Log.Printf("\n")
	Log.Printf("-d Deterministic output (default: disabled)\n")
	Log.Printf("\n")
	Log.Printf("-n Rebuild NODES.\n")
	Log.Printf("	a= Partition selection algorithm.\n")
	Log.Printf("		0 Seg balancing only\n")
	Log.Printf("		1 Visplane reduction per Lee Killough\n")
	Log.Printf("		2 Advanced visplane reduction\n")
	Log.Printf("		3 Maelstrom - fastest build speed\n")
	Log.Printf("		4 Zennode/Zokumbsp-like (tree balance and depth) \n")
	Log.Printf("	p= Priority for partition selection (if applicable).\n")
	Log.Printf("		0 Split minimization (default for AVR)\n")
	Log.Printf("		1 Depth reduction\n")
	Log.Printf("		2 Balanced tree (default for Zennode-like) \n")
	Log.Printf("	m= Multi-tree mode (experimental, SLOW).\n")
	Log.Printf("		0 Don't use, build single tree (default)\n")
	Log.Printf("		1 Try every one-sided linedef as a possible root partition\n")
	Log.Printf("		2 Try every two-sided linedef as a possible root partition\n")
	Log.Printf("		3 Try every linedef as a possible root partition\n")
	Log.Printf("	t= Number of threads to use for multi-tree\n")
	Log.Printf("		(defaults to number of cores available)\n")
	//Log.Printf("	w= Width for HARD multi-tree and mega-tree (default: 2)\n")
	//Log.Printf("	r= Reach for HARD multi-tree ang mega-tree (default: 10)\n")
	//Log.Printf("	  Warning: you get up to w^r trees with hard multi-tree\n")
	Log.Printf("	i= Cull (don't create segs from) invisible linedefs.\n")
	Log.Printf("		0 Don't cull (default)\n")
	Log.Printf("		1 Cull, use faulty check to preserve self-referencing sectors.\n")
	Log.Printf("		2 Cull, robust preservation of self-referencing sectors.\n")
	Log.Printf("	c= NODES format compatibility\n")
	Log.Printf("		v Vanilla format which all software ports recognise\n")
	Log.Printf("		d Deep nodes format\n")
	Log.Printf("		x Zdoom extended nodes format\n")
	Log.Printf("		z Zdoom compressed nodes format \n")
	Log.Printf("		  or zN where N is a single digit between 1 and 9\n")
	Log.Printf("		V Vanilla+ (remove multi-tree bias against unsigned trees)\n")
	Log.Printf("		   identical to \"v\" when not in multi-tree mode\n")
	Log.Printf("		D Prefer vanilla, switch to Deep nodes on overflow\n")
	Log.Printf("		X Prefer vanilla, switch to Zdoom extended nodes on overflow\n")
	Log.Printf("		Z Prefer vanilla, switch to Zdoom compressed nodes on overflow\n")
	Log.Printf("		  or ZN where N is a single digit between 1 and 9\n")
	Log.Printf("	f= Tuning factor (seg split cost, etc.)\n")
	Log.Printf("		17 - default seg split cost\n")
	Log.Printf("	d= Penalty factor for _diagonal_ lines\n")
	Log.Printf("		34 - default for Hexen format levels, for others 0 (disabled)\n")
	Log.Printf("		Explicitly specifying non-zero value will enable it for all\n")
	Log.Printf(" 		levels,	while explicitly specifying 0 will disable it even\n")
	Log.Printf("		for Hexen levels\n")
	Log.Printf("\n")
	Log.Printf("-r Rebuild REJECT resource.\n")
	Log.Printf("	z Insert zero-filled REJECT resource\n")
	Log.Printf("	g Use graphs to reduce LOS calculations (default: enabled)\n")
	Log.Printf("	s= If 2-sided lines have same sector on both sides\n")
	Log.Printf("		0 Mark such sectors as always visible (default)\n")
	Log.Printf("		1 Mark visible only when self-referencing sector effects are\n")
	Log.Printf("			detected\n")
	Log.Printf("		2 Be pedantic about self-referencing sector visibility\n")
	Log.Printf("		3 Assume self-referencing sectors don't exist (not recommended)\n")
	Log.Printf("	m Process RMB option file (.rej)\n")
	Log.Printf("\n")
	Log.Printf("-m (example -m:map01+map03) Rebuild only specific maps\n")
	Log.Printf("	if it is instead written -!m:map01+map03, the meaning is inverse.\n")
	Log.Printf("\n")
	Log.Printf("-v Add verbosity to text output. Use multiple times for increased verbosity.\n")
	Log.Printf("\n")
	Log.Printf("Example (1): vigilantbsp -be=2 -na=3 -rz file.wad\n")
	Log.Printf("	One of those options combination that result in fastest running time.\n")
	Log.Printf(" 	Blockmap will be built using no more than 36 offsets, but stopping at\n")
	Log.Printf("	the first one which will work in limit-removing port, nodes are built\n")
	Log.Printf("	using the fastest 'maelstrom' algorithm, and reject lump is filled\n")
	Log.Printf("	with zeroes.\n")
	Log.Printf("	'file.wad' serves as both input and output file (gets overwritten).\n")
	Log.Printf("Example (2): vigilantbsp -d -na=2 -bo=4at=6 file.wad -o file_out.wad\n")
	Log.Printf("	This builds as small as possible blockmap out of 65536 combinations,\n")
	Log.Printf("	using aggressive subset elimination and strictly 6 threads,\n")
	Log.Printf("	ensures that consecutive runs will result in the same output unless\n")
	Log.Printf("	input changes (a feature called determinism), and builds nodes\n")
	Log.Printf("	using the best visplane-reducing algorithm available.\n")
	Log.Printf("	It reads from 'file.wad' and writes results to 'file_out.wad'.\n")
	Log.Printf("	The input wad file is not modified.\n")
	Log.Printf("\n")
}

func promoteNodeType(NodeType int) int {
	switch NodeType {
	case NODETYPE_VANILLA_OR_ZCOMPRESSED:
		return NODETYPE_ZDOOM_COMPRESSED
	case NODETYPE_VANILLA_OR_ZEXTENDED:
		return NODETYPE_ZDOOM_EXTENDED
	case NODETYPE_VANILLA_OR_DEEP:
		return NODETYPE_DEEP
	default:
		return NodeType
	}
}
