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
	"os"
)

const VERSION = "0.70a"

/*
-b Rebuild BLOCKMAP.
	o= Offset configuration
		0 0,0 offset BLOCKMAP (zennode preset)
		1 8,8 offset BLOCKMAP (BSP v5.2 preset)
		2 Best of 36 offset combinations (default)
		3 Heuristic method to reduce from 65536 offsets
		4 Best of all 65536 offset combinations
		x,y Specify specific offsets
	s Subset compress BLOCKMAP.
	a Aggressive subset compression. If enabled, overrides s.
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

-d Deterministic output (default: disabled)
	NOTE enabling this *together* with non-zero endgoal for blockmap (-be=... )
	will DISABLE using multiple cores for trying multiple offsets, EVEN if
	user directly specified the number of cores using -bt=...

-n Rebuild NODES.
	a= Partition selection algorithm.
		0 Seg balancing only
		1 Visplane reduction per Lee Killough
		2 Advanced visplane reduction
		3 Maelstrom - fastest build speed
	p= Priority for partition selection.
		0 Split minimization (default)
		1 Depth reduction
	i= Cull (don't create segs from) invisible linedefs.
		0 Don't cull (default)
		1 Cull, use faulty check to preserve self-referencing sectors.
		2 Cull, robust preservation of self-referencing sectors.
	c= NODES format compatibility
		v Vanilla format which all software ports recognise
		d Deep nodes format (limited recognition)
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
		s+ will temporarily be interpreted as s=1

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
)

const (
	REJ_SELFREF_TRIVIAL  = iota // all sectors with 2-sided lines pointing to the same sector are marked always visible
	REJ_SELFREF_CHECK           // all sectors with self-referencing effect are marked always visible
	REJ_SELFREF_PEDANTIC        // visibility of sectors with self-referencing effect is determined as robustly as visibility of normal sectors
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
	PickNodeUser int              // <- This is the actual config option
	PickNode     PickNodeFunc     // don't assign directly! Derived from PickNodeUser
	CreateNodeSS CreateNodeSSFunc // don't assign directly! Derived from PickNodeUser
	// Secondary metric for picking a partition
	MinorCmpUser int               // <- This is the actual config option
	MinorCmpFunc MinorIsBetterFunc // don't assign directly! Derived from MinorCmpUser and PickNodeUser
	//
	BlockmapSearchAbortion int    // when trying multiple offsets, finish the search for a good blockmap as soon as it fits the limit
	UseGraphsForLOS        bool   // use graphs for LOS calculations (build reject faster)
	DumpSegs               bool   // seg debugging
	SegDumpFile            string // where do dumped segs go?
	DebugNoSlyForReject    bool   // reenable zennodes' buggy behavior of making self-referencing sectors always invisible
	CullInvisibleSegs      int    // do not create segs for linedefs that will be invisible anyway
	PenalizeSectorSplits   bool   // Another options which may or may not help with visplanes. Vigilant visplane algorithm only
	RejectSelfRefMode      int    // what measures are taken for self-referencing sector support in reject
	DeepNodes              bool   // use deep format for nodes. Prboom-Plus v2.5.1.5 supports these fine.
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
}

// PickNode values: PickNode_traditional, PickNode_visplaneKillough, PickNode_visplaneVigilant

var config *ProgramConfig // global variable that will be accessed from other threads too

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

func init() {
	Log.Printf("VigilantBSP ver %s\n", VERSION)
	Log.Printf("Copyright (c)   2022 VigilantDoomer\n")
	Log.Printf("This program is built upon ideas first implemented in DEU by Raphael Quinet, \n")
	Log.Printf("BSP v5.2 by Colin Reed, Lee Killough and other contributors to BSP (program),\n")
	Log.Printf("ZDBSP by Marisa Heit, Zennode by Marc Rousseau, Zokumbsp by Kim Roar Foldøy Hauge,\n")
	Log.Printf("AJ-BSP by Andrew Apted, et al, and is distributed under the terms of \n")
	Log.Printf(" GNU General Public License v2.\n")
	Log.Printf("\n")
	// Initialize with defaults
	config = &(ProgramConfig{
		BlockmapXOffset:    0,
		BlockmapYOffset:    0,
		BlockmapOffsetMode: BM_OFFSET_THIRTYSIX,
		// BlockmapThreads is the number of bees to run when trying multiple
		// offsets. 0 means "auto = the number of cores, but no more than 16".
		// If set to specific value, that value is used, and can be greater than
		// 16.
		BlockmapThreads:        0,
		UseZeroHeader:          true,
		ZeroHeaderIsZero:       false,
		SubsetCompressBlockmap: false,
		DumpSegs:               false,
		SegDumpFile:            "",
		Profile:                false,
		ProfilePath:            "",
		MemProfile:             false,
		MemProfilePath:         "",
		NodesDebugFile:         "",
		Reject:                 REJECT_NORMAL,
		Deterministic:          false,
		AggressiveSubsets:      false,
		VerbosityLevel:         0,
		PickNodeUser:           PICKNODE_TRADITIONAL,
		BlockmapSearchAbortion: BM_OFFSET_NOABORT,
		UseGraphsForLOS:        true,
		DebugNoSlyForReject:    false,
		CullInvisibleSegs:      CULL_SEGS_DONT,
		PenalizeSectorSplits:   true,
		RejectSelfRefMode:      REJ_SELFREF_TRIVIAL,
		DeepNodes:              false,
		PersistThroughInsanity: true,
		RebuildNodes:           true,
		RebuildBlockmap:        true,
		PickNodeFactor:         PICKNODE_FACTOR,
		DiagonalPenalty:        DIAGONAL_PENALTY,
		PenalizeDiagonality:    PENALIZE_DIAGONALITY_HEXEN,
		MinorCmpUser:           MINOR_CMP_BALANCE,
		DepthArtifacts:         true,
	})
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
	// Set derivative options in config
	config.PickNode = PickNodeFuncFromOption(config.PickNodeUser)
	config.CreateNodeSS = CreateNodeSSFromOption(config.PickNodeUser)
	config.MinorCmpFunc = MinorCmpFuncFromOption(config.MinorCmpUser)
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
	Log.Printf("	s Subset compress BLOCKMAP.\n")
	Log.Printf("	a Aggressive subset compression. If enabled, overrides s.\n")
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
	Log.Printf("\n")
	Log.Printf("-d Deterministic output (default: disabled)\n")
	Log.Printf("	NOTE enabling this *together* with non-zero endgoal for blockmap (-be=... )\n")
	Log.Printf("	will DISABLE using multiple cores for trying multiple offsets, EVEN if\n")
	Log.Printf("	user directly specified the number of cores using -bt=...\n")
	Log.Printf("\n")
	Log.Printf("-n Rebuild NODES.\n")
	Log.Printf("	a= Partition selection algorithm.\n")
	Log.Printf("		0 Seg balancing only\n")
	Log.Printf("		1 Visplane reduction per Lee Killough\n")
	Log.Printf("		2 Advanced visplane reduction\n")
	Log.Printf("		3 Maelstrom - fastest build speed\n")
	Log.Printf("	p= Priority for partition selection.\n")
	Log.Printf("		0 Split minimization (default)\n")
	Log.Printf("		1 Depth reduction\n")
	Log.Printf("	i= Cull (don't create segs from) invisible linedefs.\n")
	Log.Printf("		0 Don't cull (default)\n")
	Log.Printf("		1 Cull, use faulty check to preserve self-referencing sectors.\n")
	Log.Printf("		2 Cull, robust preservation of self-referencing sectors.\n")
	Log.Printf("	c= NODES format compatibility\n")
	Log.Printf("		v Vanilla format which all software ports recognise\n")
	Log.Printf("		d Deep nodes format (limited recognition)\n")
	Log.Printf("	f= Tuning factor (seg split cost, etc.)\n")
	Log.Printf("		17 - default seg split cost\n")
	Log.Printf("	d= Penalty factor for _diagonal_ lines\n")
	Log.Printf("		34 - default for Hexen format levels, for others 0 (disabled)\n")
	Log.Printf("		Explicitly specifying non-zero value will enable it for all levels,\n")
	Log.Printf("		while explicitly specifying 0 will disable it even for Hexen levels\n")
	Log.Printf("\n")
	Log.Printf("-r Rebuild REJECT resource.\n")
	Log.Printf("	z Insert zero-filled REJECT resource\n")
	Log.Printf("	g Use graphs to reduce LOS calculations (default: enabled)\n")
	Log.Printf("	s= If 2-sided lines have same sector on both sides\n")
	Log.Printf("		0 Mark such sectors as always visible (default)\n")
	Log.Printf("		1 Mark visible only when self-referencing sector effects are detected\n")
	Log.Printf("		2 Be pedantic about self-referencing sector visibility\n")
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
