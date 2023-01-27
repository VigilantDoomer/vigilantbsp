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
	"os"
	"strconv"
)

const ( // NumericOrState.whichType values
	ARG_ENABLED = iota
	ARG_DISABLED
	ARG_IS_NUMBER
)

type NumericOrState struct {
	whichType int // see consts above
	value     int
}

// Inspired by from zokumbsp's parser
func (c *ProgramConfig) FromCommandLine() bool {
	files := make([]string, 0)
	args := os.Args[1:]
	outputModifier := false
	outputModifierUsed := false
	hasOutputFile := false
	skip := false
	for argIdx, arg := range args {
		if len(arg) < 1 {
			break
		}
		if skip {
			skip = false
			continue
		}

		if arg[0] != '-' && !outputModifier {
			files = append(files, arg)
			if len(files) > 1 {
				// No logic for concatenating multiple wads into one exists
				Log.Error("This program doesn't support specifying more than one input file - aborting.")
				return false
			}
			c.InputFileName = files[0]
			continue
		}

		if outputModifier {
			c.OutputFileName = arg
			outputModifier = false
			hasOutputFile = true
			continue
		}

		if len(arg) < 2 {
			continue
		}
		switch arg[1] {
		case 'b':
			{
				enabled, rest := isEnabled([]byte(arg)[2:])
				c.RebuildBlockmap = enabled
				if c.RebuildBlockmap {
					c.parseBlockmapParams(rest)
				} else if len(rest) > 0 {
					Log.Error("Not rebuilding blockmap, but had more non-whitespace characters immediately following -b-. They will be ignored.")
				}
			}
		case 'd':
			{
				enabled, rest := isEnabled([]byte(arg)[2:])
				c.Deterministic = enabled
				if len(rest) > 0 {
					Log.Error("Syntax error: -d parameter is followed by garbage; expected -d, -d+ or -d-, no other variants allowed.")
				}
			}
		case 'n':
			{
				enabled, rest := isEnabled([]byte(arg)[2:])
				c.RebuildNodes = enabled
				if c.RebuildNodes {
					c.parseNodesParams(rest)
				} else if len(rest) > 0 {
					Log.Error("Not rebuilding nodes, but had more non-whitespace characters immediately following -n-. They will be ignored.")
				}
			}
		case 'r':
			{
				enabled, rest := isEnabled([]byte(arg)[2:])
				if !enabled {
					c.Reject = REJECT_DONTTOUCH
				}
				if c.Reject != REJECT_DONTTOUCH {
					c.parseRejectParams(rest)
				} else if len(rest) > 0 {
					Log.Error("Not rebuilding reject, but had more non-whitespace characters immediately following -r-. They will be ignored.")
				}
			}
		case 'v':
			{
				// "count" type: -v, -vv, -vvv, etc.
				vs := 0
				barg := []byte(arg)[1:]
				for i := 0; i < len(arg)-1; i++ {
					if barg[i] == 'v' {
						vs++
					} else {
						break
					}
				}
				c.VerbosityLevel += vs
			}
		case 'o':
			{
				if len(arg) != 2 {
					Log.Error("Unrecognized modified '%s' (expected '-o <file>', space between '-o' and file name) - aborting.\n",
						arg)
					return false
				}
				if outputModifierUsed {
					Log.Error("Can't specify output file twice, only one output file is supported - aborting.\n")
					return false
				}
				outputModifier = true
				outputModifierUsed = true
			}
		case '-':
			{
				// Encountered a beginning of double hyphen argument, which is
				// entire "word" followed by a double hyphen, rather than one
				// letter following a single one

				// fileSatisfied can become false in the following case:
				// 1. The argument type requires a file to be specified, and
				// 2. There was no file following argument type that takes a file
				fileSatisfied := true

				// parameter starts with double hyphen, e.g. --something
				if bytes.Equal([]byte(arg), []byte("--cpuprofile")) {
					// Parameter: write cpu profile to file following this
					// parameter
					fileSatisfied = (len(arg) > (argIdx + 1)) &&
						(args[argIdx+1] != "")
					c.Profile = true
					c.ProfilePath = args[argIdx+1]
					skip = true
				} else if bytes.Equal([]byte(arg), []byte("--memprofile")) {
					// Parameter: write memory allocations (Go "allocs")
					// profile to file following this parameter
					fileSatisfied = (len(arg) > (argIdx + 1)) &&
						(args[argIdx] != "")
					c.MemProfile = true
					c.MemProfilePath = args[argIdx+1]
					skip = true
				} else if bytes.Equal([]byte(arg), []byte("--unreversenodes")) {
					// Parameter: write BSP tree without the traditionally
					// used reversal, keeping only root node at the last index
					// Idea: Linguica, "Stupid BSP tricks" thread on doomworld
					// Explaination: usually, BSP tree is written in reversed
					// order, where indices of child nodes of each node precede
					// the index of node that references them. Anecdotal
					// evidence shows that it provides suboptimal performance
					// on old 486 processors, compared to reversing this
					// reversal, which is what this option does.
					// Caveat: root node still must be written last
					// Caveat 2: the exact algorithm used by Linguica was lost
					// (link expired), my implementation does not necessary
					// match it and hence may not fulfill performance benefits
					// See https://www.doomworld.com/forum/topic/74354-stupid-bsp-tricks/?do=findComment&comment=1524479
					c.StraightNodes = true
				} else if bytes.HasPrefix([]byte(arg), []byte("--detailnodes")) {
					// Parameter to control whether use more accurate
					// doLinesIntersect check. Does not affect extended nodes
					// (they use far more accurate variant always anyway).
					ns, _ := readNumeric("--detailnodes=", []byte(arg)[len("--detailnodes=")-1:])
					if ns.whichType == ARG_ENABLED {
						c.NodeDetailFriendliness = NODE_DETAIL_ALWAYS
					} else if ns.whichType == ARG_DISABLED {
						c.NodeDetailFriendliness = NODE_DETAIL_SUPPRESS
					} else {
						switch ns.value {
						case 0:
							c.NodeDetailFriendliness = NODE_DETAIL_AUTO
						case 1:
							c.NodeDetailFriendliness = NODE_DETAIL_SUPPRESS
						case 2:
							c.NodeDetailFriendliness = NODE_DETAIL_ALWAYS
						case 3:
							c.NodeDetailFriendliness = NODE_DETAIL_HEURISTIC
						default:
							Log.Error("Unrecognised value %s, expected 0, 1, 2 or 3\n", arg)
							return false
						}
					}
				} else {
					Log.Error("Unrecognised argument '%s' - aborting.\n", arg)
					return false
				}
				if !fileSatisfied { // argument takes a file, but no file was supplied?
					Log.Error("Modifier '%s' was present without a file name following it - aborting.\n",
						arg)
					return false
				}
			}
		case 'm':
			{
				// -m:mapxx parameter
				if c.FilterLevel != nil {
					Log.Error("You can't specify map list twice.\n")
					return false
				}
				if !c.parseMapList([]byte(arg[2:])) {
					Log.Error("There were errors parsing map list - aborting.\n")
					return false
				}
			}
		default:
			{
				Log.Error("Unrecognised argument '%s' - aborting.\n", arg)
				return false
			}
		}
	}
	if outputModifier && !hasOutputFile {
		Log.Error("Modifier '-o' was present without a file name following it - aborting.\n")
		return false
	}
	if c.Reject == REJECT_ZEROFILLED && c.UseRMB {
		// This might change in the future, NOPROCESS and the options that can
		// be used alongside it could theoretically be useful. The problem is,
		// the decision to process RMB just in case it applies is going to
		// be counterintuitive and could only be understood with documentation
		// Maybe a better way would be to implement new RMB command similar to
		// NOPROCESS that allows to start from zero-filled REJECT
		Log.Printf("Ignoring 'process RMB file' option because you requested to fill reject lump with zeros\n")
		c.UseRMB = false
	}
	if c.Reject == REJECT_DONTTOUCH && c.UseRMB {
		Log.Printf("Ignoring 'process RMB file' option because you requested not to rebuild reject lump\n")
		c.UseRMB = false
	}
	return true
}

func (c *ProgramConfig) parseMapList(p []byte) bool {
	if len(p) == 0 || p[0] != ':' {
		Log.Error("Missing a colon: map list should be specified like this -m:map01+map02\n")
		return false
	}
	c.FilterLevel = make([][]byte, 0)
	rest := p[1:]
	for len(rest) > 0 {
		pin := bytes.IndexByte(rest, '+')
		if pin == -1 {
			pin = len(rest)
		}
		lname := rest[:pin]
		if len(lname) > 0 {
			lname = bytes.ToUpper(lname)
			if IsALevel(lname) {
				c.FilterLevel = append(c.FilterLevel, lname)
			} else {
				// ARGH!
				Log.Error("%s is not a valid level name.\n", string(lname))
				return false
			}
		} else {
			Log.Error("'-m:' argument is missing a level name following or preceeding one of the separators.\n")
			return false
		}
		if pin+1 < len(rest) {
			rest = rest[pin+1:]
		} else {
			if pin == len(rest)-1 {
				Log.Error("Map list should not end with a '+'\n")
				return false
			}
			break
		}
	}
	return true
}

func (c *ProgramConfig) parseBlockmapParams(p []byte) {
	if len(p) == 0 {
		return
	}
	for len(p) > 0 {
		switch p[0] {
		case 's':
			{
				on, rest := isEnabled(p[1:])
				c.SubsetCompressBlockmap = on || c.AggressiveSubsets
				p = rest
			}
		case 'a':
			{
				on, rest := isEnabled(p[1:])
				c.AggressiveSubsets = on
				if on {
					c.SubsetCompressBlockmap = true
				}
				p = rest
			}
		case 'z':
			{
				nos, rest := readNumeric("-bz", p[1:])
				if nos.whichType == ARG_ENABLED {
					nos.value = 1
				} else if nos.whichType == ARG_DISABLED {
					nos.value = 0
				}
				if nos.value < 0 || nos.value > 2 {
					Log.Error("Unsupported numeric value for -bz: %d. Will default to enabled state.\n", nos.value)
					nos.value = 1
				}
				switch nos.value {
				case 0:
					{
						c.UseZeroHeader = false
					}
				case 1:
					{
						c.UseZeroHeader = true
						c.ZeroHeaderIsZero = false
					}
				case 2:
					{
						c.UseZeroHeader = true
						c.ZeroHeaderIsZero = true
					}
				}
				p = rest
			}
		case 't':
			{
				nos, rest := readNumeric("-bt", p[1:])
				if nos.whichType == ARG_DISABLED {
					nos.value = 1
				}
				if nos.whichType != ARG_ENABLED && int16(nos.value) < 0 {
					// negative cannot be returned anyway... yet
					Log.Error("Negative blockmap thread count is not supported - ignoring it.\n")
				} else if nos.whichType != ARG_ENABLED {
					// value of 0 is ok. It's default, "auto" mode
					c.BlockmapThreads = int16(nos.value)
				} else { // ARG_ENABLED
					// to auto mode
					c.BlockmapThreads = 0
				}
				p = rest
			}
		case 'e':
			{
				nos, rest := readNumeric("-be", p[1:])
				if nos.whichType == ARG_DISABLED {
					nos.value = 0
				} else if nos.whichType == ARG_ENABLED {
					nos.value = 1
				}
				if nos.value < 0 || nos.value > 2 {
					Log.Error("Endgoal value out of range - truncating to -be=2.")
					nos.value = 2
				}
				switch nos.value {
				case 0:
					{
						c.BlockmapSearchAbortion = BM_OFFSET_NOABORT
					}
				case 1:
					{
						c.BlockmapSearchAbortion = BM_OFFSET_FITS_VANILLA
					}
				case 2:
					{
						c.BlockmapSearchAbortion = BM_OFFSET_FITS_ANYPORT
					}
				}
				p = rest
			}
		case 'o':
			{
				handled := false
				nos, rest := readNumeric("-bo", p[1:])
				if nos.whichType == ARG_DISABLED {
					nos.value = 0
				} else if nos.whichType == ARG_ENABLED {
					nos.value = 2
				} else {
					// need to check if it is -bo=<index> or -bo=x,y
					if len(rest) > 0 && rest[0] == ',' {
						t, v, rest2 := readNumericOnly(rest[1:])
						rest = rest2
						if t {
							if int16(nos.value) >= 0 && int16(v) >= 0 {
								c.BlockmapOffsetMode = BM_OFFSET_FIXED
								c.BlockmapXOffset = int16(nos.value)
								c.BlockmapYOffset = int16(v)
								handled = true
							} else {
								Log.Error("Invalid blockmap offsets were specified, they will be ignored.\n")
								handled = true
							}
						}
					}

				}
				if !handled {
					switch nos.value {
					case 0:
						{
							c.BlockmapOffsetMode = BM_OFFSET_FIXED
							c.BlockmapXOffset = 0
							c.BlockmapYOffset = 0
						}
					case 1:
						{
							c.BlockmapOffsetMode = BM_OFFSET_FIXED
							c.BlockmapXOffset = 8
							c.BlockmapYOffset = 8
						}

					case 2:
						{
							c.BlockmapOffsetMode = BM_OFFSET_THIRTYSIX
						}
					case 3:
						{
							c.BlockmapOffsetMode = BM_OFFSET_HEURISTIC
						}
					case 4:
						{
							c.BlockmapOffsetMode = BM_OFFSET_BRUTEFORCE
						}
					default:
						{
							Log.Error("Invalid blockmap offset mode was specified, it will be ignored.\n")
						}
					}
				}
				p = rest
			}
		case 'r':
			{
				on, rest := isEnabled(p[1:])
				c.RemoveNonCollideable = on
				p = rest
			}
		default:
			{
				Log.Error("Error passing blockmap params - ignoring '%s'.\n", string(p))
				p = p[:0]
			}
		}
	}
}

func (c *ProgramConfig) parseNodesParams(p []byte) {
	for len(p) > 0 {
		switch p[0] {
		case 'a':
			{
				nos, rest := readNumeric("-na", p[1:])
				if nos.whichType == ARG_ENABLED || nos.whichType == ARG_DISABLED {
					Log.Error("You are supposted to pass -na=<digit>, not -na+ or -na-.")
				} else {
					switch nos.value {
					case 0:
						{
							c.PickNodeUser = PICKNODE_TRADITIONAL
						}
					case 1:
						{
							c.PickNodeUser = PICKNODE_VISPLANE
						}
					case 2:
						{
							c.PickNodeUser = PICKNODE_VISPLANE_ADV
						}
					case 3:
						{
							c.PickNodeUser = PICKNODE_MAELSTROM
						}
					default:
						{
							Log.Error("Ignoring invalid (out of range) value for partition selection algorithm.")
						}
					}
				}
				p = rest
			}
		case 'p': // new in v0.69a
			{
				nos, rest := readNumeric("-np", p[1:])
				if nos.whichType == ARG_ENABLED || nos.whichType == ARG_DISABLED {
					Log.Error("You are supposted to pass -np=<digit>, not -np+ or -np-.")
				} else {
					switch nos.value {
					case 0:
						{
							c.MinorCmpUser = MINOR_CMP_BALANCE
							c.PenalizeSectorSplits = true
						}
					case 1:
						{
							c.MinorCmpUser = MINOR_CMP_BALANCE
							c.PenalizeSectorSplits = false
						}
					default:
						{
							Log.Error("Ignoring invalid (out of range) value for the type of secondary score to use for partition selection.")
						}
					}
				}
				p = rest
			}
		case 'i':
			{
				nos, rest := readNumeric("-ni", p[1:])
				if nos.whichType == ARG_ENABLED {
					nos.value = 2
				} else if nos.whichType == ARG_DISABLED {
					nos.value = 0
				}
				switch nos.value {
				case 0:
					{
						c.CullInvisibleSegs = CULL_SEGS_DONT
					}
				case 1:
					{
						c.CullInvisibleSegs = CULL_SEGS_SLOPPY
					}
				case 2:
					{
						c.CullInvisibleSegs = CULL_SEGS_PROPER
					}
				default:
					{
						Log.Error("Ignoring invalid (out of range) value for whether to cull invisible linedefs\n")
					}
				}
				p = rest
			}
		case 'c':
			{
				if bytes.Equal(p[1:3], []byte("=v")) {
					c.NodeType = NODETYPE_VANILLA
				} else if bytes.Equal(p[1:3], []byte("=d")) {
					c.NodeType = NODETYPE_DEEP
				} else if bytes.Equal(p[1:3], []byte("=x")) {
					c.NodeType = NODETYPE_ZDOOM_EXTENDED
				} else if bytes.Equal(p[1:3], []byte("=z")) {
					c.NodeType = NODETYPE_ZDOOM_COMPRESSED
				} else {
					Log.Error("Unknown value for NODES format.\n")
				}
				p = p[3:]
			}
		case 'f':
			{
				nos, rest := readNumeric("-nf", p[1:])
				if nos.whichType == ARG_ENABLED || nos.whichType == ARG_DISABLED {
					Log.Error("Toggling on/off split cost factor is not supported.\n")
				} else {
					if nos.value <= 0 {
						Log.Error("Only supporting positive factors.\n")
					} else {
						c.PickNodeFactor = nos.value
					}
				}
				p = rest
			}
		case 'd':
			{
				nos, rest := readNumeric("-nd", p[1:])
				if nos.whichType == ARG_ENABLED {
					c.PenalizeDiagonality = PENALIZE_DIAGONALITY_ALWAYS
				} else if nos.whichType == ARG_DISABLED {
					c.PenalizeDiagonality = PENALIZE_DIAGONALITY_NEVER
				} else {
					if nos.value <= 0 {
						c.PenalizeDiagonality = PENALIZE_DIAGONALITY_NEVER
					}
					if nos.value >= 0 {
						if nos.value > 0 {
							c.PenalizeDiagonality = PENALIZE_DIAGONALITY_ALWAYS
						}
						c.DiagonalPenalty = nos.value
					} else {
						Log.Error("Diagonal penalty can't be negative. Will disable it instead.\n")
						c.DiagonalPenalty = 0
					}
				}
				p = rest
			}
		case 'm':
			{
				nos, rest := readNumeric("-nm", p[1:])
				switch nos.value {
				case 0:
					{
						c.MultiTreeMode = MULTITREE_NOTUSED
					}
				case 1:
					{
						c.MultiTreeMode = MULTITREE_ROOT_ONLY
						c.SpecialRootMode = MROOT_ONESIDED
					}
				case 2:
					{
						c.MultiTreeMode = MULTITREE_ROOT_ONLY
						c.SpecialRootMode = MROOT_TWOSIDED
					}
				case 3:
					{
						c.MultiTreeMode = MULTITREE_ROOT_ONLY
						c.SpecialRootMode = MROOT_EVERY
					}
				default:
					{
						Log.Error("Ignoring invalid (out of range) value for multi-tree mode\n")
					}
				}
				p = rest
			}
		case 't':
			{
				nos, rest := readNumeric("-nt", p[1:])
				if nos.whichType == ARG_DISABLED {
					nos.value = 1
				}
				if nos.whichType != ARG_ENABLED && int16(nos.value) < 0 {
					// negative cannot be returned anyway... yet
					Log.Error("Negative BSP tree thread count is not supported - ignoring it.\n")
				} else if nos.whichType != ARG_ENABLED {
					// value of 0 is ok. It's default, "auto" mode
					c.NodeThreads = int16(nos.value)
				} else { // ARG_ENABLED
					// to auto mode
					c.NodeThreads = 0
				}
				p = rest
			}
		default:
			{
				Log.Error("Error passing nodes params - ignoring '%s'.\n", string(p))
				p = p[:0]
			}
		}
	}
}

func (c *ProgramConfig) parseRejectParams(p []byte) {
	for len(p) > 0 {
		switch p[0] {
		case 'z':
			{
				t, rest := isEnabled(p[1:])
				if t {
					c.Reject = REJECT_ZEROFILLED
				} else {
					c.Reject = REJECT_NORMAL
				}
				p = rest
			}
		case 'g':
			{
				t, rest := isEnabled(p[1:])
				c.UseGraphsForLOS = t
				p = rest
			}
		case 'm':
			{
				t, rest := isEnabled(p[1:])
				c.UseRMB = t
				p = rest
			}
		case 's':
			{
				nos, rest := readNumeric("-rs", p[1:])
				if nos.whichType == ARG_ENABLED {
					// TODO until errors in pedantic mode are fixed, this
					// defaults to "CHECK" mode. Change to "PEDANTIC" (= 2) when
					// it is verified to be correct
					nos.value = 1
				} else if nos.whichType == ARG_DISABLED {
					nos.value = 0
				}
				switch nos.value {
				case 0:
					{
						c.RejectSelfRefMode = REJ_SELFREF_TRIVIAL
					}
				case 1:
					{
						c.RejectSelfRefMode = REJ_SELFREF_CHECK
					}
				case 2:
					{
						c.RejectSelfRefMode = REJ_SELFREF_PEDANTIC
					}
				default:
					{
						Log.Error("Unknown mode for treating 2-sided lines with same sector on both sides when building reject. Ignoring it.\n")
					}
				}
				p = rest
			}
		default:
			{
				Log.Error("Error passing reject params - ignoring '%s'.\n", string(p))
				p = p[:0]
			}
		}
	}
}

func isEnabled(arg []byte) (bool, []byte) {
	if len(arg) == 0 {
		return true, arg
	}
	if arg[0] == '+' {
		return true, arg[1:]
	} else if arg[0] == '-' {
		return false, arg[1:]
	} else {
		return true, arg
	}
}

// a+, a-, or a=<numeric_value_without_sign>
func readNumeric(prefix string, arg []byte) (NumericOrState, []byte) {
	if len(arg) == 0 {
		return NumericOrState{whichType: ARG_ENABLED}, arg
	}
	if arg[0] == '+' {
		return NumericOrState{whichType: ARG_ENABLED}, arg[1:]
	} else if arg[0] == '-' {
		return NumericOrState{whichType: ARG_DISABLED}, arg[1:]
	} else if arg[0] == '=' {
		// !!! doesn't support negative values, and values with explicit "+"
		// sign either
		t, v, rest := readNumericOnly(arg[1:])
		if t {
			return NumericOrState{
				whichType: ARG_IS_NUMBER,
				value:     v,
			}, rest
		} else {
			Log.Error("Couldn't properly parse '%s=%s'. Some parameters are going to be ignored as the result.\n", prefix, string(arg))
			return NumericOrState{
				whichType: ARG_ENABLED,
			}, arg[:0] // ignore the rest of parameters
		}
	} else {
		return NumericOrState{whichType: ARG_ENABLED}, arg
	}
}

func readNumericOnly(arg []byte) (bool, int, []byte) {
	if len(arg) == 0 {
		return false, 0, arg
	}
	l := 0
	for i := 0; i < len(arg); i++ {
		c := arg[i]
		if '0' <= c && c <= '9' {
			l++
		} else {
			break
		}
	}
	if l > 0 {
		v, err := strconv.Atoi(string(arg[:l]))
		if err != nil {
			Log.Error("value '%s' was too big to interpret as int.\n",
				string(arg[:l]))
			return false, 0, arg[l:]
		}
		return true, v, arg[l:]
	}
	return false, 0, arg
}
