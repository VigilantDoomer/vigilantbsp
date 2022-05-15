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
	"bufio"
	"bytes"
	"regexp"
	"strconv"
)

// RMB parser is responsible for parsing text that defines RMB options,
// into internal representation used by VigilantBSP. If you wanted to look
// at how they are applied instead, look into rejectRMB.go

// I looked into Zennode (by Marc Rousseau) for inspiration and some code to
// borrow, although my goals go beyond the level of support Zennode 1.2.1
// provides. I also plan to rewrite parser *eventually* to provide even better
// feedback on errors (printing out lines of source code, highlighting errors
// and in general more helpful error messages), faster parsing speed (to
// accomodate for potentially elaborate *.rej RMB option files) and generally
// more sanely written code.

var RMB_MAP_SEQUEL *regexp.Regexp = regexp.MustCompile(`^MAP([0-9])([0-9])$`)
var RMB_MAP_ExMx *regexp.Regexp = regexp.MustCompile(`^E([1-9])M([0-9])([0-9]?)$`)

type ParseRMBCommandFunc func(context *ParseContext, tblIdx int, cmd *RMBCommand) bool

type ParseDef struct {
	Name     []byte
	Syntax   []byte
	Type     int // rmbunit.go!RMB_<whatever> consts
	Callback ParseRMBCommandFunc
	Impl     bool
}

type ParseContext struct {
	activeFrame *RMBFrame
	idToFrame   map[RMBFrameId]*RMBFrame // for maps only
	globalFrame *RMBFrame                // global is stored separately
	allFrames   []RMBFrame
	fname       string // for printing errors "fname:liNum:<parse error message>"
	liNum       int
	command     []byte // converted to upper case
	getParseDef func(tblIdx int) *ParseDef
	wordScanner *bufio.Scanner
	// more dumb hacks to workaround Go compiler detecting definition "loops"
	recurseIntoParseRMBLine func(context *ParseContext, line []byte, fromInvert bool) bool
	rmbIndexByType          func(tpe int) int
}

// How many errors RMB parser will output before bailing out. Note that if
// there any errors at all, RMB file is considered incorrect regardless of the
// value of this constant. Outputting the error is thus simply so that user
// can eliminate a number of errors at once rather than rerunning VigilantBSP
// after each edit to get just one error until all are corrected
const RMB_ERROR_THRESHOLD = 5

// Warning: order matters! RMB syntax allows shortening commands to any
// unique prefix. All lines that share some non-unique prefix must be close
// together, the parsing algorithm uses this to check if prefix in the supplied
// RMB *.rej file is unique or not
// Credit also goes to Zennode for how to implement this
var RMB_PARSE_TABLE = []ParseDef{
	{[]byte("BAND"), []byte("NNL"), RMB_BAND, ParseBAND, true},
	{[]byte("BLIND"), []byte("NL"), RMB_BLIND, ParseGeneric, true},
	{[]byte("BLOCK"), []byte("NN"), RMB_BLOCK, ParseGeneric, false},
	{[]byte("DISTANCE"), []byte("N"), RMB_DISTANCE, ParseGeneric, true},
	{[]byte("DOOR"), []byte("N"), RMB_DOOR, ParseGeneric, false},
	{[]byte("E*M*") /*string unused*/, nil, RMB_EXMY_MAP, ParseMap, true},
	{[]byte("EXCLUDE"), []byte("LL"), RMB_EXCLUDE, ParseGeneric, true},
	{[]byte("GROUP"), []byte("NL"), RMB_GROUP, ParseGeneric, false},
	{[]byte("INCLUDE"), []byte("LL"), RMB_INCLUDE, ParseGeneric, true},
	{[]byte("INVERT"), nil, RMB_INVERT, ParseINVERT, true},
	{[]byte("LEFT"), []byte("N"), RMB_LEFT, ParseGeneric, false},
	{[]byte("LENGTH"), []byte("N"), RMB_LENGTH, ParseGeneric, true},
	{[]byte("LINE"), []byte("N"), RMB_LINE, ParseGeneric, true}, // TODO I would consider promoting argument to a list as part of extension, though
	{[]byte("MAP**") /*string unused*/, nil, RMB_MAPXY_MAP, ParseMap, true},
	{[]byte("NODOOR"), []byte("L"), RMB_NODOOR, ParseGeneric, false},
	{[]byte("NOMAP"), nil, RMB_NOMAP, ParseGeneric, false},
	{[]byte("NOPROCESS"), nil, RMB_NOPROCESS, ParseNOPROCESS, false},
	{[]byte("ONE"), []byte("NN"), RMB_ONE, ParseGeneric, false},
	{[]byte("PERFECT"), nil, RMB_PERFECT, ParseGeneric, true},
	{[]byte("PREPROCESS"), []byte("N"), RMB_PREPROCESS, ParseGeneric, false},
	{[]byte("PROCESS"), []byte("L"), RMB_PROCESS, ParseGeneric, false},
	{[]byte("REPORT"), []byte("N"), RMB_REPORT, ParseGeneric, true},
	{[]byte("RIGHT"), []byte("N"), RMB_RIGHT, ParseGeneric, false},
	{[]byte("SAFE"), []byte("NL"), RMB_SAFE, ParseGeneric, true},
	{[]byte("TRACE"), []byte("L"), RMB_TRACE, ParseGeneric, false},
}

// What INVERT prefix can be applied to?
var RMB_INVERTABLE = []int{RMB_BAND, RMB_BLIND, RMB_SAFE}

func GetParseDef(tblIdx int) *ParseDef {
	return &(RMB_PARSE_TABLE[tblIdx])
}

func rmbIndexByType(tpe int) int {
	for i, entry := range RMB_PARSE_TABLE {
		if entry.Type == tpe {
			return i
		}
	}
	panic("rmbIndexbyType: Unknown type")
	return 0
}

func rmbParseNumeric(context *ParseContext, overallIndex int) (int, bool) {
	ok := context.wordScanner.Scan()
	if !ok {
		context.LogError("option %s missing argument #%d, expected a numeric argument\n",
			string(context.command), overallIndex)
		return 0, false
	}
	arg := context.wordScanner.Text()
	num, err := strconv.Atoi(arg)
	if err != nil {
		context.LogError("option %s argument #%d is not a valid number: %s\n",
			string(context.command), overallIndex, err.Error)
		return 0, false
	}
	return num, true
}

// NOTE Lists without brackets are allowed. If the list argument is in non-tail
// position, only one numeral will be considered to constitute the list, while
// list in tail position can have any number of items, without need for brackets
// to be useds
func rmbParseList(context *ParseContext, overallIndex int, strict bool) ([]int,
	bool) {
	ok := context.wordScanner.Scan()
	if !ok {
		context.LogError("option %s missing argument #%d, expected a list argument\n",
			string(context.command), overallIndex)
		return nil, false
	}
	frsWrd := context.wordScanner.Bytes()
	if strict && frsWrd[0] != '(' {
		// The list is not a last argument but is missing an opening bracket,
		// thus it can only be a SINGLE numeral
		num, err := strconv.Atoi(string(frsWrd))
		if err != nil {
			context.LogError("option %s argument #%d is not a valid list: %s\n",
				string(context.command), overallIndex, err.Error)
			return nil, false
		}
		list := []int{num}
		return list, true
	}
	// Ok, let's see if we have an opening bracket
	closingBracket := frsWrd[0] == '('
	appendFirst := true
	if closingBracket {
		if len(frsWrd) > 1 {
			frsWrd = bytes.TrimSpace(frsWrd[1:])
			appendFirst = len(frsWrd) != 0
		} else {
			// There was only 1 char in this word, and that is '('
			appendFirst = false
		}
	}

	// See if the first word contains a number and a closing bracket, or just
	// a number. If appendFirst == false, it can't contain a number even, was
	// just an opening bracket and nothing else
	var list []int
	if appendFirst {
		earlyExit := true
		clsBracket := bytes.IndexByte(frsWrd, ')')
		if clsBracket != -1 {
			if !closingBracket {
				context.LogError("option %s argument #%d list has closing bracket without opening bracket\n",
					string(context.command), overallIndex)
				return nil, false
			}
			// closingBracket == true
			if clsBracket != (len(frsWrd) - 1) {
				context.LogError("option %s argument #%d list must have space after closing bracket\n",
					string(context.command), overallIndex)
				return nil, false
			}
		} else {
			clsBracket = len(frsWrd)
			earlyExit = false
		}
		pNum := string(frsWrd[:clsBracket])
		num, err := strconv.Atoi(pNum)
		if err != nil {
			context.LogError("option %s argument #%d list contains something that is not a valid numeral: %s\n",
				string(context.command), overallIndex, err.Error())
			return nil, false
		}
		list = []int{num}
		if earlyExit {
			return list, true
		}
	} else {
		list = make([]int, 0)
	}

	// Let's go
	keep := true
	for keep {
		ok := context.wordScanner.Scan()
		if !ok { // assuming only happens with EOF
			if closingBracket {
				context.LogError("option %s argument #%d list misses closing bracket to match the open one\n",
					string(context.command), overallIndex)
				return nil, false
			}
			break
		}
		frsWrd = context.wordScanner.Bytes()
		clsBracket := bytes.IndexByte(frsWrd, ')')
		if clsBracket != -1 {
			// has closing bracket
			keep = false
			if !closingBracket {
				context.LogError("option %s argument #%d list unexpected closing bracket (no opening one) to match\n",
					string(context.command), overallIndex)
				return nil, false
			} else if clsBracket != len(frsWrd)-1 {
				context.LogError("option %s argument #%d list must have space after closing bracket\n",
					string(context.command), overallIndex)
				return nil, false
			}
		} else {
			clsBracket = len(frsWrd)
		}
		sNum := string(frsWrd[:clsBracket])
		if len(sNum) == 0 { // was only closing bracket, no list item
			break
		}
		num, err := strconv.Atoi(string(frsWrd[:clsBracket]))
		if err != nil {
			context.LogError("option %s argument #%d list item is not a valid numeral: %s\n",
				string(context.command), overallIndex, err.Error())
			return nil, false
		}
		list = append(list, num)
	}
	return list, true
}

// Like INVERT, this command (BAND) is converted into prefix
func ParseBAND(context *ParseContext, tblIdx int, cmd *RMBCommand) bool {
	legit := false
	if context.wordScanner.Scan() {
		wrd := bytes.ToUpper(context.wordScanner.Bytes())
		if bytes.Equal(wrd, []byte("BLIND")) {
			cmd.Type = RMB_BLIND
			legit = true
		} else if bytes.Equal(wrd, []byte("SAFE")) {
			cmd.Type = RMB_SAFE
			legit = true
		}
		if legit {
			context.command = bytes.Join([][]byte{context.command,
				wrd}, []byte(" "))
		}
	}

	ok := false
	if legit {
		cmd.Band = true
		// was copying Zennode, but my code is only confused by it
		//tblIdx := context.rmbIndexByType(cmd.Type)
		ok = ParseGeneric(context, tblIdx, cmd)
	} else {
		context.LogError("unknown BAND option (supports only BLIND and SAFE, that must follow BAND)\n")
		return false
	}

	return ok
}

func ParseGeneric(context *ParseContext, tblIdx int, cmd *RMBCommand) bool {
	entry := context.getParseDef(tblIdx)
	dataIndex := 0
	listIndex := 0
	overallIndex := 0
	if entry.Syntax != nil {
		for i, synItem := range entry.Syntax {
			overallIndex++ // this is for error reporting
			switch synItem {
			case 'N': // Numeric item
				{
					var ok bool
					cmd.Data[dataIndex], ok = rmbParseNumeric(context, overallIndex)
					if !ok {
						return false
					}
					dataIndex++
				}
			case 'L': // List of numerals (sectors) item
				{
					var ok bool
					cmd.List[listIndex], ok = rmbParseList(context, overallIndex,
						i != (len(entry.Syntax)-1))
					if !ok {
						return false
					}
					listIndex++
				}
			default:
				{
					Log.Panic("programmer error (syntax item not supported: %s)", string(synItem))
				}
			}
		}

	}
	if context.wordScanner.Scan() {
		wtf := context.wordScanner.Bytes()
		if len(wtf) > 0 && wtf[0] != '#' {
			if entry.Syntax != nil {
				context.LogError("no more arguments - only %d argument(s) allowed\n", len(entry.Syntax))
				return false
			} else {
				context.LogError("%s accepts no arguments at all\n", string(entry.Name))
				return false
			}
		}
	}

	if !entry.Impl {
		context.LogWarning("%s effect acknowledged, but will not be applied because not implemented\n",
			string(entry.Name))
	}

	return true
}

func ParseMap(context *ParseContext, tblIdx int, cmd *RMBCommand) bool {
	// context.getParseDef -> workaround against compiler bug that "detects"
	// loop for RMB_PARSE_TABLE. I really don't understand what the fuck
	// is going on and why I can't reference RMB_PARSE_TABLE here
	switch context.getParseDef(tblIdx).Type {
	case RMB_EXMY_MAP:
		{
			sub := RMB_MAP_ExMx.FindSubmatch(context.command)
			// 0 is entire string
			// 1 is first group (episode number in this case)
			// 2 is first digit of map number
			// 3 is second digit of map number, if any (PrBoom-plus supports
			// maps like E9M12)
			if len(sub) > 4 || len(sub) < 3 {
				context.LogError("invalid map marker %s\n", context.command)
				return false
			}
			// Not checking for errors next - regexp should have taken care of
			// it
			cmd.Data[0], _ = strconv.Atoi(string(sub[1]))
			cmd.Data[1], _ = strconv.Atoi(string(sub[2]))
			if len(sub) == 4 && len(sub[3]) != 0 {
				sDigit, _ := strconv.Atoi(string(sub[3]))
				cmd.Data[1] = cmd.Data[1]*10 + sDigit
			}
		}
	case RMB_MAPXY_MAP:
		{
			mapNum, err := strconv.Atoi(string(context.command)[3:])
			if err != nil {
				// fuck. I need to format error using liNum. Basically,
				// all this functions need to be redesigned
				context.LogError("couldn't convert map numeral to integer: %s\n",
					err.Error())
				return false
			}
			cmd.Data[0] = mapNum
		}
	default:
		{
			// Should have been filtered out before calling ParseMap
			Log.Panic("RMB Parser: Programmer error. Unsupported map name format or wrong procedure call.\n")
		}
	}
	if context.wordScanner.Scan() {
		wtf := context.wordScanner.Bytes()
		if len(wtf) > 0 && wtf[0] != '#' {
			// Be strict rather than lax. Zennode misses this check I believe,
			// although enforces it for options handled by ParseGeneric
			context.LogError("map markers are not accepting any arguments in RMB options file\n")
			return false
		}
	} // else (it returned false) we think it is EOF rather than some error xD
	return true
}

func ParseINVERT(context *ParseContext, tblIdx int, cmd *RMBCommand) bool {
	// here context.command is entire line after INVERT, and yes we recurse
	// into parseRMBLine (scary)
	if context.recurseIntoParseRMBLine(context, context.command, true) {
		// Hack (bad): need to extract last cmd so that invert flag is applied
		// to it
		l := len(context.activeFrame.Commands)
		*cmd = context.activeFrame.Commands[l-1]
		cmd.Invert = true
		// delete old command as it will be reinserted with the new value
		// by the caller when we return
		context.activeFrame.Commands = context.activeFrame.Commands[:(l - 1)]
	} else {
		context.command = []byte("INVERT")
		return false
	}
	return true
}

func ParseNOPROCESS(context *ParseContext, tblIdx int, cmd *RMBCommand) bool {
	// The only option with two optional parameters: filename and map name

	// Since this has optional parameters, needs a way to signal their absence
	cmd.WadFileName = nil
	cmd.Data[0] = -1
	cmd.Data[1] = -1

	if !context.wordScanner.Scan() {
		// No optional parameters where specified
		return true
	}

	fname := context.wordScanner.Bytes()
	if len(fname) == 0 || fname[0] == '#' {
		return true
	}
	if fname[0] == '"' {
		// Name is specified in double quotes, can contain spaces
		// Damn, this can't be solved with this cursed scanner!
		context.LogError("filename in quotes is not supported yet. Will be fixed in next release")
		return false
	}

	cmd.WadFileName = fname

	// FIXME to read from scanner here might be incorrect when quotes will be
	// supported
	if !context.wordScanner.Scan() {
		return true
	}

	rawMapName := context.wordScanner.Bytes()
	if len(rawMapName) == 0 || rawMapName[0] == '#' {
		return true
	}
	mapName := bytes.ToUpper(rawMapName)
	validMap := false
	s1 := RMB_MAP_ExMx.FindSubmatch(mapName)
	if s1 != nil {
		// Not checking for errors next - regexp should have taken care of
		// it
		cmd.Data[0], _ = strconv.Atoi(string(s1[1]))
		cmd.Data[1], _ = strconv.Atoi(string(s1[2]))
		if len(s1) == 4 && len(s1[3]) != 0 {
			sDigit, _ := strconv.Atoi(string(s1[3]))
			cmd.Data[1] = cmd.Data[1]*10 + sDigit
		}
		validMap = true

	} else {
		s2 := RMB_MAP_SEQUEL.FindSubmatch(mapName)
		if s2 != nil {
			validMap = true
			cmd.Data[0] = -2
			d1, _ := strconv.Atoi(string(s2[1]))
			d2, _ := strconv.Atoi(string(s2[2]))
			cmd.Data[1] = d1*10 + d2
		}
	}

	if !validMap {
		context.LogError("%s is not a valid or supported map name\n", string(rawMapName))
		return false
	}

	if context.wordScanner.Scan() {
		wtf := context.wordScanner.Bytes()
		if len(wtf) > 0 && wtf[0] != '#' {
			context.LogError("NOPROCESS supports 2 arguments at most, but more were found\n")
			return false
		}
	}

	return true
}

// Will parse specified text. First return value is true if there were no errors.
// Second return value CAN be nil even if the first return value is true.
// In case of errors, parsing doesn't stop immediately but instead continues
// for a while (to provide user with hopefully more useful feedback), but the
// result will be set to false to indicate the failed RMB should not be used
func LoadRMB(src []byte, fname string) (bool, *LoadedRMB) {
	sc := bufio.NewScanner(bytes.NewReader(src))
	ok := true
	errNum := 0
	liNum := 0
	allFrames := []RMBFrame{
		{
			Id: RMBFrameId{
				Type:    RMB_FRAME_GLOBAL,
				Episode: 0,
				Map:     0,
			},
			Commands: make([]RMBCommand, 0),
			Parent:   nil,
		},
	}

	globalFrame := &(allFrames[0])
	context := ParseContext{
		activeFrame:             globalFrame,
		idToFrame:               make(map[RMBFrameId]*RMBFrame),
		globalFrame:             globalFrame,
		allFrames:               allFrames,
		fname:                   fname,
		getParseDef:             GetParseDef,
		recurseIntoParseRMBLine: parseRMBLineTrampoline,
		rmbIndexByType:          rmbIndexByType,
	}
	for sc.Scan() {
		liNum++ // lines start at 1
		li := sc.Bytes()
		context.liNum = liNum
		lineOk := context.parseRMBLine(li, false)
		ok = ok && lineOk
		if !lineOk {
			errNum++
			if errNum == RMB_ERROR_THRESHOLD {
				context.LogError("too many errors\n")
				break
			}
		}
	}
	// Now check if Go scanner itself failed with a error rather than EOF and
	// exited the above loop because of that. What could happen is that the text
	// line was too large, maybe we are parsing garbage rather than proper text,
	// or whatever
	err := sc.Err()
	if err != nil {
		ok = false // definitely failed
		context.LogError("couldn't parse line at all: %s", err.Error())
	}
	if !ok {
		return false, nil
	}
	return true, rmbParseContextToLoadedRMB(&context)
}

// Removes global RMB frame if there are no commands in it
func foldRMBFrames(src []RMBFrame) []RMBFrame {
	if len(src) == 0 {
		return src
	}
	// Only first frame is expected to be global. If it indeed is, but has
	// no commands, then it should be removed and all references to it made
	// nil
	if src[0].Id.Type == RMB_FRAME_GLOBAL && len(src[0].Commands) == 0 {
		if len(src) > 1 {
			src = src[1:]
			for _, frame := range src {
				// NOTE I do NOT check that parent is a global frame. This
				// should be the only frame ever specified as a parent
				frame.Parent = nil
			}
		} else {
			src = src[:0]
		}
	}
	return src
}

func rmbParseContextToLoadedRMB(context *ParseContext) *LoadedRMB {
	frames := foldRMBFrames(context.allFrames)
	if len(frames) == 0 {
		return nil
	}
	res := &LoadedRMB{
		mapFrames: context.idToFrame,
	}
	if frames[0].Id.Type == RMB_FRAME_GLOBAL {
		res.globalFrame = &(frames[0])
	} // otherwise globalFrame remains nil
	return res
}

func (c *ParseContext) LogError(msg string, a ...interface{}) {
	nA := make([]interface{}, 0, 2+len(a))
	nA = append(nA, c.fname)
	nA = append(nA, c.liNum)
	nA = append(nA, a...)
	Log.Error("%s:%d: ERROR "+msg, nA...)
}

func (c *ParseContext) LogWarning(msg string, a ...interface{}) {
	nA := make([]interface{}, 0, 2+len(a))
	nA = append(nA, c.fname)
	nA = append(nA, c.liNum)
	nA = append(nA, a...)
	Log.Verbose(1, "%s:%d: WARN "+msg, nA...)
}

// I am angry and on the verge to smash things with a hammer. Go compiler
// complaining about definition "loops" is really annoying. Does this mean
// that Go is unsuitable to writing parsers? What a ...
func parseRMBLineTrampoline(context *ParseContext, line []byte,
	fromInvert bool) bool {
	return context.parseRMBLine(line, fromInvert)
}

// Returns true if was successful
// TODO this is currently implemented in rather dumb, slow fashion, not only
// using scanner but also iteratively comparing against whole parse table.
// Should be rewritten to use smarter techniques - eventually
// also, about fromInvert == true: INVERT command can do that.
// parseRMBLine -> ParseINVERT -> parseRMBLine
func (c *ParseContext) parseRMBLine(line []byte, fromInvert bool) bool {
	line = bytes.TrimSpace(line)
	if len(line) == 0 || line[0] == '#' { // skip empty or comment lines
		return true
	}

	sc := bufio.NewScanner(bytes.NewReader(line))
	sc.Split(bufio.ScanWords)
	gotSome := sc.Scan()
	if !gotSome {
		err := sc.Err()
		if err != nil {
			c.LogError("(sys error) can't read command, scan failure %s\n",
				err.Error())
			return false
		} else {
			// This is very strange. Line still empty?
			c.LogError("(sys error) scanning a non-empty line, got an empty line\n")
			return false
		}
	}
	origCommand := sc.Bytes()
	command := bytes.ToUpper(origCommand)
	fnd := false
	ok := false
	if len(command) == 0 { // wtf, shouldn't happen
		c.LogError("(sys error) got empty command\n")
		return false
	}

	// Must be one of the commands in parse table
	for i, entry := range RMB_PARSE_TABLE {
		if fromInvert {
			legit := false
			for _, item := range RMB_INVERTABLE {
				if item == entry.Type {
					legit = true
					break
				}
			}
			if !legit {
				continue
			}
		}
		if entry.Type == RMB_EXMY_MAP || entry.Type == RMB_MAPXY_MAP {
			// Typically MAPXY or EXMY command
			// Must be full match, no shorthands possible here
			// Ugly code for now
			if entry.Type == RMB_EXMY_MAP {
				if len(command) > 2 && bytes.HasPrefix(command, []byte("E")) &&
					RMB_MAP_ExMx.Match(command) {
					fnd = true
				} else {
					continue
				}
			} else { // RMB_MAPXY_MAP
				if !bytes.HasPrefix(command, []byte("MAP")) {
					continue
				}
				if !RMB_MAP_SEQUEL.Match(command) {
					c.LogError("unsupported map marker, expected MAP followed by 2 digits\n")
				}
				fnd = true
			}
		} else if bytes.HasPrefix(entry.Name, command) {
			// Is unique?
			if i < len(RMB_PARSE_TABLE)-1 {
				if bytes.HasPrefix(RMB_PARSE_TABLE[i+1].Name, command) {
					// Not unique
					c.LogError("%s is not an unique shorthand for an RMB option (think: %s, %s)\n",
						string(origCommand), string(entry.Name),
						string(RMB_PARSE_TABLE[i+1].Name))
					return false
				}
			}
			fnd = true
		}
		if fnd {
			// parse this command using callback in parse table
			cmd := RMBCommand{
				SrcLine: c.liNum,
				Type:    entry.Type,
			}
			c.command = command
			c.wordScanner = sc
			if entry.Type == RMB_INVERT {
				// pass entire line without INVERT prefix as a command
				cl := len(command)
				c.command = line[cl:]
			}
			ok = entry.Callback(c, i, &cmd)
			// check if it went fine
			if ok {
				if !entry.Impl {
					c.LogWarning("recognised valid option '%s' will be ignored because it is not implemented\n",
						origCommand)
				}
				switch entry.Type {
				case RMB_EXMY_MAP:
					{
						// Leave whatever frame we had, switch to this one
						// It might or might not exist already
						// All future options, until next switch, will be local
						// to this map
						c.switchToMapFrame(RMBFrameId{
							Type:    RMB_FRAME_EXMY,
							Episode: cmd.Data[0],
							Map:     cmd.Data[1],
						})

					}
				case RMB_MAPXY_MAP:
					{
						// Leave whatever frame we had, switch to this one
						// It might or might not exist already
						// All future options, until next switch, will be local
						// to this map
						c.switchToMapFrame(RMBFrameId{
							Type:    RMB_FRAME_MAPXY,
							Episode: 0,
							Map:     cmd.Data[0],
						})
					}
				default:
					{
						// Add this command to current frame
						c.activeFrame.Commands = append(c.activeFrame.Commands,
							cmd)
					}
				}
			}
			break
		}
	}

	if !fnd {
		if fromInvert {
			c.LogError("illegal argument to INVERT, expected BAND, BLIND or SAFE\n")
		} else {
			c.LogError("unrecognised RMB option: %s\n",
				string(origCommand))
		}
	}

	return ok
}

// Switch context's active frame to a frame linked to map referenced by frameId.
// Creates new frame if one doesn't exist already
// Note that RMB allows to define map markers for the same map multiple times
// in the file, so yes frame can exist already.
// Also, don't pass global frame to it, because:
// 1. idToFrame map doesn't contain global frame
// 2. RMB doesn't support defining global options after any map marker, you
// only define global options before the first map marker
func (c *ParseContext) switchToMapFrame(frameId RMBFrameId) {
	frame, ok := c.idToFrame[frameId]
	if !ok {
		// Create new one
		c.allFrames = append(c.allFrames, RMBFrame{
			Id:       frameId,
			Commands: make([]RMBCommand, 0),
			Parent:   c.globalFrame,
		})
		frame = &(c.allFrames[len(c.allFrames)-1])
		c.idToFrame[frameId] = frame
	}
	c.activeFrame = frame
}
