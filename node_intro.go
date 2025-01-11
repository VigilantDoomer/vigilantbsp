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
	"math"
	"sort"
)

// node_intro.go
// Contains stuff executed before BSP partition evaluation and division starts,
// such as creation of initial segs from linedefs (with or without culling),
// estimating whether map is detailed etc.
// NOTE that it is nodegen.go (and not this file) that contains the actual
// vanilla/DeeP node generator start

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
	case PICKNODE_ZENLIKE:
		{
			return PickNode_ZennodeDepth
		}
	default:
		{
			Log.Panic("Invalid argument\n")
			return nil
		}
	}
}

func CreateNodeSSFromOption(userOption int) CreateNodeSSFunc {
	if userOption == PICKNODE_VISPLANE_ADV {
		return CreateNodeForSingleSector
	}
	return CreateNode
}

func StkCreateNodeSSFromOption(userOption int) StkCreateNodeSSFunc {
	if userOption == PICKNODE_VISPLANE_ADV {
		return StkCreateNodeForSingleSector
	}
	return StkCreateNode
}

func StkSingleSectorDivisorFromOption(userOption int) SingleSectorDivisorFunc {
	if userOption == PICKNODE_VISPLANE_ADV {
		return VigilantSingleSectorDivisor
	}
	return DefaultSingleSectorDivisor
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
			Log.Panic("Invalid argument\n")
			return nil
		}
	}
}

// IsTooDetailed returns whether level is detailed enough that it needs more
// accurate intersection evaluation than that provided by original
// doLinesIntersect checker from BSP v5.2 to ensure nodes are built correctly
// The improvement on accuracy does result in more seg/sector splits, however,
// which means it is not advised to be used for vanilla maps unless absolutely
// necessary
func IsTooDetailed(lines AbstractLines, linesToIgnore []bool, bounds *NodeBounds) bool {
	bmi := &BlockmapInput{
		lines:           lines,
		bounds:          *NodeBoundsToLevelBounds(bounds),
		XOffset:         0,
		YOffset:         0,
		useZeroHeader:   false,
		internalPurpose: true,
		gcShield:        nil,
		linesToIgnore:   linesToIgnore,
	}
	bm := CreateBlockmap(bmi)
	hitmap := make([]int, len(bm.blocklist))
	for i, _ := range bm.blocklist {
		score := 0
		for _, l := range bm.blocklist[i] {
			id := uint16(l)
			x1, y1, x2, y2 := lines.GetAllXY(id)
			dx := x2 - x1
			dy := y2 - y1
			if dx < 0 {
				dx = -dx
			}
			if dy < 0 {
				dy = -dy
			}
			score++
			twosided := uint16(lines.GetFlags(id))&LF_TWOSIDED == LF_TWOSIDED
			orthogonal := dx == 0 || dy == 0
			short := dx < 8 && dy < 8
			veryshort := dx < 4 && dy < 4
			if !orthogonal {
				score++
			}
			if short {
				score += 1
			}
			if veryshort {
				score += 3
			}
			if !orthogonal && short && twosided {
				score += 2
			}
		}
		hitmap[i] = score
	}
	scoremax := 0
	xl := int(bm.header.XBlocks)
	xh := xl - 1
	yl := int(bm.header.YBlocks)
	yh := yl - 1
	for i, _ := range hitmap {
		xj := i / xl
		xi := i - xj*xl
		// Consider entire 3x3 blocks of 128x128 pixels, centered on current
		// block
		score9 := hitmapAccAllIdx(xi, xj, xh, yh, hitmap)
		if scoremax < score9 {
			scoremax = score9
		}
	}
	ret := scoremax > DETAILED_LEVEL_THRESHOLD
	Log.Verbose(1, "<High amount of detail> = %t maxscore = %d threshold = %d\n", ret, scoremax, DETAILED_LEVEL_THRESHOLD)
	return ret
}

func hitmapAccIdx(i, j int, hiI, hiJ int, hitmap []int) int {
	if i < 0 || j < 0 || i > hiI || j > hiJ {
		return 0
	}
	x := (hiI+1)*j + i
	if x > len(hitmap)-1 {
		return 0 // safety measure
	}
	return hitmap[x]
}

func hitmapAccAllIdx(i, j int, hiI, hiJ int, hitmap []int) int {
	return hitmapAccIdx(i-1, j-1, hiI, hiJ, hitmap) +
		hitmapAccIdx(i, j-1, hiI, hiJ, hitmap) +
		hitmapAccIdx(i+1, j-1, hiI, hiJ, hitmap) +
		hitmapAccIdx(i-1, j, hiI, hiJ, hitmap) +
		hitmapAccIdx(i, j, hiI, hiJ, hitmap) +
		hitmapAccIdx(i+1, j, hiI, hiJ, hitmap) +
		hitmapAccIdx(i-1, j+1, hiI, hiJ, hitmap) +
		hitmapAccIdx(i, j+1, hiI, hiJ, hitmap) +
		hitmapAccIdx(i+1, j+1, hiI, hiJ, hitmap)
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
	linesToIgnore []bool, extNode bool,
	sectors []Sector) ([]*NodeSeg, []NodeVertex) {
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
				&rootCs, &lastCs, sidedefs, &res, extNode, sectors)
		}

	}
	// If culler is not nil, will identify self-referencing sectors and their
	// lines that need segs creation but were skipped. For nil culler these
	// were not skipped anyway
	cull.Analyze()

	// Now that analysis determined some lines were skipped in error, we create
	// segs for those lines here
	unculled := false
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
			&rootCs, &lastCs, sidedefs, &res, extNode, sectors)
		unculled = true
	}

	if unculled {
		// A note in Zennode claims that it is important for some effects (which
		// ones?) that order of segs match the order of linedefs.
		restoreSegOrder(res)
	}

	return res, myVertices
}

func addSegsPerLine(myVertices []NodeVertex, i uint16, lines WriteableLines, vidx1, vidx2 int,
	firstSdef, secondSdef uint16, rootCs **NodeSeg, lastCs **NodeSeg, sidedefs []Sidedef,
	res *[]*NodeSeg, extNode bool, sectors []Sector) {
	var lcs, rcs *NodeSeg
	horizon := lines.IsHorizonEffect(i)
	action := lines.GetAction(i)
	bamEffect := lines.GetBAMEffect(i) // support for linedef actions = 1080...1083
	preciousLine := lines.IsTaggedPrecious(i)
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
				sdef := &(sidedefs[firstSdef])
				lcs = addSeg(myVertices, i, vidx1, vidx2, rootCs,
					sdef, lastCs, horizon, bamEffect,
					preciousLine && !lines.SectorIgnorePrecious(sdef.Sector),
					sectors[sdef.Sector].Tag >= 900)
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
				sdef := &(sidedefs[secondSdef])
				rcs = addSeg(myVertices, i, vidx2, vidx1, rootCs,
					sdef, lastCs, horizon, bamEffect,
					preciousLine && !lines.SectorIgnorePrecious(sdef.Sector),
					sectors[sdef.Sector].Tag >= 900)
				rcs.flags |= SEG_FLAG_FLIP
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
		Log.Verbose(1, "Linedef %d has BAM or horizon effect specified, but it cannot be supported in Zdoom nodes format and so will be ignored.\n",
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
	sdef *Sidedef, lastCs **NodeSeg, horizon bool, bamEffect BAMEffect,
	precious bool, coalesce bool) *NodeSeg {
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
	if precious {
		s.flags |= SEG_FLAG_PRECIOUS
	}
	if coalesce {
		s.flags |= SEG_FLAG_COALESCE
	}

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

// restoreSegOrder orders segs so that indices of linedef they come from are
// in ascending order. This occurs naturally when segs are created, unless an
// option to remove some segs was used, which causes some segs to be inserted
// out-of-order
// NOTE not suitable for sorting segs after splits already happened - built
// under assumption there is at most 2 segs per linedef encountered in the
// supplied array
func restoreSegOrder(segs []*NodeSeg) {
	sort.Sort(InitialSegOrderByLinedef(segs))
	for i, x := range segs {
		if i < len(segs)-1 {
			x.next = segs[i+1]
		} else {
			x.next = nil
		}
	}
	// For safety, this should be already guaranteed! If/when GL nodes
	// generation is implemented, such error would be grave
	for _, x := range segs {
		if x.partner != nil && x.next != x.partner && x.partner.next != x {
			x.partner = nil
			Log.Verbose(1, "restoreSegOrder: partner was not referencing a neighbor seg!\n")
		}
	}
}

type InitialSegOrderByLinedef []*NodeSeg

func (x InitialSegOrderByLinedef) Len() int { return len(x) }
func (x InitialSegOrderByLinedef) Less(i, j int) bool {
	// primitive ordering - doesn't include offset, because damn offset
	// should be always ZERO for the intended use case
	return (x[i].Linedef < x[j].Linedef) ||
		(x[i].Linedef == x[j].Linedef && x[i].getFlip() < x[j].getFlip())
}
func (x InitialSegOrderByLinedef) Swap(i, j int) { x[i], x[j] = x[j], x[i] }

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
func (w *NodesWork) doInitialSuperblocks(rootBox *NodeBounds, forceSectors bool) *Superblock {
	var ret *Superblock
	if forceSectors {
		ret = w.newSectorUsingSuperblock()
	} else {
		ret = w.newSuperblockNoProto()
	}
	ret.nwlink = w
	ret.SetBounds(rootBox)
	for _, seg := range w.allSegs {
		ret.AddSegToSuper(seg)
	}
	return ret
}

// GetInitialStateClone (kinda like Clone or DeepCopy, but not quite) for that
// full-of-state NodesWork structure. Multi-tree algorithms depend very much on
// it!
// GetInitialStateClone won't produce data relevant to superblocks. You will
// have to call doInitialSuperblocks on the result to obtain new root superblock
// and actualize nextInSuper field in NodeSeg's
// If it is executed, it is usually executed more than once (even not just once
// per thread for multi-tree modes, but once per tree to build) but kept in
// node_intro.go nonetheless, as it is still "initialization" kind of thing that
// preceeds at least a particular tree being built
func (w *NodesWork) GetInitialStateClone() *NodesWork {
	newW := new(NodesWork)
	*newW = *w
	// TODO write tests to ensure programmers keep this function up-to-date
	// (use reflection etc. to do deep tests about references). There shall be
	// NO shared data

	if w.mlog != nil {
		newW.mlog = CreateMiniLogger()
	}

	newW.sides = make([]Sidedef, len(w.sides))
	for i := 0; i < len(newW.sides); i++ {
		newW.sides[i] = w.sides[i]
	}

	newW.sectors = make([]Sector, len(w.sectors))
	for i := 0; i < len(newW.sectors); i++ {
		newW.sectors[i] = w.sectors[i]
	}

	newW.totals = &NodesTotals{
		numNodes:               0,
		numSSectors:            0,
		numSegs:                0,
		maxSegCountInSubsector: 0,
		segSplits:              0,
	}

	newW.vertices = make([]NodeVertex, len(w.vertices))
	for i := 0; i < len(newW.vertices); i++ {
		newW.vertices[i] = w.vertices[i]
	}

	if w.vertexCache != nil {
		newW.vertexCache = make(map[SimpleVertex]int)
		for k, v := range w.vertexCache {
			newW.vertexCache[k] = v
		}
	}

	if !w.sidenessCache.readOnly {
		Log.Panic("Sideness cache [nodes] must be finalized before running multi-tree\n")
	}

	if w.vertexMap != nil {
		newW.vertexMap = w.vertexMap.Clone()
		newW.vertexMap.w = newW
	}

	if w.dgVertexMap != nil {
		newW.dgVertexMap = w.dgVertexMap.Clone()
		newW.dgVertexMap.w = newW
	}

	newW.segAliasObj = w.segAliasObj.Clone()
	newW.sectorHits = make([]byte, len(w.sectorHits))
	newW.incidental = make([]IntVertexPairC, 0)

	// NOTE newW.solidMap should not be written, only read from. Else we are screwed

	newW.nonVoidCache = make(map[int]NonVoidPerAlias)
	newW.blocksHit = make([]BlocksHit, 0)
	if newW.zenScores != nil {
		newW.zenScores = make([]DepthScoreBundle, 0, cap(w.zenScores))
	}

	newW.qallocSupers = nil // never share between threads!
	newW.stkExtra = nil     // never share between threads!

	if w.parts != nil {
		newW.parts = make([]*NodeSeg, 0)
	}

	if newW.zdoomVertexHeader != nil {
		newW.zdoomVertexHeader = new(ZdoomNode_VertexHeader)
		*newW.zdoomVertexHeader = *w.zdoomVertexHeader
	}

	// "deep copy" (or at least an approximationof such) for lines. That damn
	// interface! Especially in case of Hexen
	newW.lines = w.lines.Clone()

	newW.allSegs = make([]*NodeSeg, len(w.allSegs))
	for i := 0; i < len(newW.allSegs); i++ {
		newW.allSegs[i] = new(NodeSeg)
		*(newW.allSegs[i]) = *(w.allSegs[i])
		// zero out superblock data - no way to clone it from here
		newW.allSegs[i].nextInSuper = nil
		newW.allSegs[i].block = nil
		//
		// Fucking StartVertex and EndVertex
		newW.allSegs[i].StartVertex = &(newW.vertices[w.allSegs[i].StartVertex.idx])
		newW.allSegs[i].EndVertex = &(newW.vertices[w.allSegs[i].EndVertex.idx])
	}

	// Links between segs can only be updated after all segs are filled in
	for i := 0; i < len(newW.allSegs); i++ {
		if i < len(newW.allSegs)-1 {
			newW.allSegs[i].next = newW.allSegs[i+1]
		} else {
			newW.allSegs[i].next = nil
		}
		if w.allSegs[i].partner != nil {
			// which one, previous or next?
			if i > 0 && w.allSegs[i].partner == w.allSegs[i-1] {
				newW.allSegs[i].partner = newW.allSegs[i-1]
			} else {
				newW.allSegs[i].partner = newW.allSegs[i+1]
			}
		}
	}

	return newW
}
