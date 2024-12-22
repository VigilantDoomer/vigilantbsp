// Copyright (C) 2022-2024, VigilantDoomer
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
	"time"
)

func VanillaOrZdoomFormat_Create(w *NodesWork, ts *NodeSeg, bbox *NodeBounds,
	super *Superblock, input *NodesInput, oldNodeType int,
	linesForZdoom WriteableLines, start time.Time) *NodeInProcess {

	input2 := &NodesInput{}
	*input2 = *input
	input2.lines = linesForZdoom
	input2.nodeType = oldNodeType
	input2.bcontrol = nil   // catch unexpected calls to it
	input2.bgenerator = nil // catch unexpected calls to it
	nodesChan := make(chan NodesResult)
	input2.nodesChan = nodesChan

	var rootNode *NodeInProcess
	if config.NodeThreads == 1 { // reference to global: config
		// sequential mode
		Log.Printf("Running vanilla nodes format first (sequential run)\n")
		rootNode = CreateNode(w, ts, bbox, super)
		if w.tooManySegsCantFix(true) ||
			uint32(w.totals.numSSectors)&w.SsectorMask == w.SsectorMask {
			// Limits exceeded, must upgrade
			Log.Printf("Vanilla nodes format overflowed, switching to Zdoom nodes format (sequential run)\n")
			input.lines.AssignFrom(input2.lines)
			go ZNodesGenerator(input2)
			// this interception is because timer is only output from here
			nodeResult := <-nodesChan
			input.nodesChan <- nodeResult
			return nilAndPrintTimer(start, oldNodeType)
		}
	} else {
		// concurrent mode

		Log.Printf("Running vanilla and zdoom generators in parallel to each other (concurrent run)")
		go ZNodesGenerator(input2)
		rootNode = CreateNode(w, ts, bbox, super)
		useZdoom := false
		if w.tooManySegsCantFix(true) ||
			uint32(w.totals.numSSectors)&w.SsectorMask == w.SsectorMask {
			// Limits exceeded, must upgrade
			Log.Printf("Vanilla nodes format overflowed, switching to Zdoom nodes format (concurrent run)\n")
			useZdoom = true
		}
		// must wait for completion regardless
		// TODO implement early abort
		nodeResult := <-nodesChan
		if useZdoom {
			input.lines.AssignFrom(input2.lines)
			input.nodesChan <- nodeResult
			return nilAndPrintTimer(start, oldNodeType)
		}
	}

	Log.Printf("I have kept nodes in vanilla format.\n")
	return rootNode
}

func nilAndPrintTimer(start time.Time, oldNodeType int) *NodeInProcess {
	switch promoteNodeType(oldNodeType) {
	case NODETYPE_ZDOOM_EXTENDED:
		Log.Printf("I have switched to ZDoom extended nodes format to avoid overflow.\n")
	case NODETYPE_ZDOOM_COMPRESSED:
		Log.Printf("I have switched to ZDoom compressed nodes format to avoid overflow.\n")
	}
	Log.Printf("Nodes took %s\n", time.Since(start))
	return nil
}
