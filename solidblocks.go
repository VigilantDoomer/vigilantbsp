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

// solidblocks
package main

// put constants for controlling goroutine and generating goroutine together,
// so they are assigned different ordinals and can't be mistaken for each other
const (
	// these are constants for solids-only blockmap CONTROLLER
	// they are used for BconRequest.Message
	BCON_NEED_SOLID_BLOCKMAP = iota
	BCON_NONEED_SOLID_BLOCKMAP
	BCON_DONE_WITH_SOLID_BLOCKMAP
	// internal constant for solids-only blockmap CONTROLLER
	BCON_WAIT_FOR_REQUEST
	// these are constants for solids-only blockmap GENERATOR
	BGEN_GENERATE_BLOCKMAP
	BGEN_RETRIEVE_BLOCKMAP
	BGEN_DIE
)

const (
	// Used in BconRequest.Sender
	SOLIDBLOCKS_NODES = iota
	SOLIDBLOCKS_REJECT
)

type SolidBlocks_Input struct {
	lines         SolidLines
	bounds        LevelBounds
	control       <-chan BconRequest
	genworker     chan BgenRequest
	linesToIgnore []bool
}

type BconRequest struct {
	Sender  int
	Message int
}

type BgenRequest struct {
	Action  int
	ReplyTo chan<- *Blockmap
}

// Sentinel goroutine that keeps track of who requested solid-only blockmap,
// there may be several users or there may be none, the blockmap is built by
// another goroutine only if there is AT LEAST ONE request for it and is disposed
// of when it becomes certain NO one would be requesting it anymore for SURE
func SolidBlocks_Control(input SolidBlocks_Input) {
	users := bcon_SystemUsers()
	if len(users) == 0 {
		return
	}

	bmi := BlockmapInput{
		lines:           input.lines,
		bounds:          input.bounds,
		XOffset:         0,
		YOffset:         0,
		useZeroHeader:   false,
		internalPurpose: true,
		gcShield:        nil,
		linesToIgnore:   input.linesToIgnore,
	}
	blockmapScheduled := false
	go bmi.SolidBlocks_Generator(input.genworker)
	for msg := range input.control {
		switch msg.Message {
		// No, BCON_WAIT_FOR_REQUEST is not allowed here
		case BCON_NEED_SOLID_BLOCKMAP:
			{
				bcon_UpdateUserStatus(users, msg)
				if !blockmapScheduled {
					blockmapScheduled = true
					input.genworker <- BgenRequest{
						Action:  BGEN_GENERATE_BLOCKMAP,
						ReplyTo: nil,
					}

				}
			}
		case BCON_NONEED_SOLID_BLOCKMAP:
			{
				bcon_UpdateUserStatus(users, msg)
			}
		case BCON_DONE_WITH_SOLID_BLOCKMAP:
			{
				bcon_UpdateUserStatus(users, msg)
			}
		default:
			{ // Programmer error.
				Log.Printf("Illegal request to solid blockmap generator\n")
			}
		}
		if bcon_NeedToDie(users) {
			break
		}
	}
	// Generator must die (quit)
	input.genworker <- BgenRequest{
		Action:  BGEN_DIE,
		ReplyTo: nil,
	}
	// Now blockmap can be garbage collected, provided all users deleted
	// their reference to it
	// Sentinel also quits
}

func bcon_AddUser(us []BconRequest, u BconRequest) []BconRequest {
	return append(us, u)
}

func bcon_UpdateUserStatus(us []BconRequest, u BconRequest) {
	for i, _ := range us {
		if us[i].Sender == u.Sender {
			us[i].Message = u.Message
			return
		}
	}
	Log.Printf("Solid blockmap generator (internal purposes): update status fail: unregistered user!\n")
}

func bcon_SystemUsers() []BconRequest {
	us := make([]BconRequest, 0, 2)
	us = bcon_AddUser(us, BconRequest{
		Sender:  SOLIDBLOCKS_REJECT,
		Message: BCON_WAIT_FOR_REQUEST,
	})
	us = bcon_AddUser(us, BconRequest{
		Sender:  SOLIDBLOCKS_NODES,
		Message: BCON_WAIT_FOR_REQUEST,
	})
	return us
}

func bcon_NeedToDie(us []BconRequest) bool {
	for _, uu := range us {
		if uu.Message != BCON_NONEED_SOLID_BLOCKMAP && uu.Message != BCON_DONE_WITH_SOLID_BLOCKMAP {
			return false
		}
	}
	return true
}

func (input *BlockmapInput) SolidBlocks_Generator(recv <-chan BgenRequest) {
	var bm *Blockmap
	generated := false
	for msg := range recv {
		if msg.Action == BGEN_DIE {
			// Hopefully sent by control and not by impostor
			break
		}
		if msg.Action == BGEN_GENERATE_BLOCKMAP {
			if !generated {
				generated = true
				bm = CreateBlockmap(input)
			}
		}
		if msg.Action == BGEN_RETRIEVE_BLOCKMAP {
			if !generated {
				//Looks like a retrieve request might happen faster than control
				//notifies us of the need to generate of which control was itself
				//notified by the same thread we got request now from. Wow, race conditions are tough
				//Log.Printf("Solid blockmap requested but was not ever generated\n")
				generated = true
				bm = CreateBlockmap(input)
			}
			msg.ReplyTo <- bm
		}
	}
}
