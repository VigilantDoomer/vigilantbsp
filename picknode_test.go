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

// picknode_test.go
package main

import (
	"testing"
)

func TestHitsCrunchBenchmarkBootstrap(t *testing.T) {
	hc := hitsCrunchData
	for _, v := range hc.sectorHits {
		if v > 7 {
			t.Fatalf("sectorHits contains illegal data\n")
		}
	}
}

func BenchmarkHitsCrunchNew(b *testing.B) {
	hc := hitsCrunchData
	b.ResetTimer()

	for i := 0; i < b.N; i++ {
		for tot := range hc.sectorHits {
			switch hc.sectorHits[tot] {
			case 1:
				{
					hc.diff++
					hc.flat++
				}
			case 2:
				{
					hc.diff--
					hc.flat++
				}
			case 3:
				{
					hc.SectorsSplit++
					hc.flat++
				}
			case 4, 5, 6, 7:
				{
					hc.SectorsSplit++
					hc.unmerged++
					hc.flat++
				}
			}
		}
	}
}

func BenchmarkHitsCrunchOld(b *testing.B) {
	hc := hitsCrunchData
	b.ResetTimer()

	for i := 0; i < b.N; i++ {
		for tot := 0; tot < len(hc.sectorHits); tot++ {
			switch hc.sectorHits[tot] {
			case 1:
				{
					hc.diff++
				}
			case 2:
				{
					hc.diff--
				}
			}

			if hc.sectorHits[tot] >= 3 {
				hc.SectorsSplit++
			}
			if hc.sectorHits[tot] >= 4 {
				hc.unmerged++
			}
			if hc.sectorHits[tot] != 0 {
				hc.flat++
			}
		}
	}
}
