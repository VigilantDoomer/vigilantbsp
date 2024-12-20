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
	"math/rand"
	"testing"
	"time"
)

type hitsCrunchBenchmark struct {
	sectorHits   []uint8
	diff         int
	flat         int
	unmerged     int
	SectorsSplit int
}

var hitsCrunchData *hitsCrunchBenchmark

func TestMain(m *testing.M) {
	benchmarkRandom := rand.NewSource(time.Now().UnixNano())
	hitsCrunchData = setupHitsCrunchBenchmark(benchmarkRandom)
	m.Run()
}

func setupHitsCrunchBenchmark(benchmarkRandom rand.Source) *hitsCrunchBenchmark {
	limit := uint16(20000)
	sectorHits := make([]uint8, limit)
	for j := 1; j < 9400; j++ {
		v := benchmarkRandom.Int63()
		v = (((v&(1<<48) - 1) >> 48) | ((v&(1<<32) - 1) >> 32)) & (((v&(1<<16) - 1) >> 16) | v)
		uv := uint16(v)
		if uv >= limit {
			uv = limit - 1
		}
		sectorHits[uv] = uint8(v & 7)
	}
	return &hitsCrunchBenchmark{
		diff:         0,
		flat:         0,
		unmerged:     0,
		SectorsSplit: 0,
		sectorHits:   sectorHits,
	}
}
