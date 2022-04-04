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

// multiarray_test
package main

import (
	"math/rand"
	"testing"
)

func TestMultiArrayPlain(t *testing.T) {
	elementsToAdd := 65535 // < DEFAULT_MULTIARRAY_PAGE_SIZE
	multiArrayInsertNElements(elementsToAdd, t)
}

func TestMultiArrayIrregular(t *testing.T) {
	elementsToAdd := 600000 // > DEFAULT_MULTIARRAY_PAGE_SIZE
	multiArrayInsertNElements(elementsToAdd, t)
}

func TestReusableMultiArray(t *testing.T) {
	arrayMax := 50000
	numReps := 20
	mah := Allocate_MultiArray(arrayMax)
	elementsToAddTotal := 15000
	for rep := 0; rep < numReps; rep++ {
		l := rand.Intn(arrayMax-10) + 10
		mah.MultiArray_DeriveEmpty(l)
		elem := 0
		for i := 0; i < elementsToAddTotal; i++ {
			arrIdx := rand.Intn(l)
			mah.Append(arrIdx, uint16(elem%65536))
			elem++
		}
	}
	subReuseMultiArray(mah, arrayMax+3, t)
	subReuseMultiArray(mah, 12, t)
}

func subReuseMultiArray(mah MultiArray, newArrayMax int, t *testing.T) {
	elementsToAddTotal := 150
	mah.MultiArray_DeriveEmpty(newArrayMax)
	conventional := make([][]uint16, newArrayMax)
	for i, _ := range conventional {
		conventional[i] = make([]uint16, 0)
	}
	elem := 0
	for i := 0; i < elementsToAddTotal; i++ {
		arrIdx := rand.Intn(newArrayMax)
		conventional[arrIdx] = append(conventional[arrIdx], uint16(elem%65536))
		mah.Append(arrIdx, uint16(elem%65536))
		elem++
	}
	// now compare
	for i := 0; i < newArrayMax; i++ {
		sub := mah.subArrays[i]
		if len(conventional[i]) != sub.count {
			t.Errorf("Unequal length at %d array: %d, %d\n", i,
				len(conventional[i]), sub.count)
			return
		}
		for j := 0; j < len(conventional[i]); j++ {
			if conventional[i][j] != mah.pages[sub.page].data[sub.start+j] {
				t.Errorf("Differing elements at %d array %d element: %d, %d\n",
					i, j, conventional[i][j], mah.pages[sub.page].data[sub.start+j])
			}
		}
	}
}

func TestManyArrays(t *testing.T) {
	t.SkipNow()              // takes a lot of time to complete, so skip until we plan to use multiarray again
	arraysToCreate := 262144 // Worst case of blockmap
	mah := Allocate_MultiArray(arraysToCreate)
	conventional := make([][]uint16, 262144)
	for i, _ := range conventional {
		conventional[i] = make([]uint16, 0)
	}
	elementsToAddTotal := 150000000 // average ~572 elements per array
	elem := 0
	// insert elements into arrays in random order
	for i := 0; i < elementsToAddTotal; i++ {
		arrIdx := rand.Intn(arraysToCreate)
		conventional[arrIdx] = append(conventional[arrIdx], uint16(elem%65536))
		mah.Append(arrIdx, uint16(elem%65536))
		elem++
	}
	// now compare
	for i := 0; i < arraysToCreate; i++ {
		sub := mah.subArrays[i]
		if len(conventional[i]) != sub.count {
			t.Errorf("Unequal length at %d array: %d, %d\n", i,
				len(conventional[i]), sub.count)
			return
		}
		for j := 0; j < len(conventional[i]); j++ {
			if conventional[i][j] != mah.pages[sub.page].data[sub.start+j] {
				t.Errorf("Differing elements at %d array %d element: %d, %d\n",
					i, j, conventional[i][j], mah.pages[sub.page].data[sub.start+j])
			}
		}
	}
}

type AddElemRecord struct {
	arrIdx  int
	cntElem int
}

func _benchmarkConventionalArray() {
	arraysToCreate := 262144
	elementsToAdd := make([]AddElemRecord, arraysToCreate)
	for i, _ := range elementsToAdd {
		elementsToAdd[i] = AddElemRecord{
			arrIdx:  i,
			cntElem: i / 10000,
		}
	}
	conventional := make([][]uint16, 262144)
	for i, _ := range conventional {
		conventional[i] = make([]uint16, 0)
	}
	rand.Shuffle(arraysToCreate, func(i int, j int) {
		elementsToAdd[i], elementsToAdd[j] = elementsToAdd[j], elementsToAdd[i]
	})
	elem := 0
	for _, rec := range elementsToAdd {
		for j := 0; j < rec.cntElem; j++ {
			conventional[rec.arrIdx] = append(conventional[rec.arrIdx], uint16(elem%65536))
			elem++
		}
	}
}

func _benchmarkMultiArray() {
	arraysToCreate := 262144
	elementsToAdd := make([]AddElemRecord, arraysToCreate)
	for i, _ := range elementsToAdd {
		elementsToAdd[i] = AddElemRecord{
			arrIdx:  i,
			cntElem: i / 10000,
		}
	}
	mah := Allocate_MultiArray(arraysToCreate)
	rand.Shuffle(arraysToCreate, func(i int, j int) {
		elementsToAdd[i], elementsToAdd[j] = elementsToAdd[j], elementsToAdd[i]
	})
	elem := 0
	for _, rec := range elementsToAdd {
		for j := 0; j < rec.cntElem; j++ {
			mah.Append(rec.arrIdx, uint16(elem%65536))
			elem++
		}
	}
}

func BenchmarkConventionalArray(b *testing.B) {
	for i := 0; i < b.N; i++ {
		_benchmarkConventionalArray()
	}
}

func BenchmarkMultiArray(b *testing.B) {
	for i := 0; i < b.N; i++ {
		_benchmarkMultiArray()
	}
}

/* Will mainly test how go append works, not very interesting
func TestMultiArrayObscene(t *testing.T) {
	elementsToAdd := int(int32(600000000)) // >> DEFAULT_MULTIARRAY_PAGE_SIZE
	multiArrayInsertNElements(elementsToAdd, t)
}*/

func multiArrayInsertNElements(elementsToAdd int, t *testing.T) {
	mah := Allocate_MultiArray(1)
	// fill array with sequence 0, 1, 2, ..., elementsToAdd-1
	for i := 0; i < elementsToAdd; i++ {
		mah.Append(0, uint16(i%65536))
	}
	// test
	if mah.subArrays[0].count != elementsToAdd {
		t.Errorf("Fail %d != %d\n", mah.subArrays[0].count, elementsToAdd)
		return
	}
	if mah.subArrays[0].count > mah.subArrays[0].capacity {
		t.Errorf("Fail count > capacity: %d > %d\n.", mah.subArrays[0].count, mah.subArrays[0].capacity)
	}
	for i := 0; i < elementsToAdd; i++ {
		if mah.pages[mah.subArrays[0].page].data[mah.subArrays[0].start+i] != uint16(i%65536) {
			t.Errorf("Fail at address %d: %d != %d\n.", mah.subArrays[0].start+i,
				mah.pages[mah.subArrays[0].page].data[mah.subArrays[0].start+i],
				uint16(i%65536))
			return
		}
	}
}
