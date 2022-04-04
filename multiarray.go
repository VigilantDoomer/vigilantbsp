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

// multiarray
package main

// TODO: unused. Didn't improve performance, surprisingly. In fact, it got
// a little worse, and GC still found the reasons to penalize program. Wtf
// with that shit.
// Anyway, this might come useful when the number of blocklists grows big.
// As it turned, it actually didn't do so yet on my test data(!), and the cause
// of intensive and ineffective garbage collection is elsewhere. I didn't test
// blockmap building in isolation... maybe the way I implemented multiarray
// access in blockity when I switched to multiarray may have caused bigger
// penalty from ineffective code in blockity , contrasted with smaller gains
// in blockmap itself ?

// C++ programmer: Told you GC sucks
// ... the rebuttal is yet to be found ... it looks like the only thing I got
// from Go is producing correct program BEFORE solving the memory issues, but
// solving memory issues would take no less effort as it would have otherwise...

// A shelter of organized 2-dimensional array resistance to "Go Garbage
// Collection World Order"
// The idea is to hide the pointers to the second dimension's contiguous
// arrays from Go GC, since 2D array only ever dies at once, but the number of
// those little arrays may be huge (blockmap can have up to 262144 blocks, which
// would mean array of 262144 arrays in conventional representation via
// [][]uint16, each of this 262144 arrays evaluated by GC while the program
// runs, which adds to SECONDS or MINUTES of GC pauses when using bruteforce
// approach of building up to 65536 blockmaps to produce the smallest blockmap)
// Yes, Go GC sucks extremely for blockmap building and reject building
// workloads
// Also. Byte alignment not guaranteed for first indices of our arrays, in case
// you thought of using sync.atomic or something

const DEFAULT_MULTIARRAY_PAGE_SIZE = 65536

type MultiArray_Page struct {
	// This is where innermost arrays ([]uint16) are stored together, effective
	// in reducing in number of GC operations provided that
	// number_of(pages) < number_of(arrays)
	data []uint16
	// How many cells at the end of page remain unused (where new arrays can
	// get allocated, or old ones reallocated)
	tail int
	// Voided is number of cells that were once occupied, but then data got
	// moved away.
	voided int
}

// A pointer-free replacement for []uint16
type SubArray struct {
	page     int // which page is it stored at
	start    int // which cell within the page it starts at
	count    int // how many cells does array have
	capacity int // how many cells are reserved for this array
}

// 2-dimensional array of uint16, data is stored in pages
type MultiArray struct {
	pages         []MultiArray_Page
	subArrays     []SubArray
	firstFreePage int // first page with non-zero tail
}

func Allocate_MultiArray(subArrayCount int) MultiArray {
	// To make things simple, always have at least one page
	pages := make([]MultiArray_Page, 1)
	pages[0].data = make([]uint16, DEFAULT_MULTIARRAY_PAGE_SIZE)
	pages[0].tail = len(pages[0].data)
	pages[0].voided = 0
	return MultiArray{
		pages:         pages,
		subArrays:     make([]SubArray, subArrayCount),
		firstFreePage: 0,
	}
}

// Slen returns length of #subIdx subarray
func (mArray MultiArray) Slen(subIdx int) int {
	return mArray.subArrays[subIdx].count
}

// When only one element is needed
func (mArray MultiArray) Get(subIdx int, elemIdx int) uint16 {
	sub := mArray.subArrays[subIdx]
	return mArray.pages[sub.page].data[elemIdx+sub.start]
}

func (mArray MultiArray) GetPtr(subIdx int, elemIdx int) *uint16 {
	sub := mArray.subArrays[subIdx]
	return &(mArray.pages[sub.page].data[elemIdx+sub.start])
}

func (mArray *MultiArray) MultiArray_DeriveEmpty(newSubArrayCount int) {
	for _, page := range mArray.pages {
		page.tail = len(page.data)
		page.voided = 0
	}
	mArray.firstFreePage = 0
	l := len(mArray.subArrays)
	if l > newSubArrayCount { // downsize
		mArray.subArrays = mArray.subArrays[:newSubArrayCount]
	}
	for i, _ := range mArray.subArrays { // existing subarrays need to be set to zero length
		mArray.subArrays[i] = SubArray{}
	}
	for i := newSubArrayCount; i > l; i-- { // need more subarrays
		mArray.subArrays = append(mArray.subArrays, SubArray{})
	}
}

func (mArray *MultiArray) Append(subIdx int, elem uint16) {
	if mArray.irregularAppend(subIdx, elem) {
		// hopefully doesn't happen often
		return
	}
	// Now for the main path
	posPlus := mArray.subArrays[subIdx].count
	minCapacityToTarget := mArray.subArrays[subIdx].count + 1
	if mArray.subArrays[subIdx].capacity < minCapacityToTarget {
		mArray.grow(subIdx, minCapacityToTarget)
	}
	subArray := mArray.subArrays[subIdx]
	mArray.pages[subArray.page].data[subArray.start+posPlus] = elem
	mArray.subArrays[subIdx].count++
}

func (mArray *MultiArray) irregularAppend(subIdx int, elem uint16) bool {
	subArray := mArray.subArrays[subIdx]
	if len(mArray.pages[subArray.page].data) > DEFAULT_MULTIARRAY_PAGE_SIZE {
		// this array uses a dedicated oversized (irregular) page
		mArray.pages[subArray.page].data = append(mArray.pages[subArray.page].data[:mArray.subArrays[subIdx].count], elem)
		mArray.subArrays[subIdx].count++
		mArray.subArrays[subIdx].capacity = len(mArray.pages[subArray.page].data)
		return true
	}
	return false
}

func (mArray *MultiArray) isAtTail(subIdx int) bool {
	sArray := mArray.subArrays[subIdx]
	return sArray.start+sArray.capacity+mArray.pages[sArray.page].tail == len(mArray.pages[sArray.page].data)
}

// So that we don't reallocate when adding every single element, this rounds it
// up nicely
func roundUpCapacity(minCapacity int) int {
	if minCapacity <= 16 {
		return 16
	}
	if minCapacity < 512 { // grow with slightly increasing enthusiasm for a while
		return minCapacity + (minCapacity >> 1)
	}
	return minCapacity + 16 // fixed slow growth
}

func (mArray *MultiArray) grow(subIdx int, minCapacity int) {
	sArray := mArray.subArrays[subIdx]
	addBackToTail := 0
	pageToGetTailBack := -1
	if mArray.isAtTail(subIdx) { // subArray is in tail position on its page
		takeFromTail := minCapacity - sArray.capacity
		if takeFromTail < mArray.pages[sArray.page].tail {
			// Good. We need not move an array, as we can simply add the desired
			// capacity from page's tail
			mArray.subArrays[subIdx].capacity = minCapacity
			mArray.pages[sArray.page].tail -= takeFromTail
			return
		} else {
			// we will be removing this subarray from this page
			addBackToTail = sArray.capacity
			pageToGetTailBack = sArray.page
		}
	} else {
		// Not in tail position, means we have hollow space between used blocks
		// which we can't reallocate
		mArray.pages[sArray.page].voided += sArray.capacity
	}

	pageIdx := -1
	minCapacity = roundUpCapacity(minCapacity)
	//toIrregular := false
	if minCapacity > DEFAULT_MULTIARRAY_PAGE_SIZE {
		// Irregular page exceeding the normal page size. We have to allocate it,
		// because we can't use multiple pages to store one array. It will have
		// only this array and nothing else, until this array is moved
		mArray.pages = append(mArray.pages, MultiArray_Page{
			data:   make([]uint16, minCapacity),
			tail:   minCapacity,
			voided: 0,
		})
		pageIdx = len(mArray.pages) - 1
		//toIrregular = true
	} else {
		// Seek for page to move our array to
		found := false
		for i := mArray.firstFreePage; i < len(mArray.pages); i++ {
			if mArray.pages[i].tail >= minCapacity {
				found = true
				pageIdx = i
				break
			}
		}
		if !found { // Not found
			mArray.pages = append(mArray.pages, MultiArray_Page{
				data:   make([]uint16, DEFAULT_MULTIARRAY_PAGE_SIZE),
				tail:   DEFAULT_MULTIARRAY_PAGE_SIZE,
				voided: 0,
			})
			pageIdx = len(mArray.pages) - 1
		}
	}

	// Now, either way, there is a page addressed by pageIdx where our
	// subArray can be relocated
	newStart := len(mArray.pages[pageIdx].data) - mArray.pages[pageIdx].tail
	if sArray.count > 0 {
		// Copy existing data
		copy(mArray.pages[pageIdx].data[newStart:newStart+sArray.count],
			mArray.pages[sArray.page].data[sArray.start:sArray.start+sArray.count])
	}
	oldCount := sArray.count
	mArray.subArrays[subIdx] = SubArray{
		page:     pageIdx,
		start:    newStart,
		count:    oldCount,
		capacity: minCapacity,
	}
	mArray.pages[pageIdx].tail -= minCapacity
	// now if tail got vanquished, relocate firstFreePage to move past
	// all spent pages
	for pageIdx < len(mArray.pages) && mArray.pages[pageIdx].tail == 0 &&
		mArray.firstFreePage == pageIdx {
		pageIdx++
	}

	if addBackToTail > 0 { // some page is getting back free bytes at tail
		mArray.pages[pageToGetTailBack].tail += addBackToTail
		if mArray.firstFreePage > pageToGetTailBack {
			mArray.firstFreePage = pageToGetTailBack
		}
		whole := len(mArray.pages[pageToGetTailBack].data)
		if whole ==
			(mArray.pages[pageToGetTailBack].tail + mArray.pages[pageToGetTailBack].voided) {
			mArray.pages[pageToGetTailBack].voided = 0
			mArray.pages[pageToGetTailBack].tail = whole
			// TODO in this case, maybe perform some page deletion on some
			// condition, which can come useful when faced with poisonous data
			// which generates many irregular pages
		}
	}
}

func (mArray *MultiArray) Free() {
	mArray.firstFreePage = 0
	mArray.pages = nil
	mArray.subArrays = nil
}
