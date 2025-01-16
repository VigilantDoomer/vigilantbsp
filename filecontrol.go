// Copyright (C) 2025, VigilantDoomer
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
	"errors"
	"fmt"
	"io"
	"os"
	"path/filepath"
	"runtime/pprof"
)

// Controls lifetime of both input and output wads - ensures they are properly
// closed by the end of program, regardless of success and failure, and that
// a temporary file, if such was created because output file was not specified
// and we were to replace the input, either replaces that input file (on success)
// or is deleted (on failure)
type FileControl struct {
	success        bool
	tmp            bool
	fin            *os.File
	fout           *os.File
	frmb           *os.File
	freport        *os.File
	inputFileName  string
	outputFileName string
	rmbFileName    string
	reportFileName string
}

func (fc *FileControl) UsingTmp() bool {
	return fc.tmp
}

func (fc *FileControl) OpenInputFile(inputFileName string) (*os.File, error) {
	fc.inputFileName = inputFileName
	var err error
	fc.fin, err = os.Open(inputFileName)
	// because I'm stupid? Should have specified EXCLUSIVE
	//fc.fin, err = os.OpenFile(inputFileName, os.O_RDONLY, 0) // wouldn't protect from opening same file as output with read-write access!
	return fc.fin, err
}

func (fc *FileControl) OpenOutputFile(outputFileName string) (*os.File, string, error) {
	fc.tmp = outputFileName == ""
	var err error
	if fc.tmp {
		// Need a temporary file
		fc.fout, err = os.CreateTemp(filepath.Dir(fc.inputFileName), "tmp")
		if err == nil {
			outputFileName = fc.fout.Name()
		}
	} else {
		fc.fout, err = os.OpenFile(outputFileName, os.O_CREATE|os.O_RDWR|os.O_TRUNC,
			os.ModeExclusive|os.ModePerm)
	}
	fc.outputFileName = outputFileName
	return fc.fout, outputFileName, err
}

func (fc *FileControl) OpenRMBOptionsFile(rmbFileName string) (*os.File, error) {
	fc.rmbFileName = rmbFileName
	var err error
	fc.frmb, err = os.Open(rmbFileName)
	if err != nil { // just in case it is not set so
		fc.frmb = nil
	}
	return fc.frmb, err
}

func (fc *FileControl) CloseRMBOptionsFile() {
	if fc.frmb == nil {
		return
	}
	err := fc.frmb.Close()
	if err != nil {
		Log.Error("Couldn't close RMB file '%s': %s\n", fc.rmbFileName, err.Error())
	}
}

func (fc *FileControl) OpenReportFile() (*os.File, error) {
	var err error
	fc.freport, err = os.OpenFile(fc.reportFileName, os.O_CREATE|os.O_RDWR|os.O_TRUNC,
		os.ModeExclusive|os.ModePerm)
	if err != nil {
		fc.freport = nil
		return nil, err
	}
	return fc.freport, nil
}

func (fc *FileControl) CloseReportFile(suc bool) bool {
	if fc.freport == nil {
		return true
	}
	if suc {
		fc.freport.Write([]byte("# Report written successfully - no entries lost\n"))
	} else {
		fc.freport.Write([]byte("# Program aborted, report might be missing entries\n"))
	}
	err := fc.freport.Close()
	if err != nil {
		Log.Error("Couldn't close report file '%s': %s\n", fc.reportFileName, err.Error())
	} else {
		if suc {
			Log.Printf("Written report file %s\n", fc.reportFileName)
		} else {
			Log.Printf("Written incomplete report file %s\n", fc.reportFileName)
		}
	}
	fc.freport = nil
	return err == nil
}

func (fc *FileControl) Success() bool {
	if fc.fin == nil || fc.fout == nil {
		Log.Panic("Sanity check failed: descriptor invalid.\n")
	}
	errFin := fc.fin.Close()
	errFout := fc.fout.Close()
	sucReport := fc.CloseReportFile(true)
	hasError := errFin != nil || errFout != nil || !sucReport
	if hasError {
		if errFin != nil {
			Log.Error("Closing input file (after wad was almost ready) returned error: %s.\n",
				errFin.Error())
		}
		if errFout != nil {
			Log.Error("Closing output file (after wad was almost ready) returned error: %s.\n",
				errFout.Error())
		}
		return false
	}
	success2 := true
	if fc.tmp {
		success2 = fc.tempFileReplacesInput()
	}
	fc.success = true // nothing to clean up on program exit anyway (original file descriptors closed)
	return success2   // but the criteria to report success to user is different - no errors should have happened
}

func (fc *FileControl) tempFileReplacesInput() bool {
	success := true
	// now former output - temp file - is where we read from,
	// where as former input is the destination file we will overwrite
	fin, errFin := os.Open(fc.outputFileName)
	if errFin != nil {
		Log.Error("Couldn't reopen the temporarily file to read from it: %s.\n",
			errFin.Error())
		return false
	}
	fout, errFout := os.OpenFile(fc.inputFileName, os.O_CREATE|os.O_RDWR|os.O_TRUNC,
		os.ModeExclusive|os.ModePerm)
	if errFout != nil {
		success = false
		Log.Error("Couldn't reopen the input file to overwrite it: %s.\n",
			errFout.Error())
	} else {
		_, err := io.Copy(fout, fin)
		if err != nil {
			success = false
			// FUCK Hope the user has backup for this
			// TODO may be make backup oneself, I dunno
			Log.Error("Error when overwriting the original file: %s.\n",
				err.Error())
		}
		fout.Close()
	}
	fin.Close()
	// Now delete the temporary file
	err := os.Remove(fc.outputFileName)
	if err != nil {
		success = false
		Log.Error("Couldn't delete temporary file after overwriting the original one: %s.\n",
			err.Error())
	}
	return success
}

// Ensures we close all files when program exits. Temporary file is getting
// deleted at this moment
func (fc *FileControl) Shutdown() {
	if fc.success {
		return
	}

	var errFrmb error
	if fc.frmb != nil {
		errFrmb = fc.frmb.Close()
	}

	var errFin error
	if fc.fin != nil {
		errFin = fc.fin.Close()
	}

	var errFout error
	if fc.fout != nil {
		errFout = fc.fout.Close()
	}

	if errFrmb != nil {
		Log.Error("Couldn't close RMB file '%s': %s\n", fc.rmbFileName, errFrmb.Error())
	}

	if errFin != nil {
		Log.Error("Couldn't close input file '%s': %s\n", fc.inputFileName, errFin.Error())
	}

	if errFout != nil {
		Log.Error("Couldn't close output file '%s': %s\n", fc.outputFileName, errFout.Error())
	}

	fc.CloseReportFile(false)

	if fc.tmp { // Aborting unsuccessful operation when a temp file has been created
		if errFout != nil {
			Log.Error("Couldn't delete temporary file '%s' because failed to close it already.\n",
				fc.outputFileName)
			return
		}
		err := os.Remove(fc.outputFileName)
		if err != nil {
			Log.Error("Got error when trying to delete a temporary file '%s': %s\n", fc.outputFileName, err.Error())
		}
	}
}

// Loads RMB if it exists. ALLOWED to panic (and thus bring down the program)
// if:
// 1. File exists but couldn't be read, because, for example, permissions
// 2. Syntax error in RMB file encountered while parsing it
// If file doesn't exists, no panic occurs, but a message is printed to
// the output that file simply doesn't exist since this functions is only
// supposed to be called if user requested that RMB may be used whenever
// available. This may be useful indicator that there is a typo in RMB file name
// that user wanted to use alongside the file
func LoadAssociatedRMB(wadFullFileName string, fileControl *FileControl) *LoadedRMB {
	fext := filepath.Ext(wadFullFileName)
	cas := wadFullFileName
	if len(fext) > 0 {
		cas = cas[:(len(cas) - len(fext))]
	}
	rmbFullName := cas + ".rej"
	reportFullName := cas + ".rpt"

	RMBFile, err := fileControl.OpenRMBOptionsFile(rmbFullName)
	retry := false
	if err != nil {
		if errors.Is(err, os.ErrNotExist) {
			retry = true
		}
	}
	if retry {
		// Depending on OS and file system, names can be case-sensitive,
		// so we try both
		rmbFullName = cas + ".REJ"
		RMBFile, err = fileControl.OpenRMBOptionsFile(rmbFullName)
	}
	if err != nil {
		if errors.Is(err, os.ErrNotExist) {
			Log.Printf("Ignoring RMB option because there is no file in the same directory as the input wad file, which has same name as wad file but an extension of '.rej' or '.REJ'\n")
		} else {
			Log.Panic("Found RMB options file '%s' but opening it yielded an error: %s\n", rmbFullName, err.Error())
		}
		return nil
	}

	// if RMB options file exists, be ready to create reportFileName just in
	// case
	fileControl.reportFileName = reportFullName

	shortName := filepath.Base(rmbFullName)
	fileInfo, err := os.Stat(rmbFullName)
	if err != nil {
		Log.Panic("Couldn't obtain file size of '%s', aborting: %s\n", rmbFullName, err.Error())
	}
	rawSz := fileInfo.Size()
	sz := int(rawSz)
	if int64(sz) != rawSz {
		Log.Panic("RMB file is too large: %d\n", rawSz)
	}

	buf := make([]byte, sz)
	rsz, err := RMBFile.Read(buf)
	if err != nil && !errors.Is(err, io.EOF) {
		Log.Panic("Couldn't read RMB options file '%s': %s\n", rmbFullName, err.Error())
	}

	if rsz != sz {
		Log.Panic("Incomplete read of RMB options file '%s': number bytes read %d is different from byte size %d\n",
			rmbFullName, rsz, sz)
	}
	fileControl.CloseRMBOptionsFile()

	b, res := LoadRMB(buf, shortName)
	if !b {
		Log.Panic("Fatal error: RMB options file contains syntax errors.\n")
	}
	return res
}

// This was supposed to be used to generate test data to develop a partitioning
// scheme from "split concave polygon into convex polygons" algorithms (such as
// Hertel-Mehlhorn or Keil/Snoeyink optimal partition), before I decided that it
// might be not exactly useful for SEG minimization (or minimization of anything
// for that matter) after these convex partitions need to be redone the BSP way.
func DebugSaveDumpedSegs(where string) {
	fout, ferr := os.OpenFile(where, os.O_CREATE|os.O_RDWR|os.O_TRUNC, os.ModeExclusive|os.ModePerm)
	if ferr != nil {
		Log.Printf("An error has occured while trying to create/modify %s: %s\n", where, ferr)
		os.Exit(1)
	}
	defer fout.Close()
	n, _ := fout.WriteString(Log.GetDumpedSegs())
	Log.Printf("Wrote seg dump (%d bytes) to '%s'.\n", n, where)
}

func DumpMemoryProfile(where string) {
	fout, ferr := os.OpenFile(where, os.O_CREATE|os.O_RDWR|os.O_TRUNC, os.ModeExclusive|os.ModePerm)
	if ferr != nil {
		Log.Printf("An error has occured while trying to create/modify %s: %s\n", where, ferr)
		os.Exit(1)
	}
	defer fout.Close()
	pprof.Lookup("allocs").WriteTo(fout, 0)
}

// Print with platform-specific linebreaks indicated by CRLF argument
func WriterPrintfln(w io.Writer, CRLF bool, format string, a ...interface{}) {
	if len(format) > 0 && format[len(format)-1] == '\n' {
		format = string([]byte(format)[:len(format)-1])
	}
	w.Write([]byte(appendCRLF(CRLF, fmt.Sprintf(format, a...))))
}

func appendCRLF(CRLF bool, s string) string {
	if CRLF {
		return s + "\r\n"
	} else {
		return s + "\n"
	}
}
