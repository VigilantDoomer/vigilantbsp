// Based on go fix utility copyright (C) 2014 The Go Authors. All rights reserved
// Based on bundle utility authored by Russ Cox
// Based on gocat utility authored by Joseph Naegele, 2015
// Copyright (C) 2022, VigilantDoomer
//
// This file is part of build toolchain for VigilantBSP program.
//
// This program is free software: you can redistribute it
// and/or modify it under the terms of GNU General Public License
// as published by the Free Software Foundation, either version 2 of
// the License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <https://www.gnu.org/licenses/>.

// The below directive doesn't seem to work (gen executable still being created)
//// +build ignore
// The below one does, but now both cause LiteIDE to print out error trying
// to build, which I need still to verify correctness...
////go:build ignore
package main

// This file is not compiled into VigilantBSP executable ("+build ignore" directive
// should be present above), instead it is intended to be executed as part of
// VigilantBSP build toolchain (via "go generate" specified in Makefile) to
// produce some part of VigilantBSP source code from some other part of it.
// However, this program is still under GNU General Public License v2 or later
// license

// Usage:
// go run codegen.go -- --target=filetogenerate.go --include="file1.go;file2.go;file3.go"
// where:
// 1. first "--" is so that arguments to this program (codegen.go) are not
// mistaken for arguments to "go run"
// 2. --target specifies ONE file to be output (if exists, is overwritten)
// 3. --include specifies LIST of files to be parsed from which target file will
// be generated

// Some code in this file was borrowed from "go fix" utility written by
// Go Authors, as well as gocat utility by Joseph Nagele which itself is a
// modification of bundle utility by Russ Cox which was likewise using borrowed
// "go fix" code. Initially I planned to split borrowed stuff into separate file
// but that destroy the ploy of using "go run" to run it, so I had to put it all
// into one file

// Codegen is not perfect! Some code may be refused or incorrectly translated
// Code known to be refused:
// 1. renamed imports
// 2. Cgo-using code
// ...
// Code that may get incorrectly translated:
// 1. Shadowing functions with local variables
// 2. Same-named methods defined on different objects not all of which need
// conversion, then method calls executed on compound statements
// 3. Field names colliding with methods or functions name
// ...

import (
	"bytes"
	"flag"
	"go/ast"
	"go/format"
	"go/parser"

	"io/ioutil"
	"os"

	"go/token"
	"log"
	"strings"

	"errors"
	"fmt"
	"go/printer"
	"path"
	"strconv"
)

// CODEGEN_SIGN is a mark to be inserted into generated file
// MUST match "^//Code generated .* DO NOT EDIT\.$" regexp as per recommendation
// in go documentation, see also https://github.com/golang/go/issues/13560
const CODEGEN_SIGN = "// Code generated from other source files. DO NOT EDIT.\n"

const COPYRIGHT_STR = `// Copyright (C) 2022, VigilantDoomer
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
// along with VigilantBSP.  If not, see <https://www.gnu.org/licenses/>.`

var Log = log.New(os.Stdout, "", 0)

const (
	PRAGMA_SETPREFIX = iota
	PRAGMA_REPLACE_VANILLA
	PRAGMA_REPLACE_PROTOTYPE
	PRAGMA_INIT
)

const (
	TARGET_NOTFOUND = iota
	TARGET_FUNCDECL
	TARGET_TYPEDECL
	TARGET_VARDECL
	TARGET_CONSTDECL
)

type PragmaRec struct {
	Type int
	Op1  string // for replace pragmas, what is replaced
	Op2  string // for replace pragmas, the replacement
}

type TypeTracker struct {
	Name     string
	Affected bool // type that needs to be renamed because it is dependent on renamed or replaced type
	Pivot    bool // type which is replaced directly
	Object   *ast.Object
}

type TypeRelation struct {
	Element    *TypeTracker
	Containers []*TypeTracker
}

type PrefixOp struct {
	value   string
	renamed map[*ast.Ident]bool
}

type FuncTracker struct {
	Name   string
	Object interface{}
	Drop   bool // only tracked for destinations
}

type FuncReplaceTask struct {
	Source      *FuncTracker
	Destination *FuncTracker
}

type FuncRenameOperator struct {
	funcReplaces   []*FuncReplaceTask
	funcReplByName map[string]*FuncReplaceTask
}

func main() {
	// Check to see if first argument is "--", if so, then remove it
	// Why?
	// Well, "--" to stop parsing flags is _generic_ "go flag" library behavior
	// and NOT specific to "go run" through which this program intends to be run,
	// yet the following is to be taken into account:
	// 1. I need to pass "--" to "go run" after the name of this program and
	// before list of arguments this program will receive, so that "go run"
	// itself will not parse these arguments
	// 2. But when this program is run, this "--" is passed along too and
	// needs to be removed before flag.Parse() happens
	if len(os.Args) > 1 {
		if os.Args[1] == "--" {
			os.Args = append([]string{os.Args[0]}, os.Args[2:]...)
		}
	}
	wd, err := os.Getwd()
	if err != nil {
		Log.Fatalf("Couldn't get working directory: %s\n", err.Error())
	}
	Log.Printf("Working directory: %s\n", wd)
	var targetFileName string
	flag.StringVar(&targetFileName, "target", "", "Name of Go source file to generate")
	var includeStr string
	flag.StringVar(&includeStr, "include", "",
		"List of Go source file names, delimited by ';', from which target is generated")
	if len(os.Args) <= 1 {
		flag.Usage()
	}
	flag.Parse()

	// flag package doesn't remove quotes for us if strings used them, must do
	// ourselves
	targetFileName = UnquoteStr(targetFileName)
	includeStr = UnquoteStr(includeStr)

	if len(targetFileName) == 0 {
		Log.Fatalf("Missing --target argument.\n")
	}
	if len(includeStr) == 0 {
		Log.Fatalf("Missing --include argument.\n")
	}
	incFileNames := strings.Split(includeStr, ";")

	pragmas := make([]PragmaRec, 0)
	fset := token.NewFileSet()
	asts := make([]*ast.File, 0)
	for _, fname := range incFileNames {
		// Comments are needed to be parsed so that "#pragma" directives can be
		// found in any of the specified files. All other comments are ignored
		// and they should be never copied to the resulting file
		astI, err := parser.ParseFile(fset, fname, nil, parser.ParseComments)
		if err != nil {
			Log.Fatalf("Error parsing AST of file \"%s\": %s\n", fname, err.Error())
		}
		asts = append(asts, astI)
		ParseFilePragmas(astI.Comments, &pragmas)
		/*if fname == "zdefs.go" {
			ast.Fprint(os.Stdout, fset, astI, nil)
		}*/
	}
	if len(asts) == 0 {
		Log.Fatalf("Need at least one source file to proceed.\n")
	}
	identsToLookFor := make(map[string]int)
	prefix := PrefixOp{value: ""}
	for _, pragma := range pragmas {
		if pragma.Type == PRAGMA_SETPREFIX {
			if prefix.value != "" {
				Log.Fatalf("Setting prefix more than once is not allowed (old:%s, new:%s). \n",
					prefix.value, pragma.Op1)
			}
			prefix.value = pragma.Op1
		} else {
			identsToLookFor[pragma.Op1] = TARGET_NOTFOUND
			if pragma.Op2 != "" {
				identsToLookFor[pragma.Op2] = TARGET_NOTFOUND
			}
		}
	}

	typeToRelation := make(map[string]TypeRelation)
	isType := make(map[string]bool)
	isTop := make(map[interface{}]bool)
	for _, astI := range asts {
		// ast.Inspect(astI, func(n ast.Node) bool {
		InspectTopLevel(astI, func(n ast.Node, tok *token.Token) bool {

			switch x := n.(type) {
			case *ast.TypeSpec:

				fname := x.Name.Name
				rel, exists1 := typeToRelation[fname]
				if !exists1 {
					rel = TypeRelation{
						Element: &TypeTracker{
							Name:     fname,
							Affected: false,
							Pivot:    false,
							Object:   x.Name.Obj,
						},
						Containers: make([]*TypeTracker, 0),
					}
				}
				v, exists2 := identsToLookFor[fname]
				if exists2 {
					if v != TARGET_NOTFOUND {
						Log.Fatalf("Duplicate declaration: %s\n", fname)
					}
					rel.Element.Pivot = true
					identsToLookFor[fname] = TARGET_TYPEDECL
				}
				typeToRelation[fname] = rel
				isType[fname] = true
				isTop[x] = true
				DetectUsedTypes(rel.Element, x, typeToRelation)

			case *ast.ValueSpec:

				fname := x.Names[0].Name
				v, exists := identsToLookFor[fname]
				if exists {
					if v != TARGET_NOTFOUND {
						Log.Fatalf("Duplicate declaration: %s\n", fname)
					}
					if tok != nil && *tok == token.CONST {
						identsToLookFor[fname] = TARGET_CONSTDECL
					} else {
						identsToLookFor[fname] = TARGET_VARDECL
					}
				}
				isTop[x] = true

			case *ast.FuncDecl:

				fname := x.Name.Name
				if x.Recv != nil {
					// is a method
					fname = GetMethodReceiverTypeName(x.Recv.List[0].Type) +
						"." + fname
				}
				v, exists := identsToLookFor[fname]
				if exists {
					if v != TARGET_NOTFOUND {
						Log.Fatalf("Duplicate declaration: %s\n", fname)
					}
					identsToLookFor[fname] = TARGET_FUNCDECL
				}
				isTop[x] = true
			}

			return true
		})
	}

	allFound := true
	for k, v := range identsToLookFor {
		if v != TARGET_NOTFOUND {
			//Log.Printf("Discovered %s [%d]\n", k, v)
		} else {
			allFound = false
			Log.Printf("Target definition \"%s\" not found.\n", k)
		}
	}

	//DebugPrintPragmas(pragmas)

	relations := make([]TypeRelation, 0)
	for _, v := range typeToRelation {
		relations = append(relations, v)
	}

	for _, v := range relations {
		if v.Element.Pivot || v.Element.Affected {
			PropagateTypeAffect(v, typeToRelation)
		}
	}

	tpeMapping := make(map[string]string)
	for _, pragma := range pragmas {
		if pragma.Type == PRAGMA_REPLACE_VANILLA &&
			identsToLookFor[pragma.Op1] == TARGET_TYPEDECL {
			tpeMapping[pragma.Op1] = pragma.Op2
		}
	}
	funcOp := FuncRenameOperator{
		funcReplaces:   make([]*FuncReplaceTask, 0),
		funcReplByName: make(map[string]*FuncReplaceTask),
	}
	for _, astI := range asts {
		// where top-level declarations get renamed (but not references to
		// them)
		InspectTopLevel(astI, func(n ast.Node, tok *token.Token) bool {
			switch x := n.(type) {
			case *ast.TypeSpec:
				if typeToRelation[x.Name.Name].Element.Affected {
					prefix.Rename(x.Name)
				}
			case *ast.FuncDecl:
				fname := x.Name.Name
				if x.Recv != nil {
					// is a method, need to qualify its name to match pragma
					// notation
					fname = GetMethodReceiverTypeName(x.Recv.List[0].Type) +
						"." + fname
				}
				if fname == "init" {
					// Don't touch init() functions
				} else if identsToLookFor[fname] == TARGET_FUNCDECL {
					// function with special treatment (has directive
					// referencing it). But which kind exactly?
					for _, pragma := range pragmas {
						if funcOp.RenameFuncOnPragmaMatch(pragma, fname, x,
							typeToRelation, &prefix) {
							break
						}
					}
				} else {
					// function without special treatment which may still need
					// to be renamed
					funcOp.RenameArbitraryFunction(fname, x, typeToRelation,
						&prefix)
				}
			}

			return true
		})
	}

	// shortFuncRenames will contain only the name of function or method itself
	// (without receiver type), and only those that were changed (
	// funcOp.funcReplaces will contain sometimes replace records that have
	// same name in source and destination)
	shortFuncRenames := make(map[string]string)
	for _, it := range funcOp.funcReplaces {
		if it.Source.Name != it.Destination.Name {
			shortFuncRenames[it.Source.Name] = it.Destination.Name
		}
	}

	for _, astI := range asts {
		// borrowed from go cat - with necessary modifications
		// this renames _references_ to global declarations
		walk(astI, func(n interface{}) {
			switch x := n.(type) {
			case *ast.KeyValueExpr:

				id, ok := x.Key.(*ast.Ident)
				if ok && prefix.IsRenamed(id) &&
					strings.HasPrefix(id.Name, prefix.value) &&
					isType[id.Name[len(prefix.value):]] {
					// I don't even know when this executes... maybe it is
					// supposed to undo renames that happened by accident?
					// --VigilantDoomer
					id.Name = id.Name[len(prefix.value):]
				}
			case *ast.Ident:
				id := x
				repl, ok3 := tpeMapping[id.Name]
				if ok3 {
					// Type is replaced rather than prefixed
					_, ex := typeToRelation[repl]
					// if object is linked, make sure it is a top-level object
					// and not some local declaration with coinciding name
					ex = ex && (id.Obj == nil || isTop[id.Obj.Decl])
					if ex {
						id.Obj = nil
						id.Name = repl
						id.Obj = typeToRelation[repl].Element.Object
					} else {
						// Since it is in tpeMapping, it must be a type
						Log.Fatalf("typeRelation not found for types: %s -> %s\n", id.Name, repl)
					}
				} else {
					// normal type (that may need to get prefix)
					// or maybe not a type at all
					if id.Obj != nil {
						typeId, ok2 := id.Obj.Decl.(*ast.TypeSpec)
						ok2 = ok2 && isTop[id.Obj.Decl]
						if ok2 {
							if prefix.IsRenamed(typeId.Name) {
								prefix.Rename(id)
							}
						}
						// try as func
						funcId, ok2 := id.Obj.Decl.(*ast.FuncDecl)
						ok2 = ok2 && isTop[id.Obj.Decl]
						if ok2 {
							if funcId.Recv == nil {
								for _, repl := range funcOp.funcReplaces {
									if repl.Source.Name != repl.Destination.Name {
										if repl.Source.Object == funcId {
											id.Name = repl.Destination.Name
										}
									}

								}
							}
						}
					} else {
						// Unfortunately, some conversion require this clause.
						// They CAN and, under certain circumstances, WILL
						// result in source translated uncorrectly

						// When one runs into source code that is translated
						// incorrectly due to erroneous rename, the best thing
						// that can be done currectly is to edit the original
						// source (FROM which the resulting source is generated)
						// so that names don't collide between methods of
						// different objects, between field names, records,
						// functions and methods, etc.
						// FIXME find whatever way to contain errors to the best
						// possible minimum. Totally eliminating would be
						// impossible to do with AST alone, I think, but one
						// could add warnings about field name in some structure
						// matching method name; not converting ident's name
						// if it only matches a function (but not method) name
						// but ident is accessed via a SelectorExpt, etc.

						rel, ok2 := typeToRelation[id.Name]
						if ok2 && rel.Element.Affected {
							// Name matches one of renamed types' old name,
							// although it may refer to something other than
							// that type
							prefix.Rename(id)
						}

						funcDestName, ok2 := shortFuncRenames[id.Name]
						if ok2 {
							// Name matches one of renamed functions/methods,
							// although it may refer to something other object
							// that just happens to have the same name
							id.Name = funcDestName
						}
					}
				}

			}
		})
	}

	for _, astI := range asts {
		// Dropping top-level declarations that had not changed from the
		// originals
		DeleteFromThisLevel(astI, func(n ast.Node) bool {
			switch x := n.(type) {
			case *ast.FuncDecl:
				if x.Name.Name == "init" && x.Recv == nil {
					return false
				}
				for _, repl := range funcOp.funcReplaces {
					if x == repl.Destination.Object && !repl.Destination.Drop {
						return false
					}
				}
				return true
			case *ast.GenDecl:
				DeleteFromThisLevel(n, func(n2 ast.Node) bool {
					switch x := n2.(type) {
					case *ast.ValueSpec:
						// FIXME probably overreaching to delete all variables
						// and constants, what if some are typed with affected
						// types?
						return true
					case *ast.TypeSpec:
						return !prefix.IsRenamed(x.Name)
					}
					return false
				})
				if len(x.Specs) == 0 {
					return true
				}
				return false
			}
			return false
		})
	}

	// After we have dropped some declarations, it's time to see what kind
	// of strings that may refer to an imported package remain referenced
	importsToKeep := make([]map[string]bool, len(asts))
	for i, astI := range asts {
		importsToKeep[i] = make(map[string]bool)
		walk(astI, func(n interface{}) {
			switch x := n.(type) {
			case *ast.SelectorExpr:
				id, ok := x.X.(*ast.Ident)
				if ok { // MAY be an import, or something else
					importsToKeep[i][id.Name] = true
				}
			}
		})
	}

	for i, astI := range asts {
		// Dropping unused imports now
		DeleteFromThisLevel(astI, func(n ast.Node) bool {
			switch x := n.(type) {
			case *ast.GenDecl:
				DeleteFromThisLevel(n, func(n2 ast.Node) bool {
					switch x := n2.(type) {
					case *ast.ImportSpec:
						keep := importsToKeep[i][tailImportName(x)]
						if !keep {
							// Another structure that keeps track of imports,
							// oh wow
							for j, _ := range astI.Imports {
								if astI.Imports[j] == x {
									astI.Imports = append(astI.Imports[:j], astI.Imports[j+1:]...)
									break
								}
							}
						}
						return !keep
					}
					return false
				})
				if len(x.Specs) == 0 {
					return true
				}
				return false
			}
			return false
		})
	}

	initFunc := makeInit(pragmas, funcOp.funcReplByName)
	if initFunc != nil {
		// add it to the end of last one, so it is easier to find it in the
		// resulting file
		lastAst := asts[len(asts)-1]
		lastAst.Decls = append(lastAst.Decls, initFunc)
	}

	Log.Printf("Files parsed successfully.\n")
	if allFound {
		Log.Printf("All target definitions were found successfully.\n")
	} else {
		Log.Fatalf("Some target definitions were not found - see above.\n")
	}

	/*for _, v := range relations {
		if v.Element.Affected {
			Log.Printf("Type \"%s\" is derived from a replaced type.\n", v.Element.Name)
		}
	}*/

	Log.Printf("Merging (in memory).\n")
	bundledAST, err := ConcatAST(fset, asts, fset, targetFileName)
	if err != nil {
		Log.Fatalf("Concat failed: %s\n", err.Error())
	} else {
		Log.Printf("Concatenation succeeded.\n")
	}

	var buf bytes.Buffer
	buf.WriteString(CODEGEN_SIGN)
	buf.WriteString(COPYRIGHT_STR)
	buf.WriteString("\n")
	if err := format.Node(&buf, fset, bundledAST); err != nil {
		Log.Fatalf("Format failed: %s\n", err.Error())
	} else {
		Log.Printf("Formatting succeeded.\n")
	}

	Log.Printf("Writing %s...\n", targetFileName)
	ioutil.WriteFile(targetFileName, buf.Bytes(), os.ModeExclusive|os.ModePerm)
}

func tailImportName(s *ast.ImportSpec) string {
	fname := importPath(s)
	ns := strings.Split(fname, "/")
	return ns[len(ns)-1]
}

// InspectTopLevel - unlike ast.Inspect - shall find only *global* variables,
// methods, functions, but not any that are local to some function
func InspectTopLevel(node ast.Node, f func(ast.Node, *token.Token) bool) {
	switch n := node.(type) {
	case *ast.File:
		for _, decl := range n.Decls {
			if !f(decl, nil) {
				return
			}
			// Unlike functions and methods, declarations such as variables,
			// constants, types and imports are wrapped in GenDecl-type node
			// rather than being directly children of file's AST
			InspectTopLevel(decl, f)
		}
	case *ast.GenDecl:
		for _, decl := range n.Specs {
			if !f(decl, &n.Tok) {
				return
			}
		}
	}
}

// DeleteFromThisLevel traverses over global declarations (if passed a file) or
// a "generic declaration" children (if passed a GenDecl), calling a function on
// each one that tells whether to delete this declaration (true means delete,
// false means keep)
func DeleteFromThisLevel(node ast.Node, f func(ast.Node) bool) {
	switch n := node.(type) {
	case *ast.File:
		l := len(n.Decls)
		for i := 0; i < l; i++ {
			if f(n.Decls[i]) {
				n.Decls = append(n.Decls[:i], n.Decls[i+1:]...)
				i--
				l--
			}
		}
	case *ast.GenDecl:
		l := len(n.Specs)
		for i := 0; i < l; i++ {
			if f(n.Specs[i]) {
				n.Specs = append(n.Specs[:i], n.Specs[i+1:]...)
				i--
				l--
			}
		}
	}

}

// GetMethodReceiverTypeName returns name of the object type on which method
// is called, including "*" operands (can distinguish *NodeGen, NodeGen,
// **NodeGen, etc.)
func GetMethodReceiverTypeName(x ast.Expr) string {
	switch xx := x.(type) {
	case *ast.StarExpr:
		return "*" + GetMethodReceiverTypeName(xx.X)
	case *ast.Ident:
		return xx.Name
	default:
		Log.Fatalf("Cannot identify method receiver type: unknown expression type\n")
		return ""
	}
}

// ParseFilePragmas navigates through all comments, searches for "#pragma ..."
// directives (a made up syntax for this CODEGEN program), parses them into,
// appends their internal representation to the collection
func ParseFilePragmas(cgroup []*ast.CommentGroup, collector *[]PragmaRec) {
	for _, item := range cgroup {
		if item.List == nil {
			continue
		}
		for _, comment := range item.List {
			ParseComment(*comment, collector)
		}
	}
}

// ParseComment parses one comment for special directives
func ParseComment(comment ast.Comment, collector *[]PragmaRec) {
	if !strings.HasPrefix(comment.Text, "//") {
		return // only "//" comments can issue pragmas, but not "/*" comments
	}
	body := []byte(comment.Text)[2:]
	body = bytes.TrimSpace(body)
	if !bytes.HasPrefix(body, []byte("#pragma ")) {
		return
	}
	directive := bytes.Split(body, []byte(" "))
	directive = directive[1:] // already know #pragma is there, but what follows?
	rec := ParsePragma(directive)
	//Log.Println(comment.Text) // debug
	if rec == nil {
		// TODO better error handling, this method probably should just return
		// an indicator and not exit program directly, exiting to be handled
		// in the calling tree - print filename and position
		Log.Fatalf("Couldn't parse the following pragma: <%s>\n", comment.Text)
	}
	*collector = append(*collector, *rec)
}

// ParsePragma performs recognition of specific directive used
func ParsePragma(pragma [][]byte) *PragmaRec {
	if len(pragma) == 0 {
		return nil // invalid
	}
	if bytes.Equal(pragma[0], []byte("setprefix")) {
		if len(pragma) != 2 {
			return nil
		}
		return &PragmaRec{
			Type: PRAGMA_SETPREFIX,
			Op1:  UnquoteStr(string(pragma[1])),
			Op2:  "",
		}
	}
	if bytes.Equal(pragma[0], []byte("init")) {
		if len(pragma) != 5 || !bytes.Equal(pragma[2], []byte("with")) ||
			!bytes.Equal(pragma[3], []byte("morphed")) {
			return nil
		}
		return &PragmaRec{
			Type: PRAGMA_INIT,
			Op1:  string(pragma[1]),
			Op2:  string(pragma[4]),
		}
	}
	if bytes.Equal(pragma[0], []byte("replace")) {
		if len(pragma) != 4 || !bytes.Equal(pragma[2], []byte("with")) {
			return nil
		}
		return &PragmaRec{
			Type: PRAGMA_REPLACE_VANILLA,
			Op1:  string(pragma[1]),
			Op2:  string(pragma[3]),
		}
	}
	if bytes.Equal(pragma[0], []byte("replace_prototype")) {
		if len(pragma) != 4 || !bytes.Equal(pragma[2], []byte("with")) {
			return nil
		}
		return &PragmaRec{
			Type: PRAGMA_REPLACE_PROTOTYPE,
			Op1:  string(pragma[1]),
			Op2:  string(pragma[3]),
		}
	}
	return nil
}

func DebugPrintPragmas(pragmas []PragmaRec) {
	for _, pragma := range pragmas {
		Log.Printf("[Type: %d, Op1: \"%s\", Op2: \"%s\"]\n",
			pragma.Type, pragma.Op1, pragma.Op2)
	}
}

// UnquoteStr removes trailing quotes if string is wrapped in quotes
func UnquoteStr(s string) string {
	if IsQuoted(s) {
		es := len(s) - 1
		return s[1:es]
	}
	return s
}

// IsQuoted returns whether argument string begins AND ends with the same quote
// character
func IsQuoted(s string) bool {
	es := len(s) - 1
	if len(s) < 2 {
		return false
	}

	return (s[0] == '"' && s[es] == '"') ||
		(s[0] == '\'' && s[es] == '\'')
}

// DetectUsedTypes for type denoted by ptype and spec, creates mappings from
// all types referenced by it to ptype. Say, passing a struct will make
// rels[<fieldtypename>]<fieldtypeToStructRelation> for every field struct has
func DetectUsedTypes(ptype *TypeTracker, spec *ast.TypeSpec,
	rels map[string]TypeRelation) {
	all := GetAllTypes(spec.Type)
	for _, elem := range all {
		rel, exists := rels[elem]
		if !exists {
			rel = TypeRelation{
				Element: &TypeTracker{
					Name:     elem,
					Affected: false,
					Pivot:    false,
				},
				Containers: []*TypeTracker{ptype},
			}
		} else {
			fnd := false
			for _, cont := range rel.Containers {
				if cont == ptype {
					fnd = true
					break
				}
			}
			if !fnd {
				rel.Containers = append(rel.Containers, ptype)
			}
		}
		rels[elem] = rel
	}
}

// GetAllTypes parses potentially complex/compound type declaration (struct,
// map, pointers, whatever) to obtain type idents uses in it. So that
// map[string]bool will produce {"string","bool"} etc.
func GetAllTypes(typeNotation ast.Expr) []string {
	switch t := typeNotation.(type) {
	case *ast.Ident:
		return []string{t.Name}
	case *ast.StarExpr:
		return GetAllTypes(t.X)
	case *ast.ArrayType:
		return GetAllTypes(t.Elt)
	case *ast.MapType:
		return append(GetAllTypes(t.Key), GetAllTypes(t.Value)...)
	case *ast.ChanType:
		return GetAllTypes(t.Value)
	case *ast.StructType: // nested structs without type name are possible
		res := []string{}
		CollectFieldTypes(t.Fields, &res)
		return res
	case *ast.FuncType:
		res := []string{}
		// TypeParams - not supported before Go1.18, 
		// syntax is not used in VigilantBSP code
		// CollectFieldTypes(t.TypeParams, &res) 
		CollectFieldTypes(t.Params, &res)
		CollectFieldTypes(t.Results, &res)
		return res
	case *ast.InterfaceType:
		res := []string{}
		// Not sure what stuff is there to begin with, and if it can be
		// represented as a type
		CollectFieldTypes(t.Methods, &res)
		return res
	case *ast.SelectorExpr:
		// types defined as imports from elsewhere, for example
		// "reflect.SelectCase". Can't return them properly
		return []string{}
	case *ast.Ellipsis:
		return GetAllTypes(t.Elt)
		// TODO this may be incomplete. Be prepared to implement new clauses as
		// required
	}
	Log.Fatalf("Unhandled type notation.\n", typeNotation)
	return []string{}
}

func CollectFieldTypes(fl *ast.FieldList, collector *[]string) {
	if fl == nil {
		return
	}
	for _, f := range fl.List {
		names := GetAllTypes(f.Type)
		*collector = append(*collector, names...)
	}
}

func PropagateTypeAffect(rec TypeRelation, tracker map[string]TypeRelation) {
	for _, cont := range rec.Containers {
		if cont.Pivot {
			Log.Fatalf("Error: a compound type \"%s\" is derived from a replaced type but has an explicit replacement at the same time.\n",
				cont.Name)
		}
		if !cont.Affected {
			cont.Affected = true
			rec, exists := tracker[cont.Name]
			if exists {
				PropagateTypeAffect(rec, tracker)
			}
		}
	}
}

func (p *PrefixOp) IsRenamed(ident *ast.Ident) bool {
	if p.renamed == nil {
		p.renamed = make(map[*ast.Ident]bool)
	}
	return p.renamed[ident]
}

func (p *PrefixOp) Rename(id *ast.Ident) {
	if id != nil && !p.IsRenamed(id) && id.Name != "_" && id.Name != "" {
		id.Name = p.value + id.Name
		p.renamed[id] = true
	}
}

// CheckFuncAffected returns two values:
// 1. Whether function uses any renamed/converted type at all
// 2. Whether function needs to keep its name, which means either the preceeding
// is false, or the function is a method (has a receiver) and receiver is
// renamed/converted
func CheckFuncAffected(rmap map[string]TypeRelation, x *ast.FuncDecl) (bool, bool) {
	recvConv := FieldListHasType(rmap, x.Recv)
	restConv := FieldListHasType(rmap, x.Type.Params) ||
		// TypeParams -  not supported before Go1.18, 
		// syntax is not used in VigilantBSP code
		// FieldListHasType(rmap, x.Type.TypeParams) ||
		FieldListHasType(rmap, x.Type.Results)
	conv := recvConv || restConv
	return conv, !conv || (x.Recv != nil && recvConv)

}

func FieldListHasType(rmap map[string]TypeRelation, fl *ast.FieldList) bool {
	if fl == nil {
		return false
	}
	for _, f := range fl.List {
		names := GetAllTypes(f.Type)
		for _, n := range names {
			rel, ok := rmap[n]
			if ok {
				if rel.Element.Affected || rel.Element.Pivot {
					return true
				}
			}
		}
	}
	return false
}

// GetMethodFuncName returns only function name without receiver type and dot
func GetMethodFuncName(methodName string) string {
	tmp := strings.Split(methodName, ".")
	return tmp[len(tmp)-1]
}

// RenameFuncOnPragmaMatch identifies if directive pragma applies to current
// function (with qualified name fname and object x), if so, the action depends
// on both which kind of pragma and kind of function/method is processed, with
// following outcomes:
// 1. Function is pragma-init destination => prefixed unconditionally. Methods
// are not allowed
// 2. Function is pragma-replace source, it is scheduled to be dropped from
// source code, and to have all references to it replaced by references to
// pragma-replace destination name.
// 3. Function is pragma-replace destination, it is scheduled to be dropped from
// source code (this is fine, because its declaration will persist in original
// source files, and thus the dropping from generated source is necessary to
// avoid duplicate declaration discovered by compiler)
// 4. Function is pragma-prototype source, then pragma-prototype
// destination name is derived from prefixed source name and is
// assigned to pragma-prototype destination, (the sources
// declaration is dropped). Once again, references to it will
// be replaced to pragma-prototype destination. Here the
// condition destination and source can have the same (method)
// name is not expected and not checked, thus no special
// treatment coded for it
// 5. if function is pragma-prototype destination, see previous
// !!! Methods (as opposed to functions) get renamed only if receiver types
// is not renamed/converted
func (op *FuncRenameOperator) RenameFuncOnPragmaMatch(pragma PragmaRec,
	fname string, x *ast.FuncDecl, typeToRelation map[string]TypeRelation,
	prefix *PrefixOp) bool {
	b := false
	switch pragma.Type {
	case PRAGMA_INIT:
		if fname == pragma.Op2 {
			if strings.IndexByte(fname, '.') != -1 {
				Log.Fatalf("Target for init directive should not be a method: %s.",
					fname)
			}
			//Log.Printf("Function '%s' will be renamed.\n", fname)
			repl := &FuncReplaceTask{
				Source: &FuncTracker{
					Name:   fname,
					Object: x,
				},
				Destination: &FuncTracker{
					Name:   prefix.value + fname,
					Object: x,
					Drop:   false,
				},
			}
			op.funcReplaces = append(op.funcReplaces, repl)
			// note that here source function name is not in pragma.op1 (unlike
			// replace or replace_prototype pragma), it is in fact pragma.Op2
			op.funcReplByName[fname] = repl
			x.Name.Name = prefix.value + fname
			b = true
		}
	case PRAGMA_REPLACE_VANILLA:
		if fname == pragma.Op1 || fname == pragma.Op2 {
			rawfname1 := GetMethodFuncName(pragma.Op1)
			rawfname2 := GetMethodFuncName(pragma.Op2)
			if rawfname1 == rawfname2 {
				// Nothing to be done here. This should happen
				// when it is same function name but different
				// types, and one of those types is replaced
				// with another and the method already defined
				// on destination
			} else {
				if strings.IndexAny(pragma.Op2, ".") != -1 {
					Log.Fatalf("Can't replace a method with a method that has a different name [%s , %s].\n",
						pragma.Op1, pragma.Op2)
				}
				repl := op.openFuncReplForPragma(pragma, fname)
				if fname == pragma.Op1 {
					repl.Source = &FuncTracker{
						Name:   rawfname1,
						Object: x,
					}
				} else {
					repl.Destination = &FuncTracker{
						Name:   rawfname2,
						Object: x,
						Drop:   true,
					}
				}
				op.funcReplByName[pragma.Op1] = repl
				// references to #1 need to be replaced with
				// references to #2, but nothing else is
				// done - both function declarations are
				// dropped
			}
			// Log.Printf("Not renaming function '%s'.\n", fname)
			b = true
		}
	case PRAGMA_REPLACE_PROTOTYPE:
		if fname == pragma.Op1 || fname == pragma.Op2 {
			rawfname1 := GetMethodFuncName(pragma.Op1)
			repl := op.openFuncReplForPragma(pragma, fname)

			newname := prefix.value + rawfname1
			keepName := x.Recv != nil && FieldListHasType(typeToRelation, x.Recv)
			if keepName {
				newname = rawfname1
			}
			if fname == pragma.Op1 {
				repl.Source = &FuncTracker{
					Name:   rawfname1,
					Object: x,
				}
			} else {
				repl.Destination = &FuncTracker{
					Name:   newname,
					Object: x,
					Drop:   false,
				}
				x.Name.Name = newname
			}
			op.funcReplByName[pragma.Op1] = repl
			b = true
		}
	}
	return b
}

// openFuncReplForPragma gets (if exists) or creates record about which function
// is replaced by which, also updating a mapping between pragma directive source
// function and this record. If record already exists, but pragma differs from
// the one already linked, it will return error
func (op *FuncRenameOperator) openFuncReplForPragma(pragma PragmaRec,
	fname string) *FuncReplaceTask {
	repl := op.funcReplByName[pragma.Op1]
	if repl != nil {
		if (fname == pragma.Op1 && repl.Source != nil) ||
			(fname == pragma.Op2 && repl.Destination != nil) {
			Log.Fatalf("Duplicate mapping: new '%s'->'%s', old '%s'->'%s'\n",
				pragma.Op1, pragma.Op2,
				repl.Source.Name, repl.Destination.Name)
		}
	} else {
		repl = &FuncReplaceTask{}
		op.funcReplaces = append(op.funcReplaces, repl)
	}
	return repl
}

// RenameArbitraryFunction determines whether function deserves to be renamed,
// and if not, whether it is scheduled to be dropped from source code.
// If function is not subject to any directives, then it is prefixed if
// any of the types in declaration (in/out params, method
// receiver if it's a method) is affected or pivot, but the
// function body is not checked (yes, this kind of analysis may
// be INCOMPLETE, but that is fine
// Methods are prefixed only if the receiver type is not converted/renamed
func (op *FuncRenameOperator) RenameArbitraryFunction(fname string,
	x *ast.FuncDecl, typeToRelation map[string]TypeRelation, prefix *PrefixOp) {

	affected, keepName := CheckFuncAffected(typeToRelation, x)
	if affected {
		rawfname := GetMethodFuncName(fname)
		if keepName {
			repl := &FuncReplaceTask{
				Source: &FuncTracker{
					Name:   rawfname,
					Object: x,
				},
				Destination: &FuncTracker{
					Name:   rawfname,
					Object: x,
					Drop:   false,
				},
			}
			op.funcReplaces = append(op.funcReplaces, repl)
			op.funcReplByName[fname] = repl
		} else {
			// A non-method function that needs to be renamed
			//Log.Printf("Function '%s' will be renamed.\n", fname)
			repl := &FuncReplaceTask{
				Source: &FuncTracker{
					Name:   rawfname,
					Object: x,
				},
				Destination: &FuncTracker{
					Name:   prefix.value + rawfname,
					Object: x,
					Drop:   false,
				},
			}
			op.funcReplaces = append(op.funcReplaces, repl)
			op.funcReplByName[rawfname] = repl
			x.Name.Name = prefix.value + rawfname
		}
	} else {
		//Log.Printf("Not renaming function '%s'.\n", fname)
	}
}

// makeInit creates init() function containing assingments derived from
// PRAGMA_INIT directives. Golang supports AST have multiple init() functions in
// one file even, btw, although here we create only one this can come useful
// if any of concatenated files has init() for whatever reason
// Returns nil, if there are no PRAGMA_INIT directives
func makeInit(pragmas []PragmaRec, rmap map[string]*FuncReplaceTask) *ast.FuncDecl {
	initStatements := make([]ast.Stmt, 0)
	for _, pragma := range pragmas {
		if pragma.Type == PRAGMA_INIT {
			// <pragma.Op1> = <pragma.Op2>, i.e.
			// Callback = Function1
			fun := pragma.Op2
			repl, ok := rmap[fun]
			if ok {
				fun = repl.Destination.Name
			}
			initStatements = append(initStatements,
				&ast.AssignStmt{
					Lhs: []ast.Expr{
						&ast.Ident{
							Name: pragma.Op1,
						},
					},
					Tok: token.ASSIGN,
					Rhs: []ast.Expr{
						&ast.Ident{
							Name: fun,
						},
					},
				})
		}
	}
	if len(initStatements) == 0 {
		return nil
	}
	// total result will be:
	// func init() {
	//		Callback1 = Function1
	//		Callback2 = Function2
	//		...
	// }
	// those may be not necessary funcs and func-typed vars of course, just some
	// global idents
	return &ast.FuncDecl{
		Doc:  nil,
		Recv: nil,
		Name: &ast.Ident{
			Name: "init",
		},
		Type: &ast.FuncType{
			Params: &ast.FieldList{
				List: nil,
			},
		},
		Body: &ast.BlockStmt{
			List: initStatements,
		},
	}
}

// addImport adds the import path to the file f, if absent.
// this and methods called from this are borrowed from Go Authors code
func addImport(f *ast.File, ipath string) (added bool) {
	if imports(f, ipath) {
		return false
	}

	// Determine name of import.
	// Assume added imports follow convention of using last element.
	_, name := path.Split(ipath)

	// Rename any conflicting top-level references from name to name_.
	renameTop(f, name, name+"_")

	newImport := &ast.ImportSpec{
		Path: &ast.BasicLit{
			Kind:  token.STRING,
			Value: strconv.Quote(ipath),
		},
	}

	// Find an import decl to add to.
	var (
		bestMatch  = -1
		lastImport = -1
		impDecl    *ast.GenDecl
		impIndex   = -1
	)
	for i, decl := range f.Decls {
		gen, ok := decl.(*ast.GenDecl)
		if ok && gen.Tok == token.IMPORT {
			lastImport = i
			// Do not add to import "C", to avoid disrupting the
			// association with its doc comment, breaking cgo.
			if declImports(gen, "C") {
				continue
			}

			// Compute longest shared prefix with imports in this block.
			for j, spec := range gen.Specs {
				impspec := spec.(*ast.ImportSpec)
				n := matchLen(importPath(impspec), ipath)
				if n > bestMatch {
					bestMatch = n
					impDecl = gen
					impIndex = j
				}
			}
		}
	}

	// If no import decl found, add one after the last import.
	if impDecl == nil {
		impDecl = &ast.GenDecl{
			Tok: token.IMPORT,
		}
		f.Decls = append(f.Decls, nil)
		copy(f.Decls[lastImport+2:], f.Decls[lastImport+1:])
		f.Decls[lastImport+1] = impDecl
	}

	// Ensure the import decl has parentheses, if needed.
	if len(impDecl.Specs) > 0 && !impDecl.Lparen.IsValid() {
		impDecl.Lparen = impDecl.Pos()
	}

	insertAt := impIndex + 1
	if insertAt == 0 {
		insertAt = len(impDecl.Specs)
	}
	impDecl.Specs = append(impDecl.Specs, nil)
	copy(impDecl.Specs[insertAt+1:], impDecl.Specs[insertAt:])
	impDecl.Specs[insertAt] = newImport
	if insertAt > 0 {
		// Assign same position as the previous import,
		// so that the sorter sees it as being in the same block.
		prev := impDecl.Specs[insertAt-1]
		newImport.Path.ValuePos = prev.Pos()
		newImport.EndPos = prev.Pos()
	}

	f.Imports = append(f.Imports, newImport)
	return true
}

// walk traverses the AST x, calling visit(y) for each node y in the tree but
// also with a pointer to each ast.Expr, ast.Stmt, and *ast.BlockStmt,
// in a bottom-up traversal.
func walk(x interface{}, visit func(interface{})) {
	walkBeforeAfter(x, nop, visit)
}

func nop(interface{}) {}

// walkBeforeAfter is like walk but calls before(x) before traversing
// x's children and after(x) afterward.
// Warning: support for features introduced in Go 1.18 was removed
// to make this build in Go 1.15
func walkBeforeAfter(x interface{}, before, after func(interface{})) {
	before(x)

	switch n := x.(type) {
	default:
		panic(fmt.Errorf("unexpected type %T in walkBeforeAfter", x))

	case nil:

	// pointers to interfaces
	case *ast.Decl:
		walkBeforeAfter(*n, before, after)
	case *ast.Expr:
		walkBeforeAfter(*n, before, after)
	case *ast.Spec:
		walkBeforeAfter(*n, before, after)
	case *ast.Stmt:
		walkBeforeAfter(*n, before, after)

	// pointers to struct pointers
	case **ast.BlockStmt:
		walkBeforeAfter(*n, before, after)
	case **ast.CallExpr:
		walkBeforeAfter(*n, before, after)
	case **ast.FieldList:
		walkBeforeAfter(*n, before, after)
	case **ast.FuncType:
		walkBeforeAfter(*n, before, after)
	case **ast.Ident:
		walkBeforeAfter(*n, before, after)
	case **ast.BasicLit:
		walkBeforeAfter(*n, before, after)

	// pointers to slices
	case *[]ast.Decl:
		walkBeforeAfter(*n, before, after)
	case *[]ast.Expr:
		walkBeforeAfter(*n, before, after)
	case *[]*ast.File:
		walkBeforeAfter(*n, before, after)
	case *[]*ast.Ident:
		walkBeforeAfter(*n, before, after)
	case *[]ast.Spec:
		walkBeforeAfter(*n, before, after)
	case *[]ast.Stmt:
		walkBeforeAfter(*n, before, after)

	// These are ordered and grouped to match ../../go/ast/ast.go
	case *ast.Field:
		walkBeforeAfter(&n.Names, before, after)
		walkBeforeAfter(&n.Type, before, after)
		walkBeforeAfter(&n.Tag, before, after)
	case *ast.FieldList:
		for _, field := range n.List {
			walkBeforeAfter(field, before, after)
		}
	case *ast.BadExpr:
	case *ast.Ident:
	case *ast.Ellipsis:
		walkBeforeAfter(&n.Elt, before, after)
	case *ast.BasicLit:
	case *ast.FuncLit:
		walkBeforeAfter(&n.Type, before, after)
		walkBeforeAfter(&n.Body, before, after)
	case *ast.CompositeLit:
		walkBeforeAfter(&n.Type, before, after)
		walkBeforeAfter(&n.Elts, before, after)
	case *ast.ParenExpr:
		walkBeforeAfter(&n.X, before, after)
	case *ast.SelectorExpr:
		walkBeforeAfter(&n.X, before, after)
		// VigilantDoomer discovered omission in original walkBeforeAfter code,
		// that is fixed in go/ast walk
		walkBeforeAfter(&n.Sel, before, after)
	case *ast.IndexExpr:
		walkBeforeAfter(&n.X, before, after)
		walkBeforeAfter(&n.Index, before, after)
	// IndexListExpr - not supported before Go1.18, 
	// syntax is not used in VigilantBSP code
	//case *ast.IndexListExpr:
	//	walkBeforeAfter(&n.X, before, after)
	//	walkBeforeAfter(&n.Indices, before, after)
	case *ast.SliceExpr:
		walkBeforeAfter(&n.X, before, after)
		if n.Low != nil {
			walkBeforeAfter(&n.Low, before, after)
		}
		if n.High != nil {
			walkBeforeAfter(&n.High, before, after)
		}
	case *ast.TypeAssertExpr:
		walkBeforeAfter(&n.X, before, after)
		walkBeforeAfter(&n.Type, before, after)
	case *ast.CallExpr:
		walkBeforeAfter(&n.Fun, before, after)
		walkBeforeAfter(&n.Args, before, after)
	case *ast.StarExpr:
		walkBeforeAfter(&n.X, before, after)
	case *ast.UnaryExpr:
		walkBeforeAfter(&n.X, before, after)
	case *ast.BinaryExpr:
		walkBeforeAfter(&n.X, before, after)
		walkBeforeAfter(&n.Y, before, after)
	case *ast.KeyValueExpr:
		walkBeforeAfter(&n.Key, before, after)
		walkBeforeAfter(&n.Value, before, after)

	case *ast.ArrayType:
		walkBeforeAfter(&n.Len, before, after)
		walkBeforeAfter(&n.Elt, before, after)
	case *ast.StructType:
		walkBeforeAfter(&n.Fields, before, after)
	case *ast.FuncType:
		// TypeParams - not supported before Go1.18, 
		// syntax is not used in VigilantBSP code
		//if n.TypeParams != nil {
		//	walkBeforeAfter(&n.TypeParams, before, after)
		//}
		walkBeforeAfter(&n.Params, before, after)
		if n.Results != nil {
			walkBeforeAfter(&n.Results, before, after)
		}
	case *ast.InterfaceType:
		walkBeforeAfter(&n.Methods, before, after)
	case *ast.MapType:
		walkBeforeAfter(&n.Key, before, after)
		walkBeforeAfter(&n.Value, before, after)
	case *ast.ChanType:
		walkBeforeAfter(&n.Value, before, after)

	case *ast.BadStmt:
	case *ast.DeclStmt:
		walkBeforeAfter(&n.Decl, before, after)
	case *ast.EmptyStmt:
	case *ast.LabeledStmt:
		walkBeforeAfter(&n.Stmt, before, after)
	case *ast.ExprStmt:
		walkBeforeAfter(&n.X, before, after)
	case *ast.SendStmt:
		walkBeforeAfter(&n.Chan, before, after)
		walkBeforeAfter(&n.Value, before, after)
	case *ast.IncDecStmt:
		walkBeforeAfter(&n.X, before, after)
	case *ast.AssignStmt:
		walkBeforeAfter(&n.Lhs, before, after)
		walkBeforeAfter(&n.Rhs, before, after)
	case *ast.GoStmt:
		walkBeforeAfter(&n.Call, before, after)
	case *ast.DeferStmt:
		walkBeforeAfter(&n.Call, before, after)
	case *ast.ReturnStmt:
		walkBeforeAfter(&n.Results, before, after)
	case *ast.BranchStmt:
	case *ast.BlockStmt:
		walkBeforeAfter(&n.List, before, after)
	case *ast.IfStmt:
		walkBeforeAfter(&n.Init, before, after)
		walkBeforeAfter(&n.Cond, before, after)
		walkBeforeAfter(&n.Body, before, after)
		walkBeforeAfter(&n.Else, before, after)
	case *ast.CaseClause:
		walkBeforeAfter(&n.List, before, after)
		walkBeforeAfter(&n.Body, before, after)
	case *ast.SwitchStmt:
		walkBeforeAfter(&n.Init, before, after)
		walkBeforeAfter(&n.Tag, before, after)
		walkBeforeAfter(&n.Body, before, after)
	case *ast.TypeSwitchStmt:
		walkBeforeAfter(&n.Init, before, after)
		walkBeforeAfter(&n.Assign, before, after)
		walkBeforeAfter(&n.Body, before, after)
	case *ast.CommClause:
		walkBeforeAfter(&n.Comm, before, after)
		walkBeforeAfter(&n.Body, before, after)
	case *ast.SelectStmt:
		walkBeforeAfter(&n.Body, before, after)
	case *ast.ForStmt:
		walkBeforeAfter(&n.Init, before, after)
		walkBeforeAfter(&n.Cond, before, after)
		walkBeforeAfter(&n.Post, before, after)
		walkBeforeAfter(&n.Body, before, after)
	case *ast.RangeStmt:
		walkBeforeAfter(&n.Key, before, after)
		walkBeforeAfter(&n.Value, before, after)
		walkBeforeAfter(&n.X, before, after)
		walkBeforeAfter(&n.Body, before, after)

	case *ast.ImportSpec:
	case *ast.ValueSpec:
		walkBeforeAfter(&n.Type, before, after)
		walkBeforeAfter(&n.Values, before, after)
		walkBeforeAfter(&n.Names, before, after)
	case *ast.TypeSpec:
		// TypeParams - not supported before Go1.18, 
		// syntax is not used in VigilantBSP code
		//if n.TypeParams != nil {
		//	walkBeforeAfter(&n.TypeParams, before, after)
		//}
		walkBeforeAfter(&n.Type, before, after)

	case *ast.BadDecl:
	case *ast.GenDecl:
		walkBeforeAfter(&n.Specs, before, after)
	case *ast.FuncDecl:
		if n.Recv != nil {
			walkBeforeAfter(&n.Recv, before, after)
		}
		walkBeforeAfter(&n.Type, before, after)
		if n.Body != nil {
			walkBeforeAfter(&n.Body, before, after)
		}

	case *ast.File:
		walkBeforeAfter(&n.Decls, before, after)

	case *ast.Package:
		walkBeforeAfter(&n.Files, before, after)

	case []*ast.File:
		for i := range n {
			walkBeforeAfter(&n[i], before, after)
		}
	case []ast.Decl:
		for i := range n {
			walkBeforeAfter(&n[i], before, after)
		}
	case []ast.Expr:
		for i := range n {
			walkBeforeAfter(&n[i], before, after)
		}
	case []*ast.Ident:
		for i := range n {
			walkBeforeAfter(&n[i], before, after)
		}
	case []ast.Stmt:
		for i := range n {
			walkBeforeAfter(&n[i], before, after)
		}
	case []ast.Spec:
		for i := range n {
			walkBeforeAfter(&n[i], before, after)
		}
	}
	after(x)
}

// imports reports whether f imports path.
func imports(f *ast.File, path string) bool {
	return importSpec(f, path) != nil
}

// importSpec returns the import spec if f imports path,
// or nil otherwise.
func importSpec(f *ast.File, path string) *ast.ImportSpec {
	for _, s := range f.Imports {
		if importPath(s) == path {
			return s
		}
	}
	return nil
}

// importPath returns the unquoted import path of s,
// or "" if the path is not properly quoted.
func importPath(s *ast.ImportSpec) string {
	t, err := strconv.Unquote(s.Path.Value)
	if err == nil {
		return t
	}
	return ""
}

// renameTop renames all references to the top-level name old.
// It reports whether it makes any changes.
func renameTop(f *ast.File, old, new string) bool {
	var fixed bool

	// Rename any conflicting imports
	// (assuming package name is last element of path).
	for _, s := range f.Imports {
		if s.Name != nil {
			if s.Name.Name == old {
				s.Name.Name = new
				fixed = true
			}
		} else {
			_, thisName := path.Split(importPath(s))
			if thisName == old {
				s.Name = ast.NewIdent(new)
				fixed = true
			}
		}
	}

	// Rename any top-level declarations.
	for _, d := range f.Decls {
		switch d := d.(type) {
		case *ast.FuncDecl:
			if d.Recv == nil && d.Name.Name == old {
				d.Name.Name = new
				d.Name.Obj.Name = new
				fixed = true
			}
		case *ast.GenDecl:
			for _, s := range d.Specs {
				switch s := s.(type) {
				case *ast.TypeSpec:
					if s.Name.Name == old {
						s.Name.Name = new
						s.Name.Obj.Name = new
						fixed = true
					}
				case *ast.ValueSpec:
					for _, n := range s.Names {
						if n.Name == old {
							n.Name = new
							n.Obj.Name = new
							fixed = true
						}
					}
				}
			}
		}
	}

	// Rename top-level old to new, both unresolved names
	// (probably defined in another file) and names that resolve
	// to a declaration we renamed.
	walk(f, func(n interface{}) {
		id, ok := n.(*ast.Ident)
		if ok && isTopName(id, old) {
			id.Name = new
			fixed = true
		}
		if ok && id.Obj != nil && id.Name == old && id.Obj.Name == new {
			id.Name = id.Obj.Name
			fixed = true
		}
	})

	return fixed
}

// declImports reports whether gen contains an import of path.
func declImports(gen *ast.GenDecl, path string) bool {
	if gen.Tok != token.IMPORT {
		return false
	}
	for _, spec := range gen.Specs {
		impspec := spec.(*ast.ImportSpec)
		if importPath(impspec) == path {
			return true
		}
	}
	return false
}

// isTopName reports whether n is a top-level unresolved identifier with the given name.
func isTopName(n ast.Expr, name string) bool {
	id, ok := n.(*ast.Ident)
	return ok && id.Name == name && id.Obj == nil
}

// matchLen returns the length of the longest prefix shared by x and y.
func matchLen(x, y string) int {
	i := 0
	for i < len(x) && i < len(y) && x[i] == y[i] {
		i++
	}
	return i
}

func newErrorf(format string, a ...interface{}) error {
	return errors.New(fmt.Sprintf(format, a...))
}

// ConcatAST *DESTRUCTIVELY* merges files (which should be loaded into the
// same fileset as fset) into one stream of valid Go code, which is parsed
// into fsetOut FileSet (can be the same as fset) using bundleName as a source
// name.
// This method is a refactoring of code borrowed from gocat (gocat is a program
// by Joseph Naegele, modified from Russ Cox's bundle)
func ConcatAST(fset *token.FileSet, files []*ast.File,
	fsetOut *token.FileSet, bundleName string) (*ast.File, error) {
	var f0 *ast.File
	for i, f := range files {
		if i == 0 {
			f0 = f
		} else {
			f.Name = &ast.Ident{Name: "PACKAGE-DELETE-ME"}
			for _, spec := range f.Imports {
				if spec.Name != nil {
					return nil, newErrorf("%s: renamed import not supported",
						fset.Position(spec.Name.Pos()))
				}
				path, err := strconv.Unquote(spec.Path.Value)
				if err != nil {
					return nil, newErrorf("%s: invalid quoted string %s",
						fset.Position(spec.Name.Pos()), spec.Path.Value)
				}
				if path == "C" {
					return nil, newErrorf("%s: import \"C\" not supported",
						fset.Position(spec.Name.Pos()))
				}
				addImport(f0, path)
			}
			decls := f.Decls[:0]
			for _, d := range f.Decls {
				if d, ok := d.(*ast.GenDecl); ok && d.Tok == token.IMPORT {
					continue
				}
				decls = append(decls, d)
			}
			f.Decls = decls
		}
	}
	var buf bytes.Buffer
	for _, f := range files {
		err := printer.Fprint(&buf, fset, f)
		if err != nil {
			return nil, newErrorf("Fprint to buffer failed: %s", err.Error())
		}
	}

	data := bytes.Replace(buf.Bytes(), []byte("\npackage PACKAGE-DELETE-ME\n"), []byte("\n"), -1)

	// comments will not be parsed into this AST
	f, err := parser.ParseFile(fsetOut, bundleName, data, 0)

	return f, err
}
