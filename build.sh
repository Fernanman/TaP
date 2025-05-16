#!/bin/bash
set -e

ocamlc -c ast.ml
ocamlc -c sast.ml
ocamlyacc parser.mly
ocamlc -c parser.mli
ocamlc -c parser.ml
ocamllex scanner.mll
ocamlc -c scanner.ml
ocamlc -c semant.ml
ocamlc -c main.ml
ocamlc -o my_program ast.cmo sast.cmo parser.cmo scanner.cmo semant.cmo main.cmo

echo "Build successful! Run chmod +x build.sh once, and run ./my_program < program.tap to test."
