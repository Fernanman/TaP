(* Semantically-checked Abstract Syntax Tree and functions for printing it *)

open Ast

type sexpr = typ * sx
and sx =
    SIntLit of int
  | SNumLit of float
  | SBoolLit of bool
  | SStringLit of string
  | SId of string
  | SAs of sexpr * typ
  | SAt of sexpr * sexpr
  | SBinop of sexpr * bop * sexpr
  | SListLit of typ * sexpr list
  | SAssign of string * sexpr
  | SCall of string * sexpr list
  | SContains of sexpr * sexpr

type sstmt =
    SBlock of sstmt list
  | SExpr of sexpr
  | SIf of sexpr * sstmt
  | SWhile of sexpr * sstmt
  | SFor of string * int * int * sstmt
  | SBreak
  | SContinue
  | SReturn of sexpr

type sfunc_def = {
  srtyp: typ;
  sfname: string;
  sformals: bind list;
  slocals: bind list;
  sbody: sstmt list;
}

type sprogram = bind list * sstmt list