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
  | SAssign of string * sexpr
  | SMap
  | SSet
  | SList
  | SCall of string * sexpr list

type sstmt =
    SBlock of sstmt list
  | SExpr of sexpr
  | SIf of sexpr * sstmt
  | SWhile of sexpr * sstmt
  | SFor of string * int * int * sstmt
  | SBreak
  | SContinue
  | SFree of string
  | SReturn of sexpr

type sfunc_def = {
  srtyp: typ;
  sfname: string;
  sformals: bind list;
  slocals: bind list;
  sbody: sstmt list;
}

type sprogram = bind list * sstmt list