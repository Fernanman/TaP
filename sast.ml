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
  | SListLit of sexpr list
  | SCall of string * sexpr list
  | SContains of sexpr * sexpr

type sstmt =
    SBlock of sstmt list
  | SVDecl of typ * string
  | SFDecl of sfunc_def
  | SExpr of sexpr
  | SIf of sexpr * sstmt * sstmt option
  | SWhile of sexpr * sstmt
  | SFor of string * sexpr * sexpr * sstmt
  | SBreak
  | SContinue
  | SReturn of sexpr
  | SAssign of string * sexpr
  | SAssignAt of sexpr * sexpr * sexpr

and sfunc_def = {
  srtyp: typ;
  sfname: string;
  sformals: bind list;
  sbody: sstmt list;
}

type sprogram = bind list * sfunc_def list