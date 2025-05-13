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

(* Pretty-printing functions *)
let rec string_of_sexpr (t, e) =
  "(" ^ string_of_typ t ^ " : " ^ (match e with
    | SIntLit(l) -> string_of_int l
    | SNumLit(l) -> string_of_float l
    | SBoolLit(true) -> "true"
    | SBoolLit(false) -> "false"
    | SStringLit(s) -> "'" ^ s ^ "'"
    | SId(s) -> s
    | SBinop(e1, o, e2) ->
        string_of_sexpr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_sexpr e2
    | SAssign(v, e) -> v ^ " = " ^ string_of_sexpr e
    | SAs(e1, etype) -> string_of_sexpr e1 ^ " as " ^ string_of_typ etype
    | SAt(id, pos) -> string_of_sexpr id ^ " at " ^ string_of_sexpr pos
    | SCall(f, el) ->
        f ^ "(" ^ String.concat ", " (List.map string_of_sexpr el) ^ ")"
    | SMap -> "map"
    | SSet -> "set"
    | SList -> "list"
  ) ^ ")"

let rec string_of_sstmt = function
    SBlock(stmts) ->
        "{\n" ^ String.concat "" (List.map string_of_sstmt stmts) ^ "}\n"
  | SExpr(expr) -> string_of_sexpr expr ^ ";\n"
  | SIf(e, s) ->  "if " ^ string_of_sexpr e ^ "\n" ^
                     string_of_sstmt s ^ "end if\n"
  | SWhile(e, s) -> "while " ^ string_of_sexpr e ^ "\n" ^ string_of_sstmt s ^ "end while\n"
  | SFor(id, from_id, to_id, s) -> "for " ^ id ^ " in " ^ string_of_int from_id ^ " to " ^ string_of_int to_id ^ "\n" ^ string_of_sstmt s ^ "end for\n"
  | SBreak -> "break;\n"
  | SContinue -> "continue;\n"
  | SFree(id) -> "free " ^ id ^ ";\n"
  | SReturn e -> "return " ^ string_of_sexpr e ^ ";\n"

let string_of_sfdecl fdecl =
  string_of_typ fdecl.srtyp ^ " " ^
  fdecl.sfname ^ "(" ^ String.concat ", " (List.map snd fdecl.sformals) ^
  ")\n{\n" ^
  String.concat "" (List.map string_of_vdecl fdecl.slocals) ^
  String.concat "" (List.map string_of_sstmt fdecl.sbody) ^
  "}\n"

let string_of_sprogram (vars, funcs) =
  "\n\nSementically checked program: \n\n" ^
  String.concat "" (List.map string_of_vdecl vars) ^ "\n" ^
  String.concat "\n" (List.map string_of_sfdecl funcs)