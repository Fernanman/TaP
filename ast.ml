(* Binary operators *)
type bop = Add | Sub | Mult | Div | Equal | Neq | Less | Greater | Geq | Leq | And | Or | Mod

type typ = Num | Bool | Map | Set | List

type expr =
  | IntLit of int
  | BoolLit of bool
  | NumLit of float
  | StringLit of string
  | Id of string
  | As of expr * typ
  | At of expr * expr
  | Binop of expr * bop * expr
  | Assign of string * expr
  | Map
  | Set
  | List
  | Call of string * expr list

type stmt =
  | Block of stmt list
  | Expr of expr
  | If of expr * stmt
  | While of expr * stmt
  | For of string * int * int * stmt
  | Break
  | Continue
  | Free of string
  (* | FunDef of string * (typ * string) list * stmt list *)

type bind = typ * string

type program = {
  locals: bind list;
  body: stmt list;
}

(* Pretty-printing functions *)
let string_of_op = function
    Add -> "plus"
  | Sub -> "minus"
  | Mult -> "mult"
  | Equal -> "equals"
  | Neq -> "not equals"
  | Less -> "less than"
  | And -> "and"
  | Or -> "or"
  | Div -> "divide"
  | Greater -> "greater than"
  | Geq -> "greater equals"
  | Leq -> "less equals"

let string_of_typ = function
    Num -> "num"
  | Bool -> "bool"
  | Map -> "map"
  | Set -> "set"
  | List -> "list"

let rec string_of_expr = function
    IntLit(l) -> string_of_int l
  | NumLit(l) -> string_of_float l
  | BoolLit(true) -> "true"
  | BoolLit(false) -> "false"
  | Id(s) -> s
  | StringLit(s) -> "'" ^ s ^ "'"
  | Binop(e1, o, e2) ->
    string_of_expr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_expr e2
  | Assign(v, e) -> v ^ " is " ^ string_of_expr e
  | As(e1, etype) -> string_of_expr e1 ^ " as " ^ string_of_typ etype
  | At(id, pos) -> string_of_expr id ^ " at " ^ string_of_expr pos
  (* | Call(id, params) -> id ^ "(" ^ params ^ ")"  *)

  (* Needs work *)
let rec string_of_stmt = function
    Block(stmts) ->
    String.concat "" (List.map string_of_stmt stmts)
  | Expr(expr) -> string_of_expr expr ^ "\n";
  | If(e, s) ->  "if " ^ string_of_expr e ^ "\n" ^
                      string_of_stmt s ^ "end if\n"
  | While(e, s) -> "while " ^ string_of_expr e ^ "\n" ^ string_of_stmt s ^ "end while\n"
  | For(id, from_id, to_id, s) -> "for " ^ id ^ " in " ^ string_of_int from_id ^ " to " ^ string_of_int to_id ^ "\n" ^ string_of_stmt s ^ "end for\n"
  | Break -> "break"
  | Continue -> "continue"
  | Free(id) -> "free " ^ "id"

let string_of_program fdecl =
  "\n\nParsed program: \n\n" ^
  String.concat "" (List.map string_of_stmt fdecl.body) ^
  "\n"
