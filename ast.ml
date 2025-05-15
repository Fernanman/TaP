(* Binary operators *)
type bop = Add | Sub | Mult | Div | Equal | Neq | Less | Greater | Geq | Leq | And | Or | Mod

type typ = Int | Num | Bool | List | String

type expr =
  | IntLit of int
  | BoolLit of bool
  | NumLit of float
  | StringLit of string
  | Id of string
  | As of expr * typ
  | At of expr * expr
  | Binop of expr * bop * expr
  | ListLit of expr list
  | Assign of string * expr
  | Call of string * expr list
  | Contains of expr * expr

type stmt =
  | Block of stmt list
  | Expr of expr
  | If of expr * stmt
  | While of expr * stmt
  | For of string * int * int * stmt
  | Break
  | Continue
  | Free of string
  | Return of expr

type bind = typ * string

(* func_def: ret_typ fname formals locals body *)
type func_def = {
  rtyp: typ;
  fname: string;
  formals: bind list;
  locals: bind list;
  body: stmt list;
}

type program = bind list * func_def list