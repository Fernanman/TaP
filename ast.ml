(* Binary operators *)
type bop = Add | Sub | Mult | Div | Equal | Neq | Less | Greater | Geq | Leq | And | Or | Mod

type typ = Int | Num | Bool | List of typ | String | Null

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
  | Call of string * expr list
  | Contains of expr * expr

type bind = typ * string

type stmt =
  | Block of stmt list
  | VDecl of typ * string
  | FDecl of func_def
  | Expr of expr
  | If of expr * stmt * stmt option
  | While of expr * stmt
  | For of string * expr * expr * stmt
  | Break
  | Continue
  | Return of expr
  | Assign of string * expr
  | AssignAt of expr * expr * expr

and func_def = {
  rtyp: typ;
  fname: string;
  formals: bind list;
  body: stmt list;
}

type program = stmt list

(* Pretty-printing functions *)

let string_of_op = function
    Add -> "+"
  | Sub -> "-"
  | Mult -> "*"
  | Div -> "/"
  | Equal -> "=="
  | Neq -> "!="
  | Less -> "<"
  | Greater -> ">"
  | Geq -> ">="
  | Leq -> "<="
  | And -> "&&"
  | Or -> "||"
  | Mod -> "%"

let string_of_typ = function
  | Int -> "int"
  | Num -> "num"
  | Bool -> "bool"
  | List typ -> "list"
  | String -> "string"
  | Null -> "null"


let rec string_of_expr = function
    IntLit(l) -> string_of_int l
  | BoolLit(true) -> "true"
  | BoolLit(false) -> "false"
  | NumLit(l) -> string_of_float l
  | StringLit(l) -> "\'" ^ l ^ "\'"
  | Id(s) -> s
  | As (e, t) -> "((" ^ string_of_typ t ^ " ) " ^ string_of_expr e ^ ")"
  | At (e1, e2) -> string_of_expr e1 ^ "[" ^ string_of_expr e2 ^ "]"
  | Binop(e1, o, e2) ->
    string_of_expr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_expr e2
  | Call (fname, args) -> fname ^ "(" ^ String.concat ", " (List.map string_of_expr args) ^ ")"
  | ListLit es -> "[" ^ String.concat ", " (List.map string_of_expr es) ^ "]\n"
  | Contains (e1, e2) -> string_of_expr e1 ^ " in " ^ string_of_expr e2

let rec string_of_stmt = function
  | Block(stmts) ->
    "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
  | VDecl(t, name) -> string_of_typ t ^ " " ^ name ^ ";\n"
  | FDecl(fdecl) -> 
    string_of_typ fdecl.rtyp ^ " " ^
    fdecl.fname ^ "(" ^ String.concat ", " (List.map snd fdecl.formals) ^
    ")\n{\n" ^
    String.concat "" (List.map string_of_stmt fdecl.body) ^
    "}\n"
  | Expr(expr) -> string_of_expr expr ^ ";\n";
  | If(e, s, s2) ->  "if (" ^ string_of_expr e ^ ")\n" ^
                      string_of_stmt s ^ (match s2 with | Some stmt -> " else " ^ string_of_stmt stmt | None -> "")
  | While(e, s) -> "while (" ^ string_of_expr e ^ ") " ^ string_of_stmt s
  | For(es, ei1, ei2, s) -> "for (" ^ es ^ " in " ^ string_of_expr ei1 ^ " to " ^ string_of_expr ei2 ^ ") " ^ string_of_stmt s
  | Break -> "break"
  | Continue -> "continue"
  | Return(e) -> "return" ^ string_of_expr e
  | Assign(v, e) -> v ^ " = " ^ string_of_expr e
  | AssignAt(e1, e2, e3) -> string_of_expr e1 ^ "[" ^ string_of_expr e2 ^ "] = " ^ string_of_expr e3

let string_of_program (stmts : stmt list) =
  "\n\nParsed program: \n\n" ^
  String.concat "\n" (List.map string_of_stmt stmts)
