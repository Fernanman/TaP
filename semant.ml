open Ast
open Sast

exception Error of string

let err msg = raise (Error msg)

module StringMap = Map.Make(String)

(* Built-in function declarations *)
let built_in_decls =
  StringMap.empty
  |> StringMap.add "printint" {
      rtyp = Int;
      fname = "printint";
      formals = [(Int, "x")];
      body = [] }
  |> StringMap.add "printstring" {
      rtyp = Int;
      fname = "printstr";
      formals = [(String, "x")];
      body = [] }

(* Type checking expressions and returning annotated ones *)
let rec check_expr (env : typ StringMap.t) (funcs : func_def StringMap.t) : expr -> sexpr = function
  | IntLit l -> (Int, SIntLit l)
  | BoolLit l -> (Bool, SBoolLit l)
  | NumLit l -> (Num, SNumLit l)
  | StringLit l -> (String, SStringLit l)
  | Id s ->
    let t =
      try StringMap.find s env
      with Not_found -> err ("undeclared identifier " ^ s)
    in (t, SId s)
  | As (e, t) ->
    let (_, se) = check_expr env funcs e in
    (t, SAs ((t, se), t))
  | At (e1, e2) ->
    let (t1, se1) = check_expr env funcs e1 in
    let (t2, se2) = check_expr env funcs e2 in
    (match t1, t2 with
      | List t, Int -> (t, SAt ((t1, se1), (t2, se2)))
      | _ -> err "invalid indexing")
  | Binop (e1, op, e2) ->
    let (t1, se1) = check_expr env funcs e1 in
    let (t2, se2) = check_expr env funcs e2 in
    let result_type = match op with
      | Add | Sub | Mult | Div | Mod ->
        if t1 = Int && t2 = Int then Int
        else if t1 = Num && t2 = Num then Num
        else err "invalid arithmetic operation"
      | Equal | Neq | Less | Greater | Leq | Geq ->
        if t1 = t2 then Bool else err "comparison on mismatched types"
      | And | Or ->
        if t1 = Bool && t2 = Bool then Bool else err "logical op on non-bools"
    in
    (result_type, SBinop ((t1, se1), op, (t2, se2)))
  | ListLit es ->
    (match es with
      | [] -> (List Null, SListLit [])
      | hd::_ ->
        let (ht, _) = check_expr env funcs hd in
        let ses = List.map (check_expr env funcs) es in
        (List ht, SListLit ses))
  | Call (fname, args) ->
    let fd =
      try StringMap.find fname funcs
      with Not_found -> err ("function " ^ fname ^ " not declared")
    in
    let params = fd.formals in
    if List.length params != List.length args then
      err ("incorrect number of arguments to " ^ fname)
    else
      let sargs = List.map (check_expr env funcs) args in
      List.iter2
        (fun (pt, _) (at, _) ->
          if pt <> at then err ("argument type mismatch in call to " ^ fname))
        params sargs;
      (fd.rtyp, SCall (fname, sargs))
  | Contains (e1, e2) ->
    let (t1, se1) = check_expr env funcs e1 in
    let (t2, se2) = check_expr env funcs e2 in
    (match t2 with
      | List t when t = t1 -> (Bool, SContains ((t1, se1), (t2, se2)))
      | _ -> err "invalid 'in' expression")

(* Type check a statement list and return SAST *)
let rec check_stmt_list env funcs ret_type = function
  | [] -> (env, [])
  | hd :: tl ->
    let (env', sstmt1) = check_stmt env funcs ret_type hd in
    let (env'', sstmts) = check_stmt_list env' funcs ret_type tl in
    (env'', sstmt1 :: sstmts)

and check_stmt env funcs ret_type = function
  | Block sl ->
    let (_, sstmts) = check_stmt_list env funcs ret_type sl in
    (env, SBlock sstmts)
  | VDecl (t, name) ->
    if StringMap.mem name env then err ("duplicate variable " ^ name)
    else
      let env' = StringMap.add name t env in
      (env', SVDecl (t, name))
  | FDecl f ->
    let local_env = List.fold_left (fun e (t, n) -> StringMap.add n t e) env f.formals in
    let (_, sbody) = check_stmt_list local_env funcs f.rtyp f.body in
    let sf = {
      srtyp = f.rtyp;
      sfname = f.fname;
      sformals = f.formals;
      sbody = sbody;
    } in
    (env, SFDecl sf)
  | Expr e ->
    let se = check_expr env funcs e in
    (env, SExpr se)
  | If (cond, s) ->
    let (t, se) = check_expr env funcs cond in
    if t <> Bool then err "if condition must be boolean";
    let (_, ss) = check_stmt env funcs ret_type s in
    (env, SIf ((t, se), ss))
  | While (cond, s) ->
    let (t, se) = check_expr env funcs cond in
    if t <> Bool then err "while condition must be boolean";
    let (_, ss) = check_stmt env funcs ret_type s in
    (env, SWhile ((t, se), ss))
  | For (id, e1, e2, s) ->
    let (t1, se1) = check_expr env funcs e1 in
    let (t2, se2) = check_expr env funcs e2 in
    if t1 <> Int || t2 <> Int then err "for loop range must be int";
    let env' = StringMap.add id Int env in
    let (_, ss) = check_stmt env' funcs ret_type s in
    (env, SFor (id, (t1, se1), (t2, se2), ss))
  | Break -> (env, SBreak)
  | Continue -> (env, SContinue)
  | Return e ->
    let (rt, se) = check_expr env funcs e in
    if rt <> ret_type then err "return type mismatch";
    (env, SReturn (rt, se))
  | Assign (name, e) ->
    let (et, se) = check_expr env funcs e in
    let vt =
      try StringMap.find name env
      with Not_found -> err ("undeclared variable " ^ name)
    in
    if et <> vt then err ("assignment type mismatch");
    (env, SAssign (name, (et, se)))

(* Collect function declarations first and merge built-ins *)
let collect_funcs stmts =
  let add_func map = function
    | FDecl f ->
      if StringMap.mem f.fname map then err ("duplicate function " ^ f.fname)
      else StringMap.add f.fname f map
    | _ -> map
  in
  (* Start with built-in functions *)
  let func_map = built_in_decls in
  List.fold_left add_func func_map stmts

(* Entry point: check a program and return SAST *)
let check (stmts : stmt list) : sstmt list =
  let func_decls = collect_funcs stmts in
  let (_, sstmts) = check_stmt_list StringMap.empty func_decls Null stmts in
  sstmts
