open Ast
open Sast

exception Error of string
let err msg = raise (Error msg)

module StringMap = Map.Make(String)

(* Scoped environment is a stack of maps *)
type 'a scoped_env = 'a StringMap.t list

(* Start with a single empty scope *)
let empty_env : 'a scoped_env = [StringMap.empty]

(* Push a new scope *)
let env_push (env : 'a scoped_env) : 'a scoped_env = StringMap.empty :: env

(* Pop the top scope *)
let env_pop (env : 'a scoped_env) : 'a scoped_env =
  match env with
  | [] -> failwith "No scope to pop"
  | _ :: rest -> rest

(* Lookup key in scoped env, searching from top scope downward *)
let rec env_find (env : 'a scoped_env) (key : string) : 'a option =
  match env with
  | [] -> 
    prerr_endline ("Variable "^ key ^" not found in any scope");
    None
  | scope :: rest ->
    (match StringMap.find_opt key scope with
    | Some v -> Some v
    | None -> env_find rest key)

(* Add binding to top scope *)
let env_add (env : 'a scoped_env) (key : string) (value : 'a) : 'a scoped_env =
  match env with
  | [] -> failwith "No scope to add to"
  | scope :: rest -> (StringMap.add key value scope) :: rest

(* Built-in functions inserted in the global (top) scope *)
let built_in_decls : func_def scoped_env =
  let env1 = env_add empty_env "printint" {
    rtyp = Int;
    fname = "printint";
    formals = [(Int, "x")];
    body = [];
  } in
  let env2 = env_add env1 "printstring" {
    rtyp = Int;
    fname = "printstring";
    formals = [(String, "x")];
    body = [];
  } in
  env2

(* Type checking expressions and returning annotated ones *)
let rec check_expr (var_env : typ scoped_env) (func_env : func_def scoped_env) : expr -> sexpr = function
  | IntLit l -> (Int, SIntLit l)
  | BoolLit l -> (Bool, SBoolLit l)
  | NumLit l -> (Num, SNumLit l)
  | StringLit l -> (String, SStringLit l)
  | Id s ->
    let t =
      match env_find var_env s with
      | Some t -> t
      | None -> err ("undeclared identifier " ^ s)
    in (t, SId s)
  | As (e, t) ->
    let (_, se) = check_expr var_env func_env e in
    (t, SAs ((t, se), t))
  | At (e1, e2) ->
    let (t1, se1) = check_expr var_env func_env e1 in
    let (t2, se2) = check_expr var_env func_env e2 in
    (match t1, t2 with
      | List t, Int -> (t, SAt ((t1, se1), (t2, se2)))
      | _ -> err "invalid indexing")
  | Binop (e1, op, e2) ->
    let (t1, se1) = check_expr var_env func_env e1 in
    let (t2, se2) = check_expr var_env func_env e2 in
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
        let (ht, _) = check_expr var_env func_env hd in
        let ses = List.map (check_expr var_env func_env) es in
        (List ht, SListLit ses))
  | Call (fname, args) ->
    let fd =
      match env_find func_env fname with
      | Some f -> f
      | None -> err ("function " ^ fname ^ " not declared")
    in
    let params = fd.formals in
    if List.length params != List.length args then
      err ("incorrect number of arguments to " ^ fname)
    else
      let sargs = List.map (check_expr var_env func_env) args in
      List.iter2
        (fun (pt, _) (at, _) ->
          if pt <> at then err ("argument type mismatch in call to " ^ fname))
        params sargs;
      (fd.rtyp, SCall (fname, sargs))
  | Contains (e1, e2) ->
    let (t1, se1) = check_expr var_env func_env e1 in
    let (t2, se2) = check_expr var_env func_env e2 in
    (match t2 with
      | List t when t = t1 -> (Bool, SContains ((t1, se1), (t2, se2)))
      | _ -> err "invalid 'in' expression")

(* Type check a statement list and return SAST *)
let rec check_stmt_list
    (var_env : typ scoped_env)
    (func_env : func_def scoped_env)
    (ret_type : typ)
    (stmts : stmt list)
  : (typ scoped_env * func_def scoped_env * sstmt list) =
  match stmts with
  | [] -> (var_env, func_env, [])
  | hd :: tl ->
    let (var_env', func_env', sstmt1) = check_stmt var_env func_env ret_type hd in
    let (var_env'', func_env'', sstmts) = check_stmt_list var_env' func_env' ret_type tl in
    (var_env'', func_env'', sstmt1 :: sstmts)

and check_stmt
    (var_env : typ scoped_env)
    (func_env : func_def scoped_env)
    (ret_type : typ)
    (stmt : stmt)
  : (typ scoped_env * func_def scoped_env * sstmt) =
  match stmt with
  | Block sl ->
    let var_env' = env_push var_env in
    let func_env' = env_push func_env in
    let (var_env'', func_env'', sstmts) = check_stmt_list var_env' func_env' ret_type sl in
    let var_env_after = env_pop var_env'' in
    let func_env_after = env_pop func_env'' in
    (var_env_after, func_env_after, SBlock sstmts)

  | VDecl (t, name) ->
    (match var_env with
    | [] -> failwith "empty environment"
    | top_scope :: _ ->
      if StringMap.mem name top_scope then err ("duplicate variable " ^ name)
      else
        let var_env' = env_add var_env name t in
        (var_env', func_env, SVDecl (t, name)))

  | FDecl f ->
    (* Check duplicate function name in current function top scope *)
    let current_func_scope =
      match func_env with
      | [] -> failwith "No function env scope"
      | scope :: _ -> scope
    in
    if StringMap.mem f.fname current_func_scope then
      err ("duplicate function " ^ f.fname)
    else
      (* Add function to function env top scope *)
      let func_env' = env_add func_env f.fname f in
      (* Push new variable and function scopes for function body *)
      let var_env' = env_push var_env in
      let func_env'' = env_push func_env' in
      (* Add formals to new variable scope *)
      let var_env'' = List.fold_left (fun e (t,n) -> env_add e n t) var_env' f.formals in
      (* Check function body *)
      let (var_env_after, func_env_after, sbody) = check_stmt_list var_env'' func_env'' f.rtyp f.body in
      (* Pop scopes after function body *)
      let var_env_final = env_pop var_env_after in
      let func_env_final = env_pop func_env_after in
      let sf = {
        srtyp = f.rtyp;
        sfname = f.fname;
        sformals = f.formals;
        sbody = sbody;
      } in
      (* Return original var_env unchanged, updated func_env with new function, and SFDecl *)
      (var_env_final, func_env_final, SFDecl sf)

  | Expr e ->
    let se = check_expr var_env func_env e in
    (var_env, func_env, SExpr se)

  | If (cond, s) ->
    let (t, se) = check_expr var_env func_env cond in
    if t <> Bool then err "if condition must be boolean";
    let (var_env', func_env', ss) = check_stmt var_env func_env ret_type s in
    (var_env', func_env', SIf ((t, se), ss))

  | While (cond, s) ->
    let (t, se) = check_expr var_env func_env cond in
    if t <> Bool then err "while condition must be boolean";
    let (var_env', func_env', ss) = check_stmt var_env func_env ret_type s in
    (var_env', func_env', SWhile ((t, se), ss))

  | For (id, e1, e2, s) ->
    let (t1, se1) = check_expr var_env func_env e1 in
    let (t2, se2) = check_expr var_env func_env e2 in
    if t1 <> Int || t2 <> Int then err "for loop range must be int";
    let var_env' = env_add var_env id Int in
    let (var_env'', func_env', ss) = check_stmt var_env' func_env ret_type s in
    (var_env'', func_env', SFor (id, (t1, se1), (t2, se2), ss))

  | Break -> (var_env, func_env, SBreak)

  | Continue -> (var_env, func_env, SContinue)

  | Return e ->
    let (rt, se) = check_expr var_env func_env e in
    if rt <> ret_type then err "return type mismatch";
    (var_env, func_env, SReturn (rt, se))

  | Assign (name, e) ->
    let (et, se) = check_expr var_env func_env e in
    let vt =
      match env_find var_env name with
      | Some t -> t
      | None -> err ("undeclared variable " ^ name)
    in
    if et <> vt then err ("assignment type mismatch");
    (var_env, func_env, SAssign (name, (et, se)))

(* Entry point: check a program and return SAST *)
let check (stmts : stmt list) : sstmt list =
  (* Start with built-in functions in the global function environment *)
  let func_env = built_in_decls in
  let var_env = [StringMap.empty] in
  let (_, _, sstmts) = check_stmt_list var_env func_env Null stmts in
  sstmts
