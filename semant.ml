(* Semantic checking for the TaP compiler *)

open Ast
open Sast

module StringMap = Map.Make(String)

(* Semantic checking of the AST. Returns an SAST if successful,
   throws an exception if something is wrong.

   Check each global variable, then check each function *)

let check (globals, functions) =
  
  (* Verify a list of bindings has no duplicate names *)
  let check_binds (kind : string) (binds : (typ * string) list) =
    let rec dups = function
        [] -> ()
      |	((_,n1) :: (_,n2) :: _) when n1 = n2 ->
        raise (Failure ("duplicate " ^ kind ^ " " ^ n1))
      | _ :: t -> dups t
    in dups (List.sort (fun (_,a) (_,b) -> compare a b) binds)
  in

  (* Make sure no globals duplicate *)
  check_binds "global" globals;

  (* Collect function declarations for built-in functions: no bodies *)
  let built_in_decls =
    StringMap.empty
    |> StringMap.add "printint" {
      rtyp = Int;
      fname = "printint";
      formals = [(Int, "x")];
      locals = []; body = [] }
    |> StringMap.add "printstring" {
      rtyp = Int;
      fname = "printstr";
      formals = [(String, "x")];
      locals = []; body = [] }
    |> StringMap.add "printnum" {
      rtyp = Int;
      fname = "printnum";
      formals = [(Num, "x")];
      locals = []; body = [] }

  in

  (* Add function name to symbol table *)
  let add_func map fd =
    let built_in_err = "function " ^ fd.fname ^ " may not be defined"
    and dup_err = "duplicate function " ^ fd.fname
    and make_err er = raise (Failure er)
    and n = fd.fname (* Name of the function *)
    in match fd with (* No duplicate functions or redefinitions of built-ins *)
      _ when StringMap.mem n built_in_decls -> make_err built_in_err
    | _ when StringMap.mem n map -> make_err dup_err
    | _ ->  StringMap.add n fd map
  in

  (* Collect all function names into one symbol table *)
  let function_decls = List.fold_left add_func built_in_decls functions
  in

  (* Return a function from our symbol table *)
  let find_func s =
    try StringMap.find s function_decls
    with Not_found -> raise (Failure ("unrecognized function " ^ s))
  in

  let _ = find_func "main" in (* Ensure "main" is defined *)

  let rec check_func func =
    (* Make sure no formals or locals are void or duplicates *)
    check_binds "formal" func.formals;
    check_binds "local" func.locals;

    (* Raise an exception if the given rvalue type cannot be assigned to
       the given lvalue type *)
    let check_assign lvaluet rvaluet err =
      if lvaluet = rvaluet then lvaluet else raise (Failure err)
    in

    (* Build local symbol table of variables for this function *)
    let symbols = List.fold_left (fun m (ty, name) -> StringMap.add name ty m)
        StringMap.empty (globals @ func.formals @ func.locals )
    in

    (* Return a variable from our local symbol table *)
    let type_of_identifier s =
      try StringMap.find s symbols
      with Not_found -> raise (Failure ("undeclared identifier " ^ s))
    in

    let compatible from_t to_t = match (from_t, to_t) with
    | Int, Num -> true
    | Num, Int -> true
    | t1, t2 when t1 = t2 -> true
    | _ -> false
    in

    (* Return a semantically-checked expression, i.e., with a type *)
    let rec check_expr = function
        IntLit l -> (Int, SIntLit l)
      | BoolLit l -> (Bool, SBoolLit l)
      | NumLit l -> (Num, SNumLit l)
      | StringLit l -> (String, SStringLit l)

      (* List definition *)
      | ListLit l -> (match l with
        | [] -> (List Null, SListLit [])
        | hd :: _ -> let (head_ty, _) = check_expr hd in
          let lst_elems = List.map (fun e ->
            let (ty, se) = check_expr e in
            if not (ty = head_ty) then
              raise (Failure ("type mismatch in list literal: expected " ^ string_of_typ head_ty ^ ", got " ^ string_of_typ ty))
            else (ty, se)) l in
          (List head_ty, SListLit lst_elems)
      )

      | Id var -> (type_of_identifier var, SId var)

      (* As checking *)
      | As(e1, etype) -> 
          let (et, e') = check_expr e1 in
          if compatible et etype then (etype, SAs((et, e'), etype))
          else raise (Failure ("type mismatch in 'as' operation. type " ^ string_of_typ et ^ " incompatible with  type " ^ string_of_typ etype))
        
      (* At for indexing *)
      | At(e1, e2) -> 
          let (et1, e1') = check_expr e1
          and (et2, e2') = check_expr e2 in
          if not (et2 = Int) then raise (Failure ("invalid 'at' operation: index must be Int, got " ^ string_of_typ et2))
          else (match et1 with
          | List lst_t when lst_t = Null -> raise (Failure ("empty list not subscriptable"))
          | List lst_t -> (lst_t, SAt ((et1, e1'), (et2, e2')))
          | String -> (String, SAt ((et1, e1'), (et2, e2')))
          | _ -> raise (Failure ("type " ^ string_of_typ et1 ^ " not subscriptable")))
      
      (* Contains for checking if a list has an element *)
      | Contains (elem_expr, list_expr) ->
        let (elem_ty, se_elem) = check_expr elem_expr in
        let (list_ty, se_list) = check_expr list_expr in
        (match list_ty with
        | List t -> (Bool, SContains ((elem_ty, se_elem), (list_ty, se_list)))
        | _ -> raise (Failure ("invalid 'in' operation: expected type list, got " ^ string_of_typ list_ty)))

      (* Adding binary operations *)
      | Binop(e1, op, e2) as e ->
        let (t1, e1') = check_expr e1
        and (t2, e2') = check_expr e2 in
        let err = "illegal binary operator " ^
                  string_of_typ t1 ^ " " ^ string_of_op op ^ " " ^
                  string_of_typ t2 ^ " in " ^ string_of_expr e
        in
        if compatible t1 t2 then
          let t = match op with
              Add | Sub | Mult | Div | Mod when (t1 = Int && t2 = Int) -> Int
            | Add | Sub | Mult | Div when (t1 = Num || t2 = Num) -> Num
            | Add when (t1 = String && t2 = String) -> String
            | Equal | Neq -> Bool
            | Less | Greater | Geq | Leq when (t1 = Int || t1 = Num) -> Bool
            | And | Or when t1 = Bool -> Bool
            | _ -> raise (Failure err)
          in
          (t, SBinop((t1, e1'), op, (t2, e2')))
        else raise (Failure err)

      | Call(fname, args) as call ->
        let fd = find_func fname in
        let param_length = List.length fd.formals in
        if List.length args != param_length then
          raise (Failure ("expecting " ^ string_of_int param_length ^
                          " arguments in " ^ string_of_expr call))
        else let check_call (ft, _) e =
               let (et, e') = check_expr e in
               let err = "illegal argument found " ^ string_of_typ et ^
                         " expected " ^ string_of_typ ft ^ " in " ^ string_of_expr e
               in (check_assign ft et err, e')
          in
          let args' = List.map2 check_call fd.formals args
          in (fd.rtyp, SCall(fname, args'))
    in

    let check_bool_expr e =
      let (t, e') = check_expr e in
      match t with
      | Bool -> (t, e')
      |  _ -> raise (Failure ("expected Boolean expression in " ^ string_of_expr e ^ ", got " ^ string_of_typ t))
    in

    let rec check_stmt_list = function
        [] -> []
      | Block sl :: sl'  -> check_stmt_list (sl @ sl') (* Flatten blocks *)
      | s :: sl -> check_stmt s :: check_stmt_list sl
    (* Return a semantically-checked statement i.e. containing sexprs *)
    and check_stmt = function
      (* A block is correct if each statement is correct and nothing
         follows any Return statement.  Nested blocks are flattened. *)
        Block sl -> SBlock (check_stmt_list sl)
      | Expr e -> SExpr (check_expr e)
      | If(e, st) -> SIf(check_bool_expr e, check_stmt st)
      | While(e, st) -> SWhile(check_bool_expr e, check_stmt st)

      | Assign(var, e) ->
        let var_typ = type_of_identifier var in
        let (expr_typ, se) = check_expr e in
        let _ = check_assign var_typ expr_typ
          ("illegal assignment: " ^ string_of_typ expr_typ ^ " -> " ^ string_of_typ var_typ)
        in
        SAssign(var, (expr_typ, se))

      | For(id, start_e, end_e, st) ->
        let (start_ty, start_e') = check_expr start_e in
        let (end_ty, end_e') = check_expr end_e in
        if (not (start_ty = Int)) || (not (end_ty = Int)) then
          raise (Failure ("for loop bounds must be integers, got " ^ string_of_typ start_ty ^ " and " ^ string_of_typ end_ty))
        else
          SFor(id, (start_ty, start_e'), (end_ty, end_e'), check_stmt st)
      (* For loop step ignored for now*)

      | Break -> SBreak
      | Continue -> SContinue
      | Return e ->
        let (t, e') = check_expr e in
        if t = func.rtyp then SReturn (t, e')
        else raise (
            Failure ("return gives " ^ string_of_typ t ^ " expected " ^
                     string_of_typ func.rtyp ^ " in " ^ string_of_expr e))
    in (* body of check_func *)
    { srtyp = func.rtyp;
      sfname = func.fname;
      sformals = func.formals;
      slocals  = func.locals;
      sbody = check_stmt_list func.body
    }
  in
  (globals, List.map check_func functions)