module L = Llvm
module A = Ast
open Sast

module StringMap = Map.Make(String)

(* translate : Sast.program -> Llvm.module *)
let translate (globals, functions) =
  let context    = L.global_context () in

  (* Create the LLVM compilation module into which
     we will generate code *)
  let the_module = L.create_module context "TaP" in

  (* Get types from the context *)
  let double_t   = L.double_type context
  and i32_t      = L.i32_type    context
  and i8_t       = L.i8_type     context
  and i1_t       = L.i1_type     context 
  and string_t   = L.pointer_type (L.i8_type   context)
  and null_t     = L.pointer_type (L.i8_type   context) in

  (* Return the LLVM type for a TaP type *)
  let rec ltype_of_typ = function
      A.Int   -> i32_t
    | A.Bool  -> i1_t
    | A.Num  -> double_t
    | A.String -> string_t
    | A.List (t) -> L.pointer_type (ltype_of_typ t)  
    | A.Null -> null_t
  in

  (* Create a map of global variables after creating each *)
  let global_vars : L.llvalue StringMap.t =
    let global_var m (t, n) =
      let init = L.const_int (ltype_of_typ t) 0
      in StringMap.add n (L.define_global n init the_module) m in
    List.fold_left global_var StringMap.empty globals in

  (* External built ins *)
  let printf_t : L.lltype =
    L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
  let printf_func : L.llvalue =
    L.declare_function "printf" printf_t the_module in
  let strlen_func = 
    L.declare_function "strlen"
      (L.function_type i32_t [| string_t |]) the_module in

  (* Define each function (arguments and return type) so we can
     call it even before we've created its body *)
  let function_decls : (L.llvalue * sfunc_def) StringMap.t =
    let function_decl m fdecl =
      let name = fdecl.sfname
      and formal_types =
        Array.of_list (List.map (fun (t,_) -> ltype_of_typ t) fdecl.sformals)
      in let ftype = L.function_type (ltype_of_typ fdecl.srtyp) formal_types in
      StringMap.add name (L.define_function name ftype the_module, fdecl) m in
    List.fold_left function_decl StringMap.empty functions in

  (* Fill in the body of the given function *)
  let build_function_body fdecl =
    let (the_function, _) = StringMap.find fdecl.sfname function_decls in
    let builder = L.builder_at_end context (L.entry_block the_function) in

    let int_format_str = L.build_global_stringptr "%d\n" "fmt" builder in
    let string_format_str = L.build_global_stringptr "%s\n" "fmt" builder in

    (* Construct the function's "locals": formal arguments and locally
       declared variables.  Allocate each on the stack, initialize their
       value, if appropriate, and remember their values in the "locals" map *)
    let local_vars =
      let add_formal m (t, n) p =
        L.set_value_name n p;
        let local = L.build_alloca (ltype_of_typ t) n builder in
        ignore (L.build_store p local builder);
        StringMap.add n local m

      (* Allocate space for any locally declared variables and add the
       * resulting registers to our map *)
      and add_local m (t, n) =
        let local_var = L.build_alloca (ltype_of_typ t) n builder
        in StringMap.add n local_var m
      in

      let formals = List.fold_left2 add_formal StringMap.empty fdecl.sformals
          (Array.to_list (L.params the_function)) in
      List.fold_left add_local formals fdecl.slocals
    in

    (* Return the value for a variable or formal argument.
       Check local names first, then global names *)
    let lookup n = try StringMap.find n local_vars
      with Not_found -> StringMap.find n global_vars
    in

    (* Construct code for an expression; return its value *)
    let rec build_expr builder ((_, e) : sexpr) = match e with
        SIntLit i  -> L.const_int i32_t i
      | SNumLit f  -> L.const_float double_t f
      (* Builder expression for strings *)
      | SStringLit s -> L.build_global_stringptr s "str" builder
      | SBoolLit b  -> L.const_int i1_t (if b then 1 else 0)
      | SId s       -> L.build_load (lookup s) s builder
      | SBinop (e1, op, e2) ->
        let e1' = build_expr builder e1
        and e2' = build_expr builder e2 in
        (match op with
           A.Add     -> L.build_add
         | A.Sub     -> L.build_sub
         | A.Mult    -> L.build_mul
         | A.Div     -> L.build_sdiv
         | A.Mod     -> L.build_srem
         | A.And     -> L.build_and
         | A.Or      -> L.build_or
         | A.Equal   -> L.build_icmp L.Icmp.Eq
         | A.Neq     -> L.build_icmp L.Icmp.Ne
         | A.Greater -> L.build_icmp L.Icmp.Sgt
         | A.Geq     -> L.build_icmp L.Icmp.Sge
         | A.Leq     -> L.build_icmp L.Icmp.Sle
         | A.Less    -> L.build_icmp L.Icmp.Slt
        ) e1' e2' "tmp" builder
      (* TODO: Call to a print function, come back to later *)
      | SCall ("printint", [e]) ->
        L.build_call printf_func [| int_format_str ; (build_expr builder e) |]
          "printf_int " builder
      | SCall ("printstring", [e]) ->
        L.build_call printf_func [| string_format_str ; (build_expr builder e) |]
          "printf_str" builder
      | SCall ("strlen", [e]) ->
        let value = build_expr builder e in
        L.build_call strlen_func [| value |] "strlen" builder
      | SCall (f, args) ->
        let (fdef, fdecl) = StringMap.find f function_decls in
        let llargs = List.rev (List.map (build_expr builder) (List.rev args)) in
        let result = f ^ "_result" in
        L.build_call fdef (Array.of_list llargs) result builder
      (* Builder for list expression *)
      | SListLit elems ->
        let ll_elems = List.map (build_expr builder) elems in
          let arr = L.const_array i32_t (Array.of_list ll_elems) in
            let global = L.define_global "listlit" arr the_module in
            L.build_in_bounds_gep global [| L.const_int i32_t 0; L.const_int i32_t 0 |] "list_ptr" builder
      (* For indexing strings *)
      | SAt ((A.String, _) as lst_expr, index_expr) ->
        let str_val = build_expr builder lst_expr in
        let idx_val = build_expr builder index_expr in
        let gep = L.build_in_bounds_gep str_val [| idx_val |] "str_index" builder in
        let char_val = L.build_load gep "char" builder in

        (* Allocate space for a 2-character string: 1 char + null terminator *)
        let char_str = L.build_array_alloca i8_t (L.const_int i32_t 2) "char_str" builder in
        let c0 = L.build_in_bounds_gep char_str [| L.const_int i32_t 0 |] "c0" builder in
        ignore (L.build_store char_val c0 builder);
        let c1 = L.build_in_bounds_gep char_str [| L.const_int i32_t 1 |] "c1" builder in
        ignore (L.build_store (L.const_int i8_t 0) c1 builder);

        char_str
      (* For indexing lists *)
      | SAt (lst_expr, index_expr) ->
        let lst_val = build_expr builder lst_expr in  (* i32* *)
        let idx_val = build_expr builder index_expr in  (* i32 *)
        let gep = L.build_in_bounds_gep lst_val [| idx_val |] "at" builder in
        L.build_load gep "load_elem" builder
      | SAs (sexp, t) -> failwith "Type casting not currently supported"
      | SContains (sexp1, sexp2) -> failwith "Contains not implemented"

    in

    (* LLVM insists each basic block end with exactly one "terminator"
       instruction that transfers control.  This function runs "instr builder"
       if the current block does not already have a terminator.  Used,
       e.g., to handle the "fall off the end of the function" case. *)
    let add_terminal builder instr =
      match L.block_terminator (L.insertion_block builder) with
        Some _ -> ()
      | None -> ignore (instr builder) in

    (* Build the code for the given statement; return the builder for
       the statement's successor (i.e., the next instruction will be built
       after the one generated by this call) *)
    let rec build_stmt builder = function
        SBlock sl -> List.fold_left build_stmt builder sl
      | SExpr e -> ignore(build_expr builder e); builder
      | SReturn e -> ignore(L.build_ret (build_expr builder e) builder); builder
      | SIf (predicate, then_stmt) ->
        let bool_val = build_expr builder predicate in

        let then_bb = L.append_block context "then" the_function in
        (* ignore (build_stmt (L.builder_at_end context then_bb) then_stmt); *)
        let end_bb = L.append_block context "if_end" the_function in
        
         (* Create builder for "then" block and generate code *)
        let then_builder = build_stmt (L.builder_at_end context then_bb) then_stmt in
        add_terminal then_builder (L.build_br end_bb);

        (* Build the conditional branch *)
        ignore (L.build_cond_br bool_val then_bb end_bb builder);

    (* Continue at the end block *)
    L.builder_at_end context end_bb
      | SWhile (predicate, body) ->
        let while_bb = L.append_block context "while" the_function in
        let build_br_while = L.build_br while_bb in (* partial function *)
        ignore (build_br_while builder);
        let while_builder = L.builder_at_end context while_bb in
        let bool_val = build_expr while_builder predicate in

        let body_bb = L.append_block context "while_body" the_function in
        add_terminal (build_stmt (L.builder_at_end context body_bb) body) build_br_while;

        let end_bb = L.append_block context "while_end" the_function in

        ignore(L.build_cond_br bool_val body_bb end_bb while_builder);
        L.builder_at_end context end_bb
      (* For loop implementation *)
      | SFor (var, start_expr, end_expr, body) ->
        (* Allocate and initialize loop variable *)
        let var_alloca = L.build_alloca i32_t var builder in
        let start_val = build_expr builder start_expr in
        ignore (L.build_store start_val var_alloca builder);

        (* Create the loop blocks *)
        let loop_cond_bb = L.append_block context "for_cond" the_function in
        ignore (L.build_br loop_cond_bb builder);  (* jump into condition check *)
        let cond_builder = L.builder_at_end context loop_cond_bb in

        (* Load current loop var and evaluate condition *)
        let curr_val = L.build_load var_alloca var cond_builder in
        let end_val = build_expr cond_builder end_expr in
        let cond = L.build_icmp L.Icmp.Slt curr_val end_val "for_cond" cond_builder in

        (* Create blocks for loop body and after loop *)
        let loop_body_bb = L.append_block context "for_body" the_function in
        let after_loop_bb = L.append_block context "for_end" the_function in
        ignore (L.build_cond_br cond loop_body_bb after_loop_bb cond_builder);

        (* Inside loop body *)
        let body_builder = L.builder_at_end context loop_body_bb in

        (* Manually assign current loop variable value *)
        let loop_var_val = L.build_load var_alloca var body_builder in
        ignore (L.build_store loop_var_val (lookup var) body_builder);

        (* Now build the body *)
        let body_builder = build_stmt body_builder body in

        (* Increment loop variable *)
        let curr_val' = L.build_load var_alloca var body_builder in
        let next_val = L.build_add curr_val' (L.const_int i32_t 1) "for_inc" body_builder in
        ignore (L.build_store next_val var_alloca body_builder);

        (* Jump back to loop condition *)
        ignore (L.build_br loop_cond_bb body_builder);

        (* Continue building after loop *)
        L.builder_at_end context after_loop_bb
      | SAssign (s, e) -> let e' = build_expr builder e in
        ignore (L.build_store e' (lookup s) builder);
        builder
      | SContinue -> failwith "Continue not currently supported"
      | SBreak -> failwith "Break not implemented"

    in
    (* Build the code for each statement in the function *)
    let func_builder = build_stmt builder (SBlock fdecl.sbody) in

    (* Add a return if the last block falls off the end *)
    add_terminal func_builder (L.build_ret (L.const_int i32_t 0))

  in

  List.iter build_function_body functions;
  the_module
