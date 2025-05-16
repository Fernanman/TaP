module L = Llvm
module A = Ast
open Sast

module StringMap = Map.Make(String)

(* translate : Sast.program -> Llvm.module *)
let translate (globals, functions) =
  let context    = L.global_context () in

  (* Create the LLVM compilation module into which we will generate code *)
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

  (* TODO: Come back and change this to print and support int nums and strings *)
  let printf_t : L.lltype =
    L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
  let printf_func : L.llvalue =
    L.declare_function "printf" printf_t the_module in

  (* Define each function (arguments and return type) so we can call it even before we've created its body *)
  let function_decls : (L.llvalue * sfunc_def) StringMap.t =
    let function_decl m fdecl =
      let name = fdecl.sfname in
      let formal_types =
        Array.of_list (List.map (fun (t, _) -> ltype_of_typ t) fdecl.sformals)
      in
      let ftype = L.function_type (ltype_of_typ fdecl.srtyp) formal_types in
      StringMap.add name (L.define_function name ftype the_module, fdecl) m
    in
    List.fold_left function_decl StringMap.empty functions
  in

  (* Fill in the body of the given function *)
  let build_function_body fdecl =
    let (the_function, _) = StringMap.find fdecl.sfname function_decls in
    let builder = L.builder_at_end context (L.entry_block the_function) in

    let int_format_str = L.build_global_stringptr "%d\n" "fmt" builder in
    let string_format_str = L.build_global_stringptr "%s\n" "fmt" builder in

    let local_vars = Hashtbl.create 10 in

    let add_local name llval = Hashtbl.add local_vars name llval in
    let lookup name =
      try Hashtbl.find local_vars name
      with Not_found -> StringMap.find name global_vars
    in

    List.iter2 (fun (t, n) p ->
      L.set_value_name n p;
      let alloca = L.build_alloca (ltype_of_typ t) n builder in
      ignore (L.build_store p alloca builder);
      add_local n alloca
    ) fdecl.sformals (Array.to_list (L.params the_function));

    let rec build_expr builder ((_, e) : sexpr) = match e with
      | SIntLit i  -> L.const_int i32_t i
      | SNumLit f  -> L.const_float double_t f
      | SStringLit s -> L.build_global_stringptr s "str" builder
      | SBoolLit b  -> L.const_int i1_t (if b then 1 else 0)
      | SId s       -> L.build_load (lookup s) s builder
      | SBinop (e1, op, e2) ->
        let e1' = build_expr builder e1 in
        let e2' = build_expr builder e2 in
        (match op with
          | A.Add -> L.build_add
          | A.Sub -> L.build_sub
          | A.Mult -> L.build_mul
          | A.Div -> L.build_sdiv
          | A.Mod -> L.build_srem
          | A.And -> L.build_and
          | A.Or -> L.build_or
          | A.Equal -> L.build_icmp L.Icmp.Eq
          | A.Neq -> L.build_icmp L.Icmp.Ne
          | A.Greater -> L.build_icmp L.Icmp.Sgt
          | A.Geq -> L.build_icmp L.Icmp.Sge
          | A.Leq -> L.build_icmp L.Icmp.Sle
          | A.Less -> L.build_icmp L.Icmp.Slt
        ) e1' e2' "tmp" builder
      | SCall ("printint", [e]) ->
        L.build_call printf_func [| int_format_str; build_expr builder e |] "printf" builder
      | SCall ("printstring", [e]) ->
        L.build_call printf_func [| string_format_str; build_expr builder e |] "printf" builder
      | SCall (f, args) ->
        let (fdef, _) = StringMap.find f function_decls in
        let llargs = List.map (build_expr builder) args |> Array.of_list in
        L.build_call fdef llargs (f ^ "_call") builder
      | SListLit elems ->
        let ll_elems = List.map (build_expr builder) elems in
        let arr = L.const_array i32_t (Array.of_list ll_elems) in
        let global = L.define_global "listlit" arr the_module in
        L.build_in_bounds_gep global [| L.const_int i32_t 0; L.const_int i32_t 0 |] "list_ptr" builder
      | SAt ((A.String, _) as s, idx) ->
        let str = build_expr builder s in
        let i = build_expr builder idx in
        let gep = L.build_in_bounds_gep str [| i |] "idx" builder in
        let c = L.build_load gep "char" builder in
        let str_alloc = L.build_array_alloca i8_t (L.const_int i32_t 2) "char_str" builder in
        ignore (L.build_store c (L.build_gep str_alloc [| L.const_int i32_t 0 |] "" builder) builder);
        ignore (L.build_store (L.const_int i8_t 0) (L.build_gep str_alloc [| L.const_int i32_t 1 |] "" builder) builder);
        str_alloc
      | SAt (lst, idx) ->
        let l = build_expr builder lst in
        let i = build_expr builder idx in
        let gep = L.build_in_bounds_gep l [| i |] "at" builder in
        L.build_load gep "elem" builder
      | SAs (_, _) -> failwith "Type casting not supported"
      | SContains (_, _) -> failwith "Contains not implemented"
    in

    (* LLVM insists each basic block end with exactly one "terminator" instruction that transfers control.  This function runs "instr builder"
       if the current block does not already have a terminator.  Used, e.g., to handle the "fall off the end of the function" case. *)
    let add_terminal builder instr =
      match L.block_terminator (L.insertion_block builder) with
      | Some _ -> ()
      | None -> ignore (instr builder)
    in

    (* Build the code for the given statement; return the builder for
       the statement's successor (i.e., the next instruction will be built
       after the one generated by this call) *)
    let rec build_stmt builder = function
      | SBlock sl -> List.fold_left build_stmt builder sl
      | SExpr e -> ignore (build_expr builder e); builder
      | SReturn e -> ignore (L.build_ret (build_expr builder e) builder); builder
      | SAssign (s, e) ->
        let v = build_expr builder e in
        ignore (L.build_store v (lookup s) builder); builder
      | SVDecl (t, n) ->
        let alloca = L.build_alloca (ltype_of_typ t) n builder in
        add_local n alloca;
        builder
      | SIf (p, t) ->
        let cond = build_expr builder p in
        let then_bb = L.append_block context "then" the_function in
        let end_bb = L.append_block context "ifend" the_function in
        let then_builder = build_stmt (L.builder_at_end context then_bb) t in
        add_terminal then_builder (L.build_br end_bb);
        ignore (L.build_cond_br cond then_bb end_bb builder);
        L.builder_at_end context end_bb
      | SWhile (p, b) ->
        let pred_bb = L.append_block context "while" the_function in
        let body_bb = L.append_block context "while_body" the_function in
        let end_bb = L.append_block context "while_end" the_function in
        ignore (L.build_br pred_bb builder);
        let pred_builder = L.builder_at_end context pred_bb in
        let cond = build_expr pred_builder p in
        ignore (L.build_cond_br cond body_bb end_bb pred_builder);
        let body_builder = build_stmt (L.builder_at_end context body_bb) b in
        add_terminal body_builder (L.build_br pred_bb);
        L.builder_at_end context end_bb
      | SFor (v, e1, e2, b) ->
        let var_alloc = L.build_alloca i32_t v builder in
        add_local v var_alloc;
        ignore (L.build_store (build_expr builder e1) var_alloc builder);
        let pred_bb = L.append_block context "for_cond" the_function in
        let body_bb = L.append_block context "for_body" the_function in
        let end_bb = L.append_block context "for_end" the_function in
        ignore (L.build_br pred_bb builder);
        let cond_builder = L.builder_at_end context pred_bb in
        let curr = L.build_load var_alloc v cond_builder in
        let limit = build_expr cond_builder e2 in
        let cond = L.build_icmp L.Icmp.Slt curr limit "forcond" cond_builder in
        ignore (L.build_cond_br cond body_bb end_bb cond_builder);
        let body_builder = build_stmt (L.builder_at_end context body_bb) b in
        let next = L.build_add (L.build_load var_alloc v body_builder) (L.const_int i32_t 1) "next" body_builder in
        ignore (L.build_store next var_alloc body_builder);
        add_terminal body_builder (L.build_br pred_bb);
        L.builder_at_end context end_bb
      | SContinue -> failwith "Continue not implemented"
      | SBreak -> failwith "Break not implemented"
    in

    (* Build the code for each statement in the function *)
    let func_builder = build_stmt builder (SBlock fdecl.sbody) in

    (* Add a return if the last block falls off the end *)
    add_terminal func_builder (L.build_ret (L.const_int i32_t 0))

  in

  List.iter build_function_body functions;
  the_module
