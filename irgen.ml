module L = Llvm
module A = Ast
open Sast

module StringMap = Map.Make(String)

(* translate : sstmt list -> Llvm.module *)
let translate (stmts : sstmt list) =
  let context = L.global_context () in
  let the_module = L.create_module context "TaP" in

  (* Get types from the context *)
  let double_t   = L.double_type context
  and i32_t      = L.i32_type    context
  and i8_t       = L.i8_type     context
  and i1_t       = L.i1_type     context 
  and string_t   = L.pointer_type (L.i8_type   context)
  and null_t     = L.pointer_type (L.i8_type   context) in

  let rec ltype_of_typ = function
    | A.Int -> i32_t
    | A.Bool -> i1_t
    | A.Num -> double_t
    | A.String -> string_t
    | A.List t -> L.pointer_type (ltype_of_typ t)
    | A.Null -> null_t
  in

  let printf_t = L.var_arg_function_type i32_t [| L.pointer_type (L.i8_type context) |] in
  let printf_func = L.declare_function "printf" printf_t the_module in
  let strlen_func = 
    L.declare_function "strlen"
      (L.function_type i32_t [| string_t |]) the_module in

  let scope_stack : (string, L.llvalue) Hashtbl.t list ref = ref [Hashtbl.create 10] in
  let loop_stack = ref [] in
  (* LLVM insists each basic block end with exactly one "terminator" instruction that transfers control.  This function runs "instr builder"
      if the current block does not already have a terminator.  Used, e.g., to handle the "fall off the end of the function" case. *)
  let add_terminal builder instr =
    match L.block_terminator (L.insertion_block builder) with
    | Some _ -> ()
    | None -> ignore (instr builder)
  in
  
  let push_scope () = scope_stack := (Hashtbl.create 10) :: !scope_stack in
  let pop_scope () = scope_stack := List.tl !scope_stack in

  let add_local name llval = Hashtbl.add (List.hd !scope_stack) name llval in

  let rec lookup name =
    match List.find_opt (fun tbl -> Hashtbl.mem tbl name) !scope_stack with
    | Some scope -> Hashtbl.find scope name
    | None -> failwith ("Variable " ^ name ^ " not found")
  in

  let function_decls : (L.llvalue * sfunc_def) StringMap.t ref = ref StringMap.empty in

  let rec build_function fdecl =
    let name = fdecl.sfname in
    let formal_types = Array.of_list (List.map (fun (t, _) -> ltype_of_typ t) fdecl.sformals) in
    let ftype = L.function_type (ltype_of_typ fdecl.srtyp) formal_types in
    let the_function =
      match L.lookup_function name the_module with
      | Some f -> f
      | None -> L.define_function name ftype the_module
    in

    function_decls := StringMap.add name (the_function, fdecl) !function_decls;

    let builder = L.builder_at_end context (L.entry_block the_function) in

    push_scope ();

    List.iter2 (fun (t, n) p ->
      L.set_value_name n p;
      let alloca = L.build_alloca (ltype_of_typ t) n builder in
      ignore (L.build_store p alloca builder);
      add_local n alloca
    ) fdecl.sformals (Array.to_list (L.params the_function));

    let func_builder = build_stmt builder (SBlock fdecl.sbody) in

    if Option.is_none (L.block_terminator (L.insertion_block func_builder)) then
      ignore (L.build_ret (L.const_int i32_t 0) func_builder);

    pop_scope ()

  and build_stmt builder = function
    | SFDecl fdecl -> build_function fdecl; builder
    | SBlock sl ->
        push_scope ();
        let builder = List.fold_left build_stmt builder sl in
        pop_scope (); builder
    | SExpr e -> ignore (build_expr builder e); builder
    | SReturn e -> ignore (L.build_ret (build_expr builder e) builder); builder
    | SVDecl (t, n) ->
        let alloca = L.build_alloca (ltype_of_typ t) n builder in
        add_local n alloca; builder
    | SIf (predicate, then_stmt, else_opt) ->
      let bool_val = build_expr builder predicate in
      let the_function = L.block_parent (L.insertion_block builder) in
      let then_bb = L.append_block context "then" the_function in
      let else_bb = L.append_block context "else" the_function in
      let end_bb = L.append_block context "if_end" the_function in
      
      ignore (L.build_cond_br bool_val then_bb else_bb builder);
      
      let then_builder = build_stmt (L.builder_at_end context then_bb) then_stmt in
      add_terminal then_builder (L.build_br end_bb);
      
      let _ = match else_opt with
        | Some else_stmt ->
            let b = build_stmt (L.builder_at_end context else_bb) else_stmt in
            add_terminal b (L.build_br end_bb);
            b
        | None ->
            let b = L.builder_at_end context else_bb in
            add_terminal b (L.build_br end_bb);
            b
      in

      L.builder_at_end context end_bb

    | SWhile (predicate, body) ->
      let the_function = L.block_parent (L.insertion_block builder) in
      let while_bb = L.append_block context "while" the_function in
      let build_br_while = L.build_br while_bb in
      ignore (build_br_while builder);
      
      let while_builder = L.builder_at_end context while_bb in
      let bool_val = build_expr while_builder predicate in
      
      let body_bb = L.append_block context "while_body" the_function in
      let end_bb = L.append_block context "while_end" the_function in
      
      loop_stack := (while_bb, end_bb) :: !loop_stack;
      
      let body_builder = build_stmt (L.builder_at_end context body_bb) body in
      add_terminal body_builder build_br_while;
      
      loop_stack := List.tl !loop_stack;
      
      ignore(L.build_cond_br bool_val body_bb end_bb while_builder);
      L.builder_at_end context end_bb

  | SFor (var, start_expr, end_expr, body) ->
    let the_function = L.block_parent (L.insertion_block builder) in
    let var_alloca = L.build_alloca i32_t var builder in
    let start_val = build_expr builder start_expr in
    ignore (L.build_store start_val var_alloca builder);

    let loop_cond_bb = L.append_block context "for_cond" the_function in
    ignore (L.build_br loop_cond_bb builder);
    
    let cond_builder = L.builder_at_end context loop_cond_bb in
    let curr_val = L.build_load var_alloca var cond_builder in
    let end_val = build_expr cond_builder end_expr in
    let cond = L.build_icmp L.Icmp.Slt curr_val end_val "for_cond" cond_builder in

    let loop_body_bb = L.append_block context "for_body" the_function in
    let inc_bb = L.append_block context "for_inc" the_function in
    let after_loop_bb = L.append_block context "for_end" the_function in
    
    loop_stack := (inc_bb, after_loop_bb) :: !loop_stack; (* Continue jumps to inc_bb, break to after_loop_bb *)
    
    ignore(L.build_cond_br cond loop_body_bb after_loop_bb cond_builder);

    (* Process loop body *)
    let body_builder = L.builder_at_end context loop_body_bb in
    let loop_var_val = L.build_load var_alloca var body_builder in
    ignore (L.build_store loop_var_val (lookup var) body_builder);

    let body_builder = build_stmt body_builder body in

    (* After body, branch to inc_bb *)
    ignore (L.build_br inc_bb body_builder);

    (* Process increment block *)
    let inc_builder = L.builder_at_end context inc_bb in
    let curr_val' = L.build_load var_alloca var inc_builder in
    let next_val = L.build_add curr_val' (L.const_int i32_t 1) "for_inc" inc_builder in
    ignore (L.build_store next_val var_alloca inc_builder);
    ignore (L.build_br loop_cond_bb inc_builder);

    loop_stack := List.tl !loop_stack;

    L.builder_at_end context after_loop_bb
  | SAssign (s, e) -> let e' = build_expr builder e in
      ignore (L.build_store e' (lookup s) builder);
      builder

  | SVDecl (t, n) ->
    let alloca = L.build_alloca (ltype_of_typ t) n builder in
    add_local n alloca;
    builder

  | SAssignAt (lst_expr, index_expr, value_expr) ->
      let lst_val = build_expr builder lst_expr in 
      let idx_val = build_expr builder index_expr in
      let value_val = build_expr builder value_expr in 
      let gep = L.build_in_bounds_gep lst_val [| idx_val |] "index_ptr" builder in
      ignore (L.build_store value_val gep builder);
      builder

  and build_expr builder ((_, e) : sexpr) =
    match e with
    | SIntLit i -> L.const_int i32_t i
    | SNumLit f -> L.const_float double_t f
    | SStringLit s -> L.build_global_stringptr s "str" builder
    | SBoolLit b -> L.const_int i1_t (if b then 1 else 0)
    | SId s -> L.build_load (lookup s) s builder
    | SBinop ((t1, s1), A.Add, (t2, s2)) when t1 = A.String && t2 = A.String ->
        let e1 = (t1, s1) in
        let e2 = (t2, s2) in 
        let str1 = build_expr builder e1 in
        let str2 = build_expr builder e2 in

        let len1 = L.build_call strlen_func [| str1 |] "len1" builder in
        let len2 = L.build_call strlen_func [| str2 |] "len2" builder in
        let total_len = L.build_add len1 len2 "total_len" builder in
        let alloc_size = L.build_add total_len (L.const_int i32_t 1) "alloc_size" builder in
        let dest = L.build_array_alloca i8_t alloc_size "concat_str" builder in

        let memcpy_func = L.declare_function "llvm.memcpy.p0i8.p0i8.i32"
          (L.function_type
            (L.void_type context)
            [| L.pointer_type i8_t; L.pointer_type i8_t; i32_t; i1_t |])
          the_module
        in
        ignore (L.build_call memcpy_func [| dest; str1; len1; L.const_int i1_t 0 |] "" builder);
        let dest_offset = L.build_in_bounds_gep dest [| len1 |] "dest_offset" builder in
        ignore (L.build_call memcpy_func [| dest_offset; str2; len2; L.const_int i1_t 0 |] "" builder);
        let null_pos = L.build_in_bounds_gep dest [| total_len |] "null_pos" builder in
        ignore (L.build_store (L.const_int i8_t 0) null_pos builder);
        dest  (* <-- this is the last expression of this case *)
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
         | A.Less -> L.build_icmp L.Icmp.Slt) e1' e2' "tmp" builder
    | SCall ("printint", [e]) ->
        let fmt = L.build_global_stringptr "%d\n" "fmt" builder in
        L.build_call printf_func [| fmt; build_expr builder e |] "printf" builder
    | SCall ("printstring", [e]) ->
        let fmt = L.build_global_stringptr "%s\n" "fmt" builder in
        L.build_call printf_func [| fmt; build_expr builder e |] "printf" builder
    | SCall ("printnum", [e]) ->
      L.build_call printf_func [| num_format_str ; (build_expr builder e) |]
        "printf_num" builder
    | SCall ("strlen", [e]) ->
      let value = build_expr builder e in
      L.build_call strlen_func [| value |] "strlen" builder
    | SCall (fname, args) ->
        let (fdecl, _) =
          try StringMap.find fname !function_decls
          with Not_found -> failwith ("Unknown function: " ^ fname)
        in
        let ll_args = Array.of_list (List.map (build_expr builder) args) in
        L.build_call fdecl ll_args "calltmp" builder
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

  let main_ftype = L.function_type i32_t [||] in
  let main_func = L.define_function "main" main_ftype the_module in
  let builder = L.builder_at_end context (L.entry_block main_func) in
  let _ = push_scope () in
  let builder = build_stmt builder (SBlock stmts) in
  if Option.is_none (L.block_terminator (L.insertion_block builder)) then
    ignore (L.build_ret (L.const_int i32_t 0) builder);
  the_module