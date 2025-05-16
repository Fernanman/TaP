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

  (* Declare printf *)
  let printf_t = L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
  let printf_func = L.declare_function "printf" printf_t the_module in

  (* Local variables table *)
  let local_vars = Hashtbl.create 10 in
  let add_local name llval = Hashtbl.add local_vars name llval in
  let lookup name =
    try Hashtbl.find local_vars name
    with Not_found -> failwith ("Variable " ^ name ^ " not found")
  in

  (* Map function names to their LLVM function values and sfunc_def *)
  let function_decls : (L.llvalue * sfunc_def) StringMap.t ref = ref StringMap.empty in

  (* Build function from SFDecl *)
  let rec build_function fdecl =
    let name = fdecl.sfname in
    let formal_types = Array.of_list (List.map (fun (t, _) -> ltype_of_typ t) fdecl.sformals) in
    let ftype = L.function_type (ltype_of_typ fdecl.srtyp) formal_types in

    let the_function =
      match L.lookup_function name the_module with
      | Some f -> f
      | None -> L.define_function name ftype the_module
    in

    (* Add to function_decls map *)
    function_decls := StringMap.add name (the_function, fdecl) !function_decls;

    let builder = L.builder_at_end context (L.entry_block the_function) in

    Hashtbl.reset local_vars;

    List.iter2 (fun (t, n) p ->
      L.set_value_name n p;
      let alloca = L.build_alloca (ltype_of_typ t) n builder in
      ignore (L.build_store p alloca builder);
      add_local n alloca
    ) fdecl.sformals (Array.to_list (L.params the_function));

    let func_builder = build_stmt builder (SBlock fdecl.sbody) in

    (* Ensure function ends with a return *)
    if Option.is_none (L.block_terminator (L.insertion_block func_builder)) then
      ignore (L.build_ret (L.const_int i32_t 0) func_builder)

  (* build_stmt is mutually recursive, so declare it first *)
  and build_stmt builder s =
    match s with
    | SFDecl fdecl -> 
        build_function fdecl;
        builder
    | SBlock sl -> List.fold_left build_stmt builder sl
    | SExpr e -> ignore (build_expr builder e); builder
    | SReturn e -> ignore (L.build_ret (build_expr builder e) builder); builder
    | SAssign (s, e) ->
        let v = build_expr builder e in
        ignore (L.build_store v (lookup s) builder);
        builder
    | SVDecl (t, n) ->
        let alloca = L.build_alloca (ltype_of_typ t) n builder in
        add_local n alloca;
        builder
    | SIf (p, t) ->
        let cond = build_expr builder p in
        let the_function = L.block_parent (L.insertion_block builder) in
        let then_bb = L.append_block context "then" the_function in
        let else_bb = L.append_block context "else" the_function in
        let end_bb = L.append_block context "ifend" the_function in
        ignore (L.build_cond_br cond then_bb else_bb builder);

        let then_builder = build_stmt (L.builder_at_end context then_bb) t in
        if Option.is_none (L.block_terminator (L.insertion_block then_builder)) then
          ignore (L.build_br end_bb then_builder);

        let else_builder = L.builder_at_end context else_bb in
        if Option.is_none (L.block_terminator (L.insertion_block else_builder)) then
          ignore (L.build_br end_bb else_builder);

        L.builder_at_end context end_bb
    | SWhile (p, b) ->
        let the_function = L.block_parent (L.insertion_block builder) in
        let pred_bb = L.append_block context "while" the_function in
        let body_bb = L.append_block context "while_body" the_function in
        let end_bb = L.append_block context "while_end" the_function in

        ignore (L.build_br pred_bb builder);

        let pred_builder = L.builder_at_end context pred_bb in
        let cond = build_expr pred_builder p in
        ignore (L.build_cond_br cond body_bb end_bb pred_builder);

        let body_builder = build_stmt (L.builder_at_end context body_bb) b in
        if Option.is_none (L.block_terminator (L.insertion_block body_builder)) then
          ignore (L.build_br pred_bb body_builder);

        L.builder_at_end context end_bb
    | SFor (v, e1, e2, b) ->
        let the_function = L.block_parent (L.insertion_block builder) in
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

        if Option.is_none (L.block_terminator (L.insertion_block body_builder)) then
          ignore (L.build_br pred_bb body_builder);

        L.builder_at_end context end_bb
    | SContinue -> failwith "Continue not implemented"
    | SBreak -> failwith "Break not implemented"

  (* Expression builder *)
  and build_expr builder ((_, e) : sexpr) =
    match e with
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
      let int_format_str = L.build_global_stringptr "%d\n" "fmt" builder in
      L.build_call printf_func [| int_format_str; build_expr builder e |] "printf" builder
    | SCall ("printstring", [e]) ->
      let string_format_str = L.build_global_stringptr "%s\n" "fmt" builder in
      L.build_call printf_func [| string_format_str; build_expr builder e |] "printf" builder
    | SCall (fname, args) ->
      let (fdecl, _) =
        try StringMap.find fname !function_decls
        with Not_found -> failwith ("Unknown function: " ^ fname)
      in
      let ll_args = Array.of_list (List.map (build_expr builder) args) in
      L.build_call fdecl ll_args "calltmp" builder

  in

  (* Top-level: build a "main" function to contain top-level stmts *)
  let main_ftype = L.function_type i32_t [||] in
  let main_func = L.define_function "main" main_ftype the_module in
  let builder = L.builder_at_end context (L.entry_block main_func) in
  let builder = build_stmt builder (SBlock stmts) in
  if Option.is_none (L.block_terminator (L.insertion_block builder)) then
    ignore (L.build_ret (L.const_int i32_t 0) builder);

  the_module
