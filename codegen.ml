(* Code generation: translate takes a semantically checked AST and
produces LLVM IR
*)

module L = Llvm
module A = Ast

module StringMap = Map.Make(String)

let context = L.global_context ()
let the_module = L.create_module context "Strux"
let llbuilder = L.builder context
let f_t = L.double_type context;; (* float *)
let i8_t   = L.i8_type   context;; (* print type *)
let i1_t   = L.i1_type   context;; (* bool type *)
let void_t = L.void_type context;; (* void type *)
let str_t  = L.pointer_type (L.i8_type context) (* string *)


let rec get_pointer_type datatype = match datatype with
    A.Arraytype(t, 0) -> ltype_of_typ t
  | A.Arraytype(t, 1) -> L.pointer_type (ltype_of_typ t)
  | A.Arraytype(t, i) -> L.pointer_type (get_pointer_type (A.Arraytype(t, (i-1))))
  |   _ -> raise(Failure("InvalidStructType Array Pointer Type"))

and ltype_of_typ datatype = match datatype with (* LLVM type for AST type *)
    A.Num -> f_t
  | A.String -> str_t
  | A.Bool -> i1_t
  | A.Void -> void_t
  | A.Arraytype(data_type, i) ->  get_pointer_type (A.Array(data_type, (i)))
(*   | A.Stack -> f_t
  | A.Queue -> f_t
  | A.LinkedList -> f_t
  | A.ListNode -> f_t
  | A.BSTree -> f_t
  | A.TreeNode -> f_t *)

and gen_type exp = match exp with
    NumLit(_) -> A.Num
  | BoolLit(_) -> A.Bool
  | StringLit(_) -> A.String
  | Id(_, typ) -> typ
  | Binop(_,_,_, typ) -> typ
  | Unop(_, _, typ) -> typ
  | SCallExpr(_,_,typ) -> typ
  | _ -> raise(Failure("llvm type of " ^ string_of_sexpr exp ^ " not yet supported"))
;;

let find_function fname = 
  match (L.lookup_function fname the_module) with
      None -> raise(Failure("Function: " ^ fname ^ " not found in module."))
    | Some f -> f

(* where the code gen stuff begins *)
let translate = 
  
  (* Declare each global variable; remember its value in a map *)
  let global_vars =
    let global_var m (t, n) =
      let init = L.const_int (ltype_of_typ t) 0
      in StringMap.add n (L.define_global n init the_module) m in
    List.fold_left global_var StringMap.empty globals in

  (* -------BUILT IN FUNCTIONS----------- *)

  (* Declare our print function here *)

(* print *)
let printf_func = 
  let printf_t = L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
  L.declare_function "print" printf_t the_module
;;

  (* Define each function (arguments and return type) so we can call it *)
  let function_decls =
    let function_decl m fdecl =
      let name = fdecl.A.fname
      and formal_types =
  Array.of_list (List.map (fun (t,_) -> ltype_of_typ t) fdecl.A.formals)
      in let ftype = L.function_type (ltype_of_typ fdecl.A.typ) formal_types in
      StringMap.add name (L.define_function name ftype the_module, fdecl) m in
    List.fold_left function_decl StringMap.empty functions in
  
  (* Fill in the body of the given function *)
  let build_function_body fdecl =
    let (the_function, _) = StringMap.find fdecl.A.fname function_decls in
    let builder = L.builder_at_end context (L.entry_block the_function) in

    let int_format_str = L.build_global_stringptr "%d\n" "fmt" builder in
    
    (* Construct the function's "locals": formal arguments and locally
       declared variables.  Allocate each on the stack, initialize their
       value, if appropriate, and remember their values in the "locals" map *)
    let local_vars =
      let add_formal m (t, n) p = L.set_value_name n p;
  let local = L.build_alloca (ltype_of_typ t) n builder in
  ignore (L.build_store p local builder);
  StringMap.add n local m in

      let add_local m (t, n) =
  let local_var = L.build_alloca (ltype_of_typ t) n builder
  in StringMap.add n local_var m in

      let formals = List.fold_left2 add_formal StringMap.empty fdecl.A.formals
          (Array.to_list (L.params the_function)) in
      List.fold_left add_local formals fdecl.A.locals in

    (* Return the value for a variable or formal argument *)
    let lookup n = try StringMap.find n local_vars
                   with Not_found -> StringMap.find n global_vars
  in
 

  let rec expr_generator llbuilder = function
      A.NumLit(n) -> L.const_float f_t n
    | A.BoolLit(b) -> L.const_int i1_t (if b then 1 else 0)
    | A.StringLit(s) -> L.build_global_stringptr s "string" llbuilder
    | A.Id s -> L.build_load (lookup s) s llbuilder
    | A.Binop (e1, op, e2, typ) ->
        let e1' = expr_generator llbuilder e1
        and e2' = expr_generator llbuilder e2
        and typ = typ in

        let int_ops e1 op e2 = match op with
          A.Add     -> L.build_add e1 e2 "addtmp" llbuilder
        | A.Sub     -> L.build_sub e1 e2 "subtmp" llbuilder
        | A.Mult    -> L.build_mul e1 e2 "multtmp" llbuilder
        | A.Div     -> L.build_sdiv e1 e2 "divtmp" llbuilder
        | A.Mod     -> L.build_srem e1 e2 "sremtmp" llbuilder
        | A.And     -> L.build_and e1 e2 "andtmp" llbuilder
        | A.Or      -> L.build_or e1 e2 "ortmp" llbuilder
        | A.Equal   -> L.build_icmp L.Icmp.Eq e1 e2 "eqtmp" llbuilder
        | A.Neq     -> L.build_icmp L.Icmp.Ne e1 e2 "neqtmp" llbuilder
        | A.Less    -> L.build_icmp L.Icmp.Slt e1 e2 "lesstmp" llbuilder
        | A.Leq     -> L.build_icmp L.Icmp.Sle e1 e2 "leqtmp" llbuilder
        | A.Greater -> L.build_icmp L.Icmp.Sgt e1 e2 "sgttmp" llbuilder
        | A.Geq     -> L.build_icmp L.Icmp.Sge e1 e2 "sgetmp" llbuilder
        | _         -> raise (Failure("unsupported operator for integer arguments"))

        and num_ops e1 op e2 = match op with
            A.Add     -> L.build_fadd e1 e2 "flt_addtmp" llbuilder
          | A.Sub     -> L.build_fsub e1 e2 "flt_subtmp" llbuilder
          | A.Mult    -> L.build_fmul e1 e2 "flt_multmp" llbuilder
          | A.Div     -> L.build_fdiv e1 e2 "flt_divtmp" llbuilder
          | A.Mod     -> L.build_frem e1 e2 "flt_sremtmp" llbuilder
          | A.Equal   -> L.build_fcmp L.Fcmp.Oeq e1 e2 "flt_eqtmp" llbuilder
          | A.Neq     -> L.build_fcmp L.Fcmp.One e1 e2 "flt_neqtmp" llbuilder
          | A.Less    -> L.build_fcmp L.Fcmp.Ult e1 e2 "flt_lesstmp" llbuilder
          | A.Leq     -> L.build_fcmp L.Fcmp.Ole e1 e2 "flt_leqtmp" llbuilder
          | A.Greater -> L.build_fcmp L.Fcmp.Ogt e1 e2 "flt_sgttmp" llbuilder
          | A.Geq     -> L.build_fcmp L.Fcmp.Oge e1 e2 "flt_sgetmp" llbuilder
          | _         -> raise (Failure("unsupported operation for numbers"))
        in 
        let match_types e1 = match gen_type e1 with
            A.Bool -> int_ops e1' op e2'
          | A.Num -> num_ops e1' op e2'
          | _ -> raise(Failure("Invalid Binop types at " ^
                  string_of_sexpr e1 ^ " " ^ A.string_of_op op ^ " " ^ string_of_expr 
                  e2 ^ ": Can only do Binop on Int, Bool, or Float"))
        in
        match_types e1
    | A.Unop (op, e, typ) ->
        let e' = expr_generator llbuilder e in
        let num_unops op e' = match op with
            A.Neg     -> L.build_fneg e' "tmp" llbuilder
          | _         -> raise(Failure("Invalid " ^ A.string_of_uop op ^ " at " ^ string_of_expr e))
        in
        let match_types e = match gen_type e with
          | A.Num -> num_unops op e'
          | _ -> raise(Failure("Invalid Unop type at " ^ string_of_sexpr e))
        in
        match_types e
    | A.Assign (Id(s,_), e, t) -> 
        let e' = codegen_sexpr llbuilder e in
        ignore (L.build_store e' (var_lookup s) llbuilder); e'
    | A.FuncCall (Id("print", _), [e], typ) ->
        let num_format_str llbuilder = L.build_global_stringptr "%f\n" "fmt" llbuilder;
        and str_format_str llbuilder = L.build_global_stringptr "%s\n" "fmt" llbuilder in          
          
        let format_str e_typ llbuilder = match e_typ with
            A.Num -> num_format_str llbuilder
          | A.String -> str_format_str llbuilder
          | _ -> raise (Failure "Invalid print function type")
        in

        let e' = expr_generator llbuilder e
        and e_type = gen_type e in
        L.build_call printf_func [| format_str e_type llbuilder; e' |]
            "printf" llbuilder
    | A.FuncCall (f, act) ->
         let (fdef, fdecl) = StringMap.find f function_decls in
   let actuals = List.rev (List.map (expr llbuilder) (List.rev act)) in
   let result = (match fdecl.A.typ with A.Void -> ""
                                            | _ -> f ^ "_result") in
         L.build_call fdef (Array.of_list actuals) result llbuilder
    in

    (* Invoke "f builder" if the current block doesn't already
       have a terminal (e.g., a branch). *)
    let add_terminal llbuilder f =
      match L.block_terminator (L.insertion_block llbuilder) with
  Some _ -> ()
      | None -> ignore (f llbuilder) in


  (* Statement generator *)
  let stmt_generator llbuilder = function
    A.Block stmtlist -> generate_block stmtlist llbuilder
  | A.Return (e, stmt) -> generate_return e stmt llbuilder
  | A.Expr (se, _) -> expr_generator llbuilder se
  | A.If (predicate, s1, s2) -> generate_if predicate s1 s2 llbuilder
  | A.While (predicate, body) -> generate_while  predicate body llbuilder
  | A.For (e1, e2, e3, s) -> generate_for e1 e2 e3 s llbuilder
  | A.ForEach (typ, e1, e2, s) generate_for_each typ e1 e2 e3 s llbuilder

  and generate_block stmtlist llbuilder = 
    try List.hd (List.map (stmt_generator llbuilder) sl) with 
    | Failure("hd") -> raise(Failure("No block body"));

  and generate_return e stmt llbuilder =
    match e with
    | Noexpr -> L.build_ret_void llbuilder
    | _ -> L.build_ret (expr_generator llbuilder e) llbuilder

  and generate_if predicate s1 s2 llbuilder =
    let bool_val = expr llbuilder predicate in
    let merge_bb = L.append_block context "merge" the_function in

    let then_bb = L.append_block context "then" the_function in
    add_terminal (stmt (L.builder_at_end context then_bb) s1)
     (L.build_br merge_bb);

    let else_bb = L.append_block context "else" the_function in
    add_terminal (stmt (L.builder_at_end context else_bb) s2)
     (L.build_br merge_bb);

    ignore (L.build_cond_br bool_val then_bb else_bb llbuilder);
    L.builder_at_end context merge_bb

  and generate_while predicate body llbuilder = 
    let pred_bb = L.append_block context "while" the_function in
      ignore (L.build_br pred_bb llbuilder);

      let body_bb = L.append_block context "while_body" the_function in
      add_terminal (stmt (L.builder_at_end context body_bb) body)
        (L.build_br pred_bb);

      let pred_builder = L.builder_at_end context pred_bb in
      let bool_val = expr pred_builder predicate in

      let merge_bb = L.append_block context "merge" the_function in
      ignore (L.build_cond_br bool_val body_bb merge_bb pred_builder);
      L.builder_at_end context merge_bb

  and generate_for e1 e2 e3 s llbuilder =
    ( A.Block [A.Expr e1 ; A.While (e2, A.Block [s ; A.Expr e3]) ] )
  in

    (* Build the code for each statement in the function *)
    let llbuilder = stmt_generator llbuilder (A.Block fdecl.A.body) in

    (* Add a return if the last block falls off the end *)
    add_terminal llbuilder (match fdecl.A.typ with
        A.Void -> L.build_ret_void
      | t -> L.build_ret (L.const_int (ltype_of_typ t) 0))
  in

  List.iter build_function_body functions;
  the_module


