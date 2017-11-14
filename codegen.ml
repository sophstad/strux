(* Code generation: translate takes a semantically checked AST and
produces LLVM IR
*)

module L = Llvm
module A = Ast

module StringMap = Map.Make(String)

let translate (globals, functions) =
  let context = L.global_context () in
  let the_module = L.create_module context "Strux"
  and f_t    = L.double_type context  (* float *)
  and i8_t   = L.i8_type   context    (* print type *)
  and i1_t   = L.i1_type   context    (* bool type *)
  and void_t = L.void_type context    (* void type *)
  and str_t  = L.pointer_type (L.i8_type context) (* string *)
  and i32_t  = L.i32_type  context in

  let ltype_of_typ datatype = match datatype with (* LLVM type for AST type *)
      A.Num -> f_t
    | A.String -> str_t
    | A.Bool -> i1_t
    | A.Void -> void_t 
    | _ -> raise(Failure("Invalid Data Type"))
  in
    (* | A.Array(data_type, i) ->  get_pointer_type (A.Array(data_type, (i)))
    | A.Stack -> f_t
    | A.Queue -> f_t
    | A.LinkedList -> f_t
    | A.ListNode -> f_t
    | A.BSTree -> f_t
    | A.TreeNode -> f_t *)

  (* Declare each global variable; remember its value in a map *)
  let global_vars =
    let global_var m (t, n) =
      let init = L.const_int (ltype_of_typ t) 0
      in StringMap.add n (L.define_global n init the_module) m in
    List.fold_left global_var StringMap.empty globals in

  (* -------BUILT IN FUNCTIONS----------- *)

  (* Declare our print function here *)


  (* print *)
  (* let printf_func =
    let printf_t = L.var_arg_function_type f_t [| L.pointer_type i8_t |] in
    L.declare_function "print" printf_t the_module
  in *)
  let printf_t = L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
  let printf_func = L.declare_function "printf" printf_t the_module in


  (* Define each function (arguments and return type) so we can call it *)
  let function_decls =
    let function_decl m func_decl =
      let name = func_decl.A.fname
      and formal_types =
	Array.of_list (List.map (fun (t,_) -> ltype_of_typ t) func_decl.A.formals)
      in let ftype = L.function_type (ltype_of_typ func_decl.A.typ) formal_types in
      StringMap.add name (L.define_function name ftype the_module, func_decl) m in
    List.fold_left function_decl StringMap.empty functions in

    (* Fill in the body of the given function *)
    let build_function_body func_decl =
      let (the_function, _) = StringMap.find func_decl.A.fname function_decls in
      let llbuilder = L.builder_at_end context (L.entry_block the_function) in

      let str_format_str = L.build_global_stringptr "%s\n" "fmt" llbuilder in
      let flt_format_str = L.build_global_stringptr "%f\n" "fmt" llbuilder in
      let bool_format_str = L.build_global_stringptr "%d\n" "fmt" llbuilder in

      (* Construct the function's "locals": formal arguments and locally
         declared variables.  Allocate each on the stack, initialize their
         value, if appropriate, and remember their values in the "locals" map *)
      let local_vars =
        let add_formal m (t, n) p = L.set_value_name n p;
          let local = L.build_alloca (ltype_of_typ t) n llbuilder in
  	      ignore (L.build_store p local llbuilder);
          StringMap.add n local m
        in

        let add_local m (t, n) =
          let local_var = L.build_alloca (ltype_of_typ t) n llbuilder in
          StringMap.add n local_var m
        in

        let formals = List.fold_left2 add_formal StringMap.empty func_decl.A.formals
            (Array.to_list (L.params the_function)) in
        List.fold_left add_local formals func_decl.A.locals
      in

      (* Return the value for a variable or formal argument *)
      let lookup n = try StringMap.find n local_vars
                     with Not_found -> raise (Failure "Variable not found")
      in


    (* Define each function (arguments and return type) so we can call it *)
    let rec expr_generator llbuilder = function
        A.NumLit(n) -> L.const_float f_t n
      | A.BoolLit(b) -> L.const_int i1_t (if b then 1 else 0)
      | A.StringLit(s) -> L.build_global_stringptr s "string" llbuilder
      | A.Id s -> L.build_load (lookup s) s llbuilder
      | A.Binop (e1, op, e2) ->
          let e1' = expr_generator llbuilder e1
          and e2' = expr_generator llbuilder e2 in
          (match op with
            A.Add     -> L.build_fadd
          | A.Sub     -> L.build_fsub
          | A.Mult    -> L.build_fmul
          | A.Div     -> L.build_fdiv
          | A.Mod     -> L.build_frem
          | A.Equal   -> L.build_fcmp L.Fcmp.Oeq
          | A.Neq     -> L.build_fcmp L.Fcmp.One
          | A.Less    -> L.build_fcmp L.Fcmp.Ult
          | A.Leq     -> L.build_fcmp L.Fcmp.Ole
          | A.Greater -> L.build_fcmp L.Fcmp.Ogt
          | A.Geq     -> L.build_fcmp L.Fcmp.Oge
          | _         -> raise (Failure("unsupported operation for numbers"))
          ) e1' e2' "tmp" llbuilder
      | A.Unop (op, e) ->
          let e' = expr_generator llbuilder e in
          (match op with
            A.Neg     -> L.build_neg
          | A.Not     -> L.build_not) e' "tmp" llbuilder
      | A.Assign (s, e) ->
          let e' = expr_generator llbuilder e in
          ignore (L.build_store e' (lookup s) llbuilder); e'
      | A.FuncCall("print", [e]) ->
          let e' = expr_generator llbuilder e in
          if L.type_of e' == ltype_of_typ A.Num
          then L.build_call printf_func [| flt_format_str ; e' |] "print" llbuilder
          else if L.type_of e' == ltype_of_typ A.String
          then L.build_call printf_func [| str_format_str ; e' |] "print" llbuilder
          else if L.type_of e' == ltype_of_typ A.Bool
          then L.build_call printf_func [| bool_format_str ; e' |] "print" llbuilder
        else L.build_call printf_func [| str_format_str ; e' |] "print" llbuilder
      | A.FuncCall (f, act) ->
           let (fdef, func_decl) = StringMap.find f function_decls in
     let actuals = List.rev (List.map (expr_generator llbuilder) (List.rev act)) in
     let result = (match func_decl.A.typ with A.Void -> ""
                                              | _ -> f ^ "_result") in
           L.build_call fdef (Array.of_list actuals) result llbuilder
      in

      (* Invoke "f llbuilder" if the current block doesn't already
         have a terminal (e.g., a branch). *)
      let add_terminal llbuilder f =
        match L.block_terminator (L.insertion_block llbuilder) with
    Some _ -> ()
        | None -> ignore (f llbuilder) in


    (* Statement generator *)
    let rec stmt_generator llbuilder = function
      A.Block stmtlist -> List.fold_left stmt_generator llbuilder stmtlist
    | A.Return e -> ignore (match func_decl.A.typ with
                            A.Void -> L.build_ret_void llbuilder
                          | _      -> L.build_ret (expr_generator llbuilder e) llbuilder); llbuilder
    | A.Expr se -> ignore (expr_generator llbuilder se); llbuilder
    | A.If (predicate, s1, s2) -> generate_if predicate s1 s2 llbuilder
    | A.While (predicate, body) -> generate_while  predicate body llbuilder
    | A.For (e1, e2, e3, s) -> stmt_generator llbuilder ( A.Block [A.Expr e1 ; A.While (e2, A.Block [s ; A.Expr e3]) ] )
    (* | A.ForEach (e1, e2, s) -> generate_for_each typ e1 e2 e3 s llbuilder *)

    and generate_if predicate s1 s2 llbuilder =
      let bool_val = expr_generator llbuilder predicate in
      let merge_bb = L.append_block context "merge" the_function in

      let then_bb = L.append_block context "then" the_function in
      add_terminal (stmt_generator (L.builder_at_end context then_bb) s1)
       (L.build_br merge_bb);

      let else_bb = L.append_block context "else" the_function in
      add_terminal (stmt_generator  (L.builder_at_end context else_bb) s2)
       (L.build_br merge_bb);

      ignore (L.build_cond_br bool_val then_bb else_bb llbuilder);
      L.builder_at_end context merge_bb

    and generate_while predicate body llbuilder =
      let pred_bb = L.append_block context "while" the_function in
        ignore (L.build_br pred_bb llbuilder);

        let body_bb = L.append_block context "while_body" the_function in
        add_terminal (stmt_generator (L.builder_at_end context body_bb) body)
          (L.build_br pred_bb);

        let pred_builder = L.builder_at_end context pred_bb in
        let bool_val = expr_generator pred_builder predicate in

        let merge_bb = L.append_block context "merge" the_function in
        ignore (L.build_cond_br bool_val body_bb merge_bb pred_builder);
        L.builder_at_end context merge_bb
    in

      (* Build the code for each statement in the function *)
      let llbuilder = stmt_generator llbuilder (A.Block func_decl.A.body) in

      (* Add a return if the last block falls off the end *)
      add_terminal llbuilder (match func_decl.A.typ with
         A.String -> L.build_ret (L.build_global_stringptr "" "string" llbuilder)
        | A.Void -> L.build_ret_void
        | A.Num -> L.build_ret (L.const_float f_t 0.)
        | A.Bool -> L.build_ret (L.const_int i1_t 0)
        | _ -> L.build_ret_void)
    in
    List.iter build_function_body functions;
    the_module
