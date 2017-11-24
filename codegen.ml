(* Code generation: translate takes a semantically checked AST and
produces LLVM IR
*)
open Llvm

module L = Llvm
module A = Ast

module StringMap = Map.Make(String)

let context = L.global_context ()
let the_module = L.create_module context "Strux"
and f_t    = L.double_type context  (* float *)
and i8_t   = L.i8_type   context    (* print type *)
and i1_t   = L.i1_type   context    (* bool type *)
and void_t = L.void_type context    (* void type *)
and str_t  = L.pointer_type (L.i8_type context) (* string *)
and i32_t  = L.i32_type  context;;

let rec ltype_of_typ = function (* LLVM type for AST type *)
    A.Num -> f_t
  | A.Int -> i32_t
  | A.String -> str_t
  | A.Bool -> i1_t
  | A.Void -> void_t
  | A.Arraytype(t) -> L.pointer_type (ltype_of_typ t)
  | _ -> raise(Failure("Invalid Data Type"))
  (* | A.Array(data_type, i) ->  get_pointer_type (A.Array(data_type, (i)))
    | A.Stack -> f_t
    | A.Queue -> f_t
    | A.LinkedList -> f_t
    | A.ListNode -> f_t
    | A.BSTree -> f_t
    | A.TreeNode -> f_t *)

and translate (globals, functions) =

  (* Declare each global variable; remember its value in a map *)
  let global_vars = ref StringMap.empty in

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
      let int_format_str = L.build_global_stringptr "%d\n" "fmt" llbuilder in
      let flt_format_str = L.build_global_stringptr "%f\n" "fmt" llbuilder in
      let int_format_str = L.build_global_stringptr "%d\n" "fmt" llbuilder in
      let bool_format_str = L.build_global_stringptr "%d\n" "fmt" llbuilder in

      (* Construct the function's "locals": formal arguments and locally
         declared variables.  Allocate each on the stack, initialize their
         value, if appropriate, and remember their values in the "locals" map *)

      (* function to add local variables to a map *)
      let local_vars =
        let add_formal m (t,n) p = L.set_value_name n p;
        let local = L.build_alloca (ltype_of_typ t) n llbuilder in
        ignore (L.build_store p local llbuilder);
        StringMap.add n (t,local) m in

        (* add formals to the map *)
        ref (List.fold_left2 add_formal StringMap.empty func_decl.A.formals
          (Array.to_list (L.params the_function))) in

      (* Return the value or the type for a variable or formal argument *)
      (* All the tables have the structure (type, llvalue) *)
      let lookup n : L.llvalue =
        try (snd (StringMap.find n !local_vars))
        with Not_found -> (snd (StringMap.find n !global_vars))
      in

(*     (*Array functions*)
    let initialise_array arr arr_len init_val start_pos llbuilder =
      let new_block label =
        let f = L.block_parent (L.insertion_block llbuilder) in
        L.append_block (context) label f
      in
        let bbcurr = L.insertion_block llbuilder in
        let bbcond = new_block "array.cond" in
        let bbbody = new_block "array.init" in
        let bbdone = new_block "array.done" in
        ignore (L.build_br bbcond llbuilder);
        L.position_at_end bbcond llbuilder;

        (* Counter into the length of the array *)
        let counter = L.build_phi [const_int i32_t start_pos, bbcurr] "counter" llbuilder in
        add_incoming ((build_add counter (const_int i32_t 1) "tmp" llbuilder), bbbody) counter;
        let cmp = build_icmp Icmp.Slt counter arr_len "tmp" llbuilder in
        ignore (build_cond_br cmp bbbody bbdone llbuilder);
        position_at_end bbbody llbuilder;

        (* Assign array position to init_val *)
        let arr_ptr = build_gep arr [| counter |] "tmp" llbuilder in
        ignore (build_store init_val arr_ptr llbuilder);
        ignore (build_br bbcond llbuilder);
        position_at_end bbdone llbuilder
    in *)

    (* Array creation, initialization, access *)
    let create_array t len builder =
      let ltype = ltype_of_typ t in
      let size_t = L.build_intcast (L.size_of ltype) i32_t "tmp" builder in
      let total_size = L.build_mul size_t len "tmp" builder in
      let total_size = L.build_add total_size (L.const_int i32_t 1) "tmp" builder in
      let arr_malloc = L.build_array_malloc ltype total_size "tmp" builder in
      let arr = L.build_pointercast arr_malloc (L.pointer_type ltype) "tmp" builder in
      arr
    in

    let initialize_array t el builder =
      let len = L.const_int i32_t (List.length el) in
      let arr = create_array t len builder in
      let _ =
        let assign_value i =
          let index = L.const_int i32_t i in
          let index = L.build_add index (L.const_int i32_t 1) "tmp" builder in
          let _val = L.build_gep arr [| index |] "tmp" builder in
          L.build_store (List.nth el i) _val builder
        in
        for i = 0 to (List.length el)-1 do
          ignore (assign_value i)
        done
      in
      arr
    in

    let access_array arr index assign builder =
      let index = L.build_add index (L.const_int i32_t 1) "tmp" builder in
      let arr = L.build_load arr "tmp" builder in
      let _val = L.build_gep arr [| index |] "tmp" builder in
      if assign then _val else L.build_load _val "tmp" builder
    in

    let rec gen_type = function
        A.IntLit _ -> A.Int
      | A.NumLit _ -> A.Num
      | A.StringLit _ -> A.String
      | A.BoolLit _ -> A.Bool
      | A.ArrayLit el -> A.Arraytype (gen_type (List.nth el 0))
    in

    (* Define each function (arguments and return type) so we can call it *)
    let rec expr_generator llbuilder = function
        A.NumLit(n) -> L.const_float f_t n
      | A.IntLit(i) -> L.const_int i32_t i
      | A.BoolLit(b) -> L.const_int i1_t (if b then 1 else 0)
      | A.StringLit(s) -> L.build_global_stringptr s "string" llbuilder
      | A.Id s -> L.build_load (lookup s) s llbuilder
      | A.Binop (e1, op, e2) ->
        let e1' = expr_generator llbuilder e1 in
        let e2' = expr_generator llbuilder e2
        and num_ops = (match op with
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
          | _ -> L.build_fcmp L.Fcmp.Oeq
        )
        and int_ops = (match op with
            A.Add     -> L.build_add
          | A.Sub   -> L.build_sub
          | A.Mult   -> L.build_mul
          | A.Div  -> L.build_sdiv
          | A.Mod     -> L.build_srem
          | A.Equal   -> L.build_icmp L.Icmp.Eq
          | A.Neq     -> L.build_icmp L.Icmp.Ne
          | A.Less    -> L.build_icmp L.Icmp.Slt
          | A.Leq     -> L.build_icmp L.Icmp.Sle
          | A.Greater -> L.build_icmp L.Icmp.Sgt
          | A.Geq     -> L.build_icmp L.Icmp.Sge
          | _ -> L.build_icmp L.Icmp.Eq
        )
       (*  and str_ops = (match op with
            A.Concat -> expr_generator llbuilder (A.StringLit((string_from_expr e1) ^ (string_from_expr e2), t))
          | _ -> (L.const_int i32_t 0)
        ) *)

        (*  if ((L.type_of e1' = str_t) && (L.type_of e2' = str_t)) then str_ops
         else  *)
        in

        if ((L.type_of e1' = f_t) && (L.type_of e2' = f_t)) then num_ops e1' e2' "tmp" llbuilder
        else int_ops e1' e2' "tmp" llbuilder
      | A.Unop (op, e) ->
          let e' = expr_generator llbuilder e in
          let int_unops op e' = (match op with
              A.Neg     -> L.build_neg e' "tmp" llbuilder
            | A.Not     -> L.build_not e' "tmp" llbuilder
          )
          and num_unops op e' = (match op with
              A.Neg     -> L.build_fneg e' "tmp" llbuilder )
          in
          if ((L.type_of e' = f_t))
          then num_unops op e'
          else int_unops op e'

      | A.Postop (e, op) ->
          let e' = expr_generator llbuilder e
          and vali' = L.const_int i32_t 1
          and valn' = L.const_float f_t 1.0
          in

          let num_postops e' op = (match op with
            A.Incr     -> L.build_fadd e' valn' "tmp" llbuilder
          | A.Decr    -> L.build_fsub e' valn' "tmp" llbuilder
          | _         -> raise (Failure("unsupported operation for numbers"))
          )
          and int_postops e' op = (match op with
            A.Incr     -> L.build_add e' vali' "tmp" llbuilder
          | A.Decr    -> L.build_sub e' vali' "tmp" llbuilder
          | _         -> raise (Failure("unsupported operation for numbers"))
          ) in

          if ((L.type_of e' = f_t))
          then num_postops e' op
          else int_postops e' op
      | A.Assign (t, s, e) ->
          let _ = (match func_decl.A.fname with
            "main" ->
              let init = (match t with
                A.Int   -> expr_generator llbuilder (A.IntLit(0))
              | A.Num -> expr_generator llbuilder (A.NumLit(0.0))
              | A.String -> expr_generator llbuilder (A.StringLit(""))
              | A.Bool   -> expr_generator llbuilder (A.BoolLit(false))
              | t -> L.const_null (ltype_of_typ t)
            )
          in
                (global_vars := StringMap.add s (t, (L.define_global s init the_module)) !global_vars)
            | _ -> (local_vars := StringMap.add s (t, (L.build_alloca (ltype_of_typ t)) s llbuilder) !local_vars)
            ) in
          let e' = expr_generator llbuilder e and llval = lookup s in
          ignore (L.build_store e' llval llbuilder); e'
      | A.Reassign (s, e) ->
          let e' = expr_generator llbuilder e and llval = lookup s in
          ignore (L.build_store e' llval llbuilder); e'
      | A.ArrayCreate(typ, size) -> let len = L.const_int i32_t size in
        create_array typ len llbuilder
(*           let t = ltype_of_typ typ in

          let size = (L.const_int i32_t size) in

          let size_t = L.build_intcast (L.size_of t) i32_t "1tmp" llbuilder in

          let size = L.build_mul size_t size "2tmp" llbuilder in  (* size * length *)
          let size_real = L.build_add size (L.const_int i32_t 1) "arr_size" llbuilder in

          let arr = L.build_array_malloc t size_real "3tmp" llbuilder in
          let arr = L.build_pointercast arr (L.pointer_type t) "4tmp" llbuilder in

          let arr_len_ptr = L.build_pointercast arr (L.pointer_type i32_t) "5tmp" llbuilder in

          ignore(L.build_store size_real arr_len_ptr llbuilder); 
          initialise_array arr_len_ptr size_real (L.const_int i32_t 0) 0 llbuilder;
          arr *)
      | A.ArrayLit el -> let t = gen_type (List.nth el 0) in
        initialize_array t (List.map (expr_generator llbuilder) el) llbuilder
      | A.ArrayAccess (s, i) ->
          (* let index = expr_generator llbuilder i in *)
          let llval = lookup s in
          access_array llval (L.const_int i32_t i) false llbuilder
      | A.FuncCall("print", [e]) ->
          let e' = expr_generator llbuilder e in
          if L.type_of e' == ltype_of_typ A.Num
          then L.build_call printf_func [| flt_format_str ; e' |] "print" llbuilder
          else if L.type_of e' == ltype_of_typ A.String
          then L.build_call printf_func [| str_format_str ; e' |] "print" llbuilder
          else if L.type_of e' == ltype_of_typ A.Int
          then L.build_call printf_func [| int_format_str ; e' |] "print" llbuilder
          else if L.type_of e' == ltype_of_typ A.Bool
          then L.build_call printf_func [| bool_format_str ; e' |] "print" llbuilder
          else if L.type_of e' == ltype_of_typ A.Int
          then L.build_call printf_func [| int_format_str ; e' |] "print" llbuilder
        else L.build_call printf_func [| str_format_str ; e' |] "print" llbuilder
      | A.FuncCall (f, act) ->
          let (fdef, func_decl) = StringMap.find f function_decls in
          let actuals = List.rev (List.map (expr_generator llbuilder) (List.rev act)) in
          let result =
            (match func_decl.A.typ with
              A.Void -> ""
            | _ -> f ^ "_result")
          in
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
