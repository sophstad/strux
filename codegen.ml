(* Code generation: translate takes a semantically checked AST and
produces LLVM IR
*)
open Llvm

module L = Llvm
module A = Ast

module StringMap = Map.Make(String)

let context = L.global_context ()
let queuem = L.MemoryBuffer.of_file "queue.bc" 
let listm = L.MemoryBuffer.of_file "linkedlist.bc" 
let stackm = L.MemoryBuffer.of_file "stack.bc" 
let qqm = Llvm_bitreader.parse_bitcode context queuem 
let list_qm = Llvm_bitreader.parse_bitcode context listm 
let stack_qm = Llvm_bitreader.parse_bitcode context stackm 
let the_module = L.create_module context "Strux"
and f_t    = L.double_type context  (* float *)
and i8_t   = L.i8_type   context    (* print type *)
and i1_t   = L.i1_type   context    (* bool type *)
and void_t = L.void_type context    (* void type *)
and str_t  = L.pointer_type (L.i8_type context) (* string *)
and i32_t  = L.i32_type  context
and queue_t = L.pointer_type (match L.type_by_name qqm "struct.Queue" with
    None -> raise (Invalid_argument "Option.get queue") | Some x -> x)
and stack_t = L.pointer_type (match L.type_by_name stack_qm "struct.Stack" with
    None -> raise (Invalid_argument "Option.get stack") | Some x -> x) 
and linkedlist_t = L.pointer_type (match L.type_by_name list_qm "struct.LinkedList" with
    None -> raise (Invalid_argument "Option.get linkedlmist") | Some x -> x) ;;

let rec ltype_of_typ = function (* LLVM type for AST type *)
    A.Num -> f_t
  | A.Int -> i32_t
  | A.String -> str_t
  | A.Bool -> i1_t
  | A.Void -> void_t
  | A.Arraytype(t) -> L.pointer_type (ltype_of_typ t)
  | A.QueueType _ -> queue_t
  | A.LinkedListType _ -> linkedlist_t
  | A.StackType _ -> stack_t
  | A.AnyType -> str_t 
  | _ -> raise(Failure("Invalid Data Type"))
  (* | A.Stack -> f_t
    | A.Queue -> f_t
    | A.LinkedList -> f_t
    | A.ListNode -> f_t
    | A.BSTree -> f_t
    | A.TreeNode -> f_t *)

and translate (globals, functions) =
  let global_types =
    let global_type m (t, n) = StringMap.add n t m in
    List.fold_left global_type StringMap.empty globals
  in
  (* Declare each global variable; remember its value in a map *)
  let global_vars = ref StringMap.empty in

  (* -------BUILT IN FUNCTIONS----------- *)

  (* Declare our print function here *)


  (* print *)
  let printf_t = L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
  let printf_func = L.declare_function "printf" printf_t the_module in

  (* built-in queue functions *)
  let initQueue_t = L.function_type queue_t [| |] in
  let initQueue_f = L.declare_function "initQueue" initQueue_t the_module in
  let enqueue_t = L.function_type void_t [| queue_t; L.pointer_type i8_t|] in
  let enqueue_f = L.declare_function "enqueue" enqueue_t the_module in
  let dequeue_t = L.function_type void_t [| queue_t |] in
  let dequeue_f = L.declare_function "dequeue" dequeue_t the_module in
  let peek_t = L.function_type (L.pointer_type i8_t) [| queue_t |] in
  let peek_f = L.declare_function "peek" peek_t the_module in
  let sizeQ_t = L.function_type i32_t [| queue_t |] in 
  let sizeQ_f = L.declare_function "queue_size" sizeQ_t the_module in 
  let q_show_t = L.function_type void_t [| queue_t |] in 
  let q_show_int = L.declare_function "queue_show_int" q_show_t the_module in
  let q_show_float = L.declare_function "queue_show_float" q_show_t the_module in
  let q_show_string = L.declare_function "queue_show_string" q_show_t the_module in

  (*built-in linkedlist functions*)
  let initList_t = L.function_type linkedlist_t [| |] in
  let initList_f = L.declare_function "initList" initList_t the_module in
  let add_t = L.function_type void_t [| linkedlist_t; L.pointer_type i8_t|] in
  let add_f = L.declare_function "add" add_t the_module in
  let delete_t = L.function_type void_t [| linkedlist_t; i32_t |] in
  let delete_f = L.declare_function "delete" delete_t the_module in
  let get_t = L.function_type (L.pointer_type i8_t) [| linkedlist_t; i32_t |] in
  let get_f = L.declare_function "get" get_t the_module in
  let sizeList_t = L.function_type i32_t [| linkedlist_t |] in 
  let sizeList_f = L.declare_function "size" sizeList_t the_module in 
  let l_show_t = L.function_type void_t [| linkedlist_t |] in 
  let l_show_int = L.declare_function "ll_show_int" l_show_t the_module in
  let l_show_float = L.declare_function "ll_show_float" l_show_t the_module in
  let l_show_string = L.declare_function "ll_show_string" l_show_t the_module in

  (*built-in stack functions*)
  let initStack_t = L.function_type stack_t [| |] in 
  let initStack_f = L.declare_function "initStack" initStack_t the_module in
  let push_t = L.function_type void_t [| stack_t; L.pointer_type i8_t|] in
  let push_f = L.declare_function "push" push_t the_module in
  let pop_t = L.function_type void_t [| stack_t |] in
  let pop_f = L.declare_function "pop" pop_t the_module in
  let top_t = L.function_type (L.pointer_type i8_t) [| stack_t |] in
  let top_f = L.declare_function "top" top_t the_module in
  let sizeS_t = L.function_type i32_t [| stack_t |] in
  let sizeS_f = L.declare_function "stack_size" sizeS_t the_module in
  let s_show_t = L.function_type void_t [| stack_t |] in 
  let s_show_int = L.declare_function "stack_show_int" s_show_t the_module in
  let s_show_float = L.declare_function "stack_show_float" s_show_t the_module in
  let s_show_string = L.declare_function "stack_show_string" s_show_t the_module in


  (* quicksort array functions *)
  let cQuickSort_t = L.function_type (L.pointer_type (ltype_of_typ A.Int)) [| L.pointer_type (ltype_of_typ A.Int); i32_t |] in
  let cQuickSort_f = L.declare_function "cQuickSort" cQuickSort_t the_module in
  let cShowQuickSort_t = L.function_type (L.pointer_type (ltype_of_typ A.Int)) [| L.pointer_type (ltype_of_typ A.Int); i32_t |] in
  let cShowQuickSort_f = L.declare_function "cShowQuickSort" cShowQuickSort_t the_module in
  let cQuickfSort_t = L.function_type (L.pointer_type (ltype_of_typ A.Num)) [| L.pointer_type (ltype_of_typ A.Num); i32_t |] in
  let cQuickfSort_f = L.declare_function "cQuickfSort" cQuickfSort_t the_module in
  let cShowfQuickSort_t = L.function_type (L.pointer_type (ltype_of_typ A.Num)) [| L.pointer_type (ltype_of_typ A.Num); i32_t |] in
  let cShowfQuickSort_f = L.declare_function "cShowfQuickSort" cShowfQuickSort_t the_module in

  (*print big *)
  let printbig_t = L.function_type i32_t [| i32_t |] in
  let printbig_func = L.declare_function "printbig" printbig_t the_module in

  (* Define each function (arguments and return type) so we can call it *)
  let function_decls =
    let function_decl m func_decl =
      let name = func_decl.A.fname
      and formal_types =
	Array.of_list (List.map (fun (t,_) -> ltype_of_typ t) func_decl.A.formals)
      in let ftype = L.function_type (ltype_of_typ func_decl.A.typ) formal_types in
      StringMap.add name (L.define_function name ftype the_module, func_decl) m in
    List.fold_left function_decl StringMap.empty functions in

(*   let data_structures_str x_type b =
    let b = builder in
    let n = idtostring q in
    let q_type = getQueueType (lookup_types n)
    format_str q_type builder
    in  *)
  (* Format str for printf *)
  let string_format_str b = L.build_global_stringptr "%s\n" "fmt" b
  and int_format_str    b = L.build_global_stringptr "%d\n" "fmt" b
  and float_format_str  b = L.build_global_stringptr "%f\n" "fmt" b in

  let format_str x_type builder =
    let b = builder in
      match x_type with
        A.Int      -> int_format_str b
      | A.Num    -> float_format_str b
      | A.String  -> string_format_str b
      | A.Bool     -> int_format_str b
      (*TODO: fix this!!!!!!!!!*)
(*       | A.QueueType _ -> float_format_str b
      | A.LinkedListType _ -> float_format_str b
      | A.StackType _ -> float_format_str b *)
      | _ -> raise (Failure ("Invalid printf type"))
  in

    (* Fill in the body of the given function *)
    let build_function_body func_decl =
      let (the_function, _) = StringMap.find func_decl.A.fname function_decls in
      let llbuilder = L.builder_at_end context (L.entry_block the_function) in

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

    let local_types =
      let add_type m (t, n) = StringMap.add n t m in
      let formal_types = List.fold_left add_type StringMap.empty func_decl.A.formals in
          List.fold_left add_type formal_types func_decl.A.formals in

      (* Return the value or the type for a variable or formal argument *)
      (* All the tables have the structure (type, llvalue) *)
      let lookup n : L.llvalue =
        try (snd (StringMap.find n !local_vars))
        with Not_found -> (snd (StringMap.find n !global_vars))
      in

      let lookup_types n = try StringMap.find n global_types
        with Not_found -> StringMap.find n global_types
      in

      let name_to_type n : A.typ =
        try (fst (StringMap.find n !local_vars))
        with Not_found -> (fst (StringMap.find n !global_vars))
      in
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
      | A.ArrayElementAssign (_, _, el) -> gen_type el
      | A.Id name -> (match (name_to_type name) with
                      A.Arraytype(t) -> t
                    | _ as ty -> ty)
      | A.Unop(_,e) -> gen_type e
      | A.Binop(e1,_,_) -> gen_type e1
      | A.Postop(e, _) -> gen_type e
      | A.Assign(_,var,_) -> gen_type (A.Id(var))
      | A.Reassign(var,_) -> gen_type (A.Id(var))
      | A.FuncCall(var,_) -> let fdecl =
                            List.find (fun x -> x.A.fname = var) functions in
                            fdecl.A.typ
      | A.ArrayAccess(id,_) -> gen_type (A.Id(id))
      | A.Noexpr -> A.Void
    in

    let get_type = function 
      A.Id name -> (match (name_to_type name) with
        A.QueueType(typ) -> typ
      | A.LinkedListType(typ) -> typ
      | A.StackType(typ) -> typ
      | _ as typ -> typ)
    in 

    let call_size_ptr = function
      A.Id name -> (match (name_to_type name) with
        A.QueueType _ -> sizeQ_f
      | A.LinkedListType _ -> sizeList_f
      | A.StackType _ -> sizeS_f
      | _ -> raise (Failure ("Invalid data structure type - size function")))
    in

    let call_add_ptr = function
      A.Id name -> (match (name_to_type name) with
        A.QueueType _ -> enqueue_f
      | A.LinkedListType _ -> add_f
      | A.StackType _ -> push_f
      | _ -> raise (Failure ("Invalid data structure type - add function")))
    in

    let call_pop_ptr = function
      A.Id name -> (match (name_to_type name) with
        A.QueueType _ -> dequeue_f
      | A.StackType _ -> pop_f
      | _ -> raise (Failure ("Invalid data structure type - delete function")))
    in

  (*   let q_type_show ds_type = function  
       A.Int  -> q_show_int
      | A.Num  -> q_show_float 
      | A.String  -> q_show_string
    in 

    let s_type_show ds_type = function 
      A.Int -> s_show_int
      | A.Num -> s_show_float 
      | A.String -> s_show_string
    in 

    let l_type_show ds_type = function 
      A.Int -> l_show_int
      | A.Num -> l_show_float 
      | A.String  -> l_show_string
    in 

    let call_show_ptr obj_val ds_type = function
      A.Id name -> (match (name_to_type name) with
        A.QueueType _ -> q_type_show ds_type
      | A.StackType _ -> s_type_show ds_type
      | A.LinkedListType _ -> l_type_show ds_type
      | _ -> raise (Failure ("Invalid data structure type - show function")))
    in *)

    let call_show_ptr ds_type = function
      A.Id name -> (match (name_to_type name) with
        A.QueueType _ -> (match ds_type with
           A.Int -> q_show_int
         | A.Num -> q_show_float 
         | A.String -> q_show_string)
      | A.StackType _ -> (match ds_type with
           A.Int -> s_show_int
         | A.Num -> s_show_float 
         | A.String -> s_show_string)
      | A.LinkedListType _ -> (match ds_type with
           A.Int -> l_show_int
         | A.Num -> l_show_float 
         | A.String -> l_show_string)
      | _ -> raise (Failure ("Invalid data structure type - show function")))
    in 


    let call_peek_ptr = function
      A.Id name -> (match (name_to_type name) with
        A.QueueType _ -> peek_f
      | A.StackType _ -> top_f
      | _ -> raise (Failure ("Invalid data structure type - peek function")))
    in

    let rec expr_generator llbuilder = function
        A.NumLit(n) -> L.const_float f_t n
      | A.IntLit(i) -> L.const_int i32_t i
      | A.BoolLit(b) -> L.const_int i1_t (if b then 1 else 0)
      | A.StringLit(s) -> L.build_global_stringptr s "string" llbuilder
      | A.Id s -> L.build_load (lookup s) s llbuilder
      | A.QueueLit (typ, act) ->
        let d_ltyp = ltype_of_typ typ in
        let queue_ptr = L.build_call initQueue_f [| |] "init" llbuilder in
        let add_element elem =
          let d_ptr = match typ with
          | A.QueueType _ -> expr_generator llbuilder elem
          | _ ->
            let element = expr_generator llbuilder elem in
            let d_ptr = L.build_malloc d_ltyp "tmp" llbuilder in
            ignore (L.build_store element d_ptr llbuilder); d_ptr in
          let void_d_ptr = L.build_bitcast d_ptr (L.pointer_type i8_t) "ptr" llbuilder in
          ignore (L.build_call enqueue_f [| queue_ptr; void_d_ptr |] "" llbuilder)
        in ignore (List.map add_element act);
        queue_ptr
      | A.LinkedListLit (typ, act) ->
        let d_ltyp = ltype_of_typ typ in
        let list_ptr = L.build_call initList_f [| |] "init" llbuilder in
        let add_element elem =
          let d_ptr = match typ with
          | A.LinkedListType _ -> expr_generator llbuilder elem
          | _ ->
            let element = expr_generator llbuilder elem in
            let d_ptr = L.build_malloc d_ltyp "tmp" llbuilder in
            ignore (L.build_store element d_ptr llbuilder); d_ptr in
          let void_d_ptr = L.build_bitcast d_ptr (L.pointer_type i8_t) "ptr" llbuilder in
          ignore (L.build_call add_f [| list_ptr; void_d_ptr |] "" llbuilder)
        in ignore (List.map add_element act);
        list_ptr
      | A.StackLit (typ, act) ->
        let d_ltyp = ltype_of_typ typ in
        let stack_ptr = L.build_call initStack_f [| |] "init" llbuilder in 
        let add_element elem = 
          let d_ptr = match typ with 
          | A.StackType _ -> expr_generator llbuilder elem 
          | _ -> 
            let element = expr_generator llbuilder elem in 
            let d_ptr = L.build_malloc d_ltyp "tmp" llbuilder in 
            ignore (L.build_store element d_ptr llbuilder); d_ptr in 
          let void_d_ptr = L.build_bitcast d_ptr (L.pointer_type i8_t) "ptr" llbuilder in
          ignore (L.build_call push_f [| stack_ptr; void_d_ptr |] "" llbuilder)
        in ignore (List.map add_element act);
        stack_ptr
      | A.Binop (e1, op, e2) ->
        let e1' = expr_generator llbuilder e1 in
        let e2' = expr_generator llbuilder e2
        and num_ops = (match op with
            A.Add     -> L.build_fadd
          | A.Sub     -> L.build_fsub
          | A.Mult    -> L.build_fmul
          | A.Div     -> L.build_fdiv
          | A.Mod     -> L.build_frem
          | A.And     -> L.build_and
          | A.Or      -> L.build_or
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
          | A.And     -> L.build_and
          | A.Or      -> L.build_or
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
          let _ = (local_vars := StringMap.add s (t, (L.build_alloca (ltype_of_typ t)) s llbuilder) !local_vars) in
          let e' = expr_generator llbuilder e and llval = lookup s in
          ignore (L.build_store e' llval llbuilder); e'
      | A.Reassign (s, e) ->
          let e' = expr_generator llbuilder e and llval = lookup s in
          ignore (L.build_store e' llval llbuilder); e'
      | A.ArrayLit el -> let t = gen_type (List.nth el 0) in
          initialize_array t (List.map (expr_generator llbuilder) el) llbuilder
      | A.ArrayAccess (s, i) ->
          let index = expr_generator llbuilder i and llval = lookup s in
          (* if (List.length llval < index) || (index < 0) then raise (Failure ("Array index out of bounds")) else *)
          access_array llval index false llbuilder
      | A.ArrayElementAssign (s, i, e) ->
          let e' = expr_generator llbuilder e in
          let index = expr_generator llbuilder i in
          let llval = lookup s in
          let var = access_array llval index true llbuilder in
          ignore (L.build_store e' var llbuilder); e'
      | A.FuncCall("print", [e]) ->
          let e' = expr_generator llbuilder e in
          let e_type = gen_type e in
          L.build_call printf_func [| (format_str e_type llbuilder) ; e' |] "printf" llbuilder
     | A.FuncCall ("printbig", [e]) ->
          L.build_call printbig_func [| (expr_generator llbuilder e) |] "printbig" llbuilder
      | A.FuncCall (f, act) ->
          let (fdef, func_decl) = StringMap.find f function_decls in
          let actuals = List.rev (List.map (expr_generator llbuilder) (List.rev act)) in
          let result =
            (match func_decl.A.typ with
              A.Void -> ""
            | _ -> f ^ "_result")
          in
          L.build_call fdef (Array.of_list actuals) result llbuilder

      | A.ObjectCall (obj, "remove", []) ->
        let obj_val = expr_generator llbuilder obj in
        let obj_method = call_pop_ptr obj in
        ignore (L.build_call obj_method [| obj_val|] "" llbuilder); obj_val  
      | A.ObjectCall (obj, "peek", []) -> 
        let obj_val = expr_generator llbuilder obj in
        let obj_type = get_type obj in 
        let obj_method = call_peek_ptr obj in
        let val_ptr = L.build_call obj_method [| obj_val |] "val_ptr" llbuilder in
        let dtyp = ltype_of_typ obj_type in
        let ptr = L.build_bitcast val_ptr (L.pointer_type dtyp) "d_ptr" llbuilder in
        (L.build_load ptr "d_ptr" llbuilder)
      | A.ObjectCall (obj, "show", []) -> 
        let obj_val = expr_generator llbuilder obj in
        let obj_type = get_type obj in 
        let obj_method = call_show_ptr obj_type obj in
        ignore (L.build_call obj_method [| obj_val |] "" llbuilder); obj_val
(*  
        let q_type = get_type q in 
        (match q_type with 
        A.Int -> ignore (L.build_call show_int [| q_val|] "" llbuilder); q_val 
       | A.Num -> ignore (L.build_call show_float [| q_val|] "" llbuilder); q_val 
       | A.String -> ignore (L.build_call show_string [| q_val|] "" llbuilder); q_val) *)
      | A.ObjectCall (obj, "size", []) ->
        let e = expr_generator llbuilder obj in
        let obj_size = call_size_ptr obj in
        let size_ptr = L.build_call obj_size [| e |] "isEmpty" llbuilder in
        size_ptr
      | A.ObjectCall (obj, "add", [e]) ->
        let obj_val = expr_generator llbuilder obj in
        let e_val = expr_generator llbuilder e in 
        let d_ltyp = L.type_of e_val in 
        let d_ptr = L.build_malloc d_ltyp "tmp" llbuilder in 
        ignore(L.build_store e_val d_ptr llbuilder); 
        let obj_method = call_add_ptr obj in
        let void_e_ptr = L.build_bitcast d_ptr (L.pointer_type i8_t) "ptr" llbuilder in 
        ignore (L.build_call obj_method [| obj_val; void_e_ptr|] "" llbuilder); obj_val
      | A.ObjectCall (obj, "delete", [e]) -> 
        let obj_val = expr_generator llbuilder obj in 
        let e_val = expr_generator llbuilder e in
        ignore (L.build_call delete_f [| obj_val; e_val |] "" llbuilder);
        obj_val
      | A.ObjectCall (l, "get", [e]) ->
        let l_ptr = expr_generator llbuilder l in
        let e_val = expr_generator llbuilder e in
        let l_type = get_type l in 
        let val_ptr = L.build_call get_f [| l_ptr; e_val |] "val_ptr" llbuilder in
        let l_dtyp = ltype_of_typ l_type in
        let d_ptr = L.build_bitcast val_ptr (L.pointer_type l_dtyp) "d_ptr" llbuilder in
        (L.build_load d_ptr "d_ptr" llbuilder)
      | A.ObjectCall(a, "quickSort", [e]) ->
        let a_val = expr_generator llbuilder a in
        let e_val = expr_generator llbuilder e in
        ignore (L.build_call cQuickSort_f [| a_val; e_val|] "" llbuilder); a_val in 
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
