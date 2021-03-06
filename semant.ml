(* Semantic checking for the MicroC compiler *)

open Ast

module StringMap = Map.Make(String)

(* Semantic checking of a program. Returns void if successful,
   throws an exception if something is wrong. Checks each function *)

let check (functions) =

  (* Raise an exception if the given list has a duplicate *)
  let report_duplicate exceptf list =
    let rec helper = function
         n1 :: n2 :: _ when n1 = n2 -> raise (Failure (exceptf n1))
      | _ :: t -> helper t
      | [] -> ()
    in helper (List.sort compare list)
  in

  (* Raise an exception if a given binding is to a void type *)
  let check_not_void exceptf = function
      (Void, n) -> raise (Failure (exceptf n))
    | _ -> ()
  in

  (* Raise an exception of the given rvalue type cannot be assigned to
     the given lvalue type *)
  let check_assign lvaluet rvaluet err =
    if lvaluet = rvaluet then rvaluet
    else if lvaluet = Num && rvaluet = AnyType then lvaluet
    else if lvaluet = Int && rvaluet = AnyType then lvaluet
    else if lvaluet = String && rvaluet = AnyType then lvaluet
    else if lvaluet = Bool && rvaluet = AnyType then lvaluet
    else if lvaluet = Num && rvaluet = NumberType then lvaluet
    else if lvaluet = Int && rvaluet = NumberType then lvaluet
    else raise err
  in

  (**** Checking Functions ****)
  if List.mem "print" (List.map (fun fd -> fd.fname) functions)
  then raise (Failure ("function print may not be defined")) else ();

  if List.mem "delete" (List.map (fun fd -> fd.fname) functions)
  then raise (Failure ("function delete may not be defined")) else ();

  report_duplicate (fun n -> "duplicate function " ^ n)
    (List.map (fun fd -> fd.fname) functions);

  (* Function declaration for a named function *)
  let built_in_decls =  StringMap.add "print"
     { typ = Void; fname = "print"; formals = [(Num, "x")];
       body = [] }

       (StringMap.add "printb"
     { typ = Void; fname = "printb"; formals = [(Bool, "x")];
       body = [] }

        (StringMap.add "add"
    { typ = Void; fname = "add"; formals = [(AnyType, "x")];
        body = [] }

        (StringMap.add "show"
    { typ = Void; fname = "show"; formals = [];
        body = [] }

        (StringMap.add "peek"
    { typ = AnyType; fname = "peek"; formals = [];
        body = [] }

        (StringMap.add "remove"
    { typ = Void; fname = "remove"; formals = [];
        body = [] }

        (StringMap.add "get"
    { typ = AnyType; fname = "get"; formals = [(Int, "x")];
        body = [] }

        (StringMap.add "size"
     { typ = Int; fname = "size"; formals = [];
        body = [] }

        (StringMap.add "delete"
     { typ = Void; fname = "delete"; formals = [(NumberType, "x")];
        body = [] }

        (StringMap.add "contains"
     { typ = Bool; fname = "contains"; formals = [(NumberType, "x")];
        body = [] }

        (StringMap.add "fquickSort"
     { typ = Void; fname = "fquickSort"; formals = [];
       body = [] }

        (StringMap.add "fshowQuickSort"
     { typ = Void; fname = "fshowQuickSort"; formals = [];
       body = [] }

        (StringMap.add "quickSort"
     { typ = Void; fname = "quickSort"; formals = [];
       body = [] }

         (StringMap.singleton "showQuickSort"
      { typ = Void; fname = "showQuickSort"; formals = [];
        body = [] }

     )))))))))))))

   in

  let function_decls = List.fold_left (fun m fd -> StringMap.add fd.fname fd m)
                         built_in_decls functions
  in

  let function_decl s = try StringMap.find s function_decls
       with Not_found -> raise (Failure ("unrecognized function " ^ s))
  in

  let _ = function_decl "main" in (* Ensure "main" is defined *)

  let check_function func =

    List.iter (check_not_void (fun n -> "illegal void formal " ^ n ^
      " in " ^ func.fname)) func.formals;

    report_duplicate (fun n -> "duplicate formal " ^ n ^ " in " ^ func.fname)
      (List.map snd func.formals);

    (* Store type and names of variables - formal *)
    let variables = ref (List.fold_left (fun m (t, n) -> StringMap.add n t m)
    StringMap.empty (func.formals))
    in

    (* Helper: Returns type of identifier *)
    let type_of_identifier name =
      try StringMap.find name (!variables)
      with Not_found -> raise (Failure ("undeclared identifier " ^ name))
    in

    (* Helper: Check if variable is already declared *)
    let check_var_decl var_name err =
      if StringMap.mem var_name (!variables)
      then raise err
    in

    let array_typ = function
        Arraytype(typ, _) -> typ
      | _ -> raise(Failure("Expecting an array and was not an array"))
    in

    let arr_lit_len = function
        ArrayLit(el) -> List.length el
      | _ -> -1
    in

    let invalid_arr_size s e =
      let literal_len = arr_lit_len e in
      if literal_len == -1
      then false
      else
        let decl_len = (match s with
                          Arraytype(_, len) -> len) in
        if decl_len == literal_len then false else true
    in

    let get_type = function
        QueueType(typ) -> typ
      | LinkedListType(typ) -> typ
      | StackType(typ) -> typ
      | BSTreeType(typ) -> typ
      | _ -> Void  
    in

    (* Return the type of an expression or throw an exception *)
    let rec expr = function
        NumLit _ -> Num
      | IntLit _ -> Int
      | StringLit _ -> String
      | QueueLit (t, _) -> QueueType(t)
      | LinkedListLit (t, _) -> LinkedListType(t)
      | StackLit (t, _) -> StackType(t)
      | BSTreeLit (t, _) -> BSTreeType(t)
      | BoolLit _ -> Bool
      | Id s -> type_of_identifier s
      | Binop(e1, op, e2) as e -> let t1 = expr e1 and t2 = expr e2 in
        (match op with
          Add | Sub | Mult | Div | Mod when t1 = Int && t2 = Int -> Int
        | Add | Sub | Mult | Div | Mod when t1 = Num && t2 = Num -> Num
        | Equal | Neq when t1 = t2 -> Bool
        | Less | Leq | Greater | Geq when t1 = Int && t2 = Int -> Bool
        | Less | Leq | Greater | Geq when t1 = Num && t2 = Num -> Bool
        | And | Or when t1 = Bool && t2 = Bool -> Bool
        | _ -> raise (Failure ("illegal binary operator " ^
              string_of_typ t1 ^ " " ^ string_of_op op ^ " " ^
              string_of_typ t2 ^ " in " ^ string_of_expr e))
        )
      | Postop (e, op) ->
          let t1 = expr e in
          (match op with
            Incr when t1 = Int -> Int
          | Decr when t1 = Int -> Int
          | Incr when t1 = Num -> Num
          | Decr when t1 = Num -> Num
          | _ -> raise (Failure("illegal unary operator " ^
                string_of_op op ^ " on " ^ string_of_expr e)))
      | Unop(op, e) as ex -> let t = expr e in
        (match op with
          Neg when t = Num -> Num
        | Neg when t = Int -> Int
        | Not when t = Bool -> Bool
        | _ -> raise (Failure ("illegal unary operator " ^ string_of_uop op ^
              string_of_typ t ^ " in " ^ string_of_expr ex)))
      | Noexpr -> Void
      | Assign(typ, var, e) as ex ->
          let lt = (match typ with
            Arraytype(t, _) -> t
          | _ -> typ
          ) in
          let rt = expr e
          and invalid_arr = invalid_arr_size typ e in
          if invalid_arr then raise (Failure ("Invalid length declaration"))
          else
          if rt == Void then raise (Failure("Must initialize variable with a value."))
        else
          ignore (check_assign lt rt (Failure ("illegal assignment " ^ string_of_typ typ ^ " = " ^ string_of_typ rt ^ " in " ^ string_of_expr ex)));
          check_var_decl var (Failure ("duplicate declaration of variable " ^ var));
          let _ =
            (match func.fname with
              | _ -> variables := StringMap.add var typ (!variables)
            )
          in rt

      | Reassign(var, e) as ex ->
          let rt = expr e and lt = type_of_identifier var in
          check_assign lt rt (Failure ("illegal assignment " ^ string_of_typ lt ^ 
                              " = " ^ string_of_typ rt ^ " in " ^ string_of_expr ex))
      | FuncCall(fname, actuals) as call ->
        if fname = "print"
        then (if List.length actuals == 1
              then let arg_type = string_of_typ (expr (List.hd actuals)) in
                    if arg_type = string_of_typ (Num) ||
                       arg_type = string_of_typ (Int) ||
                       arg_type = string_of_typ (String) ||
                       arg_type = string_of_typ (Bool) ||
                       arg_type = string_of_typ (AnyType)
                    then Void
                    else raise (Failure ("illegal actual argument found in print " ^
                                string_of_typ (expr (List.hd actuals)) ^
                                " in " ^ string_of_expr (List.hd actuals)))
               else raise (Failure ("expecting 1 argument in " ^ string_of_expr call)))
        else let fd = function_decl fname in
           if List.length actuals != List.length fd.formals
           then raise (Failure ("expecting " ^ string_of_int
               (List.length fd.formals) ^ " arguments in " ^ string_of_expr call))
           else
             List.iter2 (fun (ft, _) e -> let et = expr e in
                ignore (check_assign ft et
                  (Failure ("illegal actual argument found " ^ string_of_typ et ^
                  " expected " ^ string_of_typ ft ^ " in " ^ string_of_expr e))))
               fd.formals actuals;
             fd.typ
      | ArrayLit(el) -> expr (List.nth el 0)
      | ArrayAccess(var, el) as element->
          if expr el != Int 
          then raise (Failure ("Invalid element access in " ^ string_of_expr element))
          else array_typ (type_of_identifier var)
      | ArrayElementAssign (s, i, e) as ex ->
          let lt =
            if expr i != Int 
            then raise (Failure ("invalid element access in " ^ string_of_expr ex))
            else array_typ (type_of_identifier s)
          in
          let rt = expr e in
          check_assign lt rt (Failure ("illegal assignment " ^ string_of_typ lt ^ " = " ^ string_of_typ rt ^ " in " ^ string_of_expr ex));
      | ObjectCall(oname, fname, actuals) as objectcall -> 
          let fd = function_decl fname in
          let returntype = ref (fd.typ) in
          if List.length actuals != List.length fd.formals then
            raise (Failure ("expecting " ^ string_of_int (List.length fd.formals) ^ 
                  " arguments in " ^ string_of_expr objectcall))
          else
             List.iter2 (fun (ft, _) e -> let et = expr e in
                if fname = "add" then
                   let acttype = expr oname in
                   let actqtype = get_type acttype in
                  ignore(check_assign actqtype et (Failure ("illegal actual add argument found " ^ string_of_typ et ^
                  " expected " ^ string_of_typ actqtype ^ " in " ^ string_of_expr e))) 
                else if fname = "remove" then
                   let acttype = expr oname in 
                   let actqtype = get_type acttype in 
                  ignore(check_assign actqtype et (Failure ("illegal actual remove argument found " ^ string_of_typ et ^
                  " expected " ^ string_of_typ actqtype ^ " in " ^ string_of_expr e))) 
                else if fname = "quickSort" then
                  let acttype = expr oname in
                  let actatype = array_typ acttype in
                  ignore(check_assign actatype et (Failure ("illegal actual size argument found " ^ string_of_typ et ^
                  " expected " ^ string_of_typ actatype ^ " in " ^ string_of_expr e)))
                else if fname = "delete" then
                   let acttype = expr oname in
                   match acttype with
                   | BSTreeType(actqtype) -> ignore(check_assign actqtype et (Failure ("illegal actual delete argument found " ^ string_of_typ et ^
                  " expected " ^ string_of_typ actqtype ^ " type " ^ " in " ^ string_of_expr e)))
                   | LinkedListType(_) -> ignore(check_assign Int et (Failure ("illegal actual delete argument found " ^ string_of_typ et ^
                  " expected int type " ^ " in " ^ string_of_expr e)))

                else if fname = "contains" then
                   let acttype = expr oname in
                   match acttype with
                   | BSTreeType(actqtype) -> ignore(check_assign actqtype et (Failure ("illegal actual contains argument found " ^ string_of_typ et ^
                  " expected " ^ string_of_typ actqtype ^ " type " ^ " in " ^ string_of_expr e)))
                   | _ -> raise (Failure (".contains() is only applicable to the the tree data type"))
                  
                else ignore (check_assign ft et (Failure ("illegal actual argument found " ^ string_of_typ et ^
                      " expected " ^ string_of_typ ft ^ " in " ^ string_of_expr e)))) fd.formals actuals;
                 !returntype

    in

    let check_bool_expr e = if expr e != Bool
     then raise (Failure ("expected Boolean expression in " ^ string_of_expr e))
     else () in

    (* Verify a statement or throw an exception *)
    let rec stmt = function
        Block sl -> let rec check_block = function
           [Return _ as s] -> stmt s
         | Return _ :: _ -> raise (Failure "nothing may follow a return")
         | Block sl :: ss -> check_block (sl @ ss)
         | s :: ss -> stmt s ; check_block ss
         | [] -> ()
        in check_block sl
      | Expr e -> ignore (expr e)
      | Return e -> let t = expr e in if t = func.typ then () else
         raise (Failure ("return gives " ^ string_of_typ t ^ " expected " ^
                         string_of_typ func.typ ^ " in " ^ string_of_expr e))

      | If(p, b1, b2) -> check_bool_expr p; stmt b1; stmt b2
      | For(e1, e2, e3, st) -> ignore (expr e1); check_bool_expr e2;
                               ignore (expr e3); stmt st
      | While(p, s) -> check_bool_expr p; stmt s
    in

    stmt (Block func.body)

  in
  List.iter check_function functions
