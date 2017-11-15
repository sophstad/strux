(* Abstract Syntax Tree and functions for printing it *)

type op = Add | Sub | Mult | Div | Mod | Equal | Neq | Less | Leq | Greater | Geq |
          And | Or | Incr | Decr

type uop = Neg | Not

type typ = Num | Int | String | Bool | Void | Arraytype of typ
(* | Array of typ * num | Stack | Queue | LinkedList | ListNode | BSTree | TreeNode *)

type bind = typ * string

(* and vdecl = typ * string * expr *)

type expr =
    NumLit of float
  | IntLit of int
  | StringLit of string
  | BoolLit of bool
  | Null
  | Id of string
  | Binop of expr * op * expr
  | Unop of uop * expr
  | Postop of expr * op
  | Assign of typ * string * expr
  | Reassign of string * expr
  | FuncCall of string * expr list
  (* | ArrayCreate of typ * expr list
  | ArrayAccess of expr * expr list
  | StackCreate of typ * expr list
  | QueueCreate of typ * expr list
  | LinkedListCreate of typ * expr list
  | BSTreeCreate of typ * expr list
  | Null *)
  | Noexpr
  | ArrayCreate of typ * expr
  | ArrayAccess of expr * expr


type stmt =
    Block of stmt list
  | Expr of expr
  | Return of expr
  | If of expr * stmt * stmt
  | For of expr * expr * expr * stmt
  | ForEach of typ * expr * expr * stmt (* Is typ correct here? *)
  | While of expr * stmt

type func_decl = {
    typ : typ;
    fname : string;
    formals : bind list;
    body : stmt list;
  }

type program = bind list * func_decl list

(* Pretty-printing functions *)

let string_of_op = function
    Add -> "+"
  | Sub -> "-"
  | Mult -> "*"
  | Div -> "/"
  | Mod -> "%"
  | Equal -> "=="
  | Neq -> "!="
  | Less -> "<"
  | Leq -> "<="
  | Greater -> ">"
  | Geq -> ">="
  | And -> "and"
  | Or -> "or"
  | Incr -> "++"
  | Decr -> "--"
(* Does :: belong here? *)

let string_of_uop = function
    Neg -> "-"
  | Not -> "not"

let rec string_of_typ = function
    Num -> "num"
  | Int -> "int"
  | String -> "string"
  | Bool -> "bool"
  | Void -> "void"
  | Arraytype(t) -> "array of " ^ string_of_typ t
  (* | Array -> "array"
  | Stack -> "Stack"
  | Queue -> "Queue"
  | LinkedList -> "LinkedList"
  | ListNode -> "ListNode"
  | BSTree -> "BSTree"
  | TreeNode -> "TreeNode" *)

let rec string_of_expr = function
    StringLit(s) -> s
  | NumLit(f) -> string_of_float f
  | IntLit(i) -> string_of_int i
  | BoolLit(true) -> "true"
  | BoolLit(false) -> "false"
  | Id(s) -> s
  | Binop(e1, o, e2) ->
      string_of_expr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_expr e2
  | Unop(o, e) -> string_of_uop o ^ string_of_expr e
  | Postop(e, o) -> string_of_expr e ^ string_of_op o
  | Assign(t, v, e) -> string_of_typ t ^ " " ^ v ^ " = " ^ string_of_expr e
  | Reassign(v, e) -> v ^ "=" ^ string_of_expr e
  | FuncCall(f, el) ->
      f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
  | ArrayCreate(typ, len) -> "(array of type " ^ string_of_typ typ ^ " with length " ^ string_of_expr len ^ ")"
  | ArrayAccess(arrayName, index) -> "(array name: " ^ string_of_expr arrayName ^ " index: " ^ string_of_expr index ^ ")"
  | Noexpr -> ""

let rec string_of_stmt = function
    Block(stmts) ->
      "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
  | Expr(expr) -> string_of_expr expr ^ ";\n";
  | Return(expr) -> "return " ^ string_of_expr expr ^ ";\n";
  (* TODO: implement elif *)
  | If(e, s, Block([])) -> "if (" ^ string_of_expr e ^ ")\n" ^ string_of_stmt s
  | If(e, s1, s2) ->  "if (" ^ string_of_expr e ^ ")\n" ^
      string_of_stmt s1 ^ "else\n" ^ string_of_stmt s2
  | For(e1, e2, e3, s) ->
      "for (" ^ string_of_expr e1  ^ " ; " ^ string_of_expr e2 ^ " ; " ^
      string_of_expr e3  ^ ") " ^ string_of_stmt s
  | ForEach(t, e1, e2, s) ->
      "forEach (" ^ string_of_typ t ^ " " ^ string_of_expr e1 ^ " in " ^
      string_of_expr e2 ^ ") " ^ string_of_stmt s
  | While(e, s) -> "while (" ^ string_of_expr e ^ ") " ^ string_of_stmt s

let string_of_vdecl (t, id) = string_of_typ t ^ " " ^ id ^ ";\n"

let string_of_fdecl fdecl =
  string_of_typ fdecl.typ ^ " " ^
  fdecl.fname ^ "(" ^ String.concat ", " (List.map snd fdecl.formals) ^
  ")\n{\n" ^
  String.concat "" (List.map string_of_stmt fdecl.body) ^
  "}\n"

let string_of_program (vars, funcs) =
  String.concat "" (List.map string_of_vdecl vars) ^ "\n" ^
  String.concat "\n" (List.map string_of_fdecl funcs)
