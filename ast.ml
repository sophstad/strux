(* Abstract Syntax Tree and functions for printing it *)

type op = Add | Sub | Mult | Div | Mod | Equal | Neq | Less | Leq | Greater | Geq |
          And | Or | Incr | Decr

type uop = Neg | Not

type typ = Num | Int | String | Bool | Void | AnyType | NumberType |
           Arraytype of typ * int | QueueType of typ | LinkedListType of typ |
           StackType of typ | BSTreeType of typ

type bind = typ * string

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
  | QueueLit of typ * expr list 
  | BSTreeLit of typ * expr list 
  | LinkedListLit of typ * expr list 
  | StackLit of typ * expr list
  | ObjectCall of expr * string * expr list 
  | Noexpr
  | ArrayLit of expr list
  | ArrayAccess of string * expr
  | ArrayElementAssign of string * expr * expr

type stmt =
    Block of stmt list
  | Expr of expr
  | Return of expr
  | If of expr * stmt * stmt
  | For of expr * expr * expr * stmt
  | While of expr * stmt

type func_decl = {
    typ : typ;
    fname : string;
    formals : bind list;
    body : stmt list;
  }

type program = func_decl list

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

let string_of_uop = function
    Neg -> "-"
  | Not -> "not"

let rec string_of_typ = function
    Num -> "num"
  | Int -> "int"
  | String -> "string"
  | Bool -> "bool"
  | Void -> "void"
  | Arraytype(typ, len) -> string_of_typ typ ^ "[" ^ string_of_int len ^ "]"
  | QueueType(typ) -> "Queue " ^ string_of_typ typ 
  | BSTreeType(typ) -> "BSTree " ^ string_of_typ typ 
  | LinkedListType(typ) -> "LinkedList " ^ string_of_typ typ 
  | StackType(typ) -> "Stack " ^ string_of_typ typ 
  | AnyType -> "AnyType"
  | NumberType -> "NumberType"

let rec string_of_expr = function
    StringLit(s) -> s
  | NumLit(f) -> string_of_float f
  | IntLit(i) -> string_of_int i
  | BoolLit(true) -> "true"
  | BoolLit(false) -> "false"
  | Null -> "null"
  | Id(s) -> s
  | Binop(e1, o, e2) ->
      string_of_expr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_expr e2
  | Unop(o, e) -> string_of_uop o ^ string_of_expr e
  | Postop(e, o) -> string_of_expr e ^ string_of_op o
  | Assign(t, v, e) -> string_of_typ t ^ " " ^ v ^ " = " ^ string_of_expr e
  | Reassign(v, e) -> v ^ "=" ^ string_of_expr e
  | FuncCall(f, el) ->
      f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
  | ObjectCall(o, f, e1) -> string_of_expr o ^ "." ^ f ^ "(" ^ String.concat ", " (List.map string_of_expr e1) ^ ")"
  | ArrayLit a -> "[" ^ String.concat " " (List.map string_of_expr a) ^ "]"
  | ArrayAccess(v, i) -> v ^ "[" ^ string_of_expr i ^ "]"
  | ArrayElementAssign(s, i, e) -> s ^ "[" ^ string_of_expr i ^ "]" ^ " = " ^ string_of_expr e
  | Noexpr -> ""
  | QueueLit(typ, e1) -> "new " ^ "Queue" ^ "::" ^ string_of_typ typ ^ "(" ^ String.concat ", " (List.map string_of_expr e1) ^ ")"
  | LinkedListLit(typ, e1) -> "new " ^ "LinkedList" ^ "::" ^ string_of_typ typ ^ "(" ^ String.concat ", " (List.map string_of_expr e1) ^ ")"
  | BSTreeLit(typ, e1) -> "new " ^ "BSTree" ^ "::" ^ string_of_typ typ ^ "(" ^ String.concat ", " (List.map string_of_expr e1) ^ ")"
  | StackLit(typ, e1) -> "new " ^ "Stack" ^ "::" ^ string_of_typ typ ^ "(" ^ String.concat ", " (List.map string_of_expr e1) ^ ")"

let rec string_of_stmt = function
    Block(stmts) ->
      "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
  | Expr(expr) -> string_of_expr expr ^ ";\n";
  | Return(expr) -> "return " ^ string_of_expr expr ^ ";\n";
  | If(e, s, Block([])) -> "if (" ^ string_of_expr e ^ ")\n" ^ string_of_stmt s
  | If(e, s1, s2) ->  "if (" ^ string_of_expr e ^ ")\n" ^
      string_of_stmt s1 ^ "else\n" ^ string_of_stmt s2
  | For(e1, e2, e3, s) ->
      "for (" ^ string_of_expr e1  ^ " ; " ^ string_of_expr e2 ^ " ; " ^
      string_of_expr e3  ^ ") " ^ string_of_stmt s
  | While(e, s) -> "while (" ^ string_of_expr e ^ ") " ^ string_of_stmt s

let string_of_vdecl (t, id) = string_of_typ t ^ " " ^ id ^ ";\n"

let string_of_fdecl fdecl =
  string_of_typ fdecl.typ ^ " " ^
  fdecl.fname ^ "(" ^ String.concat ", " (List.map snd fdecl.formals) ^
  ")\n{\n" ^
  String.concat "" (List.map string_of_stmt fdecl.body) ^
  "}\n"

let string_of_program (funcs) =
  String.concat "\n" (List.map string_of_fdecl funcs)
