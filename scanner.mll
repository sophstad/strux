(* Ocamllex scanner for Strux *)

{ open Parser }

let digit = ['0'-'9']
let num = ((digit+) '.' (digit+)) | digit+

rule token = parse
  [' ' '\t' '\r' '\n'] { token lexbuf } (* Whitespace *)
  | ":("      { comment lexbuf }          (* Comments *)
  | '('       { LPAREN }
  | ')'       { RPAREN }
  | '{'       { LBRACE }
  | '}'       { RBRACE }
  | '['       { LBRACK }
  | ']'       { RBRACK }
  | ';'       { SEMI }
  | ','       { COMMA }
  | '+'       { PLUS }
  | '-'       { MINUS }
  | '*'       { TIMES }
  | '/'       { DIVIDE }
  | '%'       { MODULO }
  | "++"      { INCR }
  | "--"      { DECR }
  | '='       { ASSIGN }
  | "::"      { DOUBLECOL }
  | "=="      { EQ }
  | "!="      { NEQ }
  | '<'       { LT }
  | "<="      { LEQ }
  | ">"       { GT }
  | ">="      { GEQ }
  | "and"     { AND }
  | "or"      { OR }
  | "not"     { NOT }
  | "if"      { IF }
  | "elif"    { ELIF }
  | "else"    { ELSE }
  | "for"     { FOR }
  | "forEach" { FOREACH }
  | "in"      { IN }
  | "while"   { WHILE }
  | "break"   { BREAK }
  | "continue"    { CONTINUE }
  | "return"      { RETURN }
  | "num"         { NUM }
  | "bool"        { BOOL }
  | "string"      { STRING }
  | "void"        { VOID }
  | "true"        { TRUE }
  | "false"       { FALSE }
  | "new"         { NEW }
  | "null"        { NULL }
  | "Stack"       { STACK }
  | "Queue"       { QUEUE }
  | "LinkedList"  { LINKEDLIST }
  | "ListNode"    { LISTNODE }
  | "BSTree"      { BSTREE }
  | "TreeNode"    { TREENODE }
  | num as lxm    { LITERAL(float_of_string lxm) }
  | ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm { ID(lxm) }
  | eof { EOF }
  | _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and comment = parse
  "):" { token lexbuf }
  | _    { comment lexbuf }
