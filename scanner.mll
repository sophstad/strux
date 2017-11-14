(* Ocamllex scanner for Strux *)

{ open Parser }

let digit = ['0'-'9']
let int = digit+
let float = (digit+) '.' (digit+)
let escape = '\\' ['\\' ''' '"' 'n' 'r' 't']
let ascii = ([' '-'!' '#'-'[' ']'-'~'])
let string = '"' ( (ascii | escape)* as s) '"'

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
  | '%'       { MOD }
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
  (* | "Stack"       { STACK }
  | "Queue"       { QUEUE }
  | "LinkedList"  { LINKEDLIST }
  | "ListNode"    { LISTNODE }
  | "BSTree"      { BSTREE }
  | "TreeNode"    { TREENODE } *)
  | ['0'-'9']+ as lxm { INT_LITERAL(int_of_string lxm) }
  | float as lxm  { NUM_LITERAL(float_of_string lxm) }
  | string                           { STRING_LITERAL(s) }
  | ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm { ID(lxm) }
  | eof { EOF }
  | _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and comment = parse
  "):" { token lexbuf }
  | _    { comment lexbuf }
