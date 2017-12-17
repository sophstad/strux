(* Ocamllex scanner for Strux *)

{ open Parser }

let whitespace = [' ' '\t' '\r' '\n']
let digits = ['0'-'9']
let integer = digits+
let decimal = ['.']
let esc = '\\' ['\\' ''' '"' 'n' 'r' 't']
let ascii = ([' '-'!' '#'-'[' ']'-'~'])
let string = '"' ( (ascii | esc)* as s) '"'
let esc_ch = ''' (esc) '''
let float = digits* decimal digits+ | digits+ decimal digits*
let alphabet = ['a'-'z' 'A'-'Z']
let alphanumund = alphabet | digits | '_'
let id = alphabet alphanumund*

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
  | '.'       { DOT }
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
  | "while"   { WHILE }
  | "return"      { RETURN }
  | "num"         { NUM }
  | "int"         { INT }
  | "bool"        { BOOL }
  | "string"      { STRING }
  | "void"        { VOID }
  | "true"        { TRUE }
  | "false"       { FALSE }
  | "new"         { NEW }
  | "null"        { NULL }
  | "Queue"       { QUEUE }
  | "LinkedList"  { LINKEDLIST }
  | "BSTree"      { BSTREE }
  | "Stack"       { STACK }
  | ['0'-'9']+ as lxm { INT_LITERAL(int_of_string lxm) }
  | ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm { ID(lxm) }
  | float as lxm { NUM_LITERAL(float_of_string lxm)}
  | digits+ as intlit               { INT_LITERAL(int_of_string intlit) }
  | string                           { STRING_LITERAL(s) }
  | eof { EOF }
  | _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and comment = parse
  "):" { token lexbuf }
  | _    { comment lexbuf }
