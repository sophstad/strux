(* Ocamllex scanner for Strux *)

{ open Parser }

rule token = parse
  [' ' '\t' '\r' '\n'] { token lexbuf } (* Whitespace *)
| ":("      { comment lexbuf }          (* Comments *)
| '('       { LPAREN }
| ')'       { RPAREN }
| '{'       { LBRACE }
| '}'       { RBRACE }
| ';'       { SEMI }
| ','       { COMMA }
| '+'       { PLUS }
| '-'       { MINUS }
| '*'       { TIMES }
| '/'       { DIVIDE }
| '='       { ASSIGN }
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
| "return"  { RETURN }
| "num"     { NUM }
| "bool"    { BOOL }
| "string"  { STRING }
| "void"    { VOID }
| "true"    { TRUE }
| "false"   { FALSE }
| "new"     { NEW }
| ['0'-'9']+ as lxm { LITERAL(int_of_string lxm) }
| ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm { ID(lxm) }
| eof { EOF }
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and comment = parse
  "):" { token lexbuf }
| _    { comment lexbuf }

