/* Ocamlyacc parser for Strux */

/*
 * TODO
 *
 * Implement NEW keyword
 */

%{
open Ast
%}

%token SEMI LPAREN RPAREN LBRACE RBRACE LBRACK RBRACK COMMA DOUBLECOL
%token PLUS MINUS TIMES DIVIDE INCR DECR MOD ASSIGN NOT
%token EQ NEQ LT LEQ GT GEQ TRUE FALSE AND OR
%token RETURN NULL IF ELSE ELIF BREAK CONTINUE NEW FOR FOREACH IN WHILE NUM INT BOOL STRING VOID
/*%token STACK QUEUE LINKEDLIST LISTNODE BSTREE TREENODE*/
%token <float> NUM_LITERAL
%token <int> INT_LITERAL
%token <string> STRING_LITERAL
%token <string> ID
%token EOF

%nonassoc NOELSE
%nonassoc ELSE
%nonassoc ELIF
%left INCR DECR
%right ASSIGN
%left OR
%left AND
%left EQ NEQ
%left LT GT LEQ GEQ
%left PLUS MINUS
%left TIMES DIVIDE
%right MOD
%right NOT NEG

%start program
%type <Ast.program> program

%%

program:
  decls EOF { $1 }

decls:
   /* nothing */ { [], [] }
 | decls fdecl { fst $1, ($2 :: snd $1) }

fdecl:
   typ ID LPAREN formals_opt RPAREN LBRACE stmt_list RBRACE
     { { typ = $1;
	 fname = $2;
	 formals = $4;
	 body = List.rev $7 } }

formals_opt:
    /* nothing */ { [] }
  | formal_list   { List.rev $1 }

formal_list:
    typ ID                   { [($1,$2)] }
  | formal_list COMMA typ ID { ($3,$4) :: $1 }

typ:
    NUM          { Num }
  | INT          { Int }
  | STRING       { String }
  | BOOL         { Bool }
  | VOID         { Void }
  /*| STACK        { Stack }
  | QUEUE        { Queue }
  | LINKEDLIST   { LinkedList }
  | LISTNODE     { ListNode }
  | BSTREE       { BSTree }
  | TREENODE     { TreeNode }*/



stmt_list:
    /* nothing */  { [] }
  | stmt_list stmt { $2 :: $1 }

stmt:
    expr SEMI { Expr $1 }
  | RETURN SEMI { Return Noexpr }
  | RETURN expr SEMI { Return $2 }
  | LBRACE stmt_list RBRACE { Block(List.rev $2) }
  | IF LPAREN expr RPAREN stmt %prec NOELSE { If($3, $5, Block([])) }
  | IF LPAREN expr RPAREN stmt ELSE stmt    { If($3, $5, $7) }
  | IF LPAREN expr RPAREN stmt else_stmt    { If($3, $5, $6) }
  | FOR LPAREN expr_opt SEMI expr SEMI expr_opt RPAREN stmt
     { For($3, $5, $7, $9) }
  /*| FOREACH LPAREN typ expr IN expr RPAREN stmt { ForEach($3, $4, $6, $8) }*/
  | WHILE LPAREN expr RPAREN stmt { While($3, $5) }

else_stmt:
  ELIF LPAREN expr RPAREN stmt %prec NOELSE { If($3, $5, Block([])) }
  | ELIF LPAREN expr RPAREN stmt else_stmt { If($3, $5, $6) }
  | ELIF LPAREN expr RPAREN stmt ELSE stmt { If($3, $5, $7) }

expr_opt:
    /* nothing */ { Noexpr }
  | expr          { $1 }

expr:
    STRING_LITERAL   { StringLit($1) }
  | ID               { Id($1) }
  | INT_LITERAL      { IntLit($1) }
  | NUM_LITERAL      { NumLit($1) }
  | TRUE             { BoolLit(true) }
  | FALSE            { BoolLit(false) }
  | NULL             { Null }
  | expr PLUS   expr { Binop($1, Add,   $3) }
  | expr MINUS  expr { Binop($1, Sub,   $3) }
  | expr TIMES  expr { Binop($1, Mult,  $3) }
  | expr DIVIDE expr { Binop($1, Div,   $3) }
  | expr MOD    expr { Binop($1, Mod,   $3) }
  | expr EQ     expr { Binop($1, Equal, $3) }
  | expr NEQ    expr { Binop($1, Neq,   $3) }
  | expr LT     expr { Binop($1, Less,  $3) }
  | expr LEQ    expr { Binop($1, Leq,   $3) }
  | expr GT     expr { Binop($1, Greater, $3) }
  | expr GEQ    expr { Binop($1, Geq,   $3) }
  | expr AND    expr { Binop($1, And,   $3) }
  | expr OR     expr { Binop($1, Or,    $3) }
  | MINUS expr %prec NEG  { Unop(Neg, $2) }
  | NOT expr              { Unop(Not, $2) }
  | expr INCR             { Postop($1, Incr) }
  | expr DECR             { Postop($1, Decr) }
  | typ ID                { Assign($1, $2, Noexpr) }
  | typ ID ASSIGN expr    { Assign($1, $2, $4) }
  | ID ASSIGN expr        { Reassign($1, $3) }
  | ID LPAREN actuals_opt RPAREN { FuncCall($1, $3) }
  | LPAREN expr RPAREN { $2 }

actuals_opt:
    /* nothing */ { [] }
  | actuals_list  { List.rev $1 }

actuals_list:
    expr                    { [$1] }
  | actuals_list COMMA expr { $3 :: $1 }
