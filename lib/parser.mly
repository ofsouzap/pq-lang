%{
open Ast
%}

%token <int> INT
%token PLUS TIMES LPAREN RPAREN EOF

%left PLUS
%left TIMES

%type <expr> expr
%start <Ast.expr> prog

%%

prog:
  | e = expr EOF { e }
;

expr:
  | i = INT { IntLit i }
  | LPAREN e = expr RPAREN { e }
  | e1 = expr PLUS e2 = expr { Add (e1, e2) }
  | e1 = expr TIMES e2 = expr { Mult (e1, e2) }
;
