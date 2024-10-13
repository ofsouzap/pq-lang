%{
open Ast
%}

%token END IF THEN ELSE LET IN
%token <bool> BOOL
%token PLUS MINUS TIMES LPAREN RPAREN BNOT BOR BAND ASSIGN EQ GT GTEQ LT LTEQ
%token <int> INT
%token <string> NAME
%token EOF

%left PLUS MINUS
%left TIMES
%left BOR
%left BAND
%left EQ GT GTEQ LT LTEQ

%type <expr> expr
%start <Ast.expr> prog

%%

prog:
  | e = expr EOF { e }
;

expr:
  | LPAREN e = expr RPAREN { e }
  | i = INT { IntLit i }
  | e1 = expr PLUS e2 = expr { Add (e1, e2) }
  | MINUS e = expr { Neg e }
  | e1 = expr MINUS e2 = expr { Subtr (e1, e2) }
  | e1 = expr TIMES e2 = expr { Mult (e1, e2) }
  | b = BOOL { BoolLit b }
  | BNOT e = expr { BNot (e) }
  | e1 = expr BOR e2 = expr { BOr (e1, e2) }
  | e1 = expr BAND e2 = expr { BAnd (e1, e2) }
  | e1 = expr EQ e2 = expr { Eq (e1, e2) }
  | e1 = expr GT e2 = expr { Gt (e1, e2) }
  | e1 = expr GTEQ e2 = expr { GtEq (e1, e2) }
  | e1 = expr LT e2 = expr { Lt (e1, e2) }
  | e1 = expr LTEQ e2 = expr { LtEq (e1, e2) }
  | IF e1 = expr THEN e2 = expr ELSE e3 = expr END { If (e1, e2, e3) }
  | n = NAME { Var n }
  | LET n = NAME ASSIGN e1 = expr IN e2 = expr END { Let (n, e1, e2) }
;
