%{
open Ast
%}

// Tokens
%token END IF THEN ELSE LET IN TRUE FALSE INT BOOL FUN REC
%token COLON
%token PLUS MINUS TIMES LPAREN RPAREN BNOT BOR BAND ASSIGN EQ GT GTEQ LT LTEQ ARROW
%token <int> INTLIT
%token <string> NAME
%token EOF

// Precedence and associativity rules
%nonassoc INTLIT NAME TRUE FALSE
%nonassoc LPAREN
%left VTYPE_FUN_ARROW
%nonassoc ARROW
%left BNOT
%left BOR
%left BAND
%nonassoc EQ
%nonassoc GT GTEQ LT LTEQ
%left PLUS MINUS
%left TIMES

// Non-terminal typing
%type <expr> expr
%type <expr> contained_expr
%type <vtype> vtype
%type <(string * vtype)> var_defn
%start <expr> prog

%%

prog:
  | e = expr EOF { e }
;

vtype:
  | LPAREN t = vtype RPAREN { t }
  | INT { VTypeInt }
  | BOOL { VTypeBool }
  | tx = vtype ARROW ty = vtype %prec VTYPE_FUN_ARROW { VTypeFun (tx, ty) }
;

expr:
  | e = contained_expr { e }
  | e1 = expr PLUS e2 = expr { Add (e1, e2) }
  | MINUS e = expr { Neg (e) }
  | e1 = expr MINUS e2 = expr { Subtr (e1, e2) }
  | e1 = expr TIMES e2 = expr { Mult (e1, e2) }
  | BNOT e = expr { BNot (e) }
  | e1 = expr BOR e2 = expr { BOr (e1, e2) }
  | e1 = expr BAND e2 = expr { BAnd (e1, e2) }
  | e1 = expr EQ e2 = expr { Eq (e1, e2) }
  | e1 = expr GT e2 = expr { Gt (e1, e2) }
  | e1 = expr GTEQ e2 = expr { GtEq (e1, e2) }
  | e1 = expr LT e2 = expr { Lt (e1, e2) }
  | e1 = expr LTEQ e2 = expr { LtEq (e1, e2) }
  | IF e1 = expr THEN e2 = expr ELSE e3 = expr END { If (e1, e2, e3) }
  | LET l = var_defn ASSIGN r = expr IN subexpr = expr END { Let (l, r, subexpr) }
  (* TODO - add let-rec bindings, that are immediately translated to let-bindings that use application of Fix *)
  | FUN LPAREN fdefn = var_defn RPAREN ARROW e = expr END { Fun (fdefn, e) }
  | e1 = expr e2 = contained_expr { App (e1, e2) }
;

contained_expr:
  | LPAREN e = expr RPAREN { e }
  | i = INTLIT { IntLit i }
  | TRUE { BoolLit true }
  | FALSE { BoolLit false }
  | n = NAME { Var n }
;

var_defn:
  | LPAREN x = var_defn RPAREN { x }
  | n = NAME COLON t = vtype { (n, t) }
;
