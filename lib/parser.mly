%{
open Ast

(*
 * As an example,
 *   let rec (f : ft) = fun (x : xt) -> e end in e' end
 * gets converted to
 *   let (f : ft) = fix (fun f : ft -> fun (x : xt) -> e end end) in e' end
 * i.e.
 *   Let (
 *     ("f": ft),
 *     App (
 *       Fix,
 *       Fun (
 *         ("f": ft),
 *         Fun (
 *           ("x": xt),
 *           e
 *         )
 *       ),
 *     e'
 *   )
*)

let create_let_rec (((fname : string), (ftype : vtype)), (fbody : expr), (subexpr : expr)) : expr =
  Let ((fname, ftype), App (Fix, Fun ((fname, ftype), fbody)), subexpr)
%}

// Tokens
%token END IF THEN ELSE LET IN TRUE FALSE INT BOOL FUN REC
%token COLON
%token PLUS MINUS TIMES LPAREN RPAREN BNOT BOR BAND ASSIGN EQ GT GTEQ LT LTEQ ARROW
%token <int> INTLIT
%token <string> NAME
%token EOF

// Precedence and associativity rules
%left VTYPE_FUN_ARROW // -> (in vtype)
%nonassoc ARROW // ->
%left BNOT // ~
%left BOR // ||
%left BAND // &&
%nonassoc EQ // =
%nonassoc GT GTEQ LT LTEQ // > >= < <=
%left PLUS MINUS // + -
%left TIMES // *
%nonassoc INTLIT NAME TRUE FALSE // literals
%nonassoc LPAREN // (

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
  | LET REC l = var_defn ASSIGN r = expr IN subexpr = expr END { create_let_rec (l, r, subexpr) }
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
