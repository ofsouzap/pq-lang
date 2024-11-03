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

let create_let_rec ((fname : string), (fbody : expr), (subexpr : expr)) : expr =
  Let (fname, App (Fix, Fun (fname, fbody)), subexpr)
%}

// Tokens
%token END IF THEN ELSE LET IN TRUE FALSE FUN REC
%token PLUS MINUS TIMES LPAREN RPAREN BNOT BOR BAND ASSIGN EQ GT GTEQ LT LTEQ ARROW
%token <int> INTLIT
%token <string> NAME
%token EOF

// Precedence and associativity rules
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
%start <expr> prog

%%

prog:
  | e = expr EOF { e }
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
  | LET l = NAME ASSIGN r = expr IN subexpr = expr END { Let (l, r, subexpr) }
  | LET REC l = NAME ASSIGN r = expr IN subexpr = expr END { create_let_rec (l, r, subexpr) }
  | FUN fname = NAME ARROW e = expr END { Fun (fname, e) }
  | e1 = expr e2 = contained_expr { App (e1, e2) }
;

contained_expr:
  | LPAREN e = expr RPAREN { e }
  | i = INTLIT { IntLit i }
  | TRUE { BoolLit true }
  | FALSE { BoolLit false }
  | n = NAME { Var n }
;
