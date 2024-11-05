%{
open Ast
open Parsing_errors

(*
 * As an example,
 *   let rec f = fun x -> e end in e' end
 * gets converted to
 *   let f = fix (fun f -> fun x -> e end end) in e' end
 * i.e.
 *   Let (
 *     "f",
 *     Fix (
 *       "f",
 *       "x",
 *       e
 *     ),
 *     e'
 *   )
*)

let create_let_rec ((fname : string), (fbody : plain_expr), (subexpr : plain_expr)) : plain_expr =
  match fbody with
  | Fun (_, xname, fbody') -> Let ((), fname, Fix ((), fname, xname, fbody'), subexpr)
  | _ -> raise CustomError
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
%type <plain_expr> expr
%type <plain_expr> contained_expr
%start <plain_expr> prog

%%

prog:
  | e = expr EOF { e }
;

expr:
  | e = contained_expr { e }
  | e1 = expr PLUS e2 = expr { Add ((), e1, e2) }
  | MINUS e = expr { Neg ((), e) }
  | e1 = expr MINUS e2 = expr { Subtr ((), e1, e2) }
  | e1 = expr TIMES e2 = expr { Mult ((), e1, e2) }
  | BNOT e = expr { BNot ((), e) }
  | e1 = expr BOR e2 = expr { BOr ((), e1, e2) }
  | e1 = expr BAND e2 = expr { BAnd ((), e1, e2) }
  | e1 = expr EQ e2 = expr { Eq ((), e1, e2) }
  | e1 = expr GT e2 = expr { Gt ((), e1, e2) }
  | e1 = expr GTEQ e2 = expr { GtEq ((), e1, e2) }
  | e1 = expr LT e2 = expr { Lt ((), e1, e2) }
  | e1 = expr LTEQ e2 = expr { LtEq ((), e1, e2) }
  | IF e1 = expr THEN e2 = expr ELSE e3 = expr END { If ((), e1, e2, e3) }
  | LET l = NAME ASSIGN r = expr IN subexpr = expr END { Let ((), l, r, subexpr) }
  | LET REC l = NAME ASSIGN r = expr IN subexpr = expr END { create_let_rec (l, r, subexpr) }
  | FUN fname = NAME ARROW e = expr END { Fun ((), fname, e) }
  | e1 = expr e2 = contained_expr { App ((), e1, e2) }
;

contained_expr:
  | LPAREN e = expr RPAREN { e }
  | i = INTLIT { IntLit ((), i) }
  | TRUE { BoolLit ((), true) }
  | FALSE { BoolLit ((), false) }
  | n = NAME { Var ((), n) }
;
