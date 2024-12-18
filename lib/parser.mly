%{
open Vtype
open Ast
open Parsing_errors

(*
 * As an example,
 *   let rec (f : t1 -> t2) = fun (x : tx) -> e end in e' end
 * gets converted to
 *   let f = fix (fun (f : t1 -> t2) -> fun (x : tx) -> e end end) in e' end
 * i.e.
 *   Let (
 *     ("f", VTypeFun (t1, t2)),
 *     Fix (
 *       ("f", VTypeFun (t1, t2)),
 *       ("x", tx),
 *       e
 *     ),
 *     e'
 *   )
*)

let create_let_rec (((fname : string), (_ : vtype), (_ : vtype) as f), (fbody : plain_expr), (subexpr : plain_expr)) : plain_expr =
  match fbody with
  | Fun (_, x, fbody') -> Let ((), fname, Fix ((), f, x, fbody'), subexpr)
  | _ -> raise CustomError
%}

// Tokens
%token END IF THEN ELSE LET IN TRUE FALSE FUN REC INT BOOL
%token PLUS MINUS STAR LPAREN RPAREN BNOT BOR BAND ASSIGN EQ GT GTEQ LT LTEQ ARROW COLON
%token <int> INTLIT
%token <string> NAME
%token EOF

// Precedence and associativity rules
%right ARROW // ->
%left BNOT // ~
%left BOR // ||
%left BAND // &&
%nonassoc EQ // =
%nonassoc GT GTEQ LT LTEQ // > >= < <=
%left PLUS MINUS // + -
%left STAR // *
%nonassoc INTLIT NAME TRUE FALSE // literals
%nonassoc LPAREN // (

// Non-terminal typing
%type <vtype> vtype
%type <string * vtype * vtype> typed_function_name
%type <string * vtype> typed_name
%type <plain_expr> expr
%type <plain_expr> contained_expr
%start <plain_expr> prog

%%

vtype:
  | LPAREN v = vtype RPAREN { v }
  | INT { VTypeInt }
  | BOOL { VTypeBool }
  | t1 = vtype ARROW t2 = vtype { VTypeFun (t1, t2) }
;

typed_function_name:
  | n = NAME COLON t1 = vtype ARROW t2 = vtype { (n, t1, t2) }
;

typed_name:
  | n = NAME COLON t = vtype { (n, t) }
;

(* TODO - make brackets optional in "fun (x : int) -> ..." if using a non-function type *)

expr:
  | e = contained_expr { e }  (* ( e ) *)
  | e1 = expr PLUS e2 = expr { Add ((), e1, e2) }  (* e1 + e2 *)
  | MINUS e = expr { Neg ((), e) }  (* - e *)
  | e1 = expr MINUS e2 = expr { Subtr ((), e1, e2) }  (* e1 - e2 *)
  | e1 = expr STAR e2 = expr { Mult ((), e1, e2) }  (* e1 * e2 *)
  | BNOT e = expr { BNot ((), e) }  (* ~ e *)
  | e1 = expr BOR e2 = expr { BOr ((), e1, e2) }  (* e1 || e2 *)
  | e1 = expr BAND e2 = expr { BAnd ((), e1, e2) }  (* e1 && e2 *)
  | e1 = expr EQ e2 = expr { Eq ((), e1, e2) }  (* e1 == e2 *)
  | e1 = expr GT e2 = expr { Gt ((), e1, e2) }  (* e1 > e2 *)
  | e1 = expr GTEQ e2 = expr { GtEq ((), e1, e2) }  (* e1 >= e2 *)
  | e1 = expr LT e2 = expr { Lt ((), e1, e2) }  (* e1 < e2 *)
  | e1 = expr LTEQ e2 = expr { LtEq ((), e1, e2) }  (* e1 <= e2 *)
  | IF e1 = expr THEN e2 = expr ELSE e3 = expr END { If ((), e1, e2, e3) }  (* if e1 then e2 else e3 *)
  | LET l = NAME ASSIGN r = expr IN subexpr = expr END { Let ((), l, r, subexpr) }  (* let l = r in subexpr end *)
  | LET REC LPAREN l = typed_function_name RPAREN ASSIGN r = expr IN subexpr = expr END { create_let_rec (l, r, subexpr) }  (* let rec (lname : ltype) = r in subexpr end *)
  | FUN LPAREN x = typed_name RPAREN ARROW e = expr END { Fun ((), x, e) }  (* fun (xname : xtype) -> e *)
  | e1 = expr e2 = contained_expr { App ((), e1, e2) }  (* e1 e2 *)
;

contained_expr:
  | LPAREN e = expr RPAREN { e }
  | i = INTLIT { IntLit ((), i) }
  | TRUE { BoolLit ((), true) }
  | FALSE { BoolLit ((), false) }
  | n = NAME { Var ((), n) }
;

prog:
  | e = expr EOF { e }
;
