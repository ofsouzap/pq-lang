%{
open Utils
open Vtype
open Variant_types
open Pattern
open Ast
open Quotient_types
open Program
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

let add_variant_type_definition_to_program (p : plain_program) (ct : variant_type) : plain_program =
  {
    type_defns = (VariantType ct) :: p.type_defns;
    e = p.e
  }

let add_quotient_type_definition_to_program (p : plain_program) (qt : quotient_type) : plain_program =
  {
    type_defns = (QuotientType qt) :: p.type_defns;
    e = p.e
  }
%}

// Tokens
%token END IF THEN ELSE LET IN TRUE FALSE FUN REC UNIT INT BOOL MATCH WITH TYPE QTYPE OF
%token PLUS MINUS STAR LPAREN RPAREN BNOT BOR BAND ASSIGN EQUATE GT GTEQ LT LTEQ ARROW BIG_ARROW COLON COMMA PIPE QUOTIENT UNIT_VAL
%token <int> INTLIT
%token <string> LNAME UNAME
%token EOF

// Precedence and associativity rules
%right ARROW // ->
%left BNOT // ~
%left BOR // ||
%left BAND // &&
%nonassoc EQUATE // ==
%nonassoc GT GTEQ LT LTEQ // > >= < <=
%left PLUS MINUS // + -
%left STAR // *
%nonassoc UNIT_VAL INTLIT LNAME TRUE FALSE // literals
%nonassoc UNAME  // Constructor
%nonassoc LPAREN // (

// Non-terminal typing

%type <vtype> vtype

%type <pattern> pattern
%type <pattern> contained_pattern

%type <variant_type_constructor> variant_type_constructor
%type <variant_type_constructor list> variant_type_definition_constructors_no_leading_pipe
%type <variant_type_constructor list> variant_type_definition_constructors
%type <variant_type> variant_type_definition

%type <string * vtype * vtype> typed_function_name
%type <string * vtype> typed_name

%type <pattern * plain_expr> match_case
%type <(pattern * plain_expr) Nonempty_list.t> match_cases_no_leading_pipe
%type <(pattern * plain_expr) Nonempty_list.t> match_cases

%type <plain_expr> expr
%type <plain_expr> contained_expr

%type <(string * vtype) list> quotient_type_eqcons_bindings
%type <pattern * plain_expr> quotient_type_eqcons_body
%type <quotient_type_eqcons> quotient_type_eqcons
%type <quotient_type_eqcons list> quotient_type_definition_eqconss
%type <quotient_type> quotient_type_definition

// Main program

%start <plain_program> prog

%%

vtype:
  | LPAREN v = vtype RPAREN { v }
  | UNIT { VTypeUnit }
  | INT { VTypeInt }
  | BOOL { VTypeBool }
  | t1 = vtype ARROW t2 = vtype { VTypeFun (t1, t2) }
  | t1 = vtype STAR t2 = vtype { VTypePair (t1, t2) }
  | tname = LNAME { VTypeCustom tname }
;

pattern:
  | p = contained_pattern { p }
  | n = LNAME COLON t = vtype { PatName (n, t) }
  | cname = UNAME p = contained_pattern { PatConstructor (cname, p) }
;

contained_pattern:
  | LPAREN p = pattern RPAREN { p }
  | LPAREN p1 = pattern COMMA p2 = pattern RPAREN { PatPair (p1, p2) }
;

variant_type_constructor:
  | name = UNAME OF t = vtype { (name, t) }
;

variant_type_definition_constructors_no_leading_pipe:
  | c = variant_type_constructor { [c] }
  | c = variant_type_constructor PIPE cs_tail = variant_type_definition_constructors { c :: cs_tail }
;

variant_type_definition_constructors:
  | PIPE cs = variant_type_definition_constructors_no_leading_pipe { cs }
  | cs = variant_type_definition_constructors_no_leading_pipe { cs }
;

variant_type_definition:
  | TYPE name = LNAME ASSIGN cs = variant_type_definition_constructors { (name, cs) }
;

typed_function_name:
  | n = LNAME COLON t1 = vtype ARROW t2 = vtype { (n, t1, t2) }
;

typed_name:
  | n = LNAME COLON t = vtype { (n, t) }
;

match_case:
  | p = contained_pattern ARROW e = expr { (p, e) }
;

match_cases_no_leading_pipe:
  | c = match_case { (c, []) }
  | c = match_case PIPE cs_tail = match_cases { Nonempty_list.cons c cs_tail }
;

match_cases:
  | PIPE cs = match_cases_no_leading_pipe { cs }
  | cs = match_cases_no_leading_pipe { cs }
;

expr:
  | e = contained_expr { e }  (* ( e ) *)
  | e1 = expr PLUS e2 = expr { Add ((), e1, e2) }  (* e1 + e2 *)
  | MINUS e = expr { Neg ((), e) }  (* - e *)
  | e1 = expr MINUS e2 = expr { Subtr ((), e1, e2) }  (* e1 - e2 *)
  | e1 = expr STAR e2 = expr { Mult ((), e1, e2) }  (* e1 * e2 *)
  | BNOT e = expr { BNot ((), e) }  (* ~ e *)
  | e1 = expr BOR e2 = expr { BOr ((), e1, e2) }  (* e1 || e2 *)
  | e1 = expr BAND e2 = expr { BAnd ((), e1, e2) }  (* e1 && e2 *)
  | e1 = expr EQUATE e2 = expr { Eq ((), e1, e2) }  (* e1 == e2 *)
  | e1 = expr GT e2 = expr { Gt ((), e1, e2) }  (* e1 > e2 *)
  | e1 = expr GTEQ e2 = expr { GtEq ((), e1, e2) }  (* e1 >= e2 *)
  | e1 = expr LT e2 = expr { Lt ((), e1, e2) }  (* e1 < e2 *)
  | e1 = expr LTEQ e2 = expr { LtEq ((), e1, e2) }  (* e1 <= e2 *)
  | IF e1 = expr THEN e2 = expr ELSE e3 = expr END { If ((), e1, e2, e3) }  (* if e1 then e2 else e3 *)
  | LET l = LNAME ASSIGN r = expr IN subexpr = expr END { Let ((), l, r, subexpr) }  (* let l = r in subexpr end *)
  | LET REC LPAREN l = typed_function_name RPAREN ASSIGN r = expr IN subexpr = expr END { create_let_rec (l, r, subexpr) }  (* let rec (lname : ltype) = r in subexpr end *)
  | FUN LPAREN x = typed_name RPAREN ARROW e = expr END { Fun ((), x, e) }  (* fun (xname : xtype) -> e *)
  | e1 = expr e2 = contained_expr { App ((), e1, e2) }  (* e1 e2 *)
  | MATCH e = expr WITH cs = match_cases END { Match ((), e, cs) }  (* match e with cs end *)
  | cname = UNAME e = expr { Constructor ((), cname, e) }  (* Cname e *)
;

contained_expr:
  | LPAREN e = expr RPAREN { e }  (* ( e ) *)
  | UNIT_VAL { UnitLit () }  (* () *)
  | i = INTLIT { IntLit ((), i) }  (* n *)
  | TRUE { BoolLit ((), true) }  (* true *)
  | FALSE { BoolLit ((), false) }  (* false *)
  | LPAREN e1 = expr COMMA e2 = expr RPAREN { Pair ((), e1, e2) }  (* (e1, e2) *)
  | n = LNAME { Var ((), n) }  (* var *)
;

quotient_type_eqcons_bindings:
  | LPAREN RPAREN { [] }  (* None *)
  | LPAREN x = typed_name RPAREN { [x] }  (* Singleton *)
  | LPAREN x = typed_name RPAREN ARROW vs_tail = quotient_type_eqcons_bindings { x :: vs_tail }  (* Multiple *)

quotient_type_eqcons_body:
  | p = pattern EQUATE LPAREN e = expr RPAREN { (p, e) }
;

quotient_type_eqcons:
  | bs = quotient_type_eqcons_bindings BIG_ARROW body = quotient_type_eqcons_body { { bindings=bs; body=body } }
;

quotient_type_definition_eqconss:
  | QUOTIENT eqcons = quotient_type_eqcons { [eqcons] }
  | QUOTIENT eqcons = quotient_type_eqcons QUOTIENT eqconss_tail = quotient_type_definition_eqconss { eqcons :: eqconss_tail }
;

quotient_type_definition:
  | QTYPE name = LNAME ASSIGN ct_name = LNAME eqconss = quotient_type_definition_eqconss { { name; base_type_name = ct_name; eqconss=eqconss } }
;

prog:
  | e = expr EOF { { type_defns = []; e = e } }
  | ct = variant_type_definition p = prog { add_variant_type_definition_to_program p ct }
  | qt = quotient_type_definition p = prog { add_quotient_type_definition_to_program p qt }
;
