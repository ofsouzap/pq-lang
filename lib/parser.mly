%{
open Utils
open Vtype
open Custom_types
open Pattern
open Ast
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

let add_custom_type_definition_to_program (p : plain_program) (ct : custom_type) : plain_program =
  {
    custom_types = ct :: p.custom_types;
    e = p.e
  }
%}

(* TODO - rename EQ token to EQUATE to be more-clearly distinguished from ASSIGN *)

// Tokens
%token END IF THEN ELSE LET IN TRUE FALSE FUN REC UNIT INT BOOL MATCH WITH TYPE OF
%token PLUS MINUS STAR LPAREN RPAREN BNOT BOR BAND ASSIGN EQ GT GTEQ LT LTEQ ARROW COLON COMMA PIPE UNIT_VAL
%token <int> INTLIT
%token <string> LNAME UNAME
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
%nonassoc UNIT_VAL INTLIT LNAME TRUE FALSE // literals
%nonassoc UNAME  // Constructor
%nonassoc LPAREN // (

// Non-terminal typing
%type <vtype> vtype
%type <custom_type_constructor> custom_type_constructor
%type <custom_type_constructor list> custom_type_definition_constructors_no_leading_pipe
%type <custom_type_constructor list> custom_type_definition_constructors
%type <custom_type> custom_type_definition
%type <string * vtype * vtype> typed_function_name
%type <string * vtype> typed_name
%type <pattern> pattern
%type <pattern * plain_expr> match_case
%type <(pattern * plain_expr) Nonempty_list.t> match_cases_no_leading_pipe
%type <(pattern * plain_expr) Nonempty_list.t> match_cases
%type <plain_expr> expr
%type <plain_expr> contained_expr
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

custom_type_constructor:
  | name = UNAME OF t = vtype { (name, t) }
;

custom_type_definition_constructors_no_leading_pipe:
  | c = custom_type_constructor { [c] }
  | c = custom_type_constructor PIPE cs_tail = custom_type_definition_constructors { c :: cs_tail }
;

custom_type_definition_constructors:
  | PIPE cs = custom_type_definition_constructors_no_leading_pipe { cs }
  | cs = custom_type_definition_constructors_no_leading_pipe { cs }
;

custom_type_definition:
  | TYPE name = LNAME ASSIGN cs = custom_type_definition_constructors { (name, cs) }
;

typed_function_name:
  | n = LNAME COLON t1 = vtype ARROW t2 = vtype { (n, t1, t2) }
;

typed_name:
  | n = LNAME COLON t = vtype { (n, t) }
;

pattern:
  | LPAREN p = pattern RPAREN { p }
  | LPAREN n = LNAME COLON t = vtype RPAREN { PatName (n, t) }
  | LPAREN p1 = pattern COMMA p2 = pattern RPAREN { PatPair (p1, p2) }
;

match_case:
  | p = pattern ARROW e = expr { (p, e) }
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
  | e1 = expr EQ e2 = expr { Eq ((), e1, e2) }  (* e1 == e2 *)
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

prog:
  | e = expr EOF { { custom_types = []; e = e } }
  | ct = custom_type_definition p = prog { add_custom_type_definition_to_program p ct }
;
