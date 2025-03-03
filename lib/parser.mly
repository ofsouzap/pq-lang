%{
open Utils

(* For Pattern.std_pattern constructors *)
open Pattern
module Pattern = Pattern.StdPattern
module Expr = Expr.StdExpr
module QuotientType = QuotientType.StdQuotientType
module CustomType = CustomType.StdCustomType

let add_custom_type_decl_to_program (p : (Lexing.position, Lexing.position) Program.StdProgram.t) (ct_decl : (Lexing.position, Lexing.position) Program.StdProgram.custom_type_decl) : (Lexing.position, Lexing.position) Program.StdProgram.t =
  {
    p with
    custom_types = ct_decl :: p.custom_types;
  }

let add_top_level_definition_to_program (p : (Lexing.position, Lexing.position) Program.StdProgram.t) (defn : (Lexing.position, Lexing.position) Program.StdProgram.top_level_defn) : (Lexing.position, Lexing.position) Program.StdProgram.t =
  {
    p with
    top_level_defns = defn :: p.top_level_defns;
  }
%}

// Tokens
%token END IF THEN ELSE LET IN TRUE FALSE REC UNIT INT BOOL MATCH WITH TYPE QTYPE OF PRIVATE
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

%type <Vtype.t> vtype

%type <Lexing.position Pattern.t> pattern
%type <Lexing.position Pattern.t> contained_pattern

%type <VariantType.constructor> variant_type_constructor
%type <VariantType.constructor list> variant_type_definition_constructors_no_leading_pipe
%type <VariantType.constructor list> variant_type_definition_constructors
%type <VariantType.t> variant_type_definition

%type <string * Vtype.t> typed_name

%type <Lexing.position Pattern.t> match_case_pattern
%type <Lexing.position Pattern.t * (Lexing.position, Lexing.position) Expr.t> match_case
%type <(Lexing.position Pattern.t * (Lexing.position, Lexing.position) Expr.t) Nonempty_list.t> match_cases_no_leading_pipe
%type <(Lexing.position Pattern.t * (Lexing.position, Lexing.position) Expr.t) Nonempty_list.t> match_cases

%type <(Lexing.position, Lexing.position) Expr.t> expr
%type <(Lexing.position, Lexing.position) Expr.t> contained_expr

%type <(string * Vtype.t) list> quotient_type_eqcons_bindings
%type <Lexing.position Pattern.t * (Lexing.position, Lexing.position) Expr.t> quotient_type_eqcons_body
%type <(Lexing.position, Lexing.position) QuotientType.eqcons> quotient_type_eqcons
%type <(Lexing.position, Lexing.position) QuotientType.eqcons list> quotient_type_definition_eqconss
%type <(Lexing.position, Lexing.position) QuotientType.t> quotient_type_definition

%type <(Lexing.position, Lexing.position) Program.StdProgram.custom_type_decl> custom_type_decl

%type <(string * Vtype.t)> top_level_defn_param
%type <(Lexing.position, Lexing.position) Program.StdProgram.top_level_defn> top_level_defn

// Main program

%start <(Lexing.position, Lexing.position) Program.StdProgram.t> prog

%%

vtype:
  | LPAREN v = vtype RPAREN { v }
  | UNIT { Vtype.VTypeUnit }
  | INT { Vtype.VTypeInt }
  | BOOL { Vtype.VTypeBool }
  | t1 = vtype ARROW t2 = vtype { Vtype.VTypeFun (t1, t2) }
  | t1 = vtype STAR t2 = vtype { Vtype.VTypePair (t1, t2) }
  | tname = LNAME { Vtype.VTypeCustom tname }
;

pattern:
  | p = contained_pattern { p }
  | n = LNAME COLON t = vtype { PatName ($startpos, n, t) }
  | cname = UNAME p = contained_pattern { PatConstructor ($startpos, cname, p) }
;

contained_pattern:
  | LPAREN p = pattern RPAREN { p }
  | LPAREN p1 = pattern COMMA p2 = pattern RPAREN { PatPair ($startpos, p1, p2) }
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

typed_name:
  | n = LNAME COLON t = vtype { (n, t) }
;

match_case_pattern:
  | LPAREN p = pattern RPAREN { p }
  | LPAREN p1 = pattern COMMA p2 = pattern RPAREN { PatPair ($startpos, p1, p2) }
  | cname = UNAME p = contained_pattern { PatConstructor ($startpos, cname, p) }
;

match_case:
  | p = match_case_pattern ARROW e = expr { (p, e) }
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
  | e1 = expr PLUS e2 = expr { Add ($startpos, e1, e2) }  (* e1 + e2 *)
  | MINUS e = expr { Neg ($startpos, e) }  (* - e *)
  | e1 = expr MINUS e2 = expr { Subtr ($startpos, e1, e2) }  (* e1 - e2 *)
  | e1 = expr STAR e2 = expr { Mult ($startpos, e1, e2) }  (* e1 * e2 *)
  | BNOT e = expr { BNot ($startpos, e) }  (* ~ e *)
  | e1 = expr BOR e2 = expr { BOr ($startpos, e1, e2) }  (* e1 || e2 *)
  | e1 = expr BAND e2 = expr { BAnd ($startpos, e1, e2) }  (* e1 && e2 *)
  | e1 = expr EQUATE e2 = expr { Eq ($startpos, e1, e2) }  (* e1 == e2 *)
  | e1 = expr GT e2 = expr { Gt ($startpos, e1, e2) }  (* e1 > e2 *)
  | e1 = expr GTEQ e2 = expr { GtEq ($startpos, e1, e2) }  (* e1 >= e2 *)
  | e1 = expr LT e2 = expr { Lt ($startpos, e1, e2) }  (* e1 < e2 *)
  | e1 = expr LTEQ e2 = expr { LtEq ($startpos, e1, e2) }  (* e1 <= e2 *)
  | IF e1 = expr THEN e2 = expr ELSE e3 = expr END { If ($startpos, e1, e2, e3) }  (* if e1 then e2 else e3 *)
  | LET l = LNAME ASSIGN r = expr IN subexpr = expr END { Let ($startpos, l, r, subexpr) }  (* let l = r in subexpr end *)
  | e1 = expr e2 = contained_expr { App ($startpos, e1, e2) }  (* e1 e2 *)
  | MATCH e = expr ARROW return_t = vtype WITH cs = match_cases END { Match ($startpos, e, return_t, cs) }  (* match e -> t with cs end *)
  | cname = UNAME e = expr { Constructor ($startpos, cname, e) }  (* Cname e *)
;

contained_expr:
  | LPAREN e = expr RPAREN { e }  (* ( e ) *)
  | UNIT_VAL { UnitLit $startpos }  (* () *)
  | i = INTLIT { IntLit ($startpos, i) }  (* n *)
  | TRUE { BoolLit ($startpos, true) }  (* true *)
  | FALSE { BoolLit ($startpos, false) }  (* false *)
  | LPAREN e1 = expr COMMA e2 = expr RPAREN { Pair ($startpos, e1, e2) }  (* (e1, e2) *)
  | n = LNAME { Var ($startpos, n) }  (* var *)
;

quotient_type_eqcons_bindings:
  | LPAREN RPAREN { [] }  (* None *)
  | LPAREN x = typed_name RPAREN { [x] }  (* Singleton *)
  | LPAREN x = typed_name RPAREN ARROW vs_tail = quotient_type_eqcons_bindings { x :: vs_tail }  (* Multiple *)

quotient_type_eqcons_body:
  | p = pattern EQUATE LPAREN e = expr RPAREN { (p, e) }
;

quotient_type_eqcons:
  | bs = quotient_type_eqcons_bindings BIG_ARROW body = quotient_type_eqcons_body { QuotientType.{ bindings=bs; body=body } }
;

quotient_type_definition_eqconss:
  | QUOTIENT eqcons = quotient_type_eqcons { [eqcons] }
  | QUOTIENT eqcons = quotient_type_eqcons eqconss_tail = quotient_type_definition_eqconss { eqcons :: eqconss_tail }
;

quotient_type_definition:
  | QTYPE name = LNAME ASSIGN base_type_name = LNAME eqconss = quotient_type_definition_eqconss { QuotientType.{ name; base_type_name; eqconss=eqconss } }
;

custom_type_decl:
  | vt = variant_type_definition { { private_flag=Program.StdProgram.Public; ct=(VariantType vt) } }
  | PRIVATE vt = variant_type_definition { { private_flag=Program.StdProgram.Private; ct=(VariantType vt) } }
  | qt = quotient_type_definition { { private_flag=Program.StdProgram.Public; ct=(QuotientType qt) } }
;

top_level_defn_param:
  | LPAREN x = typed_name RPAREN { x }
;

top_level_defn:
  | LET fname = LNAME param = top_level_defn_param COLON return_t = vtype ASSIGN e = expr END { Program.StdProgram.{ private_flag=Program.StdProgram.Public; recursive=false; name=fname; param; return_t; body=e } }
  | PRIVATE LET fname = LNAME param = top_level_defn_param COLON return_t = vtype ASSIGN e = expr END { Program.StdProgram.{ private_flag=Program.StdProgram.Private; recursive=false; name=fname; param; return_t; body=e } }
  | LET REC fname = LNAME param = top_level_defn_param COLON return_t = vtype ASSIGN e = expr END { Program.StdProgram.{ private_flag=Program.StdProgram.Public; recursive=true; name=fname; param; return_t; body=e } }
  | PRIVATE LET REC fname = LNAME param = top_level_defn_param COLON return_t = vtype ASSIGN e = expr END { Program.StdProgram.{ private_flag=Program.StdProgram.Private; recursive=true; name=fname; param; return_t; body=e } }
;

prog:
  | EOF { { custom_types = []; top_level_defns=[]; body = None } }  (* Without a main body *)
  | body = expr EOF { { custom_types = []; top_level_defns=[]; body = Some body } }  (* With a main body *)
  | ct_decl = custom_type_decl p = prog { add_custom_type_decl_to_program p ct_decl }
  | defn = top_level_defn p = prog { add_top_level_definition_to_program p defn }
;
