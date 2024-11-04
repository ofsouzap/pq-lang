open Core
open Pq_lang
open Parser
open Ast_executor

let sexp_of_token = function
  | END -> Sexp.Atom "END"
  | IF -> Sexp.Atom "IF"
  | THEN -> Sexp.Atom "THEN"
  | ELSE -> Sexp.Atom "ELSE"
  | LET -> Sexp.Atom "LET"
  | REC -> Sexp.Atom "REC"
  | IN -> Sexp.Atom "IN"
  | TRUE -> Sexp.Atom "TRUE"
  | FALSE -> Sexp.Atom "FALSE"
  | FUN -> Sexp.Atom "FUN"
  | PLUS -> Sexp.Atom "PLUS"
  | MINUS -> Sexp.Atom "MINUS"
  | TIMES -> Sexp.Atom "TIMES"
  | LPAREN -> Sexp.Atom "LPAREN"
  | RPAREN -> Sexp.Atom "RPAREN"
  | BNOT -> Sexp.Atom "BNOT"
  | BOR -> Sexp.Atom "BOR"
  | BAND -> Sexp.Atom "BAND"
  | ASSIGN -> Sexp.Atom "ASSIGN"
  | EQ -> Sexp.Atom "EQ"
  | GT -> Sexp.Atom "GT"
  | GTEQ -> Sexp.Atom "GTEQ"
  | LT -> Sexp.Atom "LT"
  | LTEQ -> Sexp.Atom "LTEQ"
  | ARROW -> Sexp.Atom "ARROW"
  | INTLIT i -> Sexp.List [ Sexp.Atom "INTLIT"; Sexp.Atom (string_of_int i) ]
  | NAME n -> Sexp.List [ Sexp.Atom "NAME"; Sexp.Atom n ]
  | EOF -> Sexp.Atom "EOF"

let token_printer tokens =
  String.concat ~sep:", "
    (List.map ~f:(Fn.compose Sexp.to_string sexp_of_token) tokens)

let ast_printer = Fn.compose Sexp.to_string Ast.sexp_of_expr
let show_ast = ast_printer

let override_compare_exec_res (a : exec_res) (b : exec_res) : bool =
  match (a, b) with
  | Err e1, Err e2 -> (
      match (e1, e2) with
      | TypingError _, TypingError _ -> true
      | _ -> exec_res_compare a b)
  | _ -> exec_res_compare a b
