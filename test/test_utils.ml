open Pq_lang
open Parser

let string_of_token = function
  | INT i -> string_of_int i
  | PLUS -> "+"
  | TIMES -> "*"
  | LPAREN -> "("
  | RPAREN -> ")"
  | EOF -> "EOF"

let token_printer tokens =
  String.concat ", " (List.map string_of_token tokens)

let ast_printer ast = Ast.show ast
