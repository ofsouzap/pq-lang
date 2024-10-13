open Pq_lang
open Parser

let string_of_token = function
  | END -> "END"
  | IF -> "IF"
  | THEN -> "THEN"
  | ELSE -> "ELSE"
  | LET -> "LET"
  | IN -> "IN"
  | BOOL b -> "BOOL[" ^ string_of_bool b ^ "]"
  | PLUS -> "PLUS"
  | MINUS -> "MINUS"
  | TIMES -> "TIMES"
  | LPAREN -> "LPAREN"
  | RPAREN -> "RPAREN"
  | BNOT -> "BNOT"
  | BOR -> "BOR"
  | BAND -> "BAND"
  | ASSIGN -> "ASSIGN"
  | EQ -> "EQ"
  | GT -> "GT"
  | GTEQ -> "GTEQ"
  | LT -> "LT"
  | LTEQ -> "LTEQ"
  | INT i -> "INT[" ^ string_of_int i ^ "]"
  | NAME n -> "NAME[" ^ n ^ "]"
  | EOF -> "EOF"

let token_printer tokens = String.concat ", " (List.map string_of_token tokens)
let ast_printer ast = Ast.show ast
