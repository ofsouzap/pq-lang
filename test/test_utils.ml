open Pq_lang
open Parser
open Ast_executor

let string_of_token = function
  | END -> "END"
  | IF -> "IF"
  | THEN -> "THEN"
  | ELSE -> "ELSE"
  | LET -> "LET"
  | REC -> "REC"
  | IN -> "IN"
  | TRUE -> "TRUE"
  | FALSE -> "FALSE"
  | FUN -> "FUN"
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
  | ARROW -> "ARROW"
  | INTLIT i -> "INT[" ^ string_of_int i ^ "]"
  | NAME n -> "NAME[" ^ n ^ "]"
  | EOF -> "EOF"

let token_printer tokens = String.concat ", " (List.map string_of_token tokens)
let ast_printer ast = Ast.show_ast ast

let override_compare_exec_res (a : exec_res) (b : exec_res) : bool =
  match (a, b) with
  | Err e1, Err e2 -> (
      match (e1, e2) with
      | TypingError _, TypingError _ -> true
      | _ -> exec_res_compare a b)
  | _ -> exec_res_compare a b
