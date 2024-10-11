open OUnit2
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

let test_cases_arithmetic : (string * string * token list) list = List.map (fun (x,y) -> (x,x,y)) [
  ("1 + 2", [INT 1; PLUS; INT 2]);
  ("1 * 2", [INT 1; TIMES; INT 2]);
  ("1    + 4 * (1+2 )", [INT 1; PLUS; INT 4; TIMES; LPAREN; INT 1; PLUS; INT 2; RPAREN]);
  ("(1 + 2) * 3", [LPAREN; INT 1; PLUS; INT 2; RPAREN; TIMES; INT 3]);
  ("(1 + 2) * (3 + 4)", [LPAREN; INT 1; PLUS; INT 2; RPAREN; TIMES; LPAREN; INT 3; PLUS; INT 4; RPAREN]);
]

let create_test (name, inp, exp) =
  name >:: fun _ ->
    let lexbuf = Lexing.from_string inp in
    let rec collect_tokens acc =
      match Lexer.token lexbuf with
      | Parser.EOF -> List.rev acc
      | token -> collect_tokens (token :: acc)
    in
    let out = collect_tokens [] in
    assert_equal exp out ~printer:token_printer

let suite =
  "Lexer Tests" >::: [
    "Arithmetic" >::: List.map create_test test_cases_arithmetic
  ]
