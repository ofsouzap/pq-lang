open OUnit2
open Pq_lang
open Ast

let test_cases_arithmetic : (string * string * Ast.expr option) list = List.map (fun (x,y) -> (x,x,y)) [
  ("1 + 2", Some (Add (IntLit 1, IntLit 2)));
  ("1 * 2", Some (Mult (IntLit 1, IntLit 2)));
  ("1    + 4 * (1+2 )", Some (Add (IntLit 1, Mult (IntLit 4, Add (IntLit 1, IntLit 2)))));
  ("(1 + 2) * 3", Some (Mult (Add (IntLit 1, IntLit 2), IntLit 3)));
  ("(1 + 2) * (3 + 4)", Some ((Mult (Add (IntLit 1, IntLit 2), Add (IntLit 3, IntLit 4)))));
  ("+**+", None)
]

let create_test ((name : string), (inp : string), (exp : Ast.expr option)) =
  name >:: fun _ ->
    let lexbuf = Lexing.from_string inp in
    let out =
      try
        Some (Parser.prog Lexer.token lexbuf)
      with
        _ -> Printf.printf "FAILURE\n"; None
    in
    assert_equal exp out ~printer:(fun x -> match x with | Some x -> Ast.show x | None -> "None")

let suite =
  "Parser" >::: [
    "Arithmetic" >::: List.map create_test test_cases_arithmetic
  ]
