open OUnit2
open Pq_lang

(* TODO - once the language becomes more than just arithmetic evaluation, need to change the signatures for the testing stuff *)

let test_cases_arithmetic : (string * Ast.expr * int) list =
  let open Ast in
  List.map (fun (x,y) -> (show x, x, y)) [
    (Add (IntLit 1, IntLit 2), 3);
    (Mult (IntLit 1, IntLit 2), 2);
    (Add (IntLit 1, Mult (IntLit 4, Add (IntLit 1, IntLit 2))), 13);
    (Mult (Add (IntLit 1, IntLit 2), IntLit 3), 9);
    (Mult (Add (IntLit 1, IntLit 2), Add (IntLit 3, IntLit 4)), 21);
    (Add (Add (Add (IntLit 0, IntLit 0), IntLit 0), IntLit 0), 0)
  ]

let create_test ((name : string), (inp : Ast.expr), (exp : int)) =
  name >:: fun _ ->
    let out = Ast_executor.execute inp in
    assert_equal exp out ~printer:string_of_int

let suite =
  "AST Executor" >::: [
    "Arithmetic" >::: List.map create_test test_cases_arithmetic
  ]
