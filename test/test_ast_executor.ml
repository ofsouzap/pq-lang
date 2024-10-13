open OUnit2
open Pq_lang
open Ast_executor

let test_cases_arithmetic : (string * Ast.expr * exec_res) list =
  let open Ast in
  let mapf ((x : expr), (y : int)) = (show x, x, Res (store_empty, Int y)) in
  List.map mapf
    [
      (IntLit 0, 0);
      (IntLit 5, 5);
      (IntLit (-5), -5);
      (Add (IntLit 1, IntLit 2), 3);
      (Mult (IntLit 1, IntLit 2), 2);
      (Add (IntLit 1, Mult (IntLit 4, Add (IntLit 1, IntLit 2))), 13);
      (Mult (Add (IntLit 1, IntLit 2), IntLit 3), 9);
      (Mult (Add (IntLit 1, IntLit 2), Add (IntLit 3, IntLit 4)), 21);
      (Add (Add (Add (IntLit 0, IntLit 0), IntLit 0), IntLit 0), 0);
    ]

let test_cases_booleans : (string * Ast.expr * exec_res) list =
  let open Ast in
  let mapf ((x : expr), (y : bool)) = (show x, x, Res (store_empty, Bool y)) in
  List.map mapf
    [
      (BoolLit true, true);
      (BoolLit false, false);
      (BNot (BoolLit true), false);
      (BNot (BoolLit false), true);
      (BOr (BoolLit true, BoolLit true), true);
      (BOr (BoolLit true, BoolLit false), true);
      (BOr (BoolLit false, BoolLit true), true);
      (BOr (BoolLit false, BoolLit false), false);
      (BAnd (BoolLit true, BoolLit true), true);
      (BAnd (BoolLit true, BoolLit false), false);
      (BAnd (BoolLit false, BoolLit true), false);
      (BAnd (BoolLit false, BoolLit false), false);
      (Eq (BoolLit true, BoolLit true), true);
      (Eq (BoolLit true, BoolLit false), false);
      (Eq (BoolLit false, BoolLit true), false);
      (Eq (BoolLit false, BoolLit false), true);
    ]

let test_cases_integer_comparisons : (string * Ast.expr * exec_res) list =
  let open Ast in
  let mapf ((x : expr), (y : bool)) = (show x, x, Res (store_empty, Bool y)) in
  List.map mapf
    [
      (Eq (IntLit 0, IntLit 0), true);
      (Eq (IntLit 0, IntLit 1), false);
      (Eq (IntLit 1, IntLit 0), false);
      (Eq (IntLit 1, IntLit 1), true);
      (Gt (IntLit 0, IntLit 0), false);
      (Gt (IntLit 0, IntLit 1), false);
      (Gt (IntLit 1, IntLit 0), true);
      (Gt (IntLit 1, IntLit 1), false);
      (GtEq (IntLit 0, IntLit 0), true);
      (GtEq (IntLit 0, IntLit 1), false);
      (GtEq (IntLit 1, IntLit 0), true);
      (GtEq (IntLit 1, IntLit 1), true);
      (Lt (IntLit 0, IntLit 0), false);
      (Lt (IntLit 0, IntLit 1), true);
      (Lt (IntLit 1, IntLit 0), false);
      (Lt (IntLit 1, IntLit 1), false);
      (LtEq (IntLit 0, IntLit 0), true);
      (LtEq (IntLit 0, IntLit 1), true);
      (LtEq (IntLit 1, IntLit 0), false);
      (LtEq (IntLit 1, IntLit 1), true);
    ]

let test_cases_control_flow : (string * Ast.expr * exec_res) list =
  let open Ast in
  let mapf ((x : expr), (y : exec_res)) = (show x, x, y) in
  List.map mapf
    [
      (If (BoolLit true, IntLit 1, IntLit 2), Res (store_empty, Int 1));
      (If (BoolLit false, IntLit 1, IntLit 2), Res (store_empty, Int 2));
      ( If (BoolLit true, IntLit 1, Add (IntLit 1, IntLit 2)),
        Res (store_empty, Int 1) );
      ( If (BoolLit false, IntLit 1, Add (IntLit 1, IntLit 2)),
        Res (store_empty, Int 3) );
      (If (IntLit 2, IntLit 1, IntLit 2), TypingError);
    ]

(* TODO - tests with variable assignment and references *)

let create_test ((name : string), (inp : Ast.expr), (exp : exec_res)) =
  name >:: fun _ ->
  let out = Ast_executor.execute inp in
  assert_equal exp out ~printer:Ast_executor.show_exec_res

let suite =
  "AST Executor"
  >::: [
         "Arithmetic" >::: List.map create_test test_cases_arithmetic;
         "Booleans" >::: List.map create_test test_cases_booleans;
         "Integer Comparisons"
         >::: List.map create_test test_cases_integer_comparisons;
         "Control Flow" >::: List.map create_test test_cases_control_flow;
       ]
