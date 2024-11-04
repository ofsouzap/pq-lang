open Core
open OUnit2
open Pq_lang
open Ast_executor
open Test_utils

let make_store (vars : (string * Ast_executor.value) list) : Ast_executor.store
    =
  List.fold_left vars
    ~f:(fun store (name, value) -> store_set store ~key:name ~value)
    ~init:Ast_executor.empty_store

let test_cases_arithmetic : (string * Ast.expr * exec_res) list =
  let open Ast in
  let mapf ((x : expr), (y : int)) = (show_ast x, x, Res (Int y)) in
  List.map ~f:mapf
    [
      (IntLit 0, 0);
      (IntLit 5, 5);
      (Neg (IntLit 7), -7);
      (IntLit (-5), -5);
      (Add (IntLit 1, IntLit 2), 3);
      (Mult (IntLit 1, IntLit 2), 2);
      (Mult (Neg (IntLit 1), IntLit 2), -2);
      (Add (IntLit 1, Mult (IntLit 4, Add (IntLit 1, IntLit 2))), 13);
      (Mult (Add (IntLit 1, IntLit 2), IntLit 3), 9);
      (Mult (Add (IntLit 1, IntLit 2), Add (IntLit 3, IntLit 4)), 21);
      (Add (Add (Add (IntLit 0, IntLit 0), IntLit 0), IntLit 0), 0);
    ]

let test_cases_booleans : (string * Ast.expr * exec_res) list =
  let open Ast in
  let mapf ((x : expr), (y : bool)) = (show_ast x, x, Res (Bool y)) in
  List.map ~f:mapf
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
  let mapf ((x : expr), (y : bool)) = (show_ast x, x, Res (Bool y)) in
  List.map ~f:mapf
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
  let mapf ((x : expr), (y : exec_res)) = (show_ast x, x, y) in
  List.map ~f:mapf
    [
      (If (BoolLit true, IntLit 1, IntLit 2), Res (Int 1));
      (If (BoolLit false, IntLit 1, IntLit 2), Res (Int 2));
      (If (BoolLit true, IntLit 1, Add (IntLit 1, IntLit 2)), Res (Int 1));
      (If (BoolLit false, IntLit 1, Add (IntLit 1, IntLit 2)), Res (Int 3));
      (If (IntLit 2, IntLit 1, IntLit 2), Err (TypingError empty_typing_error));
    ]

let test_cases_variables : (string * Ast.expr * exec_res) list =
  let open Ast in
  let mapf ((x : expr), (y : exec_res)) = (show_ast x, x, y) in
  List.map ~f:mapf
    [
      (Var "x", Err (UndefinedVarError "x"));
      (Let ("x", IntLit 1, Var "x"), Res (Int 1));
      (Let ("x", IntLit 1, Add (Var "x", IntLit 2)), Res (Int 3));
      ( Let ("x", IntLit 1, Let ("y", IntLit 2, Add (Var "x", Var "y"))),
        Res (Int 3) );
      (Let ("x", IntLit 1, Let ("x", IntLit 2, Var "x")), Res (Int 2));
      ( Let ("x", Let ("y", IntLit 1, Var "y"), Var "y"),
        Err (UndefinedVarError "y") );
      (Let ("x", BoolLit true, Var "x"), Res (Bool true));
      (Let ("x", BoolLit false, BOr (Var "x", Var "x")), Res (Bool false));
      (Let ("f", Fun ("x", Var "x"), App (Var "f", IntLit 8)), Res (Int 8));
    ]

let test_cases_functions : (string * Ast.expr * exec_res) list =
  let open Ast in
  let mapf ((x : expr), (y : exec_res)) = (show_ast x, x, y) in
  List.map ~f:mapf
    [
      (Fun ("x", Var "x"), Res (Closure ("x", Var "x", empty_store)));
      ( Fun ("x", BOr (Var "x", BoolLit true)),
        Res (Closure ("x", BOr (Var "x", BoolLit true), empty_store)) );
      ( App
          ( App (Fun ("a", Fun ("b", Add (Var "a", Var "b"))), IntLit 3),
            IntLit 5 ),
        Res (Int 8) );
      (App (Fun ("x", Var "y"), IntLit 3), Err (UndefinedVarError "y"));
      ( App
          ( App
              ( App
                  ( Fun
                      ("b", Fun ("x", Fun ("y", If (Var "b", Var "x", Var "y")))),
                    BoolLit true ),
                IntLit 1 ),
            IntLit 2 ),
        Res (Int 1) );
    ]

let test_cases_recursion : (string * Ast.expr * exec_res) list =
  let open Ast in
  let mapf ((x : expr), (y : exec_res)) = (show_ast x, x, y) in
  List.map ~f:mapf
    [
      ( Let
          ( "f",
            Fix
              ( "f",
                "x",
                If
                  ( Eq (Var "x", IntLit 0),
                    IntLit 0,
                    Add (Var "x", App (Var "f", Subtr (Var "x", IntLit 1))) ) ),
            App (Var "f", IntLit 5) ),
        Res (Int 15) );
    ]

let create_test ((name : string), (inp : Ast.expr), (exp : exec_res)) =
  name >:: fun _ ->
  let out = Ast_executor.execute inp in
  assert_equal exp out ~cmp:override_compare_exec_res
    ~printer:Ast_executor.show_exec_res

let suite =
  "AST Executor"
  >::: [
         "Arithmetic" >::: List.map ~f:create_test test_cases_arithmetic;
         "Booleans" >::: List.map ~f:create_test test_cases_booleans;
         "Integer Comparisons"
         >::: List.map ~f:create_test test_cases_integer_comparisons;
         "Control Flow" >::: List.map ~f:create_test test_cases_control_flow;
         "Variables" >::: List.map ~f:create_test test_cases_variables;
         "Functions" >::: List.map ~f:create_test test_cases_functions;
         "Recursion" >::: List.map ~f:create_test test_cases_recursion;
       ]
