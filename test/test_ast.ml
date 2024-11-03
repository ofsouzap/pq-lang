open OUnit2
open Pq_lang
open Ast

let test_cases_equality : test list =
  let create_positive_test ((x : expr), (y : expr)) =
    let name = Printf.sprintf "%s =? %s" (show x) (show y) in
    name >:: fun _ -> assert_bool "not equal" (x = y)
  in
  let create_negative_test ((x : expr), (y : expr)) =
    let name = Printf.sprintf "%s =? %s" (show x) (show y) in
    name >:: fun _ -> assert_bool "equal" (x <> y)
  in
  List.map create_positive_test
    [
      (IntLit 1, IntLit 1);
      (Add (IntLit 1, IntLit 2), Add (IntLit 1, IntLit 2));
      (Neg (IntLit 1), Neg (IntLit 1));
      (Subtr (IntLit 1, IntLit 2), Subtr (IntLit 1, IntLit 2));
      (Mult (IntLit 1, IntLit 2), Mult (IntLit 1, IntLit 2));
      (BoolLit true, BoolLit true);
      (BNot (BoolLit true), BNot (BoolLit true));
      (BOr (BoolLit true, BoolLit false), BOr (BoolLit true, BoolLit false));
      (BAnd (BoolLit true, BoolLit false), BAnd (BoolLit true, BoolLit false));
      (Eq (IntLit 1, IntLit 2), Eq (IntLit 1, IntLit 2));
      (Gt (IntLit 1, IntLit 2), Gt (IntLit 1, IntLit 2));
      (GtEq (IntLit 1, IntLit 2), GtEq (IntLit 1, IntLit 2));
      (Lt (IntLit 1, IntLit 2), Lt (IntLit 1, IntLit 2));
      (LtEq (IntLit 1, IntLit 2), LtEq (IntLit 1, IntLit 2));
      ( If (BoolLit true, IntLit 1, IntLit 2),
        If (BoolLit true, IntLit 1, IntLit 2) );
      (Var "x", Var "x");
      ( Let (("x", VTypeInt), IntLit 1, IntLit 2),
        Let (("x", VTypeInt), IntLit 1, IntLit 2) );
      (Fun (("x", VTypeInt), IntLit 1), Fun (("x", VTypeInt), IntLit 1));
      (App (IntLit 1, IntLit 2), App (IntLit 1, IntLit 2));
      (Fix, Fix);
      ( App
          ( Fix,
            Fun
              ( ("x", VTypeFun (VTypeInt, VTypeInt)),
                Fun (("x", VTypeInt), IntLit 1) ) ),
        App
          ( Fix,
            Fun
              ( ("x", VTypeFun (VTypeInt, VTypeInt)),
                Fun (("x", VTypeInt), IntLit 1) ) ) );
      ( Let
          ( ("f", VTypeFun (VTypeInt, VTypeInt)),
            App
              ( Fix,
                Fun
                  (("f", VTypeFun (VTypeInt, VTypeInt)), App (Var "f", IntLit 0))
              ),
            App (Var "f", IntLit 0) ),
        Let
          ( ("f", VTypeFun (VTypeInt, VTypeInt)),
            App
              ( Fix,
                Fun
                  (("f", VTypeFun (VTypeInt, VTypeInt)), App (Var "f", IntLit 0))
              ),
            App (Var "f", IntLit 0) ) );
    ]
  @ List.map create_negative_test
      [
        (IntLit 2, IntLit 1);
        (Add (IntLit 1, IntLit 2), Add (IntLit 2, IntLit 1));
        (BoolLit true, BoolLit false);
      ]

let suite = "AST Tests" >::: [ "Equality Tests" >::: test_cases_equality ]
