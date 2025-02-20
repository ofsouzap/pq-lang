open Core
open OUnit2
open Pq_lang
open Utils
open Vtype
open Pattern
open Program
open Typing
open Program_executor
open Testing_utils

type basic_test_case =
  string * (unit, unit) SimpleTypeChecker.typed_program * exec_res

let make_store (vars : (string * Program_executor.value) list) :
    Program_executor.store =
  List.fold_left vars
    ~f:(fun store (name, value) -> store_set store ~key:name ~value)
    ~init:Program_executor.empty_store

let type_expr ?(custom_types : CustomType.plain_t list option)
    ?(top_level_defns : (unit, unit) top_level_defn list option)
    (e : Expr.plain_expr) : (unit, unit) SimpleTypeChecker.typed_program =
  match
    type_program
      {
        custom_types = Option.value ~default:[] custom_types;
        top_level_defns = Option.value ~default:[] top_level_defns;
        e;
      }
  with
  | Ok x -> x
  | Error err ->
      failwith
        (sprintf "Typing error:\nExpression: %s\nError: %s"
           (Expr.to_source_code ~use_newlines:true e)
           (print_typing_error err))

let test_cases_unit_value : basic_test_case list =
  let open Expr in
  let mapf ((x : plain_expr), (y : value)) =
    (Expr.to_source_code x, type_expr x, Ok y)
  in
  List.map ~f:mapf
    [
      (UnitLit (), Unit);
      (If ((), BoolLit ((), true), UnitLit (), UnitLit ()), Unit);
    ]

let test_cases_arithmetic : basic_test_case list =
  let open Expr in
  let mapf ((x : plain_expr), (y : int)) =
    (Expr.to_source_code x, type_expr x, Ok (Int y))
  in
  List.map ~f:mapf
    [
      (IntLit ((), 0), 0);
      (IntLit ((), 5), 5);
      (Neg ((), IntLit ((), 7)), -7);
      (IntLit ((), -5), -5);
      (Add ((), IntLit ((), 1), IntLit ((), 2)), 3);
      (Mult ((), IntLit ((), 1), IntLit ((), 2)), 2);
      (Mult ((), Neg ((), IntLit ((), 1)), IntLit ((), 2)), -2);
      ( Add
          ( (),
            IntLit ((), 1),
            Mult ((), IntLit ((), 4), Add ((), IntLit ((), 1), IntLit ((), 2)))
          ),
        13 );
      (Mult ((), Add ((), IntLit ((), 1), IntLit ((), 2)), IntLit ((), 3)), 9);
      ( Mult
          ( (),
            Add ((), IntLit ((), 1), IntLit ((), 2)),
            Add ((), IntLit ((), 3), IntLit ((), 4)) ),
        21 );
      ( Add
          ( (),
            Add ((), Add ((), IntLit ((), 0), IntLit ((), 0)), IntLit ((), 0)),
            IntLit ((), 0) ),
        0 );
    ]

let test_cases_booleans : basic_test_case list =
  let open Expr in
  let mapf ((x : plain_expr), (y : bool)) =
    (Expr.to_source_code x, type_expr x, Ok (Bool y))
  in
  List.map ~f:mapf
    [
      (BoolLit ((), true), true);
      (BoolLit ((), false), false);
      (BNot ((), BoolLit ((), true)), false);
      (BNot ((), BoolLit ((), false)), true);
      (BOr ((), BoolLit ((), true), BoolLit ((), true)), true);
      (BOr ((), BoolLit ((), true), BoolLit ((), false)), true);
      (BOr ((), BoolLit ((), false), BoolLit ((), true)), true);
      (BOr ((), BoolLit ((), false), BoolLit ((), false)), false);
      (BAnd ((), BoolLit ((), true), BoolLit ((), true)), true);
      (BAnd ((), BoolLit ((), true), BoolLit ((), false)), false);
      (BAnd ((), BoolLit ((), false), BoolLit ((), true)), false);
      (BAnd ((), BoolLit ((), false), BoolLit ((), false)), false);
      (Eq ((), BoolLit ((), true), BoolLit ((), true)), true);
      (Eq ((), BoolLit ((), true), BoolLit ((), false)), false);
      (Eq ((), BoolLit ((), false), BoolLit ((), true)), false);
      (Eq ((), BoolLit ((), false), BoolLit ((), false)), true);
    ]

let test_cases_pairs : basic_test_case list =
  let open Expr in
  let mapf ((x : plain_expr), (y : value)) =
    (Expr.to_source_code x, type_expr x, Ok y)
  in
  List.map ~f:mapf
    [
      ( Pair ((), IntLit ((), 1), BoolLit ((), true)),
        Program_executor.Pair (Int 1, Bool true) );
      ( Pair
          ( (),
            Add ((), IntLit ((), 2), IntLit ((), 4)),
            BOr ((), BoolLit ((), true), BoolLit ((), false)) ),
        Program_executor.Pair (Int 6, Bool true) );
    ]

let test_cases_integer_comparisons : basic_test_case list =
  let open Expr in
  let mapf ((x : plain_expr), (y : bool)) =
    (Expr.to_source_code x, type_expr x, Ok (Bool y))
  in
  List.map ~f:mapf
    [
      (Eq ((), IntLit ((), 0), IntLit ((), 0)), true);
      (Eq ((), IntLit ((), 0), IntLit ((), 1)), false);
      (Eq ((), IntLit ((), 1), IntLit ((), 0)), false);
      (Eq ((), IntLit ((), 1), IntLit ((), 1)), true);
      (Gt ((), IntLit ((), 0), IntLit ((), 0)), false);
      (Gt ((), IntLit ((), 0), IntLit ((), 1)), false);
      (Gt ((), IntLit ((), 1), IntLit ((), 0)), true);
      (Gt ((), IntLit ((), 1), IntLit ((), 1)), false);
      (GtEq ((), IntLit ((), 0), IntLit ((), 0)), true);
      (GtEq ((), IntLit ((), 0), IntLit ((), 1)), false);
      (GtEq ((), IntLit ((), 1), IntLit ((), 0)), true);
      (GtEq ((), IntLit ((), 1), IntLit ((), 1)), true);
      (Lt ((), IntLit ((), 0), IntLit ((), 0)), false);
      (Lt ((), IntLit ((), 0), IntLit ((), 1)), true);
      (Lt ((), IntLit ((), 1), IntLit ((), 0)), false);
      (Lt ((), IntLit ((), 1), IntLit ((), 1)), false);
      (LtEq ((), IntLit ((), 0), IntLit ((), 0)), true);
      (LtEq ((), IntLit ((), 0), IntLit ((), 1)), true);
      (LtEq ((), IntLit ((), 1), IntLit ((), 0)), false);
      (LtEq ((), IntLit ((), 1), IntLit ((), 1)), true);
    ]

let test_cases_control_flow : basic_test_case list =
  let open Expr in
  let mapf ((x : plain_expr), (y : exec_res)) =
    (Expr.to_source_code x, type_expr x, y)
  in
  List.map ~f:mapf
    [
      (If ((), BoolLit ((), true), IntLit ((), 1), IntLit ((), 2)), Ok (Int 1));
      (If ((), BoolLit ((), false), IntLit ((), 1), IntLit ((), 2)), Ok (Int 2));
      ( If
          ( (),
            BoolLit ((), true),
            IntLit ((), 1),
            Add ((), IntLit ((), 1), IntLit ((), 2)) ),
        Ok (Int 1) );
      ( If
          ( (),
            BoolLit ((), false),
            IntLit ((), 1),
            Add ((), IntLit ((), 1), IntLit ((), 2)) ),
        Ok (Int 3) );
    ]

let test_cases_variables : basic_test_case list =
  let open Expr in
  let mapf ((x : plain_expr), (y : exec_res)) =
    (Expr.to_source_code x, type_expr x, y)
  in
  List.map ~f:mapf
    [
      (Let ((), "x", IntLit ((), 1), Var ((), "x")), Ok (Int 1));
      ( Let ((), "x", IntLit ((), 1), Add ((), Var ((), "x"), IntLit ((), 2))),
        Ok (Int 3) );
      ( Let
          ( (),
            "x",
            IntLit ((), 1),
            Let ((), "y", IntLit ((), 2), Add ((), Var ((), "x"), Var ((), "y")))
          ),
        Ok (Int 3) );
      ( Let
          ((), "x", IntLit ((), 1), Let ((), "x", IntLit ((), 2), Var ((), "x"))),
        Ok (Int 2) );
      (Let ((), "x", BoolLit ((), true), Var ((), "x")), Ok (Bool true));
      ( Let
          ((), "x", BoolLit ((), false), BOr ((), Var ((), "x"), Var ((), "x"))),
        Ok (Bool false) );
    ]

let test_cases_match : basic_test_case list =
  let open Expr in
  let mapf
      ( (custom_types : CustomType.plain_t list option),
        (x : plain_expr),
        (y : exec_res) ) : basic_test_case =
    (Expr.to_source_code x, type_expr ?custom_types x, y)
  in
  List.map ~f:mapf
    [
      ( None,
        Let
          ( (),
            "x",
            IntLit ((), 3),
            Match
              ( (),
                Var ((), "x"),
                VTypeInt,
                Nonempty_list.from_list_unsafe
                  [
                    ( PatName ((), "y", VTypeInt),
                      Add ((), Var ((), "y"), IntLit ((), 1)) );
                  ] ) ),
        Ok (Int 4) );
      ( None,
        Let
          ( (),
            "x",
            BoolLit ((), false),
            Match
              ( (),
                Var ((), "x"),
                VTypeInt,
                Nonempty_list.from_list_unsafe
                  [
                    ( PatName ((), "y", VTypeBool),
                      If ((), Var ((), "y"), IntLit ((), 4), IntLit ((), 0)) );
                    (PatName ((), "z", VTypeBool), IntLit ((), 9));
                  ] ) ),
        Ok (Int 0) );
      ( None,
        Let
          ( (),
            "x",
            Pair ((), BoolLit ((), true), IntLit ((), 1)),
            Match
              ( (),
                Var ((), "x"),
                VTypeInt,
                Nonempty_list.from_list_unsafe
                  [
                    ( PatPair
                        ( (),
                          PatName ((), "y", VTypeBool),
                          PatName ((), "z", VTypeInt) ),
                      If ((), Var ((), "y"), Var ((), "z"), IntLit ((), 0)) );
                  ] ) ),
        Ok (Int 1) );
      ( None,
        Let
          ( (),
            "x",
            Pair
              ( (),
                Pair ((), BoolLit ((), true), BoolLit ((), true)),
                IntLit ((), 1) ),
            Match
              ( (),
                Var ((), "x"),
                VTypePair (VTypeBool, VTypeBool),
                Nonempty_list.from_list_unsafe
                  [
                    ( PatPair
                        ( (),
                          PatName ((), "y", VTypePair (VTypeBool, VTypeBool)),
                          PatName ((), "z", VTypeInt) ),
                      Var ((), "y") );
                  ] ) ),
        Ok (Pair (Bool true, Bool true)) );
      ( Some
          [
            CustomType.VariantType ("bool_box", [ ("BoolBox", VTypeBool) ]);
            CustomType.VariantType
              ( "int_list",
                [
                  ("Nil", VTypeUnit);
                  ("Cons", VTypePair (VTypeInt, VTypeCustom "int_list"));
                ] );
          ],
        Match
          ( (),
            Constructor ((), "BoolBox", BoolLit ((), true)),
            VTypeBool,
            Nonempty_list.from_list_unsafe
              [
                ( PatConstructor ((), "BoolBox", PatName ((), "x", VTypeBool)),
                  Var ((), "x") );
              ] ),
        Ok (Bool true) );
      ( Some
          [
            CustomType.VariantType ("bool_box", [ ("BoolBox", VTypeBool) ]);
            CustomType.VariantType
              ( "int_list",
                [
                  ("Nil", VTypeUnit);
                  ("Cons", VTypePair (VTypeInt, VTypeCustom "int_list"));
                ] );
          ],
        Match
          ( (),
            Constructor
              ( (),
                "Cons",
                Pair ((), IntLit ((), 7), Constructor ((), "Nil", UnitLit ()))
              ),
            VTypeInt,
            Nonempty_list.from_list_unsafe
              [
                ( PatConstructor ((), "Nil", PatName ((), "x", VTypeUnit)),
                  IntLit ((), 0) );
                ( PatConstructor
                    ( (),
                      "Cons",
                      PatPair
                        ( (),
                          PatName ((), "x", VTypeInt),
                          PatName ((), "y", VTypeCustom "int_list") ) ),
                  Var ((), "x") );
              ] ),
        Ok (Int 7) );
    ]

let test_cases_constructor : basic_test_case list =
  let open Expr in
  let mapf
      ((variant_types : VariantType.t list), (y : plain_expr), (z : exec_res)) =
    ( Expr.to_source_code y,
      type_expr
        ~custom_types:
          (List.map ~f:(fun vt -> CustomType.VariantType vt) variant_types)
        y,
      z )
  in
  let vt_list : VariantType.t =
    ( "list",
      [ ("Nil", VTypeUnit); ("Cons", VTypePair (VTypeInt, VTypeCustom "list")) ]
    )
  in
  let vt_int_box : VariantType.t = ("int_box", [ ("IntBox", VTypeInt) ]) in
  List.map ~f:mapf
    [
      ( [ vt_list ],
        Constructor ((), "Nil", UnitLit ()),
        Ok (VariantTypeValue (vt_list, "Nil", Unit)) );
      ( [ vt_list ],
        Constructor
          ( (),
            "Cons",
            Pair ((), IntLit ((), 1), Constructor ((), "Nil", UnitLit ())) ),
        Ok
          (VariantTypeValue
             ( vt_list,
               "Cons",
               Pair (Int 1, VariantTypeValue (vt_list, "Nil", Unit)) )) );
      ( [ vt_int_box ],
        Constructor ((), "IntBox", Add ((), IntLit ((), 2), IntLit ((), 5))),
        Ok (VariantTypeValue (vt_int_box, "IntBox", Int 7)) );
      ( [ vt_int_box ],
        Let
          ( (),
            "x",
            IntLit ((), 2),
            Constructor ((), "IntBox", Add ((), Var ((), "x"), IntLit ((), 5)))
          ),
        Ok (VariantTypeValue (vt_int_box, "IntBox", Int 7)) );
    ]

(* TODO - tests with top-level definitions (recursive and non-recursive) *)

let create_test
    ( (name : string),
      (inp : (unit, unit) SimpleTypeChecker.typed_program),
      (exp : exec_res) ) =
  name >:: fun _ ->
  let out = Program_executor.SimpleExecutor.execute_program inp in
  assert_equal exp out ~cmp:override_equal_exec_res
    ~printer:Program_executor.show_exec_res

let suite =
  "Expr Executor"
  >::: [
         "Unit Value" >::: List.map ~f:create_test test_cases_unit_value;
         "Arithmetic" >::: List.map ~f:create_test test_cases_arithmetic;
         "Booleans" >::: List.map ~f:create_test test_cases_booleans;
         "Pairs" >::: List.map ~f:create_test test_cases_pairs;
         "Integer Comparisons"
         >::: List.map ~f:create_test test_cases_integer_comparisons;
         "Control Flow" >::: List.map ~f:create_test test_cases_control_flow;
         "Variables" >::: List.map ~f:create_test test_cases_variables;
         "Match" >::: List.map ~f:create_test test_cases_match;
         "Constructor" >::: List.map ~f:create_test test_cases_constructor;
       ]
