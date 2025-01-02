open Core
open OUnit2
open Pq_lang
open Utils
open Vtype
open Custom_types
open Pattern
open Typing
open Ast_executor
open Testing_utils

type basic_test_case =
  string * unit SimpleTypeChecker.typed_program_expression * exec_res

let make_store (vars : (string * Ast_executor.value) list) : Ast_executor.store
    =
  List.fold_left vars
    ~f:(fun store (name, value) -> store_set store ~key:name ~value)
    ~init:Ast_executor.empty_store

let type_expr ?(type_ctx : SetTypingTypeContext.t option) (e : Ast.plain_expr) =
  match
    type_expr
      ~type_ctx:(Option.value ~default:SetTypingTypeContext.empty type_ctx)
      e
  with
  | Ok x -> x
  | Error err ->
      failwith
        (sprintf "Typing error:\nExpression: %s\nError: %s"
           (Ast.ast_to_source_code ~use_newlines:true e)
           (print_typing_error err))

let test_cases_unit_value : basic_test_case list =
  let open Ast in
  let mapf ((x : plain_expr), (y : value)) =
    (ast_to_source_code x, type_expr x, Ok y)
  in
  List.map ~f:mapf
    [
      (UnitLit (), Unit);
      (If ((), BoolLit ((), true), UnitLit (), UnitLit ()), Unit);
    ]

let test_cases_arithmetic : basic_test_case list =
  let open Ast in
  let mapf ((x : plain_expr), (y : int)) =
    (ast_to_source_code x, type_expr x, Ok (Int y))
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
  let open Ast in
  let mapf ((x : plain_expr), (y : bool)) =
    (ast_to_source_code x, type_expr x, Ok (Bool y))
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
  let open Ast in
  let mapf ((x : plain_expr), (y : value)) =
    (ast_to_source_code x, type_expr x, Ok y)
  in
  List.map ~f:mapf
    [
      ( Pair ((), IntLit ((), 1), BoolLit ((), true)),
        Ast_executor.Pair (Int 1, Bool true) );
      ( Pair
          ( (),
            Add ((), IntLit ((), 2), IntLit ((), 4)),
            BOr ((), BoolLit ((), true), BoolLit ((), false)) ),
        Ast_executor.Pair (Int 6, Bool true) );
    ]

let test_cases_integer_comparisons : basic_test_case list =
  let open Ast in
  let mapf ((x : plain_expr), (y : bool)) =
    (ast_to_source_code x, type_expr x, Ok (Bool y))
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
  let open Ast in
  let mapf ((x : plain_expr), (y : exec_res)) =
    (ast_to_source_code x, type_expr x, y)
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
  let open Ast in
  let mapf ((x : plain_expr), (y : exec_res)) =
    (ast_to_source_code x, type_expr x, y)
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
      ( Let
          ( (),
            "f",
            Fun ((), ("x", VTypeInt), Var ((), "x")),
            App ((), Var ((), "f"), IntLit ((), 8)) ),
        Ok (Int 8) );
    ]

let test_cases_functions : basic_test_case list =
  let open Ast in
  let mapf ((x : plain_expr), (y : exec_res)) =
    (ast_to_source_code x, type_expr x, y)
  in
  List.map ~f:mapf
    [
      ( Fun ((), ("x", VTypeInt), Var ((), "x")),
        Ok
          (Closure
             {
               param = ("x", VTypeInt);
               out_type = VTypeInt;
               body = Var ((VTypeInt, ()), "x");
               store = empty_store;
             }) );
      ( Fun ((), ("x", VTypeBool), BOr ((), Var ((), "x"), BoolLit ((), true))),
        Ok
          (Closure
             {
               param = ("x", VTypeBool);
               out_type = VTypeBool;
               body =
                 BOr
                   ( (VTypeBool, ()),
                     Var ((VTypeBool, ()), "x"),
                     BoolLit ((VTypeBool, ()), true) );
               store = empty_store;
             }) );
      ( App
          ( (),
            App
              ( (),
                Fun
                  ( (),
                    ("a", VTypeInt),
                    Fun
                      ( (),
                        ("b", VTypeInt),
                        Add ((), Var ((), "a"), Var ((), "b")) ) ),
                IntLit ((), 3) ),
            IntLit ((), 5) ),
        Ok (Int 8) );
      ( App
          ( (),
            App
              ( (),
                App
                  ( (),
                    Fun
                      ( (),
                        ("b", VTypeBool),
                        Fun
                          ( (),
                            ("x", VTypeInt),
                            Fun
                              ( (),
                                ("y", VTypeInt),
                                If
                                  ( (),
                                    Var ((), "b"),
                                    Var ((), "x"),
                                    Var ((), "y") ) ) ) ),
                    BoolLit ((), true) ),
                IntLit ((), 1) ),
            IntLit ((), 2) ),
        Ok (Int 1) );
    ]

let test_cases_recursion : basic_test_case list =
  let open Ast in
  let mapf ((x : plain_expr), (y : exec_res)) =
    (ast_to_source_code x, type_expr x, y)
  in
  List.map ~f:mapf
    [
      ( Let
          ( (),
            "f",
            Fix
              ( (),
                ("f", VTypeInt, VTypeInt),
                ("x", VTypeInt),
                If
                  ( (),
                    Eq ((), Var ((), "x"), IntLit ((), 0)),
                    IntLit ((), 0),
                    Add
                      ( (),
                        Var ((), "x"),
                        App
                          ( (),
                            Var ((), "f"),
                            Subtr ((), Var ((), "x"), IntLit ((), 1)) ) ) ) ),
            App ((), Var ((), "f"), IntLit ((), 5)) ),
        Ok (Int 15) );
    ]

let test_cases_match : basic_test_case list =
  let open Ast in
  let mapf
      ( (type_ctx : SetTypingTypeContext.t option),
        (x : plain_expr),
        (y : exec_res) ) =
    ( ast_to_source_code x,
      type_expr
        ~type_ctx:(Option.value ~default:SetTypingTypeContext.empty type_ctx)
        x,
      y )
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
                Nonempty_list.from_list_unsafe
                  [
                    ( PatName ("y", VTypeInt),
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
                Nonempty_list.from_list_unsafe
                  [
                    ( PatName ("y", VTypeBool),
                      If ((), Var ((), "y"), IntLit ((), 4), IntLit ((), 0)) );
                    (PatName ("z", VTypeBool), IntLit ((), 9));
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
                Nonempty_list.from_list_unsafe
                  [
                    ( PatPair (PatName ("y", VTypeBool), PatName ("z", VTypeInt)),
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
                Nonempty_list.from_list_unsafe
                  [
                    ( PatPair
                        ( PatName ("y", VTypePair (VTypeBool, VTypeBool)),
                          PatName ("z", VTypeInt) ),
                      Var ((), "y") );
                  ] ) ),
        Ok (Pair (Bool true, Bool true)) );
      ( Some
          (SetTypingTypeContext.create
             ~custom_types:
               [
                 ("bool_box", [ ("BoolBox", VTypeBool) ]);
                 ( "int_list",
                   [
                     ("Nil", VTypeUnit);
                     ("Cons", VTypePair (VTypeInt, VTypeCustom "int_list"));
                   ] );
               ]),
        Match
          ( (),
            Constructor ((), "BoolBox", BoolLit ((), true)),
            Nonempty_list.from_list_unsafe
              [
                ( PatConstructor ("BoolBox", PatName ("x", VTypeBool)),
                  Var ((), "x") );
              ] ),
        Ok (Bool true) );
      ( Some
          (SetTypingTypeContext.create
             ~custom_types:
               [
                 ("bool_box", [ ("BoolBox", VTypeBool) ]);
                 ( "int_list",
                   [
                     ("Nil", VTypeUnit);
                     ("Cons", VTypePair (VTypeInt, VTypeCustom "int_list"));
                   ] );
               ]),
        Match
          ( (),
            Constructor
              ( (),
                "Cons",
                Pair ((), IntLit ((), 7), Constructor ((), "Nil", UnitLit ()))
              ),
            Nonempty_list.from_list_unsafe
              [
                ( PatConstructor ("Nil", PatName ("x", VTypeUnit)),
                  IntLit ((), 0) );
                ( PatConstructor
                    ( "Cons",
                      PatPair
                        ( PatName ("x", VTypeInt),
                          PatName ("y", VTypeCustom "int_list") ) ),
                  Var ((), "x") );
              ] ),
        Ok (Int 7) );
    ]

let test_cases_constructor : basic_test_case list =
  let open Ast in
  let mapf ((x : custom_type list), (y : plain_expr), (z : exec_res)) =
    ( ast_to_source_code y,
      type_expr ~type_ctx:(SetTypingTypeContext.create ~custom_types:x) y,
      z )
  in
  let ct_list : custom_type =
    ( "list",
      [ ("Nil", VTypeUnit); ("Cons", VTypePair (VTypeInt, VTypeCustom "list")) ]
    )
  in
  let ct_int_box : custom_type = ("int_box", [ ("IntBox", VTypeInt) ]) in
  List.map ~f:mapf
    [
      ( [ ct_list ],
        Constructor ((), "Nil", UnitLit ()),
        Ok (CustomTypeValue (ct_list, "Nil", Unit)) );
      ( [ ct_list ],
        Constructor
          ( (),
            "Cons",
            Pair ((), IntLit ((), 1), Constructor ((), "Nil", UnitLit ())) ),
        Ok
          (CustomTypeValue
             ( ct_list,
               "Cons",
               Pair (Int 1, CustomTypeValue (ct_list, "Nil", Unit)) )) );
      ( [ ct_int_box ],
        Constructor ((), "IntBox", Add ((), IntLit ((), 2), IntLit ((), 5))),
        Ok (CustomTypeValue (ct_int_box, "IntBox", Int 7)) );
      ( [ ct_int_box ],
        Let
          ( (),
            "x",
            IntLit ((), 2),
            Constructor ((), "IntBox", Add ((), Var ((), "x"), IntLit ((), 5)))
          ),
        Ok (CustomTypeValue (ct_int_box, "IntBox", Int 7)) );
    ]

let create_test
    ( (name : string),
      (inp : unit SimpleTypeChecker.typed_program_expression),
      (exp : exec_res) ) =
  name >:: fun _ ->
  let out = Ast_executor.SimpleExecutor.execute_program inp in
  assert_equal exp out ~cmp:override_equal_exec_res
    ~printer:Ast_executor.show_exec_res

let suite =
  "AST Executor"
  >::: [
         "Unit Value" >::: List.map ~f:create_test test_cases_unit_value;
         "Arithmetic" >::: List.map ~f:create_test test_cases_arithmetic;
         "Booleans" >::: List.map ~f:create_test test_cases_booleans;
         "Pairs" >::: List.map ~f:create_test test_cases_pairs;
         "Integer Comparisons"
         >::: List.map ~f:create_test test_cases_integer_comparisons;
         "Control Flow" >::: List.map ~f:create_test test_cases_control_flow;
         "Variables" >::: List.map ~f:create_test test_cases_variables;
         "Functions" >::: List.map ~f:create_test test_cases_functions;
         "Recursion" >::: List.map ~f:create_test test_cases_recursion;
         "Match" >::: List.map ~f:create_test test_cases_match;
         "Constructor" >::: List.map ~f:create_test test_cases_constructor;
       ]
