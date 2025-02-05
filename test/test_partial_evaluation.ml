open Core
open OUnit2
open Pq_lang
open Utils
open Varname
open Pattern
open Ast
open Partial_evaluation
open Testing_utils

let evaluation_tests =
  let open Unit_partial_evaluator in
  let create_test
      ( (name : string),
        (store : (varname * (plain_expr, closure) Either.t) list),
        (inp : plain_expr),
        (exp : plain_expr) ) =
    name >:: fun _ ->
    let open Result in
    eval ~mrd:100
      {
        store =
          List.fold store ~init:StringMap.empty ~f:(fun acc (name, value) ->
              Map.set acc ~key:name ~data:value);
        e = inp;
      }
    |> function
    | Ok out ->
        assert_equal ~cmp:equal_plain_expr ~printer:ast_to_source_code exp out
    | Error err ->
        assert_failure
          (Sexp.to_string_hum (sexp_of_partial_evaluation_error err))
  in
  List.map ~f:create_test
    [
      (* Basic literals *)
      ("Unit literal", [], UnitLit (), UnitLit ());
      ("Int literal", [], IntLit ((), 1), IntLit ((), 1));
      (* Arithmetic *)
      ( "Add literals",
        [],
        Add ((), IntLit ((), 2), IntLit ((), 3)),
        IntLit ((), 5) );
      ( "Add with defined variable",
        [ ("x", First (IntLit ((), 1))) ],
        Add ((), Var ((), "x"), IntLit ((), 1)),
        IntLit ((), 2) );
      ( "Add with undefined variable",
        [],
        Add ((), Var ((), "x"), IntLit ((), 1)),
        Add ((), Var ((), "x"), IntLit ((), 1)) );
      ("Neg literal", [], Neg ((), IntLit ((), 5)), IntLit ((), -5));
      ( "Subtr literals",
        [],
        Subtr ((), IntLit ((), 7), IntLit ((), 3)),
        IntLit ((), 4) );
      ( "Mult literals",
        [],
        Mult ((), IntLit ((), 3), IntLit ((), 4)),
        IntLit ((), 12) );
      (* Boolean *)
      ("Bool literal", [], BoolLit ((), true), BoolLit ((), true));
      ("BNot literal", [], BNot ((), BoolLit ((), true)), BoolLit ((), false));
      ( "BOr literals",
        [],
        BOr ((), BoolLit ((), false), BoolLit ((), true)),
        BoolLit ((), true) );
      ( "BAnd literals",
        [],
        BAnd ((), BoolLit ((), true), BoolLit ((), true)),
        BoolLit ((), true) );
      (* Pairs *)
      ( "Pair literals",
        [],
        Pair ((), IntLit ((), 1), BoolLit ((), true)),
        Pair ((), IntLit ((), 1), BoolLit ((), true)) );
      (* Comparisons *)
      ( "Eq literals true",
        [],
        Eq ((), IntLit ((), 5), IntLit ((), 5)),
        BoolLit ((), true) );
      ( "Eq literals false",
        [],
        Eq ((), IntLit ((), 5), IntLit ((), 6)),
        BoolLit ((), false) );
      ( "Gt literals true",
        [],
        Gt ((), IntLit ((), 6), IntLit ((), 5)),
        BoolLit ((), true) );
      ( "Lt literals true",
        [],
        Lt ((), IntLit ((), 4), IntLit ((), 5)),
        BoolLit ((), true) );
      ( "GtEq literals false",
        [],
        GtEq ((), IntLit ((), 4), IntLit ((), 5)),
        BoolLit ((), false) );
      ( "LtEq literals true",
        [],
        LtEq ((), IntLit ((), 5), IntLit ((), 5)),
        BoolLit ((), true) );
      (* Control flow *)
      ( "If true branch",
        [],
        If ((), BoolLit ((), true), IntLit ((), 1), IntLit ((), 2)),
        IntLit ((), 1) );
      ( "If with variable",
        [],
        If ((), Var ((), "x"), IntLit ((), 1), IntLit ((), 2)),
        If ((), Var ((), "x"), IntLit ((), 1), IntLit ((), 2)) );
      (* Variables and functions *)
      ( "Variable known value",
        [ ("x", First (IntLit ((), 42))) ],
        Var ((), "x"),
        IntLit ((), 42) );
      ("Variable unknown", [], Var ((), "x"), Var ((), "x"));
      ( "Let simple",
        [],
        Let ((), "x", IntLit ((), 1), Var ((), "x")),
        IntLit ((), 1) );
      ( "App unknown",
        [],
        App ((), Var ((), "f"), IntLit ((), 1)),
        App ((), Var ((), "f"), IntLit ((), 1)) );
      ( "App non-recursive function",
        [
          ( "f",
            Second
              {
                param = "x";
                body = Add ((), Var ((), "x"), IntLit ((), 1));
                store = StringMap.empty;
                recursive = `NonRecursive;
              } );
        ],
        App ((), Var ((), "f"), IntLit ((), 2)),
        IntLit ((), 3) );
      ( "App recursive function (terminating)",
        [
          ( "f",
            Second
              {
                param = "x";
                body =
                  If
                    ( (),
                      Eq ((), Var ((), "x"), IntLit ((), 0)),
                      IntLit ((), 0),
                      App
                        ( (),
                          Var ((), "f"),
                          Subtr ((), Var ((), "x"), IntLit ((), 1)) ) );
                store = StringMap.empty;
                recursive = `Recursive "f";
              } );
        ],
        App ((), Var ((), "f"), IntLit ((), 5)),
        IntLit ((), 0) );
      (* Variant types *)
      ( "Constructor simple",
        [],
        Constructor ((), "Some", IntLit ((), 1)),
        Constructor ((), "Some", IntLit ((), 1)) );
      ( "Match simple",
        [ ("x", First (Constructor ((), "Some", IntLit ((), 1)))) ],
        Match
          ( (),
            Var ((), "x"),
            Nonempty_list.from_list_unsafe
              [
                ( PatConstructor ((), "None", PatName ((), "x", VTypeUnit)),
                  Var ((), "y") );
                ( PatConstructor ((), "Some", PatName ((), "x", VTypeInt)),
                  Var ((), "z") );
              ] ),
        Var ((), "z") );
    ]

let suite =
  "Partial Evaluation" >::: [ "Evaluation tests" >::: evaluation_tests ]
