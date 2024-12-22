open Core
open OUnit2
open Pq_lang
open Ast
open Vtype
open Testing_utils

let test_cases_equality : test list =
  let create_positive_test ((x : plain_expr), (y : plain_expr)) =
    let name =
      sprintf "%s =? %s"
        (get_asp_printer (PrintSexp sexp_of_unit) x)
        (get_asp_printer (PrintSexp sexp_of_unit) y)
    in
    name >:: fun _ -> assert_bool "not equal" (equal_plain_expr x y)
  in
  let create_negative_test ((x : plain_expr), (y : plain_expr)) =
    let name =
      sprintf "%s =? %s"
        (get_asp_printer (PrintSexp sexp_of_unit) x)
        (get_asp_printer (PrintSexp sexp_of_unit) y)
    in
    name >:: fun _ -> assert_bool "equal" (not (equal_plain_expr x y))
  in
  List.map ~f:create_positive_test
    [
      (IntLit ((), 1), IntLit ((), 1));
      ( Add ((), IntLit ((), 1), IntLit ((), 2)),
        Add ((), IntLit ((), 1), IntLit ((), 2)) );
      (Neg ((), IntLit ((), 1)), Neg ((), IntLit ((), 1)));
      ( Subtr ((), IntLit ((), 1), IntLit ((), 2)),
        Subtr ((), IntLit ((), 1), IntLit ((), 2)) );
      ( Mult ((), IntLit ((), 1), IntLit ((), 2)),
        Mult ((), IntLit ((), 1), IntLit ((), 2)) );
      (BoolLit ((), true), BoolLit ((), true));
      (BNot ((), BoolLit ((), true)), BNot ((), BoolLit ((), true)));
      ( BOr ((), BoolLit ((), true), BoolLit ((), false)),
        BOr ((), BoolLit ((), true), BoolLit ((), false)) );
      ( BAnd ((), BoolLit ((), true), BoolLit ((), false)),
        BAnd ((), BoolLit ((), true), BoolLit ((), false)) );
      ( Eq ((), IntLit ((), 1), IntLit ((), 2)),
        Eq ((), IntLit ((), 1), IntLit ((), 2)) );
      ( Gt ((), IntLit ((), 1), IntLit ((), 2)),
        Gt ((), IntLit ((), 1), IntLit ((), 2)) );
      ( GtEq ((), IntLit ((), 1), IntLit ((), 2)),
        GtEq ((), IntLit ((), 1), IntLit ((), 2)) );
      ( Lt ((), IntLit ((), 1), IntLit ((), 2)),
        Lt ((), IntLit ((), 1), IntLit ((), 2)) );
      ( LtEq ((), IntLit ((), 1), IntLit ((), 2)),
        LtEq ((), IntLit ((), 1), IntLit ((), 2)) );
      ( If ((), BoolLit ((), true), IntLit ((), 1), IntLit ((), 2)),
        If ((), BoolLit ((), true), IntLit ((), 1), IntLit ((), 2)) );
      (Var ((), "x"), Var ((), "x"));
      ( Let ((), "x", IntLit ((), 1), IntLit ((), 2)),
        Let ((), "x", IntLit ((), 1), IntLit ((), 2)) );
      ( Fun ((), ("x", VTypeInt), IntLit ((), 1)),
        Fun ((), ("x", VTypeInt), IntLit ((), 1)) );
      ( App ((), IntLit ((), 1), IntLit ((), 2)),
        App ((), IntLit ((), 1), IntLit ((), 2)) );
      ( Fix
          ( (),
            ("f", VTypeInt, VTypeInt),
            ("x", VTypeInt),
            App ((), Var ((), "f"), Var ((), "x")) ),
        Fix
          ( (),
            ("f", VTypeInt, VTypeInt),
            ("x", VTypeInt),
            App ((), Var ((), "f"), Var ((), "x")) ) );
      ( App
          ( (),
            Fix
              ( (),
                ("f", VTypeInt, VTypeInt),
                ("x", VTypeInt),
                App ((), Var ((), "f"), Var ((), "x")) ),
            Fun ((), ("x", VTypeInt), Fun ((), ("x", VTypeInt), IntLit ((), 1)))
          ),
        App
          ( (),
            Fix
              ( (),
                ("f", VTypeInt, VTypeInt),
                ("x", VTypeInt),
                App ((), Var ((), "f"), Var ((), "x")) ),
            Fun ((), ("x", VTypeInt), Fun ((), ("x", VTypeInt), IntLit ((), 1)))
          ) );
      ( Let
          ( (),
            "f",
            App
              ( (),
                Fix
                  ( (),
                    ("f", VTypeInt, VTypeInt),
                    ("x", VTypeInt),
                    App ((), Var ((), "f"), Var ((), "x")) ),
                Fun
                  ( (),
                    ("f", VTypeFun (VTypeInt, VTypeInt)),
                    App ((), Var ((), "f"), IntLit ((), 0)) ) ),
            App ((), Var ((), "f"), IntLit ((), 0)) ),
        Let
          ( (),
            "f",
            App
              ( (),
                Fix
                  ( (),
                    ("f", VTypeInt, VTypeInt),
                    ("x", VTypeInt),
                    App ((), Var ((), "f"), Var ((), "x")) ),
                Fun
                  ( (),
                    ("f", VTypeFun (VTypeInt, VTypeInt)),
                    App ((), Var ((), "f"), IntLit ((), 0)) ) ),
            App ((), Var ((), "f"), IntLit ((), 0)) ) );
    ]
  @ List.map ~f:create_negative_test
      [
        (IntLit ((), 2), IntLit ((), 1));
        ( Add ((), IntLit ((), 1), IntLit ((), 2)),
          Add ((), IntLit ((), 2), IntLit ((), 1)) );
        (BoolLit ((), true), BoolLit ((), false));
      ]

let test_cases_to_source_code_inv =
  let open QCheck in
  let open Frontend in
  Test.make ~count:100 ~name:"AST to source code" plain_ast_expr_arb_any
    (fun e ->
      match run_frontend_string (ast_to_source_code e) with
      | Ok e' -> equal_plain_expr e e'
      | _ -> false)

let create_test_cases_expr_node_val (type_arb : 'a QCheck.arbitrary)
    (type_eq : 'a -> 'a -> bool) =
  let open QCheck in
  Test.make ~count:100 (pair type_arb plain_ast_expr_arb_any) (fun (x, e_raw) ->
      let e = fmap ~f:(const x) e_raw in
      type_eq (expr_node_val e) x)

let create_test_cases_expr_node_map_val (t1_arb : 'a QCheck.arbitrary)
    (t1_obs : 'a QCheck.Observable.t) (t1_sexp : 'a -> Sexp.t)
    (t1_eq : 'b -> 'b -> bool) =
  let open QCheck in
  Test.make ~count:100
    (pair (fun1 t1_obs t1_arb)
       (ast_expr_arb_any (PrintSexp t1_sexp) (QCheck.gen t1_arb)))
    (fun (f_, e) ->
      let f = QCheck.Fn.apply f_ in
      let e' = expr_node_map_val ~f e in
      t1_eq (expr_node_val e') (e |> expr_node_val |> f))

let create_test_cases_expr_fmap_root (t1_arb : 'a QCheck.arbitrary)
    (t1_obs : 'a QCheck.Observable.t) (t1_sexp : 'a -> Sexp.t)
    (t2_arb : 'b QCheck.arbitrary) (t2_eq : 'b -> 'b -> bool) =
  let open QCheck in
  Test.make ~count:100
    (triple t1_arb (fun1 t1_obs t2_arb)
       (ast_expr_arb_any (PrintSexp t1_sexp) (QCheck.gen t1_arb)))
    (fun (x, f_, e) ->
      let f = QCheck.Fn.apply f_ in
      let e' = fmap ~f:(Core.Fn.compose f (const x)) e in
      t2_eq (expr_node_val e') (f x))

let suite =
  "AST Tests"
  >::: [
         "Equality Tests" >::: test_cases_equality;
         QCheck_runner.to_ounit2_test test_cases_to_source_code_inv;
         "AST node value"
         >::: List.map
                ~f:(fun (name, test) ->
                  name >::: [ QCheck_runner.to_ounit2_test test ])
                [
                  ( "unit",
                    create_test_cases_expr_node_val QCheck.unit equal_unit );
                  ("int", create_test_cases_expr_node_val QCheck.int equal_int);
                  ( "bool",
                    create_test_cases_expr_node_val QCheck.bool equal_bool );
                  ( "string",
                    create_test_cases_expr_node_val QCheck.string equal_string
                  );
                  ( "string option",
                    create_test_cases_expr_node_val
                      QCheck.(option string)
                      (equal_option equal_string) );
                  ( "int list",
                    create_test_cases_expr_node_val
                      QCheck.(list int)
                      (equal_list equal_int) );
                ];
         "AST expr node map val"
         >::: List.map
                ~f:(fun (name, test) ->
                  name >::: [ QCheck_runner.to_ounit2_test test ])
                [
                  ( "unit",
                    create_test_cases_expr_node_map_val QCheck.unit
                      QCheck.Observable.unit sexp_of_unit equal_unit );
                  ( "int",
                    create_test_cases_expr_node_map_val QCheck.int
                      QCheck.Observable.int sexp_of_int equal_int );
                  ( "string",
                    create_test_cases_expr_node_map_val QCheck.string
                      QCheck.Observable.string sexp_of_string equal_string );
                ];
         "AST fmap root"
         >::: List.map
                ~f:(fun (name, test) ->
                  name >::: [ QCheck_runner.to_ounit2_test test ])
                [
                  ( "unit -> int",
                    create_test_cases_expr_fmap_root QCheck.unit
                      QCheck.Observable.unit sexp_of_unit QCheck.int equal_int
                  );
                  ( "int -> string",
                    create_test_cases_expr_fmap_root QCheck.int
                      QCheck.Observable.int sexp_of_int QCheck.string
                      equal_string );
                  ( "bool -> int",
                    create_test_cases_expr_fmap_root QCheck.bool
                      QCheck.Observable.bool sexp_of_bool QCheck.int equal_int
                  );
                  ( "string -> int",
                    create_test_cases_expr_fmap_root QCheck.string
                      QCheck.Observable.string sexp_of_string QCheck.int
                      equal_int );
                ];
       ]
