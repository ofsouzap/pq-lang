open Core
open OUnit2
open Pq_lang
open Expr
open Testing_utils

let test_cases_equality : test list =
  let create_positive_test ((x : plain_expr), (y : plain_expr)) =
    let name =
      sprintf "%s =? %s"
        (Unit_ast_qcheck_testing.print
           (PrintSexp (sexp_of_unit, sexp_of_unit))
           x)
        (Unit_ast_qcheck_testing.print
           (PrintSexp (sexp_of_unit, sexp_of_unit))
           y)
    in
    name >:: fun _ -> assert_bool "not equal" (equal_plain_expr x y)
  in
  let create_negative_test ((x : plain_expr), (y : plain_expr)) =
    let name =
      sprintf "%s =? %s"
        (Unit_ast_qcheck_testing.print
           (PrintSexp (sexp_of_unit, sexp_of_unit))
           x)
        (Unit_ast_qcheck_testing.print
           (PrintSexp (sexp_of_unit, sexp_of_unit))
           y)
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
      ( App ((), IntLit ((), 1), IntLit ((), 2)),
        App ((), IntLit ((), 1), IntLit ((), 2)) );
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
  Test.make ~count:1000 ~name:"Expr to source code"
    unit_program_arbitrary_with_default_options (fun prog ->
      let e = prog.e in
      match run_frontend_string (Expr.to_source_code e) with
      | Ok prog ->
          if equal_plain_expr e prog.e then true
          else
            Test.fail_reportf
              "Got different Expr. Expected:\n\n%s\n\nActual:\n\n%s"
              (Unit_ast_qcheck_testing.print
                 (PrintSexp (sexp_of_unit, sexp_of_unit))
                 e)
              (Unit_ast_qcheck_testing.print
                 (PrintSexp (sexp_of_unit, sexp_of_unit))
                 prog.e)
      | Error err ->
          Test.fail_reportf "Got frontend error: %s"
            (err |> sexp_of_frontend_error |> Sexp.to_string_hum))

let create_test_cases_expr_node_val (type_arb : 'a QCheck.arbitrary)
    (type_eq : 'a -> 'a -> bool) =
  let open QCheck in
  Test.make ~count:100
    (pair type_arb unit_program_arbitrary_with_default_options)
    (fun (x, prog) ->
      let e_raw = prog.e in
      let e = fmap ~f:(const x) e_raw in
      type_eq (expr_node_val e) x)

module SingleTagged_tests =
functor
  (Tag : sig
     type t

     val arb : t QCheck.arbitrary
     val obs : t QCheck.Observable.t
     val sexp : t -> Sexp.t
     val eq : t -> t -> bool
   end)
  ->
  struct
    module Program_qcheck_testing = Program.QCheck_testing (Tag) (UnitTag)

    let create_test_cases_expr_node_map_val =
      let open QCheck in
      Test.make ~count:100
        (pair (fun1 Tag.obs Tag.arb)
           (Program_qcheck_testing.arbitrary
              {
                gen =
                  {
                    mrd = default_max_gen_rec_depth;
                    max_variant_types = default_max_variant_type_count;
                    max_variant_type_constructors =
                      default_max_variant_type_constructor_count;
                    max_top_level_defns = default_max_top_level_defns_count;
                    allow_fun_types = false;
                    ast_type = None;
                    expr_v_gen = QCheck.get_gen Tag.arb;
                    pat_v_gen = QCheck.Gen.unit;
                  };
                print = PrintSexp (Tag.sexp, sexp_of_unit);
                shrink = { preserve_type = false };
              }))
        (fun (f_, prog) ->
          let e = prog.e in
          let f = QCheck.Fn.apply f_ in
          let e' = expr_node_map_val ~f e in
          Tag.eq (expr_node_val e') (e |> expr_node_val |> f))
  end

module DoubleTagged_tests =
functor
  (Tag1 : sig
     type t

     val arb : t QCheck.arbitrary
     val obs : t QCheck.Observable.t
     val sexp : t -> Sexp.t
   end)
  (Tag2 : sig
     type t

     val arb : t QCheck.arbitrary
     val eq : t -> t -> bool
   end)
  ->
  struct
    module Tag1_program_qcheck_testing = Program.QCheck_testing (Tag1) (UnitTag)

    let create_test_cases_expr_fmap_root =
      let open QCheck in
      Test.make ~count:100
        (triple Tag1.arb (fun1 Tag1.obs Tag2.arb)
           (Tag1_program_qcheck_testing.arbitrary
              {
                gen =
                  {
                    mrd = default_max_gen_rec_depth;
                    max_variant_types = default_max_variant_type_count;
                    max_variant_type_constructors =
                      default_max_variant_type_constructor_count;
                    max_top_level_defns = default_max_top_level_defns_count;
                    allow_fun_types = false;
                    ast_type = None;
                    expr_v_gen = QCheck.get_gen Tag1.arb;
                    pat_v_gen = QCheck.Gen.unit;
                  };
                print = PrintSexp (Tag1.sexp, sexp_of_unit);
                shrink = { preserve_type = false };
              }))
        (fun (x, f_, prog) ->
          let e = prog.e in
          let f = QCheck.Fn.apply f_ in
          let e' = fmap ~f:(Core.Fn.compose f (const x)) e in
          Tag2.eq (expr_node_val e') (f x))
  end

module Unit_singletagged_tests = SingleTagged_tests (struct
  type t = unit

  let arb = QCheck.unit
  let obs = QCheck.Observable.unit
  let sexp = sexp_of_unit
  let eq = equal_unit
end)

module Int_singletagged_tests = SingleTagged_tests (struct
  type t = int

  let arb = QCheck.int
  let obs = QCheck.Observable.int
  let sexp = sexp_of_int
  let eq = equal_int
end)

module String_singletagged_tests = SingleTagged_tests (struct
  type t = string

  let arb = QCheck.string
  let obs = QCheck.Observable.string
  let sexp = sexp_of_string
  let eq = equal_string
end)

module UnitInt_doubletagged_tests =
  DoubleTagged_tests
    (struct
      type t = unit

      let arb = QCheck.unit
      let obs = QCheck.Observable.unit
      let sexp = sexp_of_unit
    end)
    (struct
      type t = int

      let arb = QCheck.int
      let eq = equal_int
    end)

module IntString_doubletagged_tests =
  DoubleTagged_tests
    (struct
      type t = int

      let arb = QCheck.int
      let obs = QCheck.Observable.int
      let sexp = sexp_of_int
    end)
    (struct
      type t = string

      let arb = QCheck.string
      let eq = equal_string
    end)

module BoolInt_doubletagged_tests =
  DoubleTagged_tests
    (struct
      type t = bool

      let arb = QCheck.bool
      let obs = QCheck.Observable.bool
      let sexp = sexp_of_bool
    end)
    (struct
      type t = int

      let arb = QCheck.int
      let eq = equal_int
    end)

module StringInt_doubletagged_tests =
  DoubleTagged_tests
    (struct
      type t = string

      let arb = QCheck.string
      let obs = QCheck.Observable.string
      let sexp = sexp_of_string
    end)
    (struct
      type t = int

      let arb = QCheck.int
      let eq = equal_int
    end)

let suite =
  "Expr Tests"
  >::: [
         "Equality Tests" >::: test_cases_equality;
         QCheck_runner.to_ounit2_test test_cases_to_source_code_inv;
         "Expr node value"
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
         "Expr expr node map val"
         >::: List.map
                ~f:(fun (name, test) ->
                  name >::: [ QCheck_runner.to_ounit2_test test ])
                [
                  ( "unit",
                    Unit_singletagged_tests.create_test_cases_expr_node_map_val
                  );
                  ( "int",
                    Int_singletagged_tests.create_test_cases_expr_node_map_val
                  );
                  ( "string",
                    String_singletagged_tests
                    .create_test_cases_expr_node_map_val );
                ];
         "Expr fmap root"
         >::: List.map
                ~f:(fun (name, test) ->
                  name >::: [ QCheck_runner.to_ounit2_test test ])
                [
                  ( "unit -> int",
                    UnitInt_doubletagged_tests.create_test_cases_expr_fmap_root
                  );
                  ( "int -> string",
                    IntString_doubletagged_tests
                    .create_test_cases_expr_fmap_root );
                  ( "bool -> int",
                    BoolInt_doubletagged_tests.create_test_cases_expr_fmap_root
                  );
                  ( "string -> int",
                    StringInt_doubletagged_tests
                    .create_test_cases_expr_fmap_root );
                ];
       ]
