open Core
open Pq_lang
open Pq_lang.Expr.StdExpr
open Testing_utils

let test_cases_equality : unit Alcotest.test_case list =
  let create_positive_test ((x : Expr.plain_t), (y : Expr.plain_t)) =
    let name =
      sprintf "%s =? %s"
        (Unit_expr_qcheck_testing.print
           (PrintSexp (sexp_of_unit, sexp_of_unit))
           x)
        (Unit_expr_qcheck_testing.print
           (PrintSexp (sexp_of_unit, sexp_of_unit))
           y)
    in
    ( name,
      `Quick,
      fun _ -> Alcotest.(check bool) "not equal" true (Expr.equal_plain_t x y)
    )
  in
  let create_negative_test ((x : Expr.plain_t), (y : Expr.plain_t)) =
    let name =
      sprintf "%s =? %s"
        (Unit_expr_qcheck_testing.print
           (PrintSexp (sexp_of_unit, sexp_of_unit))
           x)
        (Unit_expr_qcheck_testing.print
           (PrintSexp (sexp_of_unit, sexp_of_unit))
           y)
    in
    ( name,
      `Quick,
      fun _ -> Alcotest.(check bool) "equal" false (Expr.equal_plain_t x y) )
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
          if Expr.equal_plain_t e prog.e then true
          else
            Test.fail_reportf
              "Got different Expr. Expected:\n\n%s\n\nActual:\n\n%s"
              (Unit_expr_qcheck_testing.print
                 (PrintSexp (sexp_of_unit, sexp_of_unit))
                 e)
              (Unit_expr_qcheck_testing.print
                 (PrintSexp (sexp_of_unit, sexp_of_unit))
                 prog.e)
      | Error err ->
          Test.fail_reportf "Got frontend error: %s"
            (err |> sexp_of_frontend_error |> Sexp.to_string_hum))

let create_test_cases_expr_node_val (name : string)
    (type_arb : 'a QCheck.arbitrary) (type_eq : 'a -> 'a -> bool) :
    unit Alcotest.test_case =
  let open QCheck in
  QCheck_alcotest.to_alcotest
    (Test.make ~count:100 ~name
       (pair type_arb unit_program_arbitrary_with_default_options)
       (fun (x, prog) ->
         let e_raw = prog.e in
         let e = fmap ~f:(const x) e_raw in
         type_eq (Expr.node_val e) x))

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

    let create_test_cases_expr_node_map_val (name : string) :
        unit Alcotest.test_case =
      let open QCheck in
      QCheck_alcotest.to_alcotest
        (Test.make ~count:100 ~name
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
                       body_type = None;
                       expr_v_gen = QCheck.get_gen Tag.arb;
                       pat_v_gen = QCheck.Gen.unit;
                     };
                   print = PrintSexp (Tag.sexp, sexp_of_unit);
                   shrink = { preserve_type = false };
                 }))
           (fun (f_, prog) ->
             let e = prog.e in
             let f = QCheck.Fn.apply f_ in
             let e' = Expr.node_map_val ~f e in
             Tag.eq (Expr.node_val e') (e |> Expr.node_val |> f)))
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

    let create_test_cases_expr_fmap_root (name : string) :
        unit Alcotest.test_case =
      let open QCheck in
      QCheck_alcotest.to_alcotest
        (Test.make ~count:100 ~name
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
                       body_type = None;
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
             Tag2.eq (Expr.node_val e') (f x)))
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

let suite : unit Alcotest.test_case list =
  label_tests "Equality Tests" test_cases_equality
  @ [ QCheck_alcotest.to_alcotest test_cases_to_source_code_inv ]
  @ label_tests "Expr node value"
      [
        create_test_cases_expr_node_val "unit" QCheck.unit equal_unit;
        create_test_cases_expr_node_val "int" QCheck.int equal_int;
        create_test_cases_expr_node_val "bool" QCheck.bool equal_bool;
        create_test_cases_expr_node_val "string" QCheck.string equal_string;
        create_test_cases_expr_node_val "string option"
          QCheck.(option string)
          (equal_option equal_string);
        create_test_cases_expr_node_val "int list"
          QCheck.(list int)
          (equal_list equal_int);
      ]
  @ label_tests "Expr node map val"
      [
        Unit_singletagged_tests.create_test_cases_expr_node_map_val "unit";
        Int_singletagged_tests.create_test_cases_expr_node_map_val "int";
        String_singletagged_tests.create_test_cases_expr_node_map_val "string";
      ]
  @ label_tests "Expr fmap root"
      [
        UnitInt_doubletagged_tests.create_test_cases_expr_fmap_root
          "unit -> int";
        IntString_doubletagged_tests.create_test_cases_expr_fmap_root
          "int -> string";
        BoolInt_doubletagged_tests.create_test_cases_expr_fmap_root
          "bool -> int";
        StringInt_doubletagged_tests.create_test_cases_expr_fmap_root
          "string -> int";
      ]
