open OUnit2
open Core
open Pq_lang
open Utils
open Vtype
open Pattern
open Ast
open Typing
open Testing_utils

let test_cases_expr_typing : test list =
  let open Either in
  let create_test
      ((name : string), (e : plain_expr), (t : (vtype, typing_error) Either.t))
      : test =
    name >:: fun _ ->
    let open Result in
    let out = Typing.type_expr e in
    match (out, t) with
    | Ok e', First exp_t ->
        let out_t = e' |> expr_node_val |> fst in
        assert_equal ~cmp:equal_vtype ~printer:vtype_to_source_code exp_t out_t
    | Ok _, Second _ -> assert_failure "Expected typing error but got type"
    | Error _, First _ -> assert_failure "Expected type but got typing error"
    | Error t_err, Second exp_err ->
        assert_equal ~cmp:equal_typing_error ~printer:print_typing_error exp_err
          t_err
  in
  List.map
    ~f:Fn.(compose create_test (fun (e, t) -> (ast_to_source_code e, e, t)))
    [
      (IntLit ((), 1), First VTypeInt);
      (Add ((), IntLit ((), 3), IntLit ((), 0)), First VTypeInt);
      ( Add ((), BoolLit ((), true), IntLit ((), 2)),
        Second (TypeMismatch (VTypeInt, VTypeBool)) );
      (Neg ((), IntLit ((), 3)), First VTypeInt);
      ( Neg ((), BoolLit ((), false)),
        Second (TypeMismatch (VTypeInt, VTypeBool)) );
      (Subtr ((), IntLit ((), 3), IntLit ((), 0)), First VTypeInt);
      ( Subtr ((), BoolLit ((), true), IntLit ((), 2)),
        Second (TypeMismatch (VTypeInt, VTypeBool)) );
      (Mult ((), IntLit ((), 3), IntLit ((), 0)), First VTypeInt);
      ( Mult ((), BoolLit ((), true), IntLit ((), 2)),
        Second (TypeMismatch (VTypeInt, VTypeBool)) );
      (BoolLit ((), true), First VTypeBool);
      (BNot ((), BoolLit ((), false)), First VTypeBool);
      (BNot ((), IntLit ((), 3)), Second (TypeMismatch (VTypeBool, VTypeInt)));
      (BAnd ((), BoolLit ((), false), BoolLit ((), true)), First VTypeBool);
      ( BAnd ((), BoolLit ((), true), IntLit ((), 2)),
        Second (TypeMismatch (VTypeBool, VTypeInt)) );
      (BOr ((), BoolLit ((), false), BoolLit ((), true)), First VTypeBool);
      ( BOr ((), BoolLit ((), true), IntLit ((), 2)),
        Second (TypeMismatch (VTypeBool, VTypeInt)) );
      (Eq ((), IntLit ((), 3), IntLit ((), 0)), First VTypeBool);
      ( Eq ((), BoolLit ((), true), IntLit ((), 2)),
        Second (EqualOperatorTypeMistmatch (VTypeBool, VTypeInt)) );
      ( Eq ((), IntLit ((), 3), BoolLit ((), true)),
        Second (EqualOperatorTypeMistmatch (VTypeInt, VTypeBool)) );
      (Eq ((), BoolLit ((), true), BoolLit ((), true)), First VTypeBool);
      (GtEq ((), IntLit ((), 3), IntLit ((), 0)), First VTypeBool);
      ( GtEq ((), BoolLit ((), true), IntLit ((), 2)),
        Second (TypeMismatch (VTypeInt, VTypeBool)) );
      (Gt ((), IntLit ((), 3), IntLit ((), 0)), First VTypeBool);
      ( Gt ((), BoolLit ((), true), IntLit ((), 2)),
        Second (TypeMismatch (VTypeInt, VTypeBool)) );
      (LtEq ((), IntLit ((), 3), IntLit ((), 0)), First VTypeBool);
      ( LtEq ((), BoolLit ((), true), IntLit ((), 2)),
        Second (TypeMismatch (VTypeInt, VTypeBool)) );
      (Lt ((), IntLit ((), 3), IntLit ((), 0)), First VTypeBool);
      ( Lt ((), BoolLit ((), true), IntLit ((), 2)),
        Second (TypeMismatch (VTypeInt, VTypeBool)) );
      ( If ((), BoolLit ((), true), IntLit ((), 3), IntLit ((), 0)),
        First VTypeInt );
      ( If ((), BoolLit ((), true), IntLit ((), 3), BoolLit ((), false)),
        Second (TypeMismatch (VTypeInt, VTypeBool)) );
      ( If ((), BoolLit ((), true), IntLit ((), 3), BoolLit ((), false)),
        Second (TypeMismatch (VTypeInt, VTypeBool)) );
      ( If ((), BoolLit ((), true), IntLit ((), 3), IntLit ((), 0)),
        First VTypeInt );
      (Var ((), "x"), Second (UndefinedVariable "x"));
      ( Fun ((), ("x", VTypeInt), Var ((), "x")),
        First (VTypeFun (VTypeInt, VTypeInt)) );
      (Let ((), "x", IntLit ((), 3), Var ((), "x")), First VTypeInt);
      (Let ((), "x", BoolLit ((), true), Var ((), "x")), First VTypeBool);
      (Let ((), "x", IntLit ((), 3), BoolLit ((), true)), First VTypeBool);
      ( Let
          ( (),
            "f",
            Fun ((), ("x", VTypeInt), Add ((), Var ((), "x"), IntLit ((), 1))),
            App ((), Var ((), "f"), IntLit ((), 3)) ),
        First VTypeInt );
      ( Fun ((), ("x", VTypeInt), Add ((), Var ((), "x"), IntLit ((), 1))),
        First (VTypeFun (VTypeInt, VTypeInt)) );
      ( Let
          ( (),
            "f",
            Fun ((), ("x", VTypeInt), Var ((), "x")),
            App ((), Var ((), "f"), IntLit ((), 3)) ),
        First VTypeInt );
      ( Let
          ( (),
            "f",
            Fix ((), ("f", VTypeInt, VTypeInt), ("x", VTypeInt), Var ((), "x")),
            App ((), Var ((), "f"), IntLit ((), 3)) ),
        First VTypeInt );
      ( Let
          ( (),
            "f",
            Fix
              ( (),
                ("f", VTypeInt, VTypeInt),
                ("x", VTypeInt),
                BoolLit ((), false) ),
            App ((), Var ((), "f"), IntLit ((), 3)) ),
        Second (TypeMismatch (VTypeInt, VTypeBool)) );
      ( Let
          ( (),
            "f",
            Fix ((), ("f", VTypeInt, VTypeInt), ("x", VTypeBool), Var ((), "x")),
            App ((), Var ((), "f"), IntLit ((), 3)) ),
        Second (TypeMismatch (VTypeInt, VTypeBool)) );
      ( Let
          ( (),
            "f",
            Fix ((), ("f", VTypeInt, VTypeInt), ("x", VTypeBool), Var ((), "x")),
            App ((), Var ((), "f"), BoolLit ((), false)) ),
        Second (TypeMismatch (VTypeInt, VTypeBool)) );
      ( Let
          ( (),
            "f",
            Fix
              ( (),
                ("f", VTypeInt, VTypeBool),
                ("x", VTypeInt),
                BoolLit ((), false) ),
            App ((), Var ((), "f"), IntLit ((), 3)) ),
        First VTypeBool );
      ( Match
          ( (),
            IntLit ((), 3),
            Nonempty_list.from_list_unsafe
              [ (PatName ("x", VTypeInt), BoolLit ((), true)) ] ),
        First VTypeBool );
      ( Match
          ( (),
            IntLit ((), 3),
            Nonempty_list.from_list_unsafe
              [ (PatName ("x", VTypeInt), Var ((), "x")) ] ),
        First VTypeInt );
      ( Match
          ( (),
            IntLit ((), 3),
            Nonempty_list.from_list_unsafe
              [
                (PatName ("x", VTypeInt), BoolLit ((), true));
                (PatName ("y", VTypeInt), BoolLit ((), true));
              ] ),
        First VTypeBool );
      ( Match
          ( (),
            IntLit ((), 3),
            Nonempty_list.from_list_unsafe
              [
                (PatName ("x", VTypeInt), BoolLit ((), true));
                (PatName ("y", VTypeBool), BoolLit ((), true));
              ] ),
        Second
          (PatternTypeMismatch (PatName ("y", VTypeBool), VTypeInt, VTypeBool))
      );
      ( Match
          ( (),
            IntLit ((), 3),
            Nonempty_list.from_list_unsafe
              [
                (PatName ("x", VTypeInt), BoolLit ((), true));
                (PatName ("y", VTypeInt), IntLit ((), 5));
              ] ),
        Second (TypeMismatch (VTypeBool, VTypeInt)) );
    ]

let test_cases_expr_typing_full_check : test list =
  let create_test ((name : string), (e : plain_expr), (exp : vtype expr)) : test
      =
    name >:: fun _ ->
    let open Result in
    let out = Typing.type_expr e in
    match out with
    | Ok e' ->
        let typed_out = e' >|= fst in
        assert_equal ~cmp:(equal_expr equal_vtype)
          ~printer:(get_asp_printer (PrintSexp sexp_of_vtype))
          exp typed_out
    | Error _ -> assert_failure "Failed to type"
  in
  List.map ~f:create_test
    [
      ( "Plain Arithmetic 1",
        Add
          ( (),
            Subtr ((), IntLit ((), 3), IntLit ((), 2)),
            Mult ((), IntLit ((), 2), Neg ((), IntLit ((), 7))) ),
        Add
          ( VTypeInt,
            Subtr (VTypeInt, IntLit (VTypeInt, 3), IntLit (VTypeInt, 2)),
            Mult
              ( VTypeInt,
                IntLit (VTypeInt, 2),
                Neg (VTypeInt, IntLit (VTypeInt, 7)) ) ) );
      ( "Mixed Typing 1",
        Add
          ( (),
            IntLit ((), 2),
            If ((), BoolLit ((), true), IntLit ((), 3), IntLit ((), 0)) ),
        Add
          ( VTypeInt,
            IntLit (VTypeInt, 2),
            If
              ( VTypeInt,
                BoolLit (VTypeBool, true),
                IntLit (VTypeInt, 3),
                IntLit (VTypeInt, 0) ) ) );
    ]

(* TODO - have tests that start with a variable context *)

(* TODO - tests for the variable context (e.g. that overwriting a variable's typing works).
   Do these as a module functor and then have a ground truth of an inefficient, but definitely-correct implementation
   (probably done with functions for context tracking) *)

(* TODO - tests for expressions that should fail to type *)

let test_cases_arb_compound_expr_typing : test list =
  let open QCheck in
  let open QCheck.Gen in
  let expr_gen (t : vtype) : unit expr Gen.t =
    ast_expr_arb ~t NoPrint Gen.unit |> QCheck.gen
  in
  let create_test ((name : string), (e_gen : (vtype * plain_expr) Gen.t)) : test
      =
    let e_arb =
      QCheck.make
        ~print:QCheck.Print.(pair vtype_to_source_code ast_to_source_code)
        e_gen
    in
    QCheck_ounit.to_ounit2_test
      (Test.make ~name ~count:100 e_arb (fun (exp_t, e) ->
           let open Result in
           let out = Typing.type_expr e in
           match out with
           | Ok typed_e ->
               let t = typed_e |> expr_node_val |> fst in
               equal_vtype exp_t t
           | Error _ -> false))
  in
  List.map ~f:create_test
    [
      ( "Add",
        pair (return VTypeInt)
          ( pair (expr_gen VTypeInt) (expr_gen VTypeInt) >|= fun (e1, e2) ->
            Add ((), e1, e2) ) );
      ( "Neg",
        pair (return VTypeInt) (expr_gen VTypeInt >|= fun e -> Neg ((), e)) );
      ( "Subtr",
        pair (return VTypeInt)
          ( pair (expr_gen VTypeInt) (expr_gen VTypeInt) >|= fun (e1, e2) ->
            Subtr ((), e1, e2) ) );
      ( "Mult",
        pair (return VTypeInt)
          ( pair (expr_gen VTypeInt) (expr_gen VTypeInt) >|= fun (e1, e2) ->
            Mult ((), e1, e2) ) );
      ( "BNot",
        pair (return VTypeBool) (expr_gen VTypeBool >|= fun e -> BNot ((), e))
      );
      ( "BOr",
        pair (return VTypeBool)
          ( pair (expr_gen VTypeBool) (expr_gen VTypeBool) >|= fun (e1, e2) ->
            BOr ((), e1, e2) ) );
      ( "BAnd",
        pair (return VTypeBool)
          ( pair (expr_gen VTypeBool) (expr_gen VTypeBool) >|= fun (e1, e2) ->
            BAnd ((), e1, e2) ) );
      ( "Eq - int",
        pair (return VTypeBool)
          ( pair (expr_gen VTypeInt) (expr_gen VTypeInt) >|= fun (e1, e2) ->
            Eq ((), e1, e2) ) );
      ( "Eq - bool",
        pair (return VTypeBool)
          ( pair (expr_gen VTypeBool) (expr_gen VTypeBool) >|= fun (e1, e2) ->
            Eq ((), e1, e2) ) );
      ( "Gt",
        pair (return VTypeBool)
          ( pair (expr_gen VTypeInt) (expr_gen VTypeInt) >|= fun (e1, e2) ->
            Gt ((), e1, e2) ) );
      ( "GtEq",
        pair (return VTypeBool)
          ( pair (expr_gen VTypeInt) (expr_gen VTypeInt) >|= fun (e1, e2) ->
            GtEq ((), e1, e2) ) );
      ( "Lt",
        pair (return VTypeBool)
          ( pair (expr_gen VTypeInt) (expr_gen VTypeInt) >|= fun (e1, e2) ->
            Lt ((), e1, e2) ) );
      ( "LtEq",
        pair (return VTypeBool)
          ( pair (expr_gen VTypeInt) (expr_gen VTypeInt) >|= fun (e1, e2) ->
            LtEq ((), e1, e2) ) );
      ( "If",
        vtype_gen default_max_gen_rec_depth >>= fun t ->
        triple (expr_gen VTypeBool) (expr_gen t) (expr_gen t)
        >|= fun (e1, e2, e3) -> (t, If ((), e1, e2, e3)) );
      ( "Let",
        vtype_gen default_max_gen_rec_depth >>= fun t ->
        pair varname_gen (vtype_gen default_max_gen_rec_depth)
        >>= fun (xname, xtype) ->
        pair (expr_gen xtype) (expr_gen t) >|= fun (e1, e2) ->
        (t, Let ((), xname, e1, e2))
        (* Note, the tests here (and for the Fun and Fix cases) don't work with changing variable contexts. But this should be fine *)
      );
      ( "Fun",
        vtype_gen default_max_gen_rec_depth >>= fun t2 ->
        pair varname_gen (vtype_gen default_max_gen_rec_depth)
        >>= fun (xname, t1) ->
        expr_gen t2 >|= fun e ->
        (VTypeFun (t1, t2), Ast.Fun ((), (xname, t1), e)) );
      ( "App",
        pair
          (vtype_gen default_max_gen_rec_depth)
          (vtype_gen default_max_gen_rec_depth)
        >>= fun (t1, t2) ->
        pair (expr_gen (VTypeFun (t1, t2))) (expr_gen t1) >|= fun (e1, e2) ->
        (t2, App ((), e1, e2)) );
      ( "Fix",
        pair varname_gen (vtype_gen default_max_gen_rec_depth)
        >>= fun (fname, t1) ->
        pair varname_gen (vtype_gen default_max_gen_rec_depth)
        >>= fun (xname, t2) ->
        expr_gen t2 >|= fun e ->
        (VTypeFun (t1, t2), Fix ((), (fname, t1, t2), (xname, t1), e)) );
    ]

let test_cases_typing_maintains_structure : test =
  let open QCheck in
  QCheck_ounit.to_ounit2_test
    (Test.make ~name:"Typing maintains structure" ~count:100
       (ast_expr_arb PrintExprSource Gen.unit) (fun e ->
         let open Result in
         let out = Typing.type_expr e in
         match out with
         | Ok typed_e ->
             let plain_e = expr_to_plain_expr e in
             let plain_typed_e = expr_to_plain_expr typed_e in
             equal_plain_expr plain_e plain_typed_e
         | Error _ -> false))

let suite =
  "Typing"
  >::: [
         "Expression typing" >::: test_cases_expr_typing;
         "Expression typing full hierarchy"
         >::: test_cases_expr_typing_full_check;
         "Arbitrary expression typing" >::: test_cases_arb_compound_expr_typing;
         "Typing maintains structure"
         >::: [ test_cases_typing_maintains_structure ];
       ]
