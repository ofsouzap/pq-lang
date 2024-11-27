open OUnit2
open Core
open Pq_lang
open Vtype
open Ast
open Utils

let test_cases_expr_typing : test list =
  let create_test ((name : string), (e : plain_expr), (t_opt : vtype option)) :
      test =
    name >:: fun _ ->
    let open Result in
    let out = Typing.type_expr e in
    match out with
    | Ok e' -> (
        let e_t = e' |> expr_node_val |> fst in
        match t_opt with
        | Some t -> assert_equal ~cmp:equal_vtype t e_t
        | None ->
            assert_failure
              ("Expected typing error but got type of "
              ^ (sexp_of_vtype e_t |> Sexp.to_string)))
    | Error _ -> (
        match t_opt with
        | Some t ->
            assert_failure
              ("Expected typing judgement of "
              ^ (sexp_of_vtype t |> Sexp.to_string)
              ^ " but failed to type")
        | None -> ())
  in
  List.map
    ~f:Fn.(compose create_test (fun (e, t) -> (ast_to_source_code e, e, t)))
    [
      (IntLit ((), 1), Some VTypeInt);
      (Add ((), IntLit ((), 3), IntLit ((), 0)), Some VTypeInt);
      (Add ((), BoolLit ((), true), IntLit ((), 2)), None);
      (Neg ((), IntLit ((), 3)), Some VTypeInt);
      (Neg ((), BoolLit ((), false)), None);
      (Subtr ((), IntLit ((), 3), IntLit ((), 0)), Some VTypeInt);
      (Subtr ((), BoolLit ((), true), IntLit ((), 2)), None);
      (Mult ((), IntLit ((), 3), IntLit ((), 0)), Some VTypeInt);
      (Mult ((), BoolLit ((), true), IntLit ((), 2)), None);
      (BoolLit ((), true), Some VTypeBool);
      (BNot ((), BoolLit ((), false)), Some VTypeBool);
      (BNot ((), IntLit ((), 3)), None);
      (BAnd ((), BoolLit ((), false), BoolLit ((), true)), Some VTypeBool);
      (BAnd ((), BoolLit ((), true), IntLit ((), 2)), None);
      (BOr ((), BoolLit ((), false), BoolLit ((), true)), Some VTypeBool);
      (BOr ((), BoolLit ((), true), IntLit ((), 2)), None);
      (Eq ((), IntLit ((), 3), IntLit ((), 0)), Some VTypeBool);
      (Eq ((), BoolLit ((), true), IntLit ((), 2)), None);
      (Eq ((), IntLit ((), 3), BoolLit ((), true)), None);
      (Eq ((), BoolLit ((), true), BoolLit ((), true)), Some VTypeBool);
      (GtEq ((), IntLit ((), 3), IntLit ((), 0)), Some VTypeBool);
      (GtEq ((), BoolLit ((), true), IntLit ((), 2)), None);
      (Gt ((), IntLit ((), 3), IntLit ((), 0)), Some VTypeBool);
      (Gt ((), BoolLit ((), true), IntLit ((), 2)), None);
      (LtEq ((), IntLit ((), 3), IntLit ((), 0)), Some VTypeBool);
      (LtEq ((), BoolLit ((), true), IntLit ((), 2)), None);
      (Lt ((), IntLit ((), 3), IntLit ((), 0)), Some VTypeBool);
      (Lt ((), BoolLit ((), true), IntLit ((), 2)), None);
      ( If ((), BoolLit ((), true), IntLit ((), 3), IntLit ((), 0)),
        Some VTypeInt );
      (If ((), BoolLit ((), true), IntLit ((), 3), BoolLit ((), false)), None);
      (If ((), BoolLit ((), true), IntLit ((), 3), BoolLit ((), false)), None);
      ( If ((), BoolLit ((), true), IntLit ((), 3), IntLit ((), 0)),
        Some VTypeInt );
      (Var ((), "x"), None);
      ( Fun ((), ("x", VTypeInt), Var ((), "x")),
        Some (VTypeFun (VTypeInt, VTypeInt)) );
      (Let ((), "x", IntLit ((), 3), Var ((), "x")), Some VTypeInt);
      (Let ((), "x", BoolLit ((), true), Var ((), "x")), Some VTypeBool);
      (Let ((), "x", IntLit ((), 3), BoolLit ((), true)), Some VTypeBool);
      ( Let
          ( (),
            "f",
            Fun ((), ("x", VTypeInt), Add ((), Var ((), "x"), IntLit ((), 1))),
            App ((), Var ((), "f"), IntLit ((), 3)) ),
        Some VTypeInt );
      ( Fun ((), ("x", VTypeInt), Add ((), Var ((), "x"), IntLit ((), 1))),
        Some (VTypeFun (VTypeInt, VTypeInt)) );
      ( Let
          ( (),
            "f",
            Fun ((), ("x", VTypeInt), Var ((), "x")),
            App ((), Var ((), "f"), IntLit ((), 3)) ),
        Some VTypeInt );
      ( Let
          ( (),
            "f",
            Fix ((), ("f", VTypeInt, VTypeInt), ("x", VTypeInt), Var ((), "x")),
            App ((), Var ((), "f"), IntLit ((), 3)) ),
        Some VTypeInt );
      ( Let
          ( (),
            "f",
            Fix
              ( (),
                ("f", VTypeInt, VTypeInt),
                ("x", VTypeInt),
                BoolLit ((), false) ),
            App ((), Var ((), "f"), IntLit ((), 3)) ),
        None );
      ( Let
          ( (),
            "f",
            Fix ((), ("f", VTypeInt, VTypeInt), ("x", VTypeBool), Var ((), "x")),
            App ((), Var ((), "f"), IntLit ((), 3)) ),
        None );
      ( Let
          ( (),
            "f",
            Fix ((), ("f", VTypeInt, VTypeInt), ("x", VTypeBool), Var ((), "x")),
            App ((), Var ((), "f"), BoolLit ((), false)) ),
        None );
      ( Let
          ( (),
            "f",
            Fix
              ( (),
                ("f", VTypeInt, VTypeBool),
                ("x", VTypeInt),
                BoolLit ((), false) ),
            App ((), Var ((), "f"), IntLit ((), 3)) ),
        Some VTypeBool );
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

(* TODO - negative test cases (expressions that should fail type checking) *)
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
