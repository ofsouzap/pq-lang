open Core
open OUnit2
open Pq_lang
open Ast

let show_plain_ast = Fn.compose Sexp.to_string sexp_of_plain_expr

(** Generator for a small-length, non-empty string of lowercase characters *)
let varname_gen : string QCheck.Gen.t =
  let open QCheck.Gen in
  int_range 1 5 >>= fun n ->
  list_repeat n (char_range 'a' 'z') >|= String.of_char_list

let ast_expr_arb_any : plain_expr QCheck.arbitrary =
  let open QCheck in
  let open QCheck.Gen in
  let gen =
    fix (fun self d ->
        let self' = self (d - 1) in
        let base_cases =
          [
            (nat >|= fun n -> IntLit ((), n));
            (bool >|= fun b -> BoolLit ((), b));
            (varname_gen >|= fun vname -> Var ((), vname));
          ]
        in
        let rec_cases =
          [
            (pair self' self' >|= fun (e1, e2) -> Add ((), e1, e2));
            (self' >|= fun e -> Neg ((), e));
            (pair self' self' >|= fun (e1, e2) -> Subtr ((), e1, e2));
            (pair self' self' >|= fun (e1, e2) -> Mult ((), e1, e2));
            (pair self' self' >|= fun (e1, e2) -> BOr ((), e1, e2));
            (pair self' self' >|= fun (e1, e2) -> BAnd ((), e1, e2));
            (pair self' self' >|= fun (e1, e2) -> Eq ((), e1, e2));
            (pair self' self' >|= fun (e1, e2) -> Gt ((), e1, e2));
            (pair self' self' >|= fun (e1, e2) -> GtEq ((), e1, e2));
            (pair self' self' >|= fun (e1, e2) -> Lt ((), e1, e2));
            (pair self' self' >|= fun (e1, e2) -> LtEq ((), e1, e2));
            ( triple self' self' self' >|= fun (e1, e2, e3) ->
              If ((), e1, e2, e3) );
            ( triple varname_gen self' self' >|= fun (vname, e1, e2) ->
              Let ((), vname, e1, e2) );
            (pair varname_gen self' >|= fun (vname, e) -> Ast.Fun ((), vname, e));
            (pair self' self' >|= fun (e1, e2) -> App ((), e1, e2));
            ( quad varname_gen varname_gen self' self'
            >|= fun (fname, xname, e1, e2) ->
              Let ((), fname, Fix ((), fname, xname, e1), e2) );
          ]
        in
        if d > 0 then oneof (base_cases @ rec_cases) else oneof base_cases)
  in
  make (gen 3) ~print:show_plain_ast

let test_cases_equality : test list =
  let create_positive_test ((x : plain_expr), (y : plain_expr)) =
    let name = sprintf "%s =? %s" (show_plain_ast x) (show_plain_ast y) in
    name >:: fun _ -> assert_bool "not equal" (equal_plain_expr x y)
  in
  let create_negative_test ((x : plain_expr), (y : plain_expr)) =
    let name = sprintf "%s =? %s" (show_plain_ast x) (show_plain_ast y) in
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
      (Fun ((), "x", IntLit ((), 1)), Fun ((), "x", IntLit ((), 1)));
      ( App ((), IntLit ((), 1), IntLit ((), 2)),
        App ((), IntLit ((), 1), IntLit ((), 2)) );
      ( Fix ((), "f", "x", App ((), Var ((), "f"), Var ((), "x"))),
        Fix ((), "f", "x", App ((), Var ((), "f"), Var ((), "x"))) );
      ( App
          ( (),
            Fix ((), "f", "x", App ((), Var ((), "f"), Var ((), "x"))),
            Fun ((), "x", Fun ((), "x", IntLit ((), 1))) ),
        App
          ( (),
            Fix ((), "f", "x", App ((), Var ((), "f"), Var ((), "x"))),
            Fun ((), "x", Fun ((), "x", IntLit ((), 1))) ) );
      ( Let
          ( (),
            "f",
            App
              ( (),
                Fix ((), "f", "x", App ((), Var ((), "f"), Var ((), "x"))),
                Fun ((), "f", App ((), Var ((), "f"), IntLit ((), 0))) ),
            App ((), Var ((), "f"), IntLit ((), 0)) ),
        Let
          ( (),
            "f",
            App
              ( (),
                Fix ((), "f", "x", App ((), Var ((), "f"), Var ((), "x"))),
                Fun ((), "f", App ((), Var ((), "f"), IntLit ((), 0))) ),
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
  Test.make ~count:100 ~name:"AST to source code" ast_expr_arb_any (fun e ->
      match run_frontend_string (ast_to_source_code e) with
      | Res e' -> equal_plain_expr e e'
      | _ -> false)

(* TODO - AST expr_node_val tests *)

(* TODO - AST fmap tests *)

let suite =
  "AST Tests"
  >::: [
         "Equality Tests" >::: test_cases_equality;
         QCheck_runner.to_ounit2_test test_cases_to_source_code_inv;
       ]
