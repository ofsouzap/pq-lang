open OUnit2
open Core
open Pq_lang
open Vtype
open Ast

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
    ]

(* TODO - replace the above with qcheck tests for testing typing more throughly (e.g. generator for bool-typed expressions, then can check that any `BAdd` node with bool-typed arguments is typed as a bool) *)

let suite = "Typing" >::: [ "Expression typing" >::: test_cases_expr_typing ]
