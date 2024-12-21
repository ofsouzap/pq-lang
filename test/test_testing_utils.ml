open Core
open OUnit2
open QCheck
open Pq_lang
open Vtype
open Ast
open Testing_utils

let create_typed_expr_gen_test (name : string) (t_gen : vtype Gen.t) : test =
  let open QCheck in
  QCheck_runner.to_ounit2_test
    (Test.make ~name ~count:1000
       (let open QCheck.Gen in
        let te_gen =
          t_gen >>= fun t ->
          QCheck.gen (ast_expr_arb ~t NoPrint Gen.unit) >|= fun e -> (t, e)
        in
        let te_arb =
          make
            ~print:(fun (t, e) ->
              sprintf "[type: %s] %s" (vtype_to_source_code t)
                (ast_to_source_code e))
            te_gen
        in
        te_arb)
       (fun (t, e) ->
         let typed_result = Typing.type_expr e in
         match typed_result with
         | Ok typed_e ->
             let et = Ast.expr_node_val typed_e |> fst in
             equal_vtype t et
         | Error _ -> false))

let create_typed_expr_gen_test_for_fixed_type (name : string) (t : vtype) =
  create_typed_expr_gen_test name (Gen.return t)

(* TODO - test that the TestingVarCtx module types things as the actual used one does *)

let suite =
  "Utilities Tests"
  >::: [
         "Typed expression generator"
         >::: [
                create_typed_expr_gen_test_for_fixed_type "int" VTypeInt;
                create_typed_expr_gen_test_for_fixed_type "bool" VTypeBool;
                create_typed_expr_gen_test "'a -> 'b"
                  Gen.(
                    pair
                      (vtype_gen default_max_gen_rec_depth)
                      (vtype_gen default_max_gen_rec_depth)
                    >|= fun (t1, t2) -> VTypeFun (t1, t2));
                create_typed_expr_gen_test "'a * 'b"
                  Gen.(
                    pair
                      (vtype_gen default_max_gen_rec_depth)
                      (vtype_gen default_max_gen_rec_depth)
                    >|= fun (t1, t2) -> VTypePair (t1, t2));
              ];
       ]
