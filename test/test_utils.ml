open Core
open OUnit2
open QCheck
open Pq_lang
open Vtype
open Ast
open Utils

let create_typed_expr_gen_test_for_base_type (name : string) (t : vtype) : test
    =
  QCheck_runner.to_ounit2_test
    (QCheck.Test.make ~name ~count:1000
       (ast_expr_arb ~t PrintExprSource Gen.unit) (fun e ->
         let typed_result = Typing.type_expr e in
         match typed_result with
         | Ok typed_e ->
             let et = Ast.expr_node_val typed_e |> fst in
             equal_vtype t et
         | Error _ -> false))

let create_typed_expr_gen_test_for_compound_type (name : string)
    (t_gen : vtype Gen.t) : test =
  QCheck_runner.to_ounit2_test
    (QCheck.Test.make ~name ~count:1000
       (let open QCheck.Gen in
        let t_arb = make ~print:vtype_to_source_code t_gen in
        let e_gen =
          t_gen >>= fun t ->
          QCheck.gen (ast_expr_arb ~t PrintExprSource Gen.unit)
        in
        let e_arb = make ~print:ast_to_source_code e_gen in
        QCheck.pair t_arb e_arb)
       (fun (t, e) ->
         let typed_result = Typing.type_expr e in
         match typed_result with
         | Ok typed_e ->
             let et = Ast.expr_node_val typed_e |> fst in
             equal_vtype t et
         | Error _ -> false))

let suite =
  "Utilities Tests"
  >::: [
         "Typed expression generator"
         >::: [
                create_typed_expr_gen_test_for_base_type "int" VTypeInt;
                create_typed_expr_gen_test_for_base_type "bool" VTypeBool;
                create_typed_expr_gen_test_for_compound_type "'a -> 'b"
                  Gen.(
                    pair
                      (vtype_gen default_max_gen_rec_depth)
                      (vtype_gen default_max_gen_rec_depth)
                    >|= fun (t1, t2) -> VTypeFun (t1, t2));
              ];
       ]
