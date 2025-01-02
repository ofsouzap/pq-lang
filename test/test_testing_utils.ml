open Core
open OUnit2
open QCheck
open Pq_lang
open Vtype
open Ast
open Typing
open Testing_utils

let create_typed_expr_gen_test (name : string) (t_gen : vtype Gen.t) : test =
  let open QCheck in
  QCheck_runner.to_ounit2_test
    (Test.make ~name ~count:1000
       (let open QCheck.Gen in
        let gen : (TestingTypeCtx.t * (vtype * unit Ast.expr)) QCheck.Gen.t =
          testing_type_ctx_gen
            ~max_constructors:default_max_custom_type_constructor_count
            ~max_custom_types:default_max_custom_type_count
            ~mrd:default_max_gen_rec_depth
          >>= fun type_ctx ->
          t_gen >>= fun t ->
          QCheck.gen (ast_expr_arb ~type_ctx ~t NoPrint Gen.unit) >|= fun e ->
          (type_ctx, (t, e))
        in
        QCheck.make
          ~print:(fun (_, (t, e)) ->
            sprintf "[with type ctx] [type: %s] %s" (vtype_to_source_code t)
              (ast_to_source_code e))
          gen)
       (fun (type_ctx, (t, e)) ->
         let typed_result =
           TestingTypeChecker.type_expr (type_ctx, TestingVarCtx.empty) e
         in
         match typed_result with
         | Ok tpe ->
             let et =
               tpe |> TestingTypeChecker.typed_program_expression_get_expression
               |> Ast.expr_node_val |> fst
             in
             equal_vtype t et
         | Error _ -> false))

let create_typed_expr_gen_test_for_fixed_type (name : string) (t : vtype) =
  create_typed_expr_gen_test name (Gen.return t)

let create_test_var_ctx (xs : (string * vtype) list) : TestingVarCtx.t =
  List.fold xs ~init:TestingVarCtx.empty ~f:(fun ctx (x, t) ->
      TestingVarCtx.add ctx x t)

let create_list_impl_var_ctx (xs : (string * vtype) list) :
    ListTypingVarContext.t =
  List.fold xs ~init:ListTypingVarContext.empty ~f:(fun ctx (x, t) ->
      ListTypingVarContext.add ctx x t)

let var_ctx_list_arb ~(type_ctx : TestingTypeCtx.t) =
  let open QCheck in
  list (pair string (vtype_arb ~type_ctx default_max_gen_rec_depth))

(* TODO - test that TestingTypeChecker types things the same as the SimpleTypeChecker *)

let suite =
  "Utilities Tests"
  >::: [
         "Typed expression generator"
         >::: [
                create_typed_expr_gen_test_for_fixed_type "unit" VTypeUnit;
                create_typed_expr_gen_test_for_fixed_type "int" VTypeInt;
                create_typed_expr_gen_test_for_fixed_type "bool" VTypeBool;
                create_typed_expr_gen_test "'a -> 'b"
                  Gen.(
                    default_testing_type_ctx_gen >>= fun type_ctx ->
                    pair
                      (vtype_gen ~type_ctx default_max_gen_rec_depth)
                      (vtype_gen ~type_ctx default_max_gen_rec_depth)
                    >|= fun (t1, t2) -> VTypeFun (t1, t2));
                create_typed_expr_gen_test "'a * 'b"
                  Gen.(
                    default_testing_type_ctx_gen >>= fun type_ctx ->
                    pair
                      (vtype_gen ~type_ctx default_max_gen_rec_depth)
                      (vtype_gen ~type_ctx default_max_gen_rec_depth)
                    >|= fun (t1, t2) -> VTypePair (t1, t2));
              ];
       ]
