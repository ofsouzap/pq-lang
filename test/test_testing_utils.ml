open Core
open OUnit2
open QCheck
open Pq_lang
open Vtype
open Custom_types
open Ast
open Typing
open Testing_utils

(* TODO - test that shrinking an expression with preserve_type option set to true preserves the type *)

let create_typed_expr_gen_test (name : string)
    (types_gen : (TestingTypeCtx.t * vtype) Gen.t) : test =
  let open QCheck in
  QCheck_runner.to_ounit2_test
    (Test.make ~name ~count:1000
       (let open QCheck.Gen in
        let gen : (TestingTypeCtx.t * (vtype * unit Ast.expr)) QCheck.Gen.t =
          types_gen >>= fun (type_ctx, t) ->
          QCheck.gen (ast_expr_arb ~type_ctx ~t NoPrint Gen.unit) >|= fun e ->
          (type_ctx, (t, e))
        in
        QCheck.make
          ~print:(fun (type_ctx, (t, e)) ->
            sprintf "[type ctx: %s]\n[type: %s]\n%s"
              (type_ctx |> TestingTypeCtx.sexp_of_t |> Sexp.to_string)
              (vtype_to_source_code t) (ast_to_source_code e))
          ~shrink:
            QCheck.Shrink.(
              pair nil (pair nil (expr_shrink ~preserve_type:true)))
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
  create_typed_expr_gen_test name
    QCheck.Gen.(default_testing_type_ctx_gen >|= fun type_ctx -> (type_ctx, t))

let create_test_vtype_gen_constructors_exist (name : string) : test =
  let open QCheck in
  QCheck_runner.to_ounit2_test
    (Test.make ~name ~count:1000
       (QCheck.make
          ~print:
            (Core.Fn.compose Sexp.to_string
               (let sexp_of_type_ctx (type_ctx : TestingTypeCtx.t) : Sexp.t =
                  TestingTypeCtx.to_list type_ctx
                  |> sexp_of_list sexp_of_custom_type
                in
                sexp_of_pair sexp_of_type_ctx sexp_of_vtype))
          QCheck.Gen.(
            default_testing_type_ctx_gen >>= fun type_ctx ->
            vtype_gen ~type_ctx default_max_gen_rec_depth >|= fun t ->
            (type_ctx, t)))
       (fun (type_ctx, t) ->
         match t with
         | VTypeCustom ct_name -> TestingTypeCtx.custom_exists type_ctx ct_name
         | _ -> true))

(* TODO - test that the type contexts generated don't contain duplicated constructor names or type names *)

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
         "Value type generator"
         >::: [ create_test_vtype_gen_constructors_exist "Custom types exist" ];
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
                    >|= fun (t1, t2) -> (type_ctx, VTypeFun (t1, t2)));
                create_typed_expr_gen_test "'a * 'b"
                  Gen.(
                    default_testing_type_ctx_gen >>= fun type_ctx ->
                    pair
                      (vtype_gen ~type_ctx default_max_gen_rec_depth)
                      (vtype_gen ~type_ctx default_max_gen_rec_depth)
                    >|= fun (t1, t2) -> (type_ctx, VTypePair (t1, t2)));
              ];
       ]
