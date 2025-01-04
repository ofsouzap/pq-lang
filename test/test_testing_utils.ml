open Core
open OUnit2
open QCheck
open Pq_lang
open Vtype
open Custom_types
open Ast
open Typing
open Testing_utils

let create_test_expr_shrink_can_preserve_type (name : string) : test =
  let open QCheck in
  QCheck_runner.to_ounit2_test
    (Test.make ~name ~count:100
       (let open QCheck.Gen in
        let gen : (TestingTypeCtx.t * 'a expr * 'a expr) option Gen.t =
          get_gen
            (ast_expr_arb_default_type_ctx_params PrintExprSource Gen.unit)
          >>= fun (type_ctx, e) ->
          let shrinks : 'a expr Iter.t = expr_shrink ~preserve_type:true e in
          let shrinks_list_opt : 'a expr list =
            let xs = ref [] in
            shrinks (fun x -> xs := x :: !xs);
            !xs
          in
          match shrinks_list_opt with
          | [] -> return None
          | _ :: _ as shrinks_list ->
              oneofl shrinks_list >|= fun e_shrunk ->
              Some (type_ctx, e, e_shrunk)
        in
        QCheck.make
          ~print:(function
            | None -> "None"
            | Some (type_ctx, e, e_shrunk) ->
                sprintf "[type ctx: %s]\n\ne = %s\n\ne_shrunk = %s"
                  (type_ctx |> TestingTypeCtx.sexp_of_t |> Sexp.to_string)
                  (ast_to_source_code ~use_newlines:true e)
                  (ast_to_source_code ~use_newlines:true e_shrunk))
          ~shrink:
            QCheck.Shrink.(
              option (triple nil nil (expr_shrink ~preserve_type:true)))
          gen)
       (function
         | None -> true
         | Some (type_ctx, e, e_shrunk) -> (
             match TestingTypeChecker.check_type_ctx type_ctx with
             | Error err ->
                 Test.fail_reportf "Failed to check type ctx, with error: %s"
                   (print_typing_error err)
             | Ok type_ctx -> (
                 match
                   TestingTypeChecker.type_expr
                     (type_ctx, TestingVarCtx.empty)
                     e
                 with
                 | Ok e_tpe -> (
                     let e_type =
                       e_tpe
                       |> TestingTypeChecker
                          .typed_program_expression_get_expression
                       |> Ast.expr_node_val |> fst
                     in
                     match
                       TestingTypeChecker.type_expr
                         (type_ctx, TestingVarCtx.empty)
                         e_shrunk
                     with
                     | Ok e_shrunk_tpe ->
                         let e_shrunk_type =
                           e_shrunk_tpe
                           |> TestingTypeChecker
                              .typed_program_expression_get_expression
                           |> Ast.expr_node_val |> fst
                         in
                         if equal_vtype e_type e_shrunk_type then true
                         else
                           Test.fail_reportf
                             "Types don't match. Original: %s, shrunk: %s"
                             (vtype_to_source_code e_type)
                             (vtype_to_source_code e_shrunk_type)
                     | Error err ->
                         Test.fail_reportf "Typing error for shrunk e: %s"
                           (err |> sexp_of_typing_error |> Sexp.to_string))
                 | Error err ->
                     Test.fail_reportf "Typing error for original e: %s"
                       (err |> sexp_of_typing_error |> Sexp.to_string)))))

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
              (vtype_to_source_code t)
              (ast_to_source_code ~use_newlines:true e))
          ~shrink:
            QCheck.Shrink.(
              pair nil (pair nil (expr_shrink ~preserve_type:true)))
          gen)
       (fun (type_ctx, (t, e)) ->
         match TestingTypeChecker.check_type_ctx type_ctx with
         | Error err ->
             Test.fail_reportf "Failed to check type ctx, with error: %s"
               (print_typing_error err)
         | Ok type_ctx -> (
             match
               TestingTypeChecker.type_expr (type_ctx, TestingVarCtx.empty) e
             with
             | Ok tpe ->
                 let et =
                   tpe
                   |> TestingTypeChecker.typed_program_expression_get_expression
                   |> Ast.expr_node_val |> fst
                 in
                 equal_vtype t et
             | Error _ -> false)))

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
                  TestingTypeCtx.customs_to_list type_ctx
                  |> sexp_of_list sexp_of_custom_type
                in
                sexp_of_pair sexp_of_type_ctx sexp_of_vtype))
          QCheck.Gen.(
            default_testing_type_ctx_gen >>= fun type_ctx ->
            vtype_gen ~type_ctx ~mrd:default_max_gen_rec_depth >|= fun t ->
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
  list (pair string (vtype_arb ~type_ctx ~mrd:default_max_gen_rec_depth))

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
                      (vtype_gen ~type_ctx ~mrd:default_max_gen_rec_depth)
                      (vtype_gen ~type_ctx ~mrd:default_max_gen_rec_depth)
                    >|= fun (t1, t2) -> (type_ctx, VTypeFun (t1, t2)));
                create_typed_expr_gen_test "'a * 'b"
                  Gen.(
                    default_testing_type_ctx_gen >>= fun type_ctx ->
                    pair
                      (vtype_gen ~type_ctx ~mrd:default_max_gen_rec_depth)
                      (vtype_gen ~type_ctx ~mrd:default_max_gen_rec_depth)
                    >|= fun (t1, t2) -> (type_ctx, VTypePair (t1, t2)));
              ];
       ]
