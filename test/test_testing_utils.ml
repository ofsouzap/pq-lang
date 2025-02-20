open Core
open OUnit2
open QCheck
open Pq_lang
open Utils
open Vtype
open Expr
open Custom_types
open Typing
open Testing_utils

let vtype_gen_no_fun (type_ctx : TestingTypeCtx.t) =
  Vtype.QCheck_testing.gen
    {
      variant_types =
        TestingTypeCtx.type_defns_to_ordered_list type_ctx
        |> List.filter_map ~f:(function
             | VariantType (vt_name, _) -> Some vt_name
             | QuotientType _ -> None)
        |> StringSet.of_list;
      allow_fun_types = false;
      mrd = default_max_gen_rec_depth;
    }

let vtype_arb_no_fun_type (type_ctx : TestingTypeCtx.t) =
  Vtype.QCheck_testing.arbitrary
    {
      variant_types =
        TestingTypeCtx.type_defns_to_ordered_list type_ctx
        |> List.filter_map ~f:(function
             | VariantType (vt_name, _) -> Some vt_name
             | QuotientType _ -> None)
        |> StringSet.of_list;
      allow_fun_types = false;
      mrd = default_max_gen_rec_depth;
    }

let create_test_expr_shrink_can_preserve_type (name : string) : test =
  let open QCheck in
  QCheck_runner.to_ounit2_test
    (Test.make ~name ~count:100
       (let open QCheck.Gen in
        let gen :
            (TestingTypeCtx.t * ('tag_e, 'tag_p) expr * ('tag_e, 'tag_p) expr)
            option
            Gen.t =
          get_gen unit_program_arbitrary_with_default_options >>= fun prog ->
          let type_ctx = prog.custom_types |> TestingTypeCtx.from_list in
          let e = prog.e in
          let shrinks : ('tag_e, 'tag_p) expr Iter.t =
            Unit_ast_qcheck_testing.shrink { preserve_type = true } e
          in
          let shrinks_list_opt : ('tag_e, 'tag_p) expr list =
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
                  (type_ctx |> TestingTypeCtx.sexp_of_t |> Sexp.to_string_hum)
                  (Expr.to_source_code ~use_newlines:true e)
                  (Expr.to_source_code ~use_newlines:true e_shrunk))
          ~shrink:
            QCheck.Shrink.(
              option
                (triple nil nil
                   (Unit_ast_qcheck_testing.shrink { preserve_type = true })))
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
                 | Ok e_typed -> (
                     let e_type = e_typed |> Expr.expr_node_val |> fst in
                     match
                       TestingTypeChecker.type_expr
                         (type_ctx, TestingVarCtx.empty)
                         e_shrunk
                     with
                     | Ok e_shrunk_typed ->
                         let e_shrunk_type =
                           e_shrunk_typed |> Expr.expr_node_val |> fst
                         in
                         if equal_vtype e_type e_shrunk_type then true
                         else
                           Test.fail_reportf
                             "Types don't match. Original: %s, shrunk: %s"
                             (vtype_to_source_code e_type)
                             (vtype_to_source_code e_shrunk_type)
                     | Error err ->
                         Test.fail_reportf "Typing error for shrunk e: %s"
                           (err |> sexp_of_typing_error |> Sexp.to_string_hum))
                 | Error err ->
                     Test.fail_reportf "Typing error for original e: %s"
                       (err |> sexp_of_typing_error |> Sexp.to_string_hum)))))

let create_typed_expr_gen_test (name : string)
    (types_gen : (TestingTypeCtx.t * vtype) Gen.t) : test =
  let open QCheck in
  QCheck_runner.to_ounit2_test
    (Test.make ~name ~count:1000
       (let open QCheck.Gen in
        let gen :
            (TestingTypeCtx.t * (vtype * (unit, unit) Expr.expr)) QCheck.Gen.t =
          types_gen >>= fun (type_ctx, t) ->
          Unit_ast_qcheck_testing.gen
            {
              t = Some (Unit_ast_qcheck_testing.vtype_to_gen_vtype_unsafe t);
              variant_types =
                TestingTypeCtx.type_defns_to_ordered_list type_ctx
                |> List.filter_map ~f:(function
                     | VariantType vt -> Some vt
                     | QuotientType _ -> None);
              top_level_defns = [] (* TODO - have this arbitrary too *);
              v_gen = QCheck.Gen.unit;
              pat_v_gen = QCheck.Gen.unit;
              mrd = default_max_gen_rec_depth;
            }
          >|= fun e -> (type_ctx, (t, e))
        in
        QCheck.make
          ~print:(fun (type_ctx, (t, e)) ->
            sprintf "[type ctx: %s]\n[type: %s]\n%s"
              (type_ctx |> TestingTypeCtx.sexp_of_t |> Sexp.to_string_hum)
              (vtype_to_source_code t)
              (Expr.to_source_code ~use_newlines:true e))
          ~shrink:
            QCheck.Shrink.(
              pair nil
                (pair nil
                   (Unit_ast_qcheck_testing.shrink { preserve_type = true })))
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
             | Ok e_typed ->
                 let et = e_typed |> Expr.expr_node_val |> fst in
                 equal_vtype t et
             | Error _ -> false)))

let create_typed_expr_gen_test_for_fixed_type (name : string) (t : vtype) =
  create_typed_expr_gen_test name
    QCheck.Gen.(default_testing_type_ctx_gen >|= fun type_ctx -> (type_ctx, t))

(* TODO - typed program generation tests.
   Just reuse the code for expression generation tests but type context comes from program *)

let create_test_vtype_gen_constructors_exist (name : string) : test =
  let open QCheck in
  QCheck_runner.to_ounit2_test
    (Test.make ~name ~count:1000
       (QCheck.make
          ~print:
            QCheck.Print.(
              pair (TestingTypeCtx.QCheck_testing.print ()) vtype_to_source_code)
          QCheck.Gen.(
            default_testing_type_ctx_gen >>= fun type_ctx ->
            vtype_gen_no_fun type_ctx >|= fun t -> (type_ctx, t)))
       (fun (type_ctx, t) ->
         match t with
         | VTypeCustom vt_name ->
             TestingTypeCtx.type_defn_exists type_ctx vt_name
         | _ -> true))

let create_test_type_ctx_gen_valid (name : string) : test =
  let open QCheck in
  QCheck_runner.to_ounit2_test
    (Test.make ~name ~count:1000
       (TestingTypeCtx.QCheck_testing.arbitrary
          {
            max_variant_types = default_max_variant_type_count;
            max_constructors = default_max_variant_type_constructor_count;
            max_quotient_types = default_max_quotient_type_count;
            mrd = default_max_gen_rec_depth;
          }
       |>
       (* Don't allow shrinking as we are considering the generated type contexts *)
       QCheck.set_shrink QCheck.Shrink.nil)
       (fun type_ctx ->
         match TestingTypeChecker.check_type_ctx type_ctx with
         | Ok _ -> true
         | Error err ->
             Test.fail_reportf "Failed to check type ctx, with error: %s"
               (print_typing_error err)))

let create_test_var_ctx (xs : (string * vtype) list) : TestingVarCtx.t =
  List.fold xs ~init:TestingVarCtx.empty ~f:(fun ctx (x, t) ->
      TestingVarCtx.add ctx x t)

let create_list_impl_var_ctx (xs : (string * vtype) list) :
    ListTypingVarContext.t =
  List.fold xs ~init:ListTypingVarContext.empty ~f:(fun ctx (x, t) ->
      ListTypingVarContext.add ctx x t)

let var_ctx_list_arb ~(type_ctx : TestingTypeCtx.t) =
  let open QCheck in
  list (pair string (vtype_arb_no_fun_type type_ctx))

(* TODO - test that TestingTypeChecker types things the same as the SimpleTypeChecker *)

let suite =
  "Testing Utilities Tests"
  >::: [
         "Value type generator"
         >::: [ create_test_vtype_gen_constructors_exist "Variant types exist" ];
         "Type context generator"
         >::: [ create_test_type_ctx_gen_valid "Type context is valid" ];
         "Typed expression generator"
         >::: [
                create_typed_expr_gen_test_for_fixed_type "unit" VTypeUnit;
                create_typed_expr_gen_test_for_fixed_type "int" VTypeInt;
                create_typed_expr_gen_test_for_fixed_type "bool" VTypeBool;
                create_typed_expr_gen_test "'a * 'b"
                  Gen.(
                    default_testing_type_ctx_gen >>= fun type_ctx ->
                    pair (vtype_gen_no_fun type_ctx) (vtype_gen_no_fun type_ctx)
                    >|= fun (t1, t2) -> (type_ctx, VTypePair (t1, t2)));
              ];
       ]
