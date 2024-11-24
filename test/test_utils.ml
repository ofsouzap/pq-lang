open Core
open OUnit2
open QCheck
open Pq_lang
open Vtype
open Utils

let create_test_for_base_type (name : string) (t : vtype) : test =
  QCheck_runner.to_ounit2_test
    (QCheck.Test.make ~name ~count:1000
       (ast_expr_arb ~val_sexp:sexp_of_unit ~t Gen.unit) (fun e ->
         let typed_result = Typing.type_expr e in
         match typed_result with
         | Ok typed_e ->
             let et = Ast.expr_node_val typed_e |> fst in
             equal_vtype t et
         | Error _ -> false))

let create_test_for_compound_type (name : string) (t_gen : vtype Gen.t) : test =
  QCheck_runner.to_ounit2_test
    (QCheck.Test.make ~name ~count:1000
       (let open QCheck.Gen in
        let t_arb =
          make ~print:(Core.Fn.compose Sexp.to_string sexp_of_vtype) t_gen
        in
        let e_gen =
          t_gen >>= fun t ->
          QCheck.gen (ast_expr_arb ~val_sexp:sexp_of_unit ~t Gen.unit)
        in
        let e_arb =
          make
            ~print:
              (Core.Fn.compose Sexp.to_string (Ast.sexp_of_expr sexp_of_unit))
            e_gen
        in
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
                create_test_for_base_type "int" VTypeInt;
                create_test_for_base_type "bool" VTypeBool;
                create_test_for_compound_type "'a -> 'b"
                  Gen.(
                    pair
                      (vtype_gen default_max_gen_rec_depth)
                      (vtype_gen default_max_gen_rec_depth)
                    >|= fun (t1, t2) -> VTypeFun (t1, t2));
              ];
       ]
