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

let create_test_var_ctx (xs : (string * vtype) list) : TestingVarCtx.t =
  List.fold xs ~init:TestingVarCtx.empty ~f:(fun ctx (x, t) ->
      TestingVarCtx.add ctx x t)

let create_list_impl_var_ctx (xs : (string * vtype) list) :
    ListTypingVarContext.t =
  List.fold xs ~init:ListTypingVarContext.empty ~f:(fun ctx (x, t) ->
      ListTypingVarContext.add ctx x t)

let var_ctx_list_arb =
  let open QCheck in
  list (pair string (vtype_arb default_max_gen_rec_depth))

let compare_test_impl_with_list_impl (ct : TestingVarCtx.t)
    (cl : ListTypingVarContext.t) : bool =
  let ct_list = TestingVarCtx.to_list ct in
  List.fold ~init:true ct_list ~f:(fun acc (x, t) ->
      acc
      &&
      match ListTypingVarContext.find cl x with
      | Some t' -> equal_vtype t t'
      | None -> false)

let test_testing_utils_var_ctx_operation_add : test =
  QCheck.Test.make ~name:"Add" ~count:100
    (triple var_ctx_list_arb string (vtype_arb default_max_gen_rec_depth))
    (fun (ctx_inp, xname, xtype) ->
      let ctx_testing = create_test_var_ctx ctx_inp in
      let ctx_list_impl = create_list_impl_var_ctx ctx_inp in

      let ctx_testing' = TestingVarCtx.add ctx_testing xname xtype in
      let ctx_list_impl' = ListTypingVarContext.add ctx_list_impl xname xtype in

      compare_test_impl_with_list_impl ctx_testing' ctx_list_impl')
  |> QCheck_runner.to_ounit2_test

let test_testing_utils_var_ctx_operation_find_exists : test =
  let var_ctx_list_and_item_gen :
      ((string * vtype) list * (string * vtype)) QCheck.Gen.t =
    let open QCheck.Gen in
    let l_gen = QCheck.get_gen var_ctx_list_arb in
    pair (pair string (vtype_gen default_max_gen_rec_depth)) l_gen
    >>= fun (h, ts) ->
    let ctx_list = h :: ts in
    let len = List.length ctx_list in
    int_range 0 (len - 1) >|= fun i ->
    let v =
      ctx_list |> Core.Fn.flip List.drop i |> List.hd |> Option.value_exn
    in
    (ctx_list, v)
  in
  let var_ctx_list_and_item_arb :
      ((string * vtype) list * (string * vtype)) QCheck.arbitrary =
    QCheck.make
      ~print:
        QCheck.Print.(
          pair
            (list (pair string vtype_to_source_code))
            (pair string vtype_to_source_code))
      var_ctx_list_and_item_gen
  in
  QCheck.Test.make ~name:"Find (exists)" ~count:100 var_ctx_list_and_item_arb
    (fun (ctx_inp, (xname, _)) ->
      let ctx_testing = create_test_var_ctx ctx_inp in
      let ctx_list_impl = create_list_impl_var_ctx ctx_inp in

      (equal_option equal_vtype)
        (TestingVarCtx.find ctx_testing xname)
        (ListTypingVarContext.find ctx_list_impl xname))
  |> QCheck_runner.to_ounit2_test

let test_testing_utils_var_ctx_operation_find_might_exist : test =
  QCheck.Test.make ~name:"Find (might exist)" ~count:100
    (pair var_ctx_list_arb string) (fun (ctx_inp, xname) ->
      let ctx_testing = create_test_var_ctx ctx_inp in
      let ctx_list_impl = create_list_impl_var_ctx ctx_inp in

      (equal_option equal_vtype)
        (TestingVarCtx.find ctx_testing xname)
        (ListTypingVarContext.find ctx_list_impl xname))
  |> QCheck_runner.to_ounit2_test

let test_testing_utils_var_ctx_operation_singleton : test =
  QCheck.Test.make ~name:"Singleton" ~count:100
    (pair string (vtype_arb default_max_gen_rec_depth))
    (fun (xname, xtype) ->
      let ctx_testing = TestingVarCtx.singleton xname xtype in
      let ctx_list_impl = ListTypingVarContext.singleton xname xtype in

      compare_test_impl_with_list_impl ctx_testing ctx_list_impl)
  |> QCheck_runner.to_ounit2_test

let test_testing_utils_var_ctx_operation_append : test =
  QCheck.Test.make ~name:"Append" ~count:100
    (pair var_ctx_list_arb var_ctx_list_arb) (fun (ctx_inp1, ctx_inp2) ->
      let ctx_testing1 = create_test_var_ctx ctx_inp1 in
      let ctx_list_impl1 = create_list_impl_var_ctx ctx_inp1 in

      let ctx_testing2 = create_test_var_ctx ctx_inp2 in
      let ctx_list_impl2 = create_list_impl_var_ctx ctx_inp2 in

      let ctx_testing_appended =
        TestingVarCtx.append ctx_testing1 ctx_testing2
      in
      let ctx_list_impl_appended =
        ListTypingVarContext.append ctx_list_impl1 ctx_list_impl2
      in

      compare_test_impl_with_list_impl ctx_testing_appended
        ctx_list_impl_appended)
  |> QCheck_runner.to_ounit2_test

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
         "Testing Variable Context"
         >::: [
                test_testing_utils_var_ctx_operation_add;
                test_testing_utils_var_ctx_operation_find_exists;
                test_testing_utils_var_ctx_operation_find_might_exist;
                test_testing_utils_var_ctx_operation_singleton;
                test_testing_utils_var_ctx_operation_append;
              ];
       ]
