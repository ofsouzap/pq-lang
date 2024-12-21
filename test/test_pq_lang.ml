open OUnit2

let () =
  let suite =
    "PQ Lang Tests"
    >::: [
           Test_testing_utils.suite;
           Test_utils.suite;
           Test_ast.suite;
           Test_frontend.suite;
           Test_ast_executor.suite;
           Test_ast_executor_store.suite;
           Test_interpreting_integration.suite;
           Test_typing.suite;
         ]
  in
  run_test_tt_main suite
