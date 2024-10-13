open OUnit2

let () =
  let suite =
    "PQ Lang Tests"
    >::: [
           Test_frontend.suite;
           Test_ast_executor.suite;
           Test_ast_executor_store.suite;
         ]
  in
  run_test_tt_main suite
