open OUnit2

let () =
  let suite =
    "PQ Lang Tests"
    >::: [
           Test_testing_utils.suite;
           Test_utils.suite;
           Test_ast.suite;
           Test_program.suite;
           Test_lexer_parser.suite;
           Test_ast_executor.suite;
           Test_ast_executor_store.suite;
           Test_interpreting_integration.suite;
           Test_typing.suite;
           (* Test_quotient_type_checking.suite; *)
         ]
  in
  run_test_tt_main suite
