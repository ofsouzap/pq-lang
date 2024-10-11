open OUnit2

let () =
  let suite =
    "PQ Lang Tests" >::: [
      Test_lexer.suite;
      Test_parser.suite;
      Test_ast_executor.suite;
    ]
  in
  run_test_tt_main suite
