open OUnit2

let () =
  let suite =
    "PQ Lang Tests" >::: [
      Test_lexer.suite;
    ]
  in
  run_test_tt_main suite
