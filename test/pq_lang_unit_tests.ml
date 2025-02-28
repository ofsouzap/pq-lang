let () =
  Alcotest.run "PQ Lang Unit Tests"
    [
      ("Testing Utils", Pq_lang_unit_testing_utils.suite);
      ("Utils", Pq_lang_unit_utils.suite);
      ("Expr", Pq_lang_unit_expr.suite);
      ("Program", Pq_lang_unit_program.suite);
      ("Lexer/parser", Pq_lang_unit_lexer_parser.suite);
      ("Program Executor", Pq_lang_unit_programExecutor.suite);
      ("Program Executor Store", Pq_lang_unit_programExecutor_store.suite);
      ("Type Checker", Pq_lang_unit_typeChecker.suite);
      ("Quotient Type Checker", Pq_lang_unit_quotientTypeChecker.suite);
    ]
