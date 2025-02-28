let () =
  Alcotest.run "PQ Lang Unit Tests"
    [
      ("Testing Utils", Test_testing_utils.suite);
      ("Utils", Test_utils.suite);
      ("Expr", Test_expr.suite);
      ("Program", Test_program.suite);
      ("Lexer/parser", Test_lexer_parser.suite);
      ("Program Executor", Test_programExecutor.suite);
      ("Program Executor Store", Test_programExecutor_store.suite);
      ("Type Checker", Test_typeChecker.suite);
      ("Quotient Type Checker", Test_quotientTypeChecker.suite);
      ("OCaml Converter", Test_ocamlConverter.suite);
    ]
