let () =
  Alcotest.run "PQ Lang Integration Tests"
    [ ("Interpreting", Test_interpreting.suite) ]
