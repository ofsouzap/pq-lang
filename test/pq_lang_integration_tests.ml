let () =
  Alcotest.run "PQ Lang Integration Tests"
    [ ("Interpreting", Pq_lang_integration_interpreting.suite) ]
