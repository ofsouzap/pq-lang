open Core
open OUnit2
open Pq_lang
open Testing_utils

let test_cases_to_source_code_inv =
  let open QCheck in
  let open Frontend in
  Test.make ~count:1000 ~name:"Program to source code"
    unit_program_arbitrary_with_default_options (fun prog ->
      match run_frontend_string (Program.program_to_source_code prog) with
      | Ok prog_out ->
          if Program.equal_program equal_unit equal_unit prog prog_out then true
          else
            Test.fail_reportf
              "Got different Program. Expected:\n\n%s\n\nActual:\n\n%s"
              (Unit_program_qcheck_testing.print
                 Unit_expr_qcheck_testing.PrintExprSource prog)
              (Unit_program_qcheck_testing.print PrintExprSource prog_out)
      | Error err ->
          Test.fail_reportf "Got frontend error: %s"
            (err |> sexp_of_frontend_error |> Sexp.to_string_hum))

let suite =
  "Program" >::: [ QCheck_runner.to_ounit2_test test_cases_to_source_code_inv ]
