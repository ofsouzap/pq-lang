open Core

let cwd = Sys_unix.getcwd ()
let reldir = Filename.concat cwd

let get_positive_test_case_file_paths () : string list =
  Sys_unix.readdir (reldir "test_cases/positive")
  |> Array.to_list
  |> List.filter ~f:(fun file_path ->
         Filename.split_extension file_path |> function
         | _, Some ext when equal_string ext "pq" -> true
         | _ -> false)

let get_negative_test_case_file_paths () : string list =
  Sys_unix.readdir (reldir "test_cases/negative")
  |> Array.to_list
  |> List.filter ~f:(fun file_path ->
         Filename.split_extension file_path |> function
         | _, Some ext when equal_string ext "pq" -> true
         | _ -> false)

let get_expected_output_ml_file_path (pq_file_path : string) : string =
  Filename.concat
    (Filename.dirname pq_file_path)
    (Filename.chop_extension pq_file_path ^ ".ml")

let get_expected_output_mli_file_path (pq_file_path : string) : string =
  Filename.concat
    (Filename.dirname pq_file_path)
    (Filename.chop_extension pq_file_path ^ ".mli")

let run_exec_on (file_path : string) :
    Core_unix.(Process_info.t * Exit_or_signal.t) =
  let process =
    Core_unix.create_process ~prog:"dune"
      ~args:[ "exec"; "pq_to_ocaml"; file_path ]
  in
  (process, Core_unix.waitpid process.pid)

let check_file_exists (file_path : string) : bool =
  Sys_unix.file_exists ~follow_symlinks:false file_path |> function
  | `Yes -> true
  | `No | `Unknown -> false

let try_compile_output ~(ml_source_path : string) ~(mli_source_path : string) :
    Core_unix.(Process_info.t * Exit_or_signal.t) =
  let process =
    Core_unix.create_process ~prog:"ocamlc"
      ~args:[ "-c"; mli_source_path; ml_source_path ]
  in
  (process, Core_unix.waitpid process.pid)

let test_positive ~(test_name : string) (file_path : string) :
    unit Alcotest.test_case =
  ( test_name,
    `Quick,
    fun () ->
      (* Try run the executable *)
      let _, exec_result = run_exec_on file_path in
      match exec_result with
      | Error (`Exit_non_zero (exit_code : int)) ->
          Alcotest.failf "Executable failed with exit code: %d" exit_code
      | Error (`Signal signal) ->
          Alcotest.failf "Executable failed with signal: %s"
            (Signal.to_string signal)
      | Ok () -> (
          let ml_path = get_expected_output_ml_file_path file_path in
          let mli_path = get_expected_output_mli_file_path file_path in
          (* Check the expected output files exist *)
          if not (check_file_exists ml_path) then
            Alcotest.failf
              "Expected output ML file \"%s\" to exist but it doesn't" ml_path
          else if not (check_file_exists mli_path) then
            Alcotest.failf
              "Expected output MLi file \"%s\" to exist but it doesn't" mli_path
          else
            (* Try to compile the outputs and check that it works *)
            let _, compile_result =
              try_compile_output ~ml_source_path:ml_path
                ~mli_source_path:mli_path
            in
            match compile_result with
            | Error (`Exit_non_zero (exit_code : int)) ->
                Alcotest.failf "Compilation failed with exit code: %d" exit_code
            | Error (`Signal signal) ->
                Alcotest.failf "Compilation failed with signal: %s"
                  (Signal.to_string signal)
            | Ok () -> ()) )

let test_negative ~(test_name : string) (file_path : string) :
    unit Alcotest.test_case =
  ( test_name,
    `Quick,
    fun () ->
      let _, result = run_exec_on file_path in
      match result with
      | Error (`Exit_non_zero (_ : int)) ->
          (* To simplify the tests, I don't check the exit code or error message for this execution *)
          ()
      | Error (`Signal signal) ->
          Alcotest.failf "Executable failed with signal: %s"
            (Signal.to_string signal)
      | Ok () -> Alcotest.fail "Expected executable to fail but it didn't" )

let () =
  Alcotest.run "PQ to OCaml Integration Tests"
    [
      ( "positive cases",
        List.map (get_positive_test_case_file_paths ()) ~f:(fun file_path ->
            let name = Filename.basename file_path in
            test_positive ~test_name:name file_path) );
      ( "negative cases",
        List.map (get_negative_test_case_file_paths ()) ~f:(fun file_path ->
            let name = Filename.basename file_path in
            test_negative ~test_name:name file_path) );
    ]
