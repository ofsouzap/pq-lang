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

let get_expected_output_file_paths (pq_file_path : string) : string list =
  let file_no_ext = Filename.chop_extension pq_file_path in
  let base_dir = Filename.dirname pq_file_path in
  [
    Filename.concat base_dir (file_no_ext ^ ".ml");
    Filename.concat base_dir (file_no_ext ^ ".mli");
  ]

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

(* TODO - test that created outputs can compile using ocamlc *)

let test_positive ~(test_name : string) (file_path : string) :
    unit Alcotest.test_case =
  ( test_name,
    `Quick,
    fun () ->
      let _, result = run_exec_on file_path in
      match result with
      | Error (`Exit_non_zero (exit_code : int)) ->
          Alcotest.failf "Executable failed with exit code: %d" exit_code
      | Error (`Signal signal) ->
          Alcotest.failf "Executable failed with signal: %s"
            (Signal.to_string signal)
      | Ok () ->
          List.iter (get_expected_output_file_paths file_path) ~f:(fun path ->
              if check_file_exists path then ()
              else
                Alcotest.failf
                  "Expected output file \"%s\" to exist but it doesn't" path);
          () )

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
