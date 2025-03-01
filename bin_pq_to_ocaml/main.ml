open Core
open Pq_lang
module Program = Program.StdProgram
module QuotientTypeChecker = QuotientTypeChecker.MakeZ3
module ProgramExecutor = ProgramExecutor.SimpleExecutor
module TypeChecker = ProgramExecutor.TypeChecker

type exit_code = ExitCode of int
type error_exit = exit_code * string

let pq_filename_to_out_ml_filename (filename : string) : string =
  Filename.chop_extension filename ^ ".ml"

let pq_filename_to_out_mli_filename (filename : string) : string =
  Filename.chop_extension filename ^ ".mli"

let read_all_file (filename : string) : (string, error_exit) Result.t =
  try Ok (In_channel.read_all filename)
  with Sys_error msg -> Error (ExitCode 1, msg)

let process_pq_file (filename : string) : (Program.plain_t, error_exit) Result.t
    =
  let open Result in
  read_all_file filename >>= fun contents ->
  Frontend.run_frontend_string contents
  |> Result.map_error ~f:(fun err ->
         ( ExitCode 3,
           sprintf "Frontend error:\n%s"
             Frontend.(
               err |> function
               | LexingError c -> sprintf "Lexing error: %c" c
               | ParsingError -> "Parsing error") ))
  >>= fun prog ->
  TypeChecker.type_program
    ~get_source_position:(function First v -> Some v | Second v -> Some v)
    prog
  |> Result.map_error ~f:(fun err ->
         ( ExitCode 3,
           sprintf "Typing error:\n%s" (TypeChecker.TypingError.print err) ))
  >>= fun tp ->
  let prog_for_quotient_type_checking =
    TypeChecker.typed_program_get_program tp
    |> Program.fmap_pattern ~f:(fun (t, source_pos) ->
           ({ t; source_pos } : QuotientTypeChecker.node_tag))
    |> Program.fmap_expr ~f:(fun (t, source_pos) ->
           ({ t; source_pos } : QuotientTypeChecker.node_tag))
  in
  QuotientTypeChecker.check_program prog_for_quotient_type_checking
  |> Result.map_error ~f:(fun err ->
         ( ExitCode 3,
           sprintf "Quotient type checking error:\n%s"
             (err |> QuotientTypeChecker.sexp_of_quotient_typing_error
            |> Sexp.to_string_hum) ))
  >>= function
  | Error err ->
      Error
        ( ExitCode 2,
          sprintf "Quotient type check failed:\n%s"
            (QuotientTypeChecker.QuotientTypeCheckingFailure.print err) )
  | Ok () -> Ok (prog |> Program.to_plain_t)

let write_outputs (input_path : string) (outputs : OcamlConverter.StdM.output) :
    (unit, error_exit) Result.t =
  let open Result in
  let open Out_channel in
  let write_output (filename : string) (contents : string) :
      (unit, error_exit) Result.t =
    try
      let out_file = create ~append:false ~fail_if_exists:true filename in
      output_string out_file contents;
      close out_file;
      Ok ()
    with Sys_error msg -> Error (ExitCode 1, msg)
  in
  write_output (pq_filename_to_out_ml_filename input_path) outputs.ml_source
  >>= fun () ->
  write_output (pq_filename_to_out_mli_filename input_path) outputs.mli_source

let compile_file (input_path : string) : (unit, error_exit) Result.t =
  let open Result in
  let module OcamlConverter = OcamlConverter.StdM in
  process_pq_file input_path >>= fun prog ->
  let output = OcamlConverter.program_to_ocaml prog in
  write_outputs input_path output

let () =
  Sys.get_argv () |> function
  | [| _; input_path |] -> (
      compile_file input_path |> function
      | Ok () -> ()
      | Error (ExitCode exit_code, err_msg) ->
          eprintf "Error:\n%s\n" err_msg;
          exit exit_code)
  | _ ->
      eprintf
        {|Usage: pq_to_ocaml <input_file>

Exit Codes:
  0 - All fine
  1 - Error in calling system functions
  2 - Quotient typing error
  3 - Other internal error|}
