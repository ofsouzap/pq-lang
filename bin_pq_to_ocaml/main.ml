open Core
open Pq_lang
module Program = Program.StdProgram
module QuotientTypeChecker = QuotientTypeChecker.MakeZ3
module ProgramExecutor = ProgramExecutor.SimpleExecutor
module TypeChecker = ProgramExecutor.TypeChecker

let pq_filename_to_out_ml_filename (filename : string) : string =
  Filename.chop_extension filename ^ ".ml"

let pq_filename_to_out_mli_filename (filename : string) : string =
  Filename.chop_extension filename ^ ".mli"

let read_all_file (filename : string) : (string, string) Result.t =
  try Ok (In_channel.read_all filename) with Sys_error msg -> Error msg

let process_pq_file (filename : string) : (Program.plain_t, string) Result.t =
  let open Result in
  read_all_file filename >>= fun contents ->
  Frontend.run_frontend_string contents
  |> Result.map_error ~f:(fun err ->
         sprintf "Frontend error: %s"
           Frontend.(
             err |> function
             | LexingError c -> sprintf "Lexing error: %c" c
             | ParsingError -> "Parsing error"))
  >>= fun prog ->
  TypeChecker.type_program prog
  |> Result.map_error ~f:(fun err ->
         sprintf "Typing error: %s" (TypeChecker.TypingError.print err))
  >>= fun tp ->
  let prog_for_quotient_type_checking =
    TypeChecker.typed_program_get_program tp
    |> Program.fmap_pattern ~f:(fun (t, ()) ->
           ({ t } : QuotientTypeChecker.Smt.pattern_tag))
    |> Program.fmap_expr ~f:(fun (t, ()) ->
           ({ t } : QuotientTypeChecker.Smt.expr_tag))
  in
  QuotientTypeChecker.check_program prog_for_quotient_type_checking
  |> Result.map_error ~f:(fun err ->
         sprintf "Quotient type checking error: %s"
           (err |> QuotientTypeChecker.sexp_of_quotient_typing_error
          |> Sexp.to_string_hum))
  >>= function
  | Error () -> Error "Quotient type check failed"
  | Ok () -> Ok prog

let write_outputs (input_path : string) (outputs : OcamlConverter.StdM.output) :
    (unit, string) Result.t =
  let open Result in
  let open Out_channel in
  let write_output (filename : string) (contents : string) :
      (unit, string) Result.t =
    try
      let out_file = create ~append:false ~fail_if_exists:true filename in
      output_string out_file contents;
      close out_file;
      Ok ()
    with Sys_error msg -> Error msg
  in
  write_output (pq_filename_to_out_ml_filename input_path) outputs.ml_source
  >>= fun () ->
  write_output (pq_filename_to_out_mli_filename input_path) outputs.mli_source

let compile_file (input_path : string) : (unit, string) Result.t =
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
      | Error err_msg -> eprintf "%s\n" err_msg)
  | _ -> eprintf "Usage: pq_to_ocaml <input_file>\n"
