open Core
open Cmdliner
open Pq_lang
module Program = Program.StdProgram
module QuotientTypeChecker = QuotientTypeChecker.MakeZ3
module ProgramExecutor = ProgramExecutor.SimpleExecutor
module TypeChecker = ProgramExecutor.TypeChecker

type exit = ErrCallingSystemFunctions | ErrQuotientTypeChecking | ErrInternal

let exit_code : exit -> int = function
  | ErrCallingSystemFunctions -> 1
  | ErrQuotientTypeChecking -> 2
  | ErrInternal -> 3

let exit_info (e : exit) : Cmd.Exit.info =
  let open Cmd.Exit in
  (match e with
  | ErrCallingSystemFunctions -> info ~doc:"Error calling system functions"
  | ErrQuotientTypeChecking -> info ~doc:"Quotient type checking error"
  | ErrInternal -> info ~doc:"Internal error")
    (exit_code e)

type error_exit = exit * string

let pq_filename_to_out_ml_filename (filename : string) : string =
  Filename.chop_extension filename ^ ".ml"

let pq_filename_to_out_mli_filename (filename : string) : string =
  Filename.chop_extension filename ^ ".mli"

let read_all_file (filename : string) : (string, error_exit) Result.t =
  try Ok (In_channel.read_all filename)
  with Sys_error msg -> Error (ErrCallingSystemFunctions, msg)

let process_pq_file (filename : string) :
    ( (Frontend.source_position, Frontend.source_position) Program.t,
      error_exit )
    Result.t =
  let open Result in
  read_all_file filename >>= fun contents ->
  Frontend.run_frontend_string ~filename contents
  |> Result.map_error ~f:(fun err ->
         ( ErrInternal,
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
         ( ErrInternal,
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
         ( ErrInternal,
           sprintf "Quotient type checking error:\n%s"
             (err |> QuotientTypeChecker.sexp_of_quotient_typing_error
            |> Sexp.to_string_hum) ))
  >>= function
  | Error err ->
      Error
        ( ErrQuotientTypeChecking,
          sprintf "Quotient type check failed:\n%s"
            (QuotientTypeChecker.QuotientTypeCheckingFailure.print err) )
  | Ok () -> Ok prog

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
    with Sys_error msg -> Error (ErrCallingSystemFunctions, msg)
  in
  write_output (pq_filename_to_out_ml_filename input_path) outputs.ml_source
  >>= fun () ->
  write_output (pq_filename_to_out_mli_filename input_path) outputs.mli_source

let compile_file (input_path : string) : (unit, error_exit) Result.t =
  let open Result in
  let module OcamlConverter = OcamlConverter.StdM in
  process_pq_file input_path >>= fun prog ->
  let output =
    OcamlConverter.program_to_ocaml ~get_source_position:Fn.id prog
  in
  write_outputs input_path output

let run_main (input_path : string) : int =
  match compile_file input_path with
  | Ok () -> Cmd.Exit.ok
  | Error (e, err_msg) ->
      eprintf "Error:\n%s\n" err_msg;
      exit_code e

let file_path_t : string Term.t =
  let doc = "Path to the file containing the PQ program" in
  Arg.(required & pos 0 (some string) None & info [] ~docv:"FILE" ~doc)

let compile_t : int Term.t = Term.(const run_main $ file_path_t)

let get_exits () : Cmd.Exit.info list =
  Cmd.Exit.defaults
  @ List.map ~f:exit_info
      [ ErrCallingSystemFunctions; ErrQuotientTypeChecking; ErrInternal ]

let compile_info : Cmd.info =
  let doc = "Compile PQ source code to an OCaml module" in
  Cmd.info ~doc ~exits:(get_exits ()) "pq_to_ocaml"

let () = Cmd.(Core.exit @@ eval' (v compile_info compile_t))
