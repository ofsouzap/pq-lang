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

let run (run_frontend : unit -> Frontend.run_frontend_res) : int =
  let open Result in
  let res =
    run_frontend ()
    |> Result.map_error ~f:(fun err ->
           ( exit_code ErrInternal,
             sprintf "Frontend error: %s"
               Frontend.(
                 err |> function
                 | LexingError c -> sprintf "Lexing error: %c" c
                 | ParsingError -> "Parsing error") ))
    >>= fun prog ->
    TypeChecker.type_program
      ~get_source_position:(function First v -> Some v | Second v -> Some v)
      prog
    |> Result.map_error ~f:(fun err ->
           ( exit_code ErrInternal,
             sprintf "Typing error: %s" (TypeChecker.TypingError.print err) ))
    >>= fun tp ->
    QuotientTypeChecker.check_program
      (TypeChecker.typed_program_get_program tp
      |> Program.fmap_pattern ~f:(fun (t, source_pos) ->
             ({ t; source_pos } : QuotientTypeChecker.node_tag))
      |> Program.fmap_expr ~f:(fun (t, source_pos) ->
             ({ t; source_pos } : QuotientTypeChecker.node_tag)))
    |> Result.map_error ~f:(fun err ->
           ( exit_code ErrInternal,
             sprintf "Quotient type checking error: %s"
               (err |> QuotientTypeChecker.sexp_of_quotient_typing_error
              |> Sexp.to_string_hum) ))
    >>= function
    | Error err ->
        Error
          ( exit_code ErrQuotientTypeChecking,
            sprintf "Quotient type check failed:\n%s"
              (QuotientTypeChecker.QuotientTypeCheckingFailure.print err) )
    | Ok () ->
        ProgramExecutor.execute_program tp
        |> Result.map_error ~f:(fun err ->
               ( exit_code ErrInternal,
                 sprintf "Execution error: %s"
                   (ProgramExecutor.print_exec_err err) ))
        >>| fun v -> ProgramExecutor.Store.sexp_of_value v |> Sexp.to_string_hum
  in
  match res with
  | Ok ok_msg ->
      printf "%s\n" ok_msg;
      Cmd.Exit.ok
  | Error (e, err_msg) ->
      eprintf "%s\n" err_msg;
      e

let file_path_arg_to_frontend :
    string option -> unit -> Frontend.run_frontend_res = function
  | None -> fun () -> Frontend.run_frontend_channel In_channel.stdin
  | Some file_path ->
      fun () ->
        Frontend.run_frontend_string ~filename:file_path
          (In_channel.read_all file_path)

let file_path_t : string option Term.t =
  let doc =
    "Path to the file containing the PQ program. If none is provided then \
     stdin is read"
  in
  Arg.(
    value
    & opt (some string) None
    & info [ "i"; "input-file" ] ~docv:"FILE" ~doc)

let exec_t : int Term.t =
  Term.(const run $ map file_path_arg_to_frontend file_path_t)

let get_exits () : Cmd.Exit.info list =
  Cmd.Exit.defaults
  @ List.map ~f:exit_info
      [ ErrCallingSystemFunctions; ErrQuotientTypeChecking; ErrInternal ]

let exec_info : Cmd.info =
  let doc = "Execute a PQ program" in
  Cmd.info ~doc ~exits:(get_exits ()) "pq_exec"

let () = Cmd.(Core.exit @@ eval' (v exec_info exec_t))
