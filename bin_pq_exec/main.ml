open Core
open Pq_lang
module Program = Program.StdProgram
module QuotientTypeChecker = QuotientTypeChecker.MakeZ3
module ProgramExecutor = ProgramExecutor.SimpleExecutor
module TypeChecker = ProgramExecutor.TypeChecker

let () =
  let open Result in
  let res =
    Frontend.run_frontend_channel In_channel.stdin
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
    QuotientTypeChecker.check_program
      (TypeChecker.typed_program_get_program tp
      |> Program.fmap_pattern ~f:(fun (t, ()) ->
             ({ t } : QuotientTypeChecker.Smt.pattern_tag))
      |> Program.fmap_expr ~f:(fun (t, ()) ->
             ({ t } : QuotientTypeChecker.Smt.expr_tag)))
    |> Result.map_error ~f:(fun err ->
           sprintf "Quotient type checking error: %s"
             (err |> QuotientTypeChecker.sexp_of_quotient_typing_error
            |> Sexp.to_string_hum))
    >>= function
    | Error err ->
        Error
          (sprintf "Quotient type check failed:\n%s"
             (QuotientTypeChecker.print_quotient_type_checking_failure err))
    | Ok () ->
        ProgramExecutor.execute_program tp
        |> Result.map_error ~f:(fun err ->
               sprintf "Execution error: %s"
                 (ProgramExecutor.print_exec_err err))
        >>| fun v -> ProgramExecutor.Store.sexp_of_value v |> Sexp.to_string_hum
  in
  match res with
  | Ok ok_msg -> printf "%s\n" ok_msg
  | Error err_msg -> eprintf "%s\n" err_msg
