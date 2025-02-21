open Core
open Pq_lang

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
           sprintf "Typing error: %s" (TypeChecker.print_typing_error err))
    >>= fun tp ->
    Quotient_type_checking.check_program
      (TypeChecker.SimpleTypeChecker.typed_program_get_program tp
      |> Program.fmap_pattern ~f:(fun (t, ()) ->
             ({ t } : Quotient_type_checking.pattern_tag))
      |> Program.fmap_expr ~f:(fun (t, ()) ->
             ({ t } : Quotient_type_checking.expr_tag)))
    |> Result.map_error ~f:(fun err ->
           sprintf "Quotient type checking error: %s"
             (err |> Quotient_type_checking.sexp_of_quotient_typing_error
            |> Sexp.to_string_hum))
    >>= fun () ->
    Program_executor.SimpleExecutor.execute_program tp
    |> Result.map_error ~f:(fun err ->
           sprintf "Execution error: %s" (Program_executor.print_exec_err err))
    >>| fun v -> Program_executor.sexp_of_value v |> Sexp.to_string_hum
  in
  match res with
  | Ok ok_msg -> printf "%s\n" ok_msg
  | Error err_msg -> eprintf "%s\n" err_msg
