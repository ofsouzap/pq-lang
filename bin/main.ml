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
    Typing.type_program prog
    |> Result.map_error ~f:(fun err ->
           sprintf "Typing error: %s" (Typing.print_typing_error err))
    (* TODO - quotient type checking *)
    >>= fun tp ->
    Ast_executor.SimpleExecutor.execute_program tp
    |> Result.map_error ~f:(fun err ->
           sprintf "Execution error: %s" (Ast_executor.print_exec_err err))
    >>| fun v -> Ast_executor.sexp_of_value v |> Sexp.to_string_hum
  in
  match res with
  | Ok ok_msg -> printf "%s\n" ok_msg
  | Error err_msg -> eprintf "%s\n" err_msg
