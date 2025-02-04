open Core
open Pq_lang

let () =
  let open Frontend in
  match run_frontend_channel In_channel.stdin with
  | Error err -> (
      match err with
      | LexingError c -> eprintf "Lexing error: %c\n" c
      | ParsingError -> eprintf "Parsing error\n")
  | Ok prog -> (
      let open Typing in
      match type_program prog with
      | Error err -> eprintf "Typing error: %s\n" (print_typing_error err)
      | Ok tpe ->
          let result = Ast_executor.SimpleExecutor.execute_program tpe in
          printf "%s\n" (Ast_executor.show_exec_res result))
