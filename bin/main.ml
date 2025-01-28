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
      match SetTypingTypeContext.create ~custom_types:prog.custom_types with
      | Error err -> eprintf "Typing error: %s\n" (print_typing_error err)
      | Ok type_ctx -> (
          match type_expr ~type_ctx prog.e with
          | Error err -> eprintf "Typing error: %s\n" (print_typing_error err)
          | Ok tpe ->
              let result = Ast_executor.SimpleExecutor.execute_program tpe in
              printf "%s\n" (Ast_executor.SimpleExecutor.show_exec_res result)))
