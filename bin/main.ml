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
      let type_ctx =
        SetTypingTypeContext.create
          ~custom_types:
            (List.filter_map
               ~f:(function CustomType ct -> Some ct | _ -> None)
               prog.type_defns)
      in
      match type_expr ~type_ctx prog.e with
      | Ok tpe ->
          let result = Ast_executor.SimpleExecutor.execute_program tpe in
          printf "%s\n" (Ast_executor.show_exec_res result)
      | Error _ -> eprintf "Typing error\n")
