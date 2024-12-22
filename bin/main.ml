open Core
open Pq_lang

let () =
  let open Frontend in
  match run_frontend_channel In_channel.stdin with
  | Error err -> (
      match err with
      | LexingError c -> eprintf "Lexing error: %c\n" c
      | ParsingError -> eprintf "Parsing error\n")
  | Ok _ -> (
      let ast =
        failwith "TODO"
        (* TODO - this will need to be changed once typing uses the custom types and execution too *)
      in
      match Typing.type_expr ast with
      | Ok typed_e ->
          let result = Ast_executor.execute typed_e in
          printf "%s\n" (Ast_executor.show_exec_res result)
      | Error _ -> eprintf "Typing error\n")
