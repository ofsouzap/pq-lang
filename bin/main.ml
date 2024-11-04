open Core
open Pq_lang

let () =
  let open Frontend in
  match run_frontend_channel In_channel.stdin with
  | LexingError c -> Printf.printf "Lexing error: %c\n" c
  | ParsingError -> Printf.printf "Parsing error\n"
  | Res ast ->
      let result = Ast_executor.execute ast in
      Printf.printf "%s\n" (Ast_executor.show_exec_res result)
