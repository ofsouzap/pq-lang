open Core
open Pq_lang

let () =
  let open Frontend in
  match run_frontend_channel In_channel.stdin with
  | Error err -> (
      match err with
      | LexingError c -> eprintf "Lexing error: %c\n" c
      | ParsingError -> eprintf "Parsing error\n")
  | Ok _ -> failwith "TODO"
