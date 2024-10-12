open Pq_lang

let () =
  let lexbuf = Lexing.from_channel stdin in
  let ast = Parser.prog Lexer.token lexbuf in
  let result = Ast_executor.execute ast in
  Printf.printf "%s\n" (Ast_executor.show_exec_res result)
