open Pq_lang

let rec eval expr =
  let open Ast in
  match expr with
  | IntLit n -> n
  | Add (e1, e2) -> eval e1 + eval e2
  | Mult (e1, e2) -> eval e1 * eval e2

let () =
  let lexbuf = Lexing.from_channel stdin in
  let ast = Parser.prog Lexer.token lexbuf in
  let result = eval ast in
  Printf.printf "%d\n" result
