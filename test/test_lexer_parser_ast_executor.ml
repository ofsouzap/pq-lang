open OUnit2
open Pq_lang
open Ast_executor
open Test_utils

let create_test ((name : string), (inp : string), (exp : exec_res)) =
  name >:: fun _ ->
  let lexbuf = Lexing.from_string inp in
  let ast = Parser.prog Lexer.token lexbuf in
  let result = Ast_executor.execute ast in
  assert_equal exp result ~cmp:override_compare_exec_res
    ~printer:Ast_executor.show_exec_res

let suite =
  "Lexer-Parser-AST Executor"
  >::: List.map create_test
         [
           ( "Program 1a",
             "(((fun (b : bool) -> (fun (x : int) -> fun (y : int) -> if b \
              then x else y end )) true) 1) 2",
             Res (Int 1) );
           ( "Program 1b",
             "(((fun (b : bool) -> (fun (x : int) -> fun (y : int) -> if b \
              then x else y end )) false) 1) 2",
             Res (Int 2) );
         ]
