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
             "(fun b -> fun x -> fun y -> if b then x else y end end end end) \
              true 1 2",
             Res (Int 1) );
           ( "Program 1b",
             "(fun b -> fun x -> fun y -> if b then x else y end end end end) \
              false 1 2",
             Res (Int 2) );
           ( "Program Triangles-a",
             "let rec f = fun x -> if x == 0 then 0 else x + f (x - 1) end end \
              in f 0 end",
             Res (Int 0) );
           ( "Program Triangles-b",
             "let rec f = fun x -> if x == 0 then 0 else x + f (x - 1) end end \
              in f 5 end",
             Res (Int 15) );
         ]
