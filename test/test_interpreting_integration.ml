open Core
open OUnit2
open Pq_lang
open Ast_executor
open Utils

let create_test ((name : string), (inp : string), (exp : exec_res)) =
  name >:: fun _ ->
  let lexbuf = Lexing.from_string inp in
  let ast = Parser.prog Lexer.token lexbuf in
  let typed_e =
    match Typing.type_expr ast with
    | Ok x -> x
    | Error _ -> failwith "Failed to type expression"
  in
  let result = Ast_executor.execute typed_e in
  assert_equal exp result ~cmp:override_compare_exec_res
    ~printer:Ast_executor.show_exec_res

let suite =
  "Lexer-Parser-AST Executor"
  >::: List.map ~f:create_test
         [
           ( "Program 1a",
             "(fun (b : bool) -> fun (x : int) -> fun (y : int) -> if b then x \
              else y end end end end) true 1 2",
             Res (Int 1) );
           ( "Program 1b",
             "(fun (b : bool) -> fun (x : int) -> fun (y : int) -> if b then x \
              else y end end end end) false 1 2",
             Res (Int 2) );
           ( "Program Triangles-a",
             "let rec (f : int -> int) = fun (x : int) -> if x == 0 then 0 \
              else x + f (x - 1) end end in f 0 end",
             Res (Int 0) );
           ( "Program Triangles-b",
             "let rec (f : int -> int) = fun (x : int) -> if x == 0 then 0 \
              else x + f (x - 1) end end in f 5 end",
             Res (Int 15) );
           ( "Program Pairs0",
             "let x = 4 in (x + 1, if false then x else x * 2 end) end",
             Res (Pair (Int 5, Int 8)) );
         ]
