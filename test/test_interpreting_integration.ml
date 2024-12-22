open Core
open OUnit2
open Pq_lang
open Ast_executor
open Testing_utils

let program_pred_or_zero (x : int) =
  sprintf
    {|
let rec (f : (int * (int * int)) -> int) =
  fun (p : (int * (int * int))) ->
    match p with
    | ((x : int), ((acc : int), (pred : int))) ->
      if
        x == acc
        then
          pred
        else
          f (x, (acc + 1, acc))
      end
    end
  end
in
  let predOrZero =
    fun (x : int) ->
      f (x, (0, 0))
    end
  in
    predOrZero %d
  end
end
|}
    x

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
  assert_equal exp result ~cmp:override_equal_exec_res
    ~printer:Ast_executor.show_exec_res

let suite =
  "Lexer-Parser-AST Executor"
  >::: List.map ~f:create_test
         [
           ( "Program 1a",
             "(fun (b : bool) -> fun (x : int) -> fun (y : int) -> if b then x \
              else y end end end end) true 1 2",
             Ok (Int 1) );
           ( "Program 1b",
             "(fun (b : bool) -> fun (x : int) -> fun (y : int) -> if b then x \
              else y end end end end) false 1 2",
             Ok (Int 2) );
           ( "Program Triangles-a",
             "let rec (f : int -> int) = fun (x : int) -> if x == 0 then 0 \
              else x + f (x - 1) end end in f 0 end",
             Ok (Int 0) );
           ( "Program Triangles-b",
             "let rec (f : int -> int) = fun (x : int) -> if x == 0 then 0 \
              else x + f (x - 1) end end in f 5 end",
             Ok (Int 15) );
           ( "Program Pairs0",
             "let x = 4 in (x + 1, if false then x else x * 2 end) end",
             Ok (Pair (Int 5, Int 8)) );
           ( "Program Match Pairs",
             "let p = (true, 1) in match p with | ((b : bool), (x : int)) -> \
              if b then x else 0 end end end",
             Ok (Int 1) );
           ("Program Pred-or-Zero-a", program_pred_or_zero 0, Ok (Int 0));
           ("Program Pred-or-Zero-a", program_pred_or_zero 5, Ok (Int 4));
         ]

(* TODO - custom type definitions *)
