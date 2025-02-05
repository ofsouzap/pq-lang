open Core
open OUnit2
open Pq_lang
open Ast_executor
open Testing_utils

let program_triangles (x : int) =
  sprintf
    {|
let rec f (x : int) : int =
  if x == 0
  then
    0
  else
    x + f (x - 1)
  end
end

f %d
|}
    x

let program_pred_or_zero (x : int) =
  sprintf
    {|
let rec f (p : int * (int * int)) : int =
  match p with
  | ((x : int), ((acc : int), (pred : int))) ->
    if x == acc
    then
      pred
    else
      f (x, (acc + 1, acc))
    end
  end
end

let predOrZero (x : int) : int =
  f (x, (0, 0))
end

predOrZero %d
|}
    x

let create_test ((name : string), (inp : string), (exp : exec_res)) : test =
  name >:: fun _ ->
  let open Result in
  match Frontend.run_frontend_string inp with
  | Ok prog -> (
      match Typing.type_program prog with
      | Ok typed_prog ->
          let result : Ast_executor.exec_res =
            Ast_executor.SimpleExecutor.execute_program typed_prog
          in
          assert_equal ~cmp:override_equal_exec_res
            ~printer:Ast_executor.show_exec_res exp result
      | Error err ->
          failwith
            (sprintf "Error in typing: %s" (Typing.print_typing_error err)))
  | Error err ->
      failwith
        (sprintf "Error in frontend: %s"
           (err |> Frontend.sexp_of_frontend_error |> Sexp.to_string_hum))

let suite =
  "Interpreting Integration Tests"
  >::: List.map ~f:create_test
         [
           ("Program Triangles-a", program_triangles 0, Ok (Int 0));
           ("Program Triangles-b", program_triangles 5, Ok (Int 15));
           ( "Program Pairs0",
             "let x = 4 in (x + 1, if false then x else x * 2 end) end",
             Ok (Pair (Int 5, Int 8)) );
           ( "Program Match Pairs",
             "let p = (true, 1) in match p with | ((b : bool), (x : int)) -> \
              if b then x else 0 end end end",
             Ok (Int 1) );
           ("Program Pred-or-Zero-a", program_pred_or_zero 0, Ok (Int 0));
           ("Program Pred-or-Zero-a", program_pred_or_zero 5, Ok (Int 4));
           ( "Program int list sum",
             {|
type int_list =
  | Nil of unit
  | Cons of (int * int_list)

let rec sum_int_list (xs : int_list) : int =
  match xs with
  | Nil (x : unit) -> 0
  | Cons ((h : int), (ts : int_list)) -> h + sum_int_list ts
  end
end

sum_int_list (Cons (1, Cons (2, Cons (3, Cons (4, Nil ())))))
|},
             Ok (Int 10) );
           ( "Program very recursive variant data type",
             {|
type my_type =
  | A of my_type
  | B of (my_type * int)
  | C of (int * my_type)
  | D of unit

let x1 =
  A (A (B (D (), 5)))
in
  let x2 =
    C (3, A (A (D ())))
  in
    ()
  end
end
|},
             Ok Unit );
           ( "Program using first-class functions",
             {|
let f (x : int) : int =
  x + 1
end

let apply (p : ((int -> int) * int)) : int =
  match p with
  | ((f : int -> int), (x : int)) -> f x
  end
end

apply (f, 5)
|},
             Ok (Int 6) );
         ]
