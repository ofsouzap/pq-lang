(* TODO - have backends as a module thing, so that they can more easily be swapped in *)
(* TODO - once language is improved beyond arithmetic evaluation, will require signature changes *)

let execute (ast : Ast.expr) : int =
  let open Ast in
  let rec eval = function
    | IntLit i -> i
    | Add (e1, e2) -> (eval e1) + (eval e2)
    | Mult (e1, e2) -> (eval e1) * (eval e2)
  in
  eval ast
