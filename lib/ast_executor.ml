(* TODO - have backends as a module thing, so that they can more easily be swapped in *)
(* TODO - once language is improved beyond arithmetic evaluation, will require signature changes *)

type value = Int of int | Bool of bool
type exec_res = Value of value | TypingError

let show_value = function
  | Int i -> string_of_int i
  | Bool b -> string_of_bool b

let show_exec_res = function
  | Value v -> show_value v
  | TypingError -> "[TYPING ERROR]"

(** Apply a function to the execution value if it is an integer, otherwise return a typing error *)
let apply_to_int (cnt : int -> exec_res) (x : value) : exec_res =
  match x with Int i -> cnt i | _ -> TypingError

(** Apply a function to the execution value if it is a boolean, otherwise return a typing error *)
let apply_to_bool (cnt : bool -> exec_res) (x : value) : exec_res =
  match x with Bool i -> cnt i | _ -> TypingError

(** Similar to the Haskell bind operator *)
let ( >>= ) (x : exec_res) (f : value -> exec_res) : exec_res =
  match x with Value v -> f v | _ -> x

(** Evaluate a subexpression and then handle it if it successfully evaluates and evaluates to an integer *)
let rec eval_apply_to_int (x : Ast.expr) (cnt : int -> exec_res) : exec_res =
  eval x >>= apply_to_int cnt

(** Evaluate a subexpression and then handle it if it successfully evaluates and evaluates to an boolean *)
and eval_apply_to_bool (x : Ast.expr) (cnt : bool -> exec_res) : exec_res =
  eval x >>= apply_to_bool cnt

(** Evaluate an AST subtree *)
and eval (e : Ast.expr) : exec_res =
  match e with
  | IntLit i -> Value (Int i)
  | Add (e1, e2) ->
      eval_apply_to_int e1 (fun i1 ->
          eval_apply_to_int e2 (fun i2 -> Value (Int (i1 + i2))))
  | Subtr (e1, e2) ->
      eval_apply_to_int e1 (fun i1 ->
          eval_apply_to_int e2 (fun i2 -> Value (Int (i1 - i2))))
  | Mult (e1, e2) ->
      eval_apply_to_int e1 (fun i1 ->
          eval_apply_to_int e2 (fun i2 -> Value (Int (i1 * i2))))
  | BoolLit b -> Value (Bool b)
  | BNot e -> eval_apply_to_bool e (fun b -> Value (Bool (not b)))
  | BOr (e1, e2) ->
      eval_apply_to_bool e1 (fun b1 ->
          eval_apply_to_bool e2 (fun b2 -> Value (Bool (b1 || b2))))
  | BAnd (e1, e2) ->
      eval_apply_to_bool e1 (fun b1 ->
          eval_apply_to_bool e2 (fun b2 -> Value (Bool (b1 && b2))))
  | Eq (e1, e2) -> (
      eval e1 >>= fun v1 ->
      eval e2 >>= fun v2 ->
      match (v1, v2) with
      | Int i1, Int i2 -> Value (Bool (i1 = i2))
      | Bool b1, Bool b2 -> Value (Bool (b1 = b2))
      | _ -> TypingError)
  | Gt (e1, e2) ->
      eval_apply_to_int e1 (fun i1 ->
          eval_apply_to_int e2 (fun i2 -> Value (Bool (i1 > i2))))
  | GtEq (e1, e2) ->
      eval_apply_to_int e1 (fun i1 ->
          eval_apply_to_int e2 (fun i2 -> Value (Bool (i1 >= i2))))
  | Lt (e1, e2) ->
      eval_apply_to_int e1 (fun i1 ->
          eval_apply_to_int e2 (fun i2 -> Value (Bool (i1 < i2))))
  | LtEq (e1, e2) ->
      eval_apply_to_int e1 (fun i1 ->
          eval_apply_to_int e2 (fun i2 -> Value (Bool (i1 <= i2))))
  | If (e_cond, e_then, e_else) ->
      eval_apply_to_bool e_cond (fun b ->
          if b then eval e_then else eval e_else)

let execute = eval
