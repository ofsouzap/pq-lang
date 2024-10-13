(* TODO - have backends as a module thing, so that they can more easily be swapped in *)

type value = Int of int | Bool of bool
type varname = string
type store = (string * value) list

(* TODO - change stores to be represented as search trees, indexed by the variable names. Then also can implement equality between stores more easily *)

type exec_res =
  | Res of store * value
  | TypingError
  | UndefinedVarError of string

(* N.B. I am storing the store as a list and I am rewriting values just by overwriting them, not by overwriting their entries.
   Therefore, keys may appear twice, and only the first occurences should be used. *)
let store_empty : store = []

let store_get (vname : varname) (s : store) : value option =
  List.assoc_opt vname s

let store_set (vname : varname) (v : value) (s : store) : store =
  (vname, v) :: s

let show_value = function
  | Int i -> string_of_int i
  | Bool b -> string_of_bool b

let show_exec_res = function
  | Res (_, v) -> show_value v
  | TypingError -> "[TYPING ERROR]"
  | UndefinedVarError x -> "[UNDEFINED VAR: " ^ x ^ "]"

let ( >>= ) (x : exec_res) (f : store * value -> exec_res) : exec_res =
  match x with Res (store, v) -> f (store, v) | _ -> x

(** Apply a function to the execution value if it is an integer, otherwise return a typing error *)
let apply_to_int (cnt : store * int -> exec_res) ((store : store), (x : value))
    : exec_res =
  match x with Int i -> cnt (store, i) | _ -> TypingError

(** Apply a function to the execution value if it is a boolean, otherwise return a typing error *)
let apply_to_bool (cnt : store * bool -> exec_res) ((store : store), (x : value))
    : exec_res =
  match x with Bool b -> cnt (store, b) | _ -> TypingError

(** Evaluate a subexpression, then apply a continuation function to the result if it an integer and give a typing error otherwise *)
let rec eval_apply_to_int (store : store) (x : Ast.expr)
    (cnt : store * int -> exec_res) : exec_res =
  let eval_res = eval store x in
  match eval_res with
  | Res (store', v) -> apply_to_int cnt (store', v)
  | _ -> eval_res

(** Evaluate a subexpression, then apply a continuation function to the result if it an boolean and give a typing error otherwise *)
and eval_apply_to_bool (store : store) (x : Ast.expr)
    (cnt : store * bool -> exec_res) : exec_res =
  let eval_res = eval store x in
  match eval_res with
  | Res (store', v) -> apply_to_bool cnt (store', v)
  | _ -> eval_res

(** Evaluate an AST subtree *)
and eval (store : store) (e : Ast.expr) : exec_res =
  match e with
  | IntLit i -> Res (store, Int i)
  | Add (e1, e2) ->
      eval_apply_to_int store e1 (fun (store', i1) ->
          eval_apply_to_int store' e2 (fun (store'', i2) ->
              Res (store'', Int (i1 + i2))))
  | Subtr (e1, e2) ->
      eval_apply_to_int store e1 (fun (store', i1) ->
          eval_apply_to_int store' e2 (fun (store'', i2) ->
              Res (store'', Int (i1 - i2))))
  | Mult (e1, e2) ->
      eval_apply_to_int store e1 (fun (store', i1) ->
          eval_apply_to_int store' e2 (fun (store'', i2) ->
              Res (store'', Int (i1 * i2))))
  | BoolLit b -> Res (store, Bool b)
  | BNot e ->
      eval_apply_to_bool store e (fun (store', b) -> Res (store', Bool (not b)))
  | BOr (e1, e2) ->
      eval_apply_to_bool store e1 (fun (store', b1) ->
          eval_apply_to_bool store' e2 (fun (store'', b2) ->
              Res (store'', Bool (b1 || b2))))
  | BAnd (e1, e2) ->
      eval_apply_to_bool store e1 (fun (store', b1) ->
          eval_apply_to_bool store' e2 (fun (store'', b2) ->
              Res (store'', Bool (b1 && b2))))
  | Eq (e1, e2) -> (
      eval store e1 >>= fun (store', v1) ->
      eval store' e2 >>= fun (store'', v2) ->
      match (v1, v2) with
      | Int i1, Int i2 -> Res (store'', Bool (i1 = i2))
      | Bool b1, Bool b2 -> Res (store'', Bool (b1 = b2))
      | _ -> TypingError)
  | Gt (e1, e2) ->
      eval_apply_to_int store e1 (fun (store', i1) ->
          eval_apply_to_int store' e2 (fun (store'', i2) ->
              Res (store'', Bool (i1 > i2))))
  | GtEq (e1, e2) ->
      eval_apply_to_int store e1 (fun (store', i1) ->
          eval_apply_to_int store' e2 (fun (store'', i2) ->
              Res (store'', Bool (i1 >= i2))))
  | Lt (e1, e2) ->
      eval_apply_to_int store e1 (fun (store', i1) ->
          eval_apply_to_int store' e2 (fun (store'', i2) ->
              Res (store'', Bool (i1 < i2))))
  | LtEq (e1, e2) ->
      eval_apply_to_int store e1 (fun (store', i1) ->
          eval_apply_to_int store' e2 (fun (store'', i2) ->
              Res (store'', Bool (i1 <= i2))))
  (* TODO - replace the cases below with store functionality stuff *)
  | If (e_cond, e_then, e_else) ->
      eval_apply_to_bool store e_cond (fun (store', b) ->
          let next_e = if b then e_then else e_else in
          eval store' next_e)
  | Var x -> (
      match store_get x store with
      | Some v -> Res (store, v)
      | None -> UndefinedVarError x)
  | Let (x, e1, e2) ->
      eval store e1 >>= fun (store', v) -> eval (store_set x v store') e2

let execute = eval store_empty
