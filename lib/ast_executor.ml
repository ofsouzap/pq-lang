(* TODO - have backends as a module thing, so that they can more easily be swapped in *)

type value = Int of int | Bool of bool
type varname = string

module Varname = struct
  type t = varname

  let compare = String.compare
end

module VarnameMap = Map.Make (Varname)

type store = value VarnameMap.t

let store_empty : store = VarnameMap.empty
let store_get = VarnameMap.find_opt
let store_set = VarnameMap.add
let store_compare = VarnameMap.equal ( = )
let store_traverse = VarnameMap.to_list

type exec_err = TypingError | UndefinedVarError of string
type exec_res = Res of value | Err of exec_err

let exec_res_compare a b =
  match (a, b) with
  | Res v1, Res v2 -> v1 = v2
  | Err e1, Err e2 -> (
      match (e1, e2) with
      | TypingError, TypingError -> true
      | UndefinedVarError x, UndefinedVarError y -> x = y
      | _ -> false)
  | _ -> false

let show_value = function
  | Int i -> "Int " ^ string_of_int i
  | Bool b -> "Bool " ^ string_of_bool b

let show_exec_res = function
  | Res v -> show_value v
  | Err e -> (
      match e with
      | TypingError -> "[TYPING ERROR]"
      | UndefinedVarError x -> "[UNDEFINED VAR: " ^ x ^ "]")

let ( >>= ) (x : exec_res) (f : value -> exec_res) : exec_res =
  match x with Res v -> f v | _ -> x

let check_value_vtype (v : value) (vtype : Ast.vtype) : bool =
  match (v, vtype) with
  | Int _, VTypeInt -> true
  | Bool _, VTypeBool -> true
  | _, _ -> false

(** Apply a function to the execution value if it is an integer, otherwise return a typing error *)
let apply_to_int (cnt : int -> exec_res) (x : value) : exec_res =
  match x with Int i -> cnt i | _ -> Err TypingError

(** Apply a function to the execution value if it is a boolean, otherwise return a typing error *)
let apply_to_bool (cnt : bool -> exec_res) (x : value) : exec_res =
  match x with Bool b -> cnt b | _ -> Err TypingError

(** Evaluate a subexpression, then apply a continuation function to the result if it an integer and give a typing error otherwise *)
let rec eval_apply_to_int (store : store) (x : Ast.expr) (cnt : int -> exec_res)
    : exec_res =
  eval store x >>= fun v -> apply_to_int cnt v

(** Evaluate a subexpression, then apply a continuation function to the result if it an boolean and give a typing error otherwise *)
and eval_apply_to_bool (store : store) (x : Ast.expr) (cnt : bool -> exec_res) :
    exec_res =
  eval store x >>= fun v -> apply_to_bool cnt v

(** Evaluate an AST subtree *)
and eval (store : store) (e : Ast.expr) : exec_res =
  match e with
  | IntLit i -> Res (Int i)
  | Add (e1, e2) ->
      eval_apply_to_int store e1 (fun i1 ->
          eval_apply_to_int store e2 (fun i2 -> Res (Int (i1 + i2))))
  | Neg e -> eval_apply_to_int store e (fun i -> Res (Int (-i)))
  | Subtr (e1, e2) ->
      eval_apply_to_int store e1 (fun i1 ->
          eval_apply_to_int store e2 (fun i2 -> Res (Int (i1 - i2))))
  | Mult (e1, e2) ->
      eval_apply_to_int store e1 (fun i1 ->
          eval_apply_to_int store e2 (fun i2 -> Res (Int (i1 * i2))))
  | BoolLit b -> Res (Bool b)
  | BNot e -> eval_apply_to_bool store e (fun b -> Res (Bool (not b)))
  | BOr (e1, e2) ->
      eval_apply_to_bool store e1 (fun b1 ->
          eval_apply_to_bool store e2 (fun b2 -> Res (Bool (b1 || b2))))
  | BAnd (e1, e2) ->
      eval_apply_to_bool store e1 (fun b1 ->
          eval_apply_to_bool store e2 (fun b2 -> Res (Bool (b1 && b2))))
  | Eq (e1, e2) -> (
      eval store e1 >>= fun v1 ->
      eval store e2 >>= fun v2 ->
      match (v1, v2) with
      | Int i1, Int i2 -> Res (Bool (i1 = i2))
      | Bool b1, Bool b2 -> Res (Bool (b1 = b2))
      | _ -> Err TypingError)
  | Gt (e1, e2) ->
      eval_apply_to_int store e1 (fun i1 ->
          eval_apply_to_int store e2 (fun i2 -> Res (Bool (i1 > i2))))
  | GtEq (e1, e2) ->
      eval_apply_to_int store e1 (fun i1 ->
          eval_apply_to_int store e2 (fun i2 -> Res (Bool (i1 >= i2))))
  | Lt (e1, e2) ->
      eval_apply_to_int store e1 (fun i1 ->
          eval_apply_to_int store e2 (fun i2 -> Res (Bool (i1 < i2))))
  | LtEq (e1, e2) ->
      eval_apply_to_int store e1 (fun i1 ->
          eval_apply_to_int store e2 (fun i2 -> Res (Bool (i1 <= i2))))
  | If (e_cond, e_then, e_else) ->
      eval_apply_to_bool store e_cond (fun b ->
          let next_e = if b then e_then else e_else in
          eval store next_e)
  | Var x -> (
      match store_get x store with
      | Some v -> Res v
      | None -> Err (UndefinedVarError x))
  | Let ((xname, xtype), e1, e2) ->
      eval store e1 >>= fun v ->
      if check_value_vtype v xtype then eval (store_set xname v store) e2
      else Err TypingError

let execute = eval store_empty
