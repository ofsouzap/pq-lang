(* TODO - have backends as a module thing, so that they can more easily be swapped in *)

type varname = string

module Varname = struct
  type t = varname

  let compare = String.compare
end

module VarnameMap = Map.Make (Varname)

type closure_props = varname * Ast.vtype * Ast.expr * store
and value = Int of int | Bool of bool | Closure of closure_props
and store = value VarnameMap.t

let store_empty : store = VarnameMap.empty
let store_get = VarnameMap.find_opt
let store_set = VarnameMap.add
let store_compare = VarnameMap.equal ( = )
let store_traverse = VarnameMap.to_list

type typing_error = {
  expected_type : (Ast.vtype, string) Either.t option;
  actual_type : (Ast.vtype, string) Either.t option;
  variable_name : varname option;
  custom_message : string option;
}

let typing_error_show_type = function
  | Either.Left t -> Ast.show_vtype t
  | Either.Right s -> s

let empty_typing_error =
  {
    expected_type = None;
    actual_type = None;
    variable_name = None;
    custom_message = None;
  }

let show_typing_error (terr : typing_error) : string =
  String.concat ", "
    ((match terr.variable_name with
     | Some vname -> [ "Variable name: " ^ vname ]
     | None -> [])
    @ (match terr.expected_type with
      | Some t -> [ "Expected type: " ^ typing_error_show_type t ]
      | None -> [])
    @ (match terr.actual_type with
      | Some t -> [ "Actual type: " ^ typing_error_show_type t ]
      | None -> [])
    @ (match terr.custom_message with Some msg -> [ msg ] | None -> [])
    @ [])

let typing_error_compare (a : typing_error) (b : typing_error) : bool =
  a.expected_type = b.expected_type
  && a.actual_type = b.actual_type
  && a.variable_name = b.variable_name

type exec_err = TypingError of typing_error | UndefinedVarError of string
type exec_res = Res of value | Err of exec_err

let exec_res_compare a b =
  match (a, b) with
  | Res v1, Res v2 -> v1 = v2
  | Err e1, Err e2 -> (
      match (e1, e2) with
      | TypingError terr1, TypingError terr2 -> typing_error_compare terr1 terr2
      | UndefinedVarError x, UndefinedVarError y -> x = y
      | _ -> false)
  | _ -> false

let rec show_store store =
  "["
  ^ String.concat ", "
      (List.map
         (fun (k, v) -> Printf.sprintf "(%s, %s)" k (show_value v))
         (store_traverse store))
  ^ "]"

and show_value = function
  | Int i -> "Int " ^ string_of_int i
  | Bool b -> "Bool " ^ string_of_bool b
  | Closure (xname, xtype, e, store) ->
      Printf.sprintf "Closure (%s : %s, %s, %s)" xname (Ast.show_vtype xtype)
        (Ast.show e) (show_store store)

let show_exec_res = function
  | Res v -> show_value v
  | Err e -> (
      match e with
      | TypingError terr -> "[TYPING ERROR: " ^ show_typing_error terr ^ "]"
      | UndefinedVarError x -> "[UNDEFINED VAR: " ^ x ^ "]")

let ( >>= ) (x : exec_res) (f : value -> exec_res) : exec_res =
  match x with Res v -> f v | _ -> x

(** Apply a function to the execution value if it is an integer, otherwise return a typing error *)
let apply_to_int (cnt : int -> exec_res) (x : value) : exec_res =
  match x with
  | Int i -> cnt i
  | _ ->
      Err
        (TypingError
           { empty_typing_error with expected_type = Some (Left VTypeInt) })

(** Apply a function to the execution value if it is a boolean, otherwise return a typing error *)
let apply_to_bool (cnt : bool -> exec_res) (x : value) : exec_res =
  match x with
  | Bool b -> cnt b
  | _ ->
      Err
        (TypingError
           { empty_typing_error with expected_type = Some (Left VTypeBool) })

(** Apply a function to the execution value if it is a function, otherwise return a typing error *)
let apply_to_closure (cnt : closure_props -> exec_res) (x : value) : exec_res =
  match x with
  | Closure x -> cnt x
  | _ ->
      Err
        (TypingError
           { empty_typing_error with expected_type = Some (Right "Closure") })

(** Evaluate a subexpression, then apply a continuation function to the result if it an integer and give a typing error otherwise *)
let rec eval_apply_to_int (store : store) (x : Ast.expr) (cnt : int -> exec_res)
    : exec_res =
  eval store x >>= apply_to_int cnt

(** Evaluate a subexpression, then apply a continuation function to the result if it an boolean and give a typing error otherwise *)
and eval_apply_to_bool (store : store) (x : Ast.expr) (cnt : bool -> exec_res) :
    exec_res =
  eval store x >>= apply_to_bool cnt

(** Evaluate a subexpression, then apply a continuation function to the result if it an function and give a typing error otherwise *)
and eval_apply_to_closure (store : store) (x : Ast.expr)
    (cnt : closure_props -> exec_res) : exec_res =
  eval store x >>= apply_to_closure cnt

(* TODO - allow recursion in functions *)

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
      | _ ->
          Err
            (TypingError
               {
                 empty_typing_error with
                 custom_message =
                   Some
                     "Left and right sides of equality must be of the same, \
                      compatible type";
               }))
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
  | Let ((xname, _), e1, e2) ->
      eval store e1 >>= fun v -> eval (store_set xname v store) e2
  | Fun ((xname, xtype), e) -> Res (Closure (xname, xtype, e, store))
  | App (e1, e2) ->
      (* This uses call-by-value semantics *)
      eval_apply_to_closure store e1 (fun (argname, _, fe, fs) ->
          eval store e2 >>= fun v2 -> eval (store_set argname v2 fs) fe)

let execute = eval store_empty
