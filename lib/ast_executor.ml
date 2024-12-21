open Core
open Vtype

(* TODO - have backends as a module thing, so that they can more easily be swapped in *)

type ast_tag = unit [@@deriving sexp, equal]
type varname = string [@@deriving sexp, equal]

module Varname = String
module VarnameMap = Map.Make_using_comparator (Varname)

type closure_props = varname * ast_tag Ast.typed_expr * store
[@@deriving sexp, equal]

and value =
  | Int of int
  | Bool of bool
  | Closure of closure_props
  | Pair of value * value
[@@deriving sexp, equal]

and store = value VarnameMap.t [@@deriving sexp, equal]

let empty_store = (VarnameMap.empty : store)
let store_get store key = Map.find store key
let store_set store ~key ~value = Map.set store ~key ~data:value
let store_compare = equal_store
let store_traverse = Map.to_alist ~key_order:`Increasing

type typing_error = {
  expected_type : string option;
  actual_type : string option;
  variable_name : varname option;
  custom_message : string option;
}

let empty_typing_error =
  {
    expected_type = None;
    actual_type = None;
    variable_name = None;
    custom_message = None;
  }

let show_typing_error (terr : typing_error) : string =
  (terr.variable_name
  |> Option.value_map
       ~f:(fun vname -> [ "Variable name: " ^ vname ])
       ~default:[])
  @ (terr.expected_type
    |> Option.value_map
         ~f:(fun vname -> [ "Expected type: " ^ vname ])
         ~default:[])
  @ (terr.actual_type
    |> Option.value_map
         ~f:(fun vname -> [ "Actual type: " ^ vname ])
         ~default:[])
  @ (terr.custom_message
    |> Option.value_map ~f:(fun vname -> [ vname ]) ~default:[])
  |> String.concat ~sep:", "

let typing_error_compare (a : typing_error) (b : typing_error) : bool =
  equal_option equal_string a.expected_type b.expected_type
  && equal_option equal_string a.actual_type b.actual_type
  && equal_option equal_string a.variable_name b.variable_name

type exec_err =
  | TypingError of typing_error
  | UndefinedVarError of varname
  | MisplacedFixError
  | FixApplicationError
  | MaxRecursionDepthExceeded

type exec_res = (value, exec_err) Result.t

let equal_exec_res a b =
  match (a, b) with
  | Ok v1, Ok v2 -> equal_value v1 v2
  | Error e1, Error e2 -> (
      match (e1, e2) with
      | TypingError terr1, TypingError terr2 -> typing_error_compare terr1 terr2
      | TypingError _, _ -> false
      | UndefinedVarError x, UndefinedVarError y -> equal_varname x y
      | UndefinedVarError _, _ -> false
      | MisplacedFixError, MisplacedFixError -> true
      | MisplacedFixError, _ -> false
      | FixApplicationError, FixApplicationError -> true
      | FixApplicationError, _ -> false
      | MaxRecursionDepthExceeded, MaxRecursionDepthExceeded -> true
      | MaxRecursionDepthExceeded, _ -> false)
  | _ -> false

let show_exec_res = function
  | Ok v -> sexp_of_value v |> Sexp.to_string
  | Error e -> (
      match e with
      | TypingError terr -> "[TYPING ERROR: " ^ show_typing_error terr ^ "]"
      | UndefinedVarError x -> "[UNDEFINED VAR: " ^ x ^ "]"
      | MisplacedFixError -> "[MISPLACED FIX NODE]"
      | FixApplicationError -> "[Fix APPLICATION ERROR]"
      | MaxRecursionDepthExceeded -> "[MAXIMUM RECURSION DEPTH EXCEEDED]")

(** Apply a function to the execution value if it is an integer, otherwise return a typing error *)
let apply_to_int (cnt : int -> exec_res) (x : value) : exec_res =
  match x with
  | Int i -> cnt i
  | _ ->
      Error (TypingError { empty_typing_error with expected_type = Some "Int" })

(** Apply a function to the execution value if it is a boolean, otherwise return a typing error *)
let apply_to_bool (cnt : bool -> exec_res) (x : value) : exec_res =
  match x with
  | Bool b -> cnt b
  | _ ->
      Error
        (TypingError { empty_typing_error with expected_type = Some "Bool" })

(** Apply a function to the execution value if it is a function, otherwise return a typing error *)
let apply_to_closure (cnt : closure_props -> exec_res) (x : value) : exec_res =
  match x with
  | Closure x -> cnt x
  | _ ->
      Error
        (TypingError { empty_typing_error with expected_type = Some "Closure" })

(** Evaluate a subexpression, then apply a continuation function to the result if it an integer and give a typing error otherwise *)
let rec eval_apply_to_int (store : store) (x : ast_tag Ast.typed_expr)
    (cnt : int -> exec_res) : exec_res =
  let open Result in
  eval store x >>= apply_to_int cnt

(** Evaluate a subexpression, then apply a continuation function to the result if it an boolean and give a typing error otherwise *)
and eval_apply_to_bool (store : store) (x : ast_tag Ast.typed_expr)
    (cnt : bool -> exec_res) : exec_res =
  let open Result in
  eval store x >>= apply_to_bool cnt

(** Evaluate a subexpression, then apply a continuation function to the result if it an function and give a typing error otherwise *)
and eval_apply_to_closure (store : store) (x : ast_tag Ast.typed_expr)
    (cnt : closure_props -> exec_res) : exec_res =
  let open Result in
  eval store x >>= apply_to_closure cnt

(** Evaluate an AST subtree *)
and eval (store : store) (e : ast_tag Ast.typed_expr) : exec_res =
  let open Result in
  match e with
  | IntLit (_, i) -> Ok (Int i)
  | Add (_, e1, e2) ->
      eval_apply_to_int store e1 (fun i1 ->
          eval_apply_to_int store e2 (fun i2 -> Ok (Int (i1 + i2))))
  | Neg (_, e) -> eval_apply_to_int store e (fun i -> Ok (Int (-i)))
  | Subtr (_, e1, e2) ->
      eval_apply_to_int store e1 (fun i1 ->
          eval_apply_to_int store e2 (fun i2 -> Ok (Int (i1 - i2))))
  | Mult (_, e1, e2) ->
      eval_apply_to_int store e1 (fun i1 ->
          eval_apply_to_int store e2 (fun i2 -> Ok (Int (i1 * i2))))
  | BoolLit (_, b) -> Ok (Bool b)
  | BNot (_, e) -> eval_apply_to_bool store e (fun b -> Ok (Bool (not b)))
  | BOr (_, e1, e2) ->
      eval_apply_to_bool store e1 (fun b1 ->
          eval_apply_to_bool store e2 (fun b2 -> Ok (Bool (b1 || b2))))
  | BAnd (_, e1, e2) ->
      eval_apply_to_bool store e1 (fun b1 ->
          eval_apply_to_bool store e2 (fun b2 -> Ok (Bool (b1 && b2))))
  | Pair (_, e1, e2) ->
      eval store e1 >>= fun v1 ->
      eval store e2 >>= fun v2 -> Ok (Pair (v1, v2))
  | Eq (_, e1, e2) -> (
      eval store e1 >>= fun v1 ->
      eval store e2 >>= fun v2 ->
      match (v1, v2) with
      | Int i1, Int i2 -> Ok (Bool (i1 = i2))
      | Bool b1, Bool b2 -> Ok (Bool (equal_bool b1 b2))
      | _ ->
          Error
            (TypingError
               {
                 empty_typing_error with
                 custom_message =
                   Some
                     "Left and right sides of equality must be of the same, \
                      compatible type";
               }))
  | Gt (_, e1, e2) ->
      eval_apply_to_int store e1 (fun i1 ->
          eval_apply_to_int store e2 (fun i2 -> Ok (Bool (i1 > i2))))
  | GtEq (_, e1, e2) ->
      eval_apply_to_int store e1 (fun i1 ->
          eval_apply_to_int store e2 (fun i2 -> Ok (Bool (i1 >= i2))))
  | Lt (_, e1, e2) ->
      eval_apply_to_int store e1 (fun i1 ->
          eval_apply_to_int store e2 (fun i2 -> Ok (Bool (i1 < i2))))
  | LtEq (_, e1, e2) ->
      eval_apply_to_int store e1 (fun i1 ->
          eval_apply_to_int store e2 (fun i2 -> Ok (Bool (i1 <= i2))))
  | If (_, e_cond, e_then, e_else) ->
      eval_apply_to_bool store e_cond (fun b ->
          let next_e = if b then e_then else e_else in
          eval store next_e)
  | Var (_, x) ->
      store_get store x
      |> Option.value_map ~default:(Error (UndefinedVarError x)) ~f:(fun v ->
             Ok v)
  | Let (_, xname, e1, e2) ->
      eval store e1 >>= fun v -> eval (store_set store ~key:xname ~value:v) e2
  | Fun (_, (xname, _), e) -> Ok (Closure (xname, e, store))
  | App (_, e1, e2) ->
      (* This uses call-by-value semantics *)
      eval_apply_to_closure store e1 (fun (argname, fe, fs) ->
          eval store e2 >>= fun v2 ->
          eval (store_set fs ~key:argname ~value:v2) fe)
  | Fix (_, (fname, ftype1, ftype2), ((xname, xtype) as x), fxbody) as e ->
      (* fix (\f. \x. e2) ~> \x. [(\f. \x. e2) (fix (\f. \x. e2))] x *)
      let ftype = VTypeFun (ftype1, ftype2) in
      eval store
        (Fun
           ( (ftype, ()),
             x,
             App
               ( (ftype2, ()),
                 App
                   ( (ftype, ()),
                     Fun
                       ( (VTypeFun (ftype, ftype), ()),
                         (fname, ftype),
                         Fun ((ftype, ()), x, fxbody) ),
                     e ),
                 Var ((xtype, ()), xname) ) ))
  | Match _ -> failwith "TODO"

let execute (e : 'a Ast.typed_expr) =
  eval (VarnameMap.empty : store) (Ast.fmap ~f:(fun (t, _) -> (t, ())) e)
