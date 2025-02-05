open Core
open Utils
open Vtype
open Variant_types
open Varname
open Pattern
open Program

(* TODO - perhaps re-make this module by just using the partial evaluator *)

type ast_tag = unit [@@deriving sexp, equal]
type pattern_tag = unit [@@deriving sexp, equal]

type closure_props = {
  param : varname * vtype;
  out_type : vtype;
  body : (ast_tag, pattern_tag) Ast.typed_expr;
  store : store;
  recursive : [ `Recursive of varname | `NonRecursive ];
}
[@@deriving sexp, equal]

and value =
  | Unit
  | Int of int
  | Bool of bool
  | Closure of closure_props
  | Pair of value * value
  | VariantTypeValue of variant_type * string * value
[@@deriving sexp, equal]

and store = value VarnameMap.t [@@deriving sexp, equal]

let empty_store = (VarnameMap.empty : store)
let store_get store key = Map.find store key
let store_set store ~key ~value = Map.set store ~key ~data:value
let store_compare = equal_store
let store_traverse = Map.to_alist ~key_order:`Increasing

let rec value_type = function
  | Unit -> VTypeUnit
  | Int _ -> VTypeInt
  | Bool _ -> VTypeBool
  | Closure closure -> VTypeFun (snd closure.param, closure.out_type)
  | Pair (v1, v2) -> VTypePair (value_type v1, value_type v2)
  | VariantTypeValue ((vt_name, _), _, _) -> VTypeCustom vt_name

type typing_error = {
  expected_type : string option;
  actual_type : string option;
  variable_name : varname option;
  custom_message : string option;
}
[@@deriving sexp, equal]

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

type exec_err =
  | TypeContextCreationError of Typing.typing_error
  | TypingError of typing_error
  | UndefinedVarError of varname
  | MisplacedFixError
  | FixApplicationError
  | MaxRecursionDepthExceeded
  | IncompleteMatchError
  | UnknownVariantTypeConstructor of string
[@@deriving sexp, equal]

type exec_res = (value, exec_err) Result.t [@@deriving sexp, equal]

let show_exec_res = function
  | Ok v -> sexp_of_value v |> Sexp.to_string_hum
  | Error e -> (
      match e with
      | TypeContextCreationError terr ->
          "[TYPE CONTEXT CREATION ERROR: "
          ^ Typing.print_typing_error terr
          ^ "]"
      | TypingError terr -> "[TYPING ERROR: " ^ show_typing_error terr ^ "]"
      | UndefinedVarError x -> "[UNDEFINED VAR: " ^ x ^ "]"
      | MisplacedFixError -> "[MISPLACED FIX NODE]"
      | FixApplicationError -> "[Fix APPLICATION ERROR]"
      | MaxRecursionDepthExceeded -> "[MAXIMUM RECURSION DEPTH EXCEEDED]"
      | IncompleteMatchError -> "[INCOMPLETE MATCH]"
      | UnknownVariantTypeConstructor x ->
          "[UNKNOWN VARIANT TYPE CONSTRUCTOR: " ^ x ^ "]")

let rec match_pattern (p : 'tag_p pattern) (v : value) :
    (varname * value) list option =
  let open Option in
  match (p, v) with
  | PatName (_, xname, xtype), v ->
      if value_type v |> equal_vtype xtype then Some [ (xname, v) ] else None
  | PatPair (_, p1, p2), Pair (v1, v2) ->
      match_pattern p1 v1 >>= fun m1 ->
      match_pattern p2 v2 >>= fun m2 -> Some (m1 @ m2)
  | PatPair _, _ -> None
  | PatConstructor (_, p_c_name, p1), VariantTypeValue (_, v_c_name, v') ->
      if equal_string p_c_name v_c_name then match_pattern p1 v' else None
  | PatConstructor _, _ -> None

(** Apply a function to the execution value if it is an integer, otherwise
    return a typing error *)
let apply_to_int (cnt : int -> exec_res) (x : value) : exec_res =
  match x with
  | Int i -> cnt i
  | _ ->
      Error (TypingError { empty_typing_error with expected_type = Some "Int" })

(** Apply a function to the execution value if it is a boolean, otherwise return
    a typing error *)
let apply_to_bool (cnt : bool -> exec_res) (x : value) : exec_res =
  match x with
  | Bool b -> cnt b
  | _ ->
      Error
        (TypingError { empty_typing_error with expected_type = Some "Bool" })

(** Apply a function to the execution value if it is a function, otherwise
    return a typing error *)
let apply_to_closure (cnt : closure_props -> exec_res) (x : value) : exec_res =
  match x with
  | Closure x -> cnt x
  | _ ->
      Error
        (TypingError { empty_typing_error with expected_type = Some "Closure" })

module Executor
    (TypeCtx : Typing.TypingTypeContext)
    (VarCtx : Typing.TypingVarContext) =
struct
  module TypeChecker = Typing.TypeChecker (TypeCtx) (VarCtx)

  (** Evaluate a subexpression, then apply a continuation function to the result
      if it an integer and give a typing error otherwise *)
  let rec eval_apply_to_int ~(type_ctx : TypeCtx.t) (store : store)
      (x : (ast_tag, pattern_tag) Ast.typed_expr) (cnt : int -> exec_res) :
      exec_res =
    let open Result in
    eval ~type_ctx store x >>= apply_to_int cnt

  (** Evaluate a subexpression, then apply a continuation function to the result
      if it an boolean and give a typing error otherwise *)
  and eval_apply_to_bool ~(type_ctx : TypeCtx.t) (store : store)
      (x : (ast_tag, pattern_tag) Ast.typed_expr) (cnt : bool -> exec_res) :
      exec_res =
    let open Result in
    eval ~type_ctx store x >>= apply_to_bool cnt

  (** Evaluate a subexpression, then apply a continuation function to the result
      if it an function and give a typing error otherwise *)
  and eval_apply_to_closure ~(type_ctx : TypeCtx.t) (store : store)
      (x : (ast_tag, pattern_tag) Ast.typed_expr)
      (cnt : closure_props -> exec_res) : exec_res =
    let open Result in
    eval ~type_ctx store x >>= apply_to_closure cnt

  (** Evaluate an AST subtree *)
  and eval ~(type_ctx : TypeCtx.t) (store : store)
      (e : (ast_tag, pattern_tag) Ast.typed_expr) : exec_res =
    let open Result in
    match e with
    | UnitLit _ -> Ok Unit
    | IntLit (_, i) -> Ok (Int i)
    | Add (_, e1, e2) ->
        eval_apply_to_int ~type_ctx store e1 (fun i1 ->
            eval_apply_to_int ~type_ctx store e2 (fun i2 -> Ok (Int (i1 + i2))))
    | Neg (_, e) -> eval_apply_to_int ~type_ctx store e (fun i -> Ok (Int (-i)))
    | Subtr (_, e1, e2) ->
        eval_apply_to_int ~type_ctx store e1 (fun i1 ->
            eval_apply_to_int ~type_ctx store e2 (fun i2 -> Ok (Int (i1 - i2))))
    | Mult (_, e1, e2) ->
        eval_apply_to_int ~type_ctx store e1 (fun i1 ->
            eval_apply_to_int ~type_ctx store e2 (fun i2 -> Ok (Int (i1 * i2))))
    | BoolLit (_, b) -> Ok (Bool b)
    | BNot (_, e) ->
        eval_apply_to_bool ~type_ctx store e (fun b -> Ok (Bool (not b)))
    | BOr (_, e1, e2) ->
        eval_apply_to_bool ~type_ctx store e1 (fun b1 ->
            eval_apply_to_bool ~type_ctx store e2 (fun b2 ->
                Ok (Bool (b1 || b2))))
    | BAnd (_, e1, e2) ->
        eval_apply_to_bool ~type_ctx store e1 (fun b1 ->
            eval_apply_to_bool ~type_ctx store e2 (fun b2 ->
                Ok (Bool (b1 && b2))))
    | Pair (_, e1, e2) ->
        eval ~type_ctx store e1 >>= fun v1 ->
        eval ~type_ctx store e2 >>= fun v2 -> Ok (Pair (v1, v2))
    | Eq (_, e1, e2) -> (
        eval ~type_ctx store e1 >>= fun v1 ->
        eval ~type_ctx store e2 >>= fun v2 ->
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
        eval_apply_to_int ~type_ctx store e1 (fun i1 ->
            eval_apply_to_int ~type_ctx store e2 (fun i2 -> Ok (Bool (i1 > i2))))
    | GtEq (_, e1, e2) ->
        eval_apply_to_int ~type_ctx store e1 (fun i1 ->
            eval_apply_to_int ~type_ctx store e2 (fun i2 ->
                Ok (Bool (i1 >= i2))))
    | Lt (_, e1, e2) ->
        eval_apply_to_int ~type_ctx store e1 (fun i1 ->
            eval_apply_to_int ~type_ctx store e2 (fun i2 -> Ok (Bool (i1 < i2))))
    | LtEq (_, e1, e2) ->
        eval_apply_to_int ~type_ctx store e1 (fun i1 ->
            eval_apply_to_int ~type_ctx store e2 (fun i2 ->
                Ok (Bool (i1 <= i2))))
    | If (_, e_cond, e_then, e_else) ->
        eval_apply_to_bool ~type_ctx store e_cond (fun b ->
            let next_e = if b then e_then else e_else in
            eval ~type_ctx store next_e)
    | Var (_, x) ->
        store_get store x
        |> Option.value_map ~default:(Error (UndefinedVarError x)) ~f:(fun v ->
               Ok v)
    | Let (_, xname, e1, e2) ->
        eval ~type_ctx store e1 >>= fun v ->
        eval ~type_ctx (store_set store ~key:xname ~value:v) e2
    | App (_, e1, e2) ->
        (* This uses call-by-value semantics *)
        eval_apply_to_closure ~type_ctx store e1 (fun closure ->
            eval ~type_ctx store e2 >>= fun v2 ->
            let new_store =
              store_set closure.store ~key:(closure.param |> fst) ~value:v2
            in
            let new_store =
              (* If the function is recursive, make it re-accessible in the execution of the function *)
              match closure.recursive with
              | `Recursive fname ->
                  store_set new_store ~key:fname ~value:(Closure closure)
              | `NonRecursive -> new_store
            in
            eval ~type_ctx new_store closure.body)
    | Match (_, e1, cs) -> (
        eval ~type_ctx store e1 >>= fun v1 ->
        let matched_c_e :
            ((varname * value) list * (ast_tag, pattern_tag) Ast.typed_expr)
            option =
          Nonempty_list.fold ~init:None
            ~f:(fun acc (p, c_e) ->
              match acc with
              | Some _ -> acc
              | None -> match_pattern p v1 |> Option.map ~f:(fun m -> (m, c_e)))
            cs
        in
        match matched_c_e with
        | None -> Error IncompleteMatchError
        | Some (m, c_e) ->
            eval ~type_ctx
              (List.fold ~init:store
                 ~f:(fun acc (xname, xval) ->
                   store_set acc ~key:xname ~value:xval)
                 m)
              c_e)
    | Constructor (_, c_name, e1) ->
        eval ~type_ctx store e1 >>= fun v1 ->
        TypeCtx.find_variant_type_with_constructor type_ctx c_name
        |> Result.of_option ~error:(UnknownVariantTypeConstructor c_name)
        >>= fun (vt, _) -> Ok (VariantTypeValue (vt, c_name, v1))

  let execute_program (tprog : ('tag_e, 'tag_p) TypeChecker.typed_program) =
    let open Result in
    let prog = TypeChecker.typed_program_get_program tprog in
    TypeCtx.create ~custom_types:prog.custom_types
    |> Result.map_error ~f:(fun err -> TypeContextCreationError err)
    >>= fun type_ctx ->
    let store =
      List.fold
        ~init:(VarnameMap.empty : store)
        ~f:(fun acc defn ->
          let plain_typed_body =
            defn.body
            |> Ast.fmap ~f:(fun (t, _) -> (t, ()))
            |> Ast.fmap_pattern ~f:(fun (t, _) -> (t, ()))
          in
          Map.set acc ~key:defn.name
            ~data:
              (Closure
                 {
                   param = defn.param;
                   out_type = defn.return_t;
                   body = plain_typed_body;
                   store = acc;
                   recursive =
                     (if defn.recursive then `Recursive defn.name
                      else `NonRecursive);
                 }))
        prog.top_level_defns
    in
    let e = prog.e in
    eval ~type_ctx store
      (e
      |>
      (* Remove AST tags except type *)
      Ast.fmap ~f:(fun (t, _) -> (t, ()))
      |>
      (* Remove pattern tags except type *)
      Ast.fmap_pattern ~f:(fun (t, _) -> (t, ())))
end

module SimpleExecutor =
  Executor (Typing.SetTypingTypeContext) (Typing.ListTypingVarContext)
