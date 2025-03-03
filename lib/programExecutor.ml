open Core
open Utils

type expr_tag = unit [@@deriving sexp, equal]
type pattern_tag = unit [@@deriving sexp, equal]

module type S = sig
  module Pattern : Pattern.S
  module Expr : Expr.S with module Pattern = Pattern

  module Program :
    Program.S with module Pattern = Pattern and module Expr = Expr

  module TypingError :
    TypeChecker.TypingError.S
      with module Pattern = Pattern
       and module Expr = Expr

  module TypeCtx :
    TypeChecker.TypeContext.S with module CustomType = Program.CustomType

  module VarCtx : TypeChecker.VarContext.S

  module TypeChecker :
    TypeChecker.S
      with module Pattern = Pattern
       and module Expr = Expr
       and module Program = Program
       and module TypingError = TypingError
       and module TypeCtx = TypeCtx
       and module VarCtx = VarCtx

  module Store : sig
    (** Properties of a closure *)
    type closure_props = {
      param : Varname.t * Vtype.t;
          (** The function's parameter's name and type *)
      out_type : Vtype.t;  (** The output type of the function *)
      body : (expr_tag, pattern_tag) Expr.typed_t;
          (** The body of the function *)
      store : store;  (** The store to use when executing the function *)
      recursive : [ `Recursive of Varname.t | `NonRecursive ];
          (** Whether the function is recursive or not. If so, the name of the
              function itself, that is made re-accessible when executing the
              function *)
    }
    [@@deriving sexp, equal]

    (** A resulting value from executing an Expr *)
    and value =
      | Unit  (** The unit value *)
      | Int of int  (** An integer value *)
      | Bool of bool  (** A boolean value *)
      | Closure of closure_props  (** A function closure *)
      | Pair of value * value  (** A pair value *)
      | VariantTypeValue of VariantType.t * string * value
          (** A value of a variant type, with the type itself and the
              constructor name specified *)
    [@@deriving sexp, equal]

    (** A store, containing the values of variables under the current context *)
    and store [@@deriving sexp, equal]

    (** The empty store *)
    val empty_store : store

    (** Get a named variable's value from a store *)
    val store_get : store -> Varname.t -> value option

    (** Set a named variable's value to the provided value in the provided store
        and return the resulting store *)
    val store_set : store -> key:Varname.t -> value:value -> store

    (** Check if two stores are equal. This is, they have the exact same set of
        keys, and each key maps to the same value in both stores *)
    val store_compare : store -> store -> bool

    (** Traverse the entire store's variable names and values in arbitrary order
        into a list *)
    val store_traverse : store -> (Varname.t * value) list
  end

  (** Details of a typing error. Fields are optional in case they can't be
      provided *)
  type typing_error = {
    expected_type : string option;
    actual_type : string option;
    variable_name : Varname.t option;
    custom_message : string option;
  }
  [@@deriving sexp, equal]

  (** Details for a typing error that are the default empty values *)
  val empty_typing_error : typing_error

  type exec_err =
    | TypingError of typing_error
        (** Execution was halted due to a typing error *)
    | UndefinedVarError of Varname.t
        (** Execution was halted due to usage of an undefined variable of the
            provided name *)
    | MisplacedFixError  (** Fix node was inappropriately used in the Expr *)
    | FixApplicationError
        (** Application of the Fix node was done on an invalid target *)
    | MaxRecursionDepthExceeded
        (** The maximum recursion depth of the execution has been exceeded so
            the program was terminated *)
    | IncompleteMatchError
        (** No cases could be found that match the provided value in a match
            statement *)
    | UnknownVariantTypeConstructor of string
        (** No variant type could be found with a constructor of the specified
            name *)
  [@@deriving sexp, equal]

  (** Print human-readable string representation of an execution error *)
  val print_exec_err : exec_err -> string

  (** The result of executing an Expr *)
  type exec_res = (Store.value, exec_err) Result.t [@@deriving sexp, equal]

  (** String representation of an execution result *)
  val show_exec_res : exec_res -> string

  (** Execute a typed program using the type checker constructed from TypeCtx
      and VarCtx *)
  val execute_program : ('tag_e, 'tag_p) TypeChecker.typed_program -> exec_res
end

module MakeStd
    (TypeChecker :
      TypeChecker.S
        with module Pattern = Pattern.StdPattern
         and module Expr = Expr.StdExpr
         and module Program = Program.StdProgram
         and module TypingError = TypeChecker.TypingError.StdTypingError
         and module TypeCtx.CustomType = CustomType.StdCustomType
         and module TypeCtx.TypingError = TypeChecker.TypingError.StdTypingError) :
  S
    with module Pattern = Pattern.StdPattern
     and module Expr = Expr.StdExpr
     and module Program = Program.StdProgram
     and module TypingError = TypeChecker.TypingError
     and module TypeCtx = TypeChecker.TypeCtx
     and module VarCtx = TypeChecker.VarCtx
     and module TypeChecker = TypeChecker = struct
  module Pattern = TypeChecker.Pattern
  module Expr = TypeChecker.Expr
  module QuotientType = TypeChecker.Program.QuotientType
  module CustomType = TypeChecker.Program.CustomType
  module Program = TypeChecker.Program
  module TypingError = TypeChecker.TypingError
  module TypeCtx = TypeChecker.TypeCtx
  module VarCtx = TypeChecker.VarCtx
  module TypeChecker = TypeChecker

  module Store = struct
    type closure_props = {
      param : Varname.t * Vtype.t;
      out_type : Vtype.t;
      body : (expr_tag, pattern_tag) Expr.typed_t;
      store : store;
      recursive : [ `Recursive of Varname.t | `NonRecursive ];
    }
    [@@deriving sexp, equal]

    and value =
      | Unit
      | Int of int
      | Bool of bool
      | Closure of closure_props
      | Pair of value * value
      | VariantTypeValue of VariantType.t * string * value
    [@@deriving sexp, equal]

    and store = value Varname.Map.t [@@deriving sexp, equal]

    let empty_store = (Varname.Map.empty : store)
    let store_get store key = Map.find store key
    let store_set store ~key ~value = Map.set store ~key ~data:value
    let store_compare = equal_store

    let store_traverse : store -> (Varname.t * value) list =
      Map.to_alist ~key_order:`Increasing
  end

  open Store

  type typing_error = {
    expected_type : string option;
    actual_type : string option;
    variable_name : Varname.t option;
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
    | TypingError of typing_error
    | UndefinedVarError of Varname.t
    | MisplacedFixError
    | FixApplicationError
    | MaxRecursionDepthExceeded
    | IncompleteMatchError
    | UnknownVariantTypeConstructor of string
  [@@deriving sexp, equal]

  let print_exec_err = function
    | TypingError terr -> "[TYPING ERROR: " ^ show_typing_error terr ^ "]"
    | UndefinedVarError x -> "[UNDEFINED VAR: " ^ x ^ "]"
    | MisplacedFixError -> "[MISPLACED FIX NODE]"
    | FixApplicationError -> "[Fix APPLICATION ERROR]"
    | MaxRecursionDepthExceeded -> "[MAXIMUM RECURSION DEPTH EXCEEDED]"
    | IncompleteMatchError -> "[INCOMPLETE MATCH]"
    | UnknownVariantTypeConstructor x ->
        "[UNKNOWN VARIANT TYPE CONSTRUCTOR: " ^ x ^ "]"

  type exec_res = (value, exec_err) Result.t [@@deriving sexp, equal]

  let show_exec_res = function
    | Ok v -> sexp_of_value v |> Sexp.to_string_hum
    | Error e -> print_exec_err e

  let rec match_pattern (p : 'tag_p Pattern.t) (v : value) :
      (Varname.t * value) list option =
    let open Option in
    match (p, v) with
    | PatName (_, xname, _), v ->
        (* Since the program must have been type-checked, we don't need to compare the type of the pattern to the type of the value *)
        Some [ (xname, v) ]
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
        Error
          (TypingError { empty_typing_error with expected_type = Some "Int" })

  (** Apply a function to the execution value if it is a boolean, otherwise
      return a typing error *)
  let apply_to_bool (cnt : bool -> exec_res) (x : value) : exec_res =
    match x with
    | Bool b -> cnt b
    | _ ->
        Error
          (TypingError { empty_typing_error with expected_type = Some "Bool" })

  (** Apply a function to the execution value if it is a function, otherwise
      return a typing error *)
  let apply_to_closure (cnt : closure_props -> exec_res) (x : value) : exec_res
      =
    match x with
    | Closure x -> cnt x
    | _ ->
        Error
          (TypingError
             { empty_typing_error with expected_type = Some "Closure" })

  (** Evaluate a subexpression, then apply a continuation function to the result
      if it an integer and give a typing error otherwise *)
  let rec eval_apply_to_int ~(type_ctx : TypeCtx.t) (store : store)
      (x : (expr_tag, pattern_tag) Expr.typed_t) (cnt : int -> exec_res) :
      exec_res =
    let open Result in
    eval ~type_ctx store x >>= apply_to_int cnt

  (** Evaluate a subexpression, then apply a continuation function to the result
      if it an boolean and give a typing error otherwise *)
  and eval_apply_to_bool ~(type_ctx : TypeCtx.t) (store : store)
      (x : (expr_tag, pattern_tag) Expr.typed_t) (cnt : bool -> exec_res) :
      exec_res =
    let open Result in
    eval ~type_ctx store x >>= apply_to_bool cnt

  (** Evaluate a subexpression, then apply a continuation function to the result
      if it an function and give a typing error otherwise *)
  and eval_apply_to_closure ~(type_ctx : TypeCtx.t) (store : store)
      (x : (expr_tag, pattern_tag) Expr.typed_t)
      (cnt : closure_props -> exec_res) : exec_res =
    let open Result in
    eval ~type_ctx store x >>= apply_to_closure cnt

  (** Evaluate an Expr subtree *)
  and eval ~(type_ctx : TypeCtx.t) (store : store)
      (e : (expr_tag, pattern_tag) Expr.typed_t) : exec_res =
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
    | Match (_, e1, _, cs) -> (
        eval ~type_ctx store e1 >>= fun v1 ->
        let matched_c_e :
            ((Varname.t * value) list * (expr_tag, pattern_tag) Expr.typed_t)
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
    let prog = TypeChecker.typed_program_get_program tprog in
    let type_ctx = TypeChecker.typed_program_get_type_ctx tprog in
    let store =
      List.fold
        ~init:(Varname.Map.empty : store)
        ~f:(fun acc defn ->
          let plain_typed_body =
            defn.body
            |> Expr.fmap ~f:(fun (t, _) -> (t, ()))
            |> Expr.fmap_pattern ~f:(fun (t, _) -> (t, ()))
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
    match prog.body with
    | Some e ->
        eval ~type_ctx store
          (e
          |>
          (* Remove Expr tags except type *)
          Expr.fmap ~f:(fun (t, _) -> (t, ()))
          |>
          (* Remove pattern tags except type *)
          Expr.fmap_pattern ~f:(fun (t, _) -> (t, ())))
    | None -> Ok Unit
end

(** An implementation of the executor using the simple type checker
    implementation *)
module SimpleExecutor :
  S
    with module Pattern = Pattern.StdPattern
     and module Expr = Expr.StdExpr
     and module Program = Program.StdProgram
     and module TypingError = TypeChecker.TypingError.StdTypingError
     and module TypeCtx = TypeChecker.TypeContext.StdSetTypeContext
     and module VarCtx = TypeChecker.VarContext.ListTypingVarContext
     and module TypeChecker = TypeChecker.StdSimpleTypeChecker =
  MakeStd (TypeChecker.StdSimpleTypeChecker)
