(** This module provides functionality for directly executing an AST of a program. *)

open Vtype
open Variant_types
open Varname

(** The data that the executor tags the interpreted AST with *)
type ast_tag = unit [@@deriving sexp, equal]

(** Properties of a closure *)
type closure_props = {
  param : varname * vtype;  (** The function's parameter's name and type *)
  out_type : vtype;  (** The output type of the function *)
  body : ast_tag Ast.typed_expr;  (** The body of the function *)
  store : store;  (** The store to use when executing the function *)
}
[@@deriving sexp, equal]

(** A resulting value from executing an AST *)
and value =
  | Unit  (** The unit value *)
  | Int of int  (** An integer value *)
  | Bool of bool  (** A boolean value *)
  | Closure of closure_props  (** A function closure *)
  | Pair of value * value  (** A pair value *)
  | VariantTypeValue of variant_type * string * value
      (** A value of a variant type, with the type itself and the constructor name specified *)
[@@deriving sexp, equal]

(** A store, containing the values of variables under the current context *)
and store [@@deriving sexp, equal]

(** The empty store *)
val empty_store : store

(** Get a named variable's value from a store *)
val store_get : store -> varname -> value option

(** Set a named variable's value to the provided value in the provided store and return the resulting store *)
val store_set : store -> key:varname -> value:value -> store

(** Check if two stores are equal. This is, they have the exact same set of keys, and each key maps to the same value in both stores *)
val store_compare : store -> store -> bool

(** Traverse the entire store's variable names and values in arbitrary order into a list *)
val store_traverse : store -> (varname * value) list

(** Details of a typing error. Fields are optional in case they can't be provided *)
type typing_error = {
  expected_type : string option;
  actual_type : string option;
  variable_name : varname option;
  custom_message : string option;
}
[@@deriving sexp, equal]

(** Details for a typing error that are the default empty values *)
val empty_typing_error : typing_error

type exec_err =
  | TypingError of typing_error
      (** Execution was halted due to a typing error *)
  | UndefinedVarError of varname
      (** Execution was halted due to usage of an undefined variable of the provided name *)
  | MisplacedFixError  (** Fix node was inappropriately used in the AST *)
  | FixApplicationError
      (** Application of the Fix node was done on an invalid target *)
  | MaxRecursionDepthExceeded
      (** The maximum recursion depth of the execution has been exceeded so the program was terminated *)
  | IncompleteMatchError
      (** No cases could be found that match the provided value in a match statement *)
  | UnknownVariantTypeConstructor of string
      (** No variant type could be found with a constructor of the specified name *)
[@@deriving sexp, equal]

(** The result of executing an AST *)
type exec_res = (value, exec_err) Result.t [@@deriving sexp, equal]

(** String representation of an execution result *)
val show_exec_res : exec_res -> string

(** Provides AST execution functionality, given a typing context and variable context used for a type checker implementation *)
module Executor : functor
  (TypeCtx : Typing.TypingTypeContext)
  (VarCtx : Typing.TypingVarContext)
  -> sig
  (** Execute a typed program using the type checker constructed from TypeCtx and VarCtx *)
  val execute_program :
    'a Typing.TypeChecker(TypeCtx)(VarCtx).typed_program_expression -> exec_res
end

(** An implementation of the AST executor using the simple type checker implementation *)
module SimpleExecutor : sig
  include module type of
      Executor (Typing.SetTypingTypeContext) (Typing.ListTypingVarContext)
end
