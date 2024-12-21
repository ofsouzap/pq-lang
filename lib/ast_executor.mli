(** This module provides functionality for directly executing an AST of a program. *)

(** The data that the executor tags the interpreted AST with *)
type ast_tag = unit [@@deriving sexp, equal]

(** The type of a variable's name in the store *)
type varname = string

(** Properties of a closure *)
type closure_props = varname * ast_tag Ast.typed_expr * store
[@@deriving sexp, equal]

(** A resulting value from executing an AST *)
and value =
  | Int of int  (** An integer value *)
  | Bool of bool  (** A boolean value *)
  | Closure of closure_props  (** A function closure *)
  | Pair of value * value  (** A pair value *)
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

(** The result of executing an AST *)
type exec_res = (value, exec_err) Result.t

(** Check if two execution results are equal *)
val equal_exec_res : exec_res -> exec_res -> bool

(** String representation of an execution result *)
val show_exec_res : exec_res -> string

(** Execute an AST representation of a program *)
val execute : 'a Ast.typed_expr -> exec_res
