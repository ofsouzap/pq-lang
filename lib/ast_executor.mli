(** This module provides functionality for directly executing an AST of a program. *)

(** The type of a variable's name in the store *)
type varname = string

(** Properties of a closure *)
type closure_props = varname * Ast.vtype * Ast.expr * store

(** A resulting value from executing an AST *)
and value =
  | Int of int  (** An integer values *)
  | Bool of bool  (** A boolean value *)
  | Closure of closure_props  (** A function closure *)

(** A store, containing the values of variables under the current context *)
and store

(** The empty store *)
val store_empty : store

(** Get a named variable's value from a store *)
val store_get : varname -> store -> value option

(** Set a named variable's value to the provided value in the provided store and return the resulting store *)
val store_set : varname -> value -> store -> store

(** Check if two stores are equal. This is, they have the exact same set of keys, and each key maps to the same value in both stores *)
val store_compare : store -> store -> bool

(** Traverse the entire store's variable names and values in arbitrary order into a list *)
val store_traverse : store -> (varname * value) list

(** String representation of a store, for debugging purposes *)
val show_store : store -> string

type exec_err =
  | TypingError  (** Execution was halted due to a typing error *)
  | UndefinedVarError of string
      (** Execution was halted due to usage of an undefined variable of the provided name *)

(** The result of executing an AST *)
type exec_res =
  | Res of value  (** The execution terminated with the provided value *)
  | Err of exec_err  (** The execution terminated with an error *)

(** Check if two execution results are equal *)
val exec_res_compare : exec_res -> exec_res -> bool

(** String representation of a value *)
val show_value : value -> string

(** String representation of an execution result *)
val show_exec_res : exec_res -> string

(** Execute an AST representation of a program *)
val execute : Ast.expr -> exec_res
