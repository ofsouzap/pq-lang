open Core

(** The type of a variable's name in the store *)
type varname = string [@@deriving sexp, equal]

(** A map with variable names as keys *)
module VarnameMap : Map.S with type Key.t = varname
