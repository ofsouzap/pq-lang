open Core
open Utils

(** The type of a variable's name in the store *)
type varname = string [@@deriving sexp, equal]

(** A map with variable names as keys *)
module VarnameMap : Map.S with type Key.t = varname

module QCheck_testing :
  QCheck_testing_sig
    with type t = varname
     and type gen_options = unit
     and type print_options = unit
     and type shrink_options = unit
     and type arb_options = unit
