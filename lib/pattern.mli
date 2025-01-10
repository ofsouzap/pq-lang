open Utils
open Varname
open Vtype

(** A pattern in the language *)
type pattern =
  | PatName of varname * vtype  (** A named and typed variable in a pattern *)
  | PatPair of pattern * pattern  (** A pair pattern *)
  | PatConstructor of string * pattern  (** A constructor pattern *)
[@@deriving sexp, equal]

(** Convert a pattern to a source code representation *)
val pattern_to_source_code : pattern -> string

module QCheck_testing : sig
  type gen_options = {
    get_custom_type_constructors :
      string -> Custom_types.custom_type_constructor list;
    t : vtype;
  }

  include
    QCheck_testing_sig
      with type t = pattern * (string * vtype) list
       and type gen_options := gen_options
       and type print_options = unit
       and type shrink_options = unit
       and type arb_options = gen_options
end
