open Utils
open Vtype

(** A single constructor for a custom data type, with a name and mandatory attached data type *)
type custom_type_constructor = string * vtype [@@deriving sexp, equal]

(** Convert a custom type constuctor definition into source code.  *)
val custom_type_constructor_to_source_code : custom_type_constructor -> string

type _gen_options_constructors = {
  used_custom_type_names : StringSet.t;
  used_custom_type_constructor_names : StringSet.t;
  mrd : int;
}

module QCheck_testing_constructors :
  QCheck_testing_sig
    with type t = custom_type_constructor
     and type gen_options = _gen_options_constructors
     and type print_options = unit
     and type shrink_options = unit
     and type arb_options = _gen_options

(** A custom data type with a name and list of constructors *)
type custom_type = string * custom_type_constructor list
[@@deriving sexp, equal]

(** Convert a custom type definition into source code. *)
val custom_type_to_source_code : custom_type -> string

type _gen_options = {
  used_custom_type_names : StringSet.t;
  used_custom_type_constructor_names : StringSet.t;
  max_constructors : int;
  mrd : int;
}

module QCheck_testing :
  QCheck_testing_sig
    with type t = custom_type
     and type gen_options = _gen_options
     and type print_options = unit
     and type shrink_options = unit
     and type arb_options = _gen_options
