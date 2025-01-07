open Utils

(** The type of a value or expression *)
type vtype =
  | VTypeUnit
  | VTypeInt
  | VTypeBool
  | VTypePair of vtype * vtype
  | VTypeFun of vtype * vtype
  | VTypeCustom of string
[@@deriving sexp, equal]

(** Convert a vtype to a string representation compatible with the source code *)
val vtype_to_source_code : vtype -> string

type _gen_options = { custom_types : StringSet.t; mrd : int }

module QCheck_utils :
  Utils.QCheck_testing_sig
    with type t = vtype
     and type gen_options = _gen_options
     and type print_options = unit
     and type shrink_options = unit
     and type arb_options = _gen_options
