open Utils

(** The type of a value or expression *)
type vtype =
  | VTypeUnit  (** The unit type *)
  | VTypeInt  (** The type of integers *)
  | VTypeBool  (** The type of booleans *)
  | VTypePair of vtype * vtype  (** Pairs of two subtypes *)
  | VTypeFun of vtype * vtype  (** Functions from one type to another *)
  | VTypeCustom of string
      (** Custom types (ie. either variant or quotient types) specified by name *)
[@@deriving sexp, equal]

(** Convert a vtype to a string representation compatible with the source code *)
val vtype_to_source_code : vtype -> string

module QCheck_testing : sig
  type gen_options = { variant_types : StringSet.t; mrd : int }

  include
    Utils.QCheck_testing_sig
      with type t = vtype
       and type gen_options := gen_options
       and type print_options = unit
       and type shrink_options = unit
       and type arb_options := gen_options
end
