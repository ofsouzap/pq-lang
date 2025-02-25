open Utils

(** The type of a value or expression *)
type t =
  | VTypeUnit  (** The unit type *)
  | VTypeInt  (** The type of integers *)
  | VTypeBool  (** The type of booleans *)
  | VTypePair of t * t  (** Pairs of two subtypes *)
  | VTypeFun of t * t  (** Functions from one type to another *)
  | VTypeCustom of string
      (** Custom types (ie. either variant or quotient types) specified by name
      *)
[@@deriving sexp, equal, compare]

(** Convert a Vtype.t to a string representation compatible with the source code
*)
val to_source_code : t -> string

module QCheck_testing : sig
  type gen_options = {
    variant_types : StringSet.t;
    allow_fun_types : bool;
    mrd : int;
  }

  include
    Utils.QCheck_testing_sig
      with type t = t
       and type gen_options := gen_options
       and type print_options = unit
       and type shrink_options = unit
       and type arb_options := gen_options
end
