open Utils
open Vtype

(** A single constructor for a variant data type, with a name and mandatory attached data type *)
type variant_type_constructor = string * vtype [@@deriving sexp, equal]

(** Convert a variant type constuctor definition into source code.  *)
val variant_type_constructor_to_source_code : variant_type_constructor -> string

module QCheck_testing_constructors : sig
  type gen_options = {
    used_variant_type_names : StringSet.t;
    used_variant_type_constructor_names : StringSet.t;
    mrd : int;
  }

  include
    QCheck_testing_sig
      with type t = variant_type_constructor
       and type gen_options := gen_options
       and type print_options = unit
       and type shrink_options = unit
       and type arb_options := gen_options
end

(** A variant data type with a name and list of constructors *)
type variant_type = string * variant_type_constructor list
[@@deriving sexp, equal]

(** Convert a variant type definition into source code. *)
val variant_type_to_source_code : variant_type -> string

module QCheck_testing : sig
  type gen_options = {
    used_variant_type_names : StringSet.t;
    used_variant_type_constructor_names : StringSet.t;
    max_constructors : int;
    mrd : int;
  }

  include
    QCheck_testing_sig
      with type t = variant_type
       and type gen_options := gen_options
       and type print_options = unit
       and type shrink_options = unit
       and type arb_options := gen_options
end
