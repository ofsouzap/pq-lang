open Utils

(** A single constructor for a variant data type, with a name and mandatory
    attached data type *)
type constructor = string * Vtype.t [@@deriving sexp, equal]

(** Get all names used in a variant type constructor *)
val constructor_existing_names : constructor -> StringSet.t

(** Convert a variant type constuctor definition into source code. *)
val constructor_to_source_code : constructor -> string

module QCheck_testing_constructors : sig
  type gen_options = {
    used_variant_type_names : StringSet.t;
    used_variant_type_constructor_names : StringSet.t;
    allow_fun_types : bool;
    mrd : int;
  }

  include
    QCheck_testing_sig
      with type t = constructor
       and type gen_options := gen_options
       and type print_options = unit
       and type shrink_options = unit
       and type arb_options := gen_options
end

(** A variant data type with a name and list of constructors *)
type t = string * constructor list [@@deriving sexp, equal]

(** Get all names used in a variant type definition *)
val existing_names : t -> StringSet.t

(** Convert a variant type definition into source code. *)
val to_source_code : t -> string

module QCheck_testing : sig
  type gen_options = {
    used_variant_type_names : StringSet.t;
    used_variant_type_constructor_names : StringSet.t;
    allow_fun_types : bool;
    max_constructors : int;
    mrd : int;
  }

  include
    QCheck_testing_sig
      with type t = t
       and type gen_options := gen_options
       and type print_options = unit
       and type shrink_options = unit
       and type arb_options := gen_options
end
