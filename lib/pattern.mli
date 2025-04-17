open Utils

module type S = sig
  (** A pattern in the language *)
  type 'a t [@@deriving sexp, equal]

  (** A pattern with type information *)
  type 'a typed_t = (Vtype.t * 'a) t [@@deriving sexp, equal]

  (** Pattern with no tagging information *)
  type plain_t = unit t [@@deriving sexp, equal]

  (** Convert a tagged pattern to an untagged pattern *)
  val to_plain_t : 'a t -> plain_t

  (** Get the value attached to a pattern node *)
  val node_val : 'a t -> 'a

  (** Map a function onto values in a pattern *)
  val fmap : f:('a -> 'b) -> 'a t -> 'b t

  (** Rename a variable in a pattern *)
  val rename_var : old_name:Varname.t -> new_name:Varname.t -> 'a t -> 'a t

  (** Get all names used in a pattern *)
  val existing_names : 'a t -> StringSet.t

  (** Get the list of variables and their types that this pattern introduces to
      its case expression's variable context *)
  val defined_vars : 'a t -> (Varname.t * Vtype.t) list

  (** Convert a pattern to a source code representation *)
  val to_source_code : 'a t -> string

  module QCheck_testing : functor
    (Tag : sig
       type t
     end)
    -> sig
    type gen_options = {
      get_variant_type_constructors : string -> VariantType.constructor list;
      v_gen : Tag.t QCheck.Gen.t;
      t : Vtype.t;
    }

    include
      QCheck_testing_sig
        with type t = Tag.t t * (string * Vtype.t) list
         and type gen_options := gen_options
         and type print_options = unit
         and type shrink_options = unit
         and type arb_options = gen_options

    val gen_pat_name :
      Vtype.t -> Tag.t QCheck.Gen.t -> (t * Varname.t) QCheck.Gen.t
  end
end

type 'a std_pattern =
  | PatName of 'a * Varname.t * Vtype.t
      (** A named and typed variable in a pattern *)
  | PatPair of 'a * 'a std_pattern * 'a std_pattern  (** A pair pattern *)
  | PatConstructor of 'a * string * 'a std_pattern  (** A constructor pattern *)
[@@deriving sexp, equal]

module StdPattern : S with type 'a t = 'a std_pattern
