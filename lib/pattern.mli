open Utils
open Varname

(** A pattern in the language *)
type 'a t =
  | PatName of 'a * varname * Vtype.t
      (** A named and typed variable in a pattern *)
  | PatPair of 'a * 'a t * 'a t  (** A pair pattern *)
  | PatConstructor of 'a * string * 'a t  (** A constructor pattern *)
[@@deriving sexp, equal]

(** A pattern with type information *)
type 'a typed_t = (Vtype.t * 'a) t [@@deriving sexp, equal]

(** Pattern with no tagging information *)
type plain_t = unit t [@@deriving sexp, equal]

(** Convert a tagged pattern to an untagged pattern *)
val to_plain_pattern : 'a t -> plain_t

(** Get the value attached to a pattern node *)
val node_val : 'a t -> 'a

(** Map a function onto values in a pattern *)
val fmap : f:('a -> 'b) -> 'a t -> 'b t

(** Rename a variable in a pattern *)
val rename_var : old_name:varname -> new_name:varname -> 'a t -> 'a t

(** Get all names used in a pattern *)
val existing_names : 'a t -> StringSet.t

(** Get the list of variables and their types that this pattern introduces to
    its case expression's variable context *)
val defined_vars : 'a t -> (varname * Vtype.t) list

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
end
