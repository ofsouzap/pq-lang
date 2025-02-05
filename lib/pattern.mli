open Utils
open Varname
open Vtype

(** A pattern in the language *)
type 'a pattern =
  | PatName of 'a * varname * vtype
      (** A named and typed variable in a pattern *)
  | PatPair of 'a * 'a pattern * 'a pattern  (** A pair pattern *)
  | PatConstructor of 'a * string * 'a pattern  (** A constructor pattern *)
[@@deriving sexp, equal]

(** A pattern with type information *)
type 'a typed_pattern = (vtype * 'a) pattern [@@deriving sexp, equal]

(** Pattern with no tagging information *)
type plain_pattern = unit pattern [@@deriving sexp, equal]

(** Convert a tagged pattern to an untagged pattern *)
val pattern_to_plain_pattern : 'a pattern -> plain_pattern

(** Get the value attached to a pattern node *)
val pattern_node_val : 'a pattern -> 'a

(** Map a function onto values in a pattern *)
val fmap : f:('a -> 'b) -> 'a pattern -> 'b pattern

(** Get all names used in a pattern *)
val existing_names : 'a pattern -> StringSet.t

(** Get the list of variables and their types that this pattern introduces to
    its case expression's variable context *)
val defined_vars : 'a pattern -> (varname * vtype) list

(** Convert a pattern to a source code representation *)
val pattern_to_source_code : 'a pattern -> string

module QCheck_testing : functor
  (Tag : sig
     type t
   end)
  -> sig
  type gen_options = {
    get_variant_type_constructors :
      string -> Variant_types.variant_type_constructor list;
    v_gen : Tag.t QCheck.Gen.t;
    t : vtype;
  }

  include
    QCheck_testing_sig
      with type t = Tag.t pattern * (string * vtype) list
       and type gen_options := gen_options
       and type print_options = unit
       and type shrink_options = unit
       and type arb_options = gen_options
end
