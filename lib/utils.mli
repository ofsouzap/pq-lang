open Core
module StringSet : Set.S with type Elt.t = String.t

(** Keywords for the lexer, to not be used in variable name generation *)
val lexer_keywords : string list

module QCheck_utils : sig
  (** Arbitrary generator for the result type *)
  val result_arb :
    'a QCheck.arbitrary ->
    'b QCheck.arbitrary ->
    ('a, 'b) Result.t QCheck.arbitrary

  (** Filter a generator so that it keeps trying until the generated value satisfies the specified predicate.
    NOTE - this has a risk of causing infinite or near-infinite looping in testing *)
  val filter_gen :
    ?max_attempts:int -> 'a QCheck.Gen.t -> f:('a -> bool) -> 'a QCheck.Gen.t
end

(** Module signature for the QCheck-related utilities for a program-related type.
  Includes generator/shrinking options for preserving type *)
module type QCheck_testing_sig = sig
  open QCheck

  (** The type in consideration *)
  type t

  (** Options for generation *)
  type gen_options

  (** Options for printing *)
  type print_options

  (** Options for shrinking *)
  type shrink_options

  (** All arbitrary instance options *)
  type arb_options

  (** Generator *)
  val gen : gen_options -> t Gen.t

  (** Printer *)
  val print : print_options -> t Print.t

  (** Shrinker *)
  val shrink : shrink_options -> t Shrink.t

  (** Arbitrary instance *)
  val arbitrary : arb_options -> t arbitrary
end

module type Nonempty_list_sig = sig
  type 'a t = 'a * 'a list [@@deriving sexp, equal]

  val make : 'a * 'a list -> 'a t
  val to_list : 'a t -> 'a list
  val from_list_unsafe : 'a list -> 'a t
  val head : 'a t -> 'a
  val tail : 'a t -> 'a list
  val singleton : 'a -> 'a t
  val cons : 'a -> 'a t -> 'a t
  val map : f:('a -> 'b) -> 'a t -> 'b t
  val fold : 'a t -> init:'b -> f:('b -> 'a -> 'b) -> 'b
  val rev : 'a t -> 'a t

  val fold_result :
    'a t -> init:'b -> f:('b -> 'a -> ('b, 'c) Result.t) -> ('b, 'c) Result.t

  (** A variant of fold_result that is known not to return the initial value, as the list is non-empty *)
  val fold_result_consume_init :
    'a t ->
    init:'init ->
    f:(('init, 'acc) Either.t -> 'a -> ('acc, 'err) Result.t) ->
    ('acc, 'err) Result.t

  (** Arbitrary generator for a non-empty list *)
  val nonempty_list_arb : 'a QCheck.arbitrary -> 'a t QCheck.arbitrary
end

module Nonempty_list : Nonempty_list_sig
