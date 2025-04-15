open Core
module StringSet : Set.S with type Elt.t = String.t
module StringMap : Map.S with type Key.t = String.t

(** Keywords for the lexer, to not be used in variable name generation *)
val lexer_keywords : string list

module QCheck_utils : sig
  exception Filter_ran_out_of_attempts

  (** Filter a generator so that it keeps trying until the generated value
      satisfies the specified predicate. NOTE - this has a risk of causing
      infinite or near-infinite looping in testing *)
  val filter_gen :
    ?max_attempts:int -> 'a QCheck.Gen.t -> f:('a -> bool) -> 'a QCheck.Gen.t

  (** A generator to take two values and keep generating until they are not
      equal *)
  val gen_unique_pair :
    ?max_attempts:int ->
    equal:('a -> 'a -> bool) ->
    'a QCheck.Gen.t ->
    ('a * 'a) QCheck.Gen.t

  (** Arbitrary generator for the result type *)
  val result_arb :
    'a QCheck.arbitrary ->
    'b QCheck.arbitrary ->
    ('a, 'b) Result.t QCheck.arbitrary
end

(** Module signature for the QCheck-related utilities for a program-related
    type. Includes generator/shrinking options for preserving type *)
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
  val length : 'a t -> int
  val head : 'a t -> 'a
  val tail : 'a t -> 'a list
  val singleton : 'a -> 'a t
  val cons : 'a -> 'a t -> 'a t
  val map : f:('a -> 'b) -> 'a t -> 'b t
  val fold : 'a t -> init:'b -> f:('b -> 'a -> 'b) -> 'b

  val fold_consume_init :
    'a t -> init:'init -> f:(('init, 'acc) Either.t -> 'a -> 'acc) -> 'acc

  val rev : 'a t -> 'a t

  (** Zip two non-empty lists together, returning None if they have unequal
      lengths *)
  val zip : 'a t -> 'b t -> ('a * 'b) t option

  val result_all : ('a, 'err) Result.t t -> ('a t, 'err) Result.t

  val fold_result :
    'a t -> init:'b -> f:('b -> 'a -> ('b, 'c) Result.t) -> ('b, 'c) Result.t

  (** A variant of fold_result that is known not to return the initial value, as
      the list is non-empty *)
  val fold_result_consume_init :
    'a t ->
    init:'init ->
    f:(('init, 'acc) Either.t -> 'a -> ('acc, 'err) Result.t) ->
    ('acc, 'err) Result.t

  module QCheck_testing : functor
    (V : sig
       type t

       val gen : t QCheck.Gen.t
       val print : t QCheck.Print.t
       val shrink : t QCheck.Shrink.t
     end)
    ->
    QCheck_testing_sig
      with type t = V.t t
       and type gen_options = unit
       and type print_options = unit
       and type shrink_options = unit
       and type arb_options = unit
end

module Nonempty_list : Nonempty_list_sig

module type SourceCodeBuilderSig = sig
  (** Type describing a state of the builder *)
  type state

  (** Utility operator for chaining functions. Useful for chaining state
      operators *)
  val ( |.> ) : ('a -> 'b) -> ('b -> 'c) -> 'a -> 'c

  (** Create the initial state from the given initial parameters *)
  val init : use_newlines:bool -> state

  (** Do nothing to a state. This is the identity function. Can be a useful
      shortcut for writing source code builders *)
  val nothing : state -> state

  (** Write text to the builder *)
  val write : string -> state -> state

  (** If the current line has contents, create a new empty one with the current
      indent level, otherwise just set the line's indent level to the current
      indent level *)
  val endline : state -> state

  (** Wrap a state operator in a block to make a block of code *)
  val block : (state -> state) -> state -> state

  (** Build the state into the source code output *)
  val build : state -> string

  (** Shortcut for creating a source code builder from a conversion function
      that returns state operators for given inputs *)
  val from_converter :
    converter:('a -> state -> state) -> use_newlines:bool -> 'a -> string
end

module SourceCodeBuilder : SourceCodeBuilderSig
