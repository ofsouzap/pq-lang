open Core

module type S = sig
  module TypeChecker = TypeChecker.StdSimpleTypeChecker
  module Smt : SmtIntf.S

  type node_tag = { source_pos : Frontend.source_position; t : Vtype.t }

  type quotient_typing_error =
    | SmtUnknownResult
    | PatternFlatteningError of Flattener.flattening_error
    | SmtIntfError of Smt.smt_intf_error
  [@@deriving sexp, equal]

  module QuotientTypeCheckingFailure : sig
    type t [@@deriving sexp, equal]

    (** Get the source position at which the problematic match case is *)
    val get_match_case_source_pos : t -> Frontend.source_position

    (** Print a quotient type checking failure for a human reader *)
    val print : t -> string
  end

  (** Check if a program's quotient type usage is valid. Returns a result with
      the Ok constructor containing another result describing the actual result
      of the checking *)
  val check_program :
    get_expr_node_tag:(Vtype.t * 'tag_e -> node_tag) ->
    get_pattern_node_tag:(Vtype.t * 'tag_p -> node_tag) ->
    ('tag_e, 'tag_p) TypeChecker.typed_program ->
    ( (unit, QuotientTypeCheckingFailure.t) Result.t,
      quotient_typing_error )
    Result.t
end

module MakeZ3 : S
