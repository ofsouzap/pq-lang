open Core

module type S = sig
  module Smt : SmtIntf.S

  type node_tag = { source_pos : Frontend.source_position; t : Vtype.t }

  type quotient_typing_error =
    | SmtUnknownResult
    | PatternFlatteningError of FlatPattern.flattening_error
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
    (node_tag, node_tag) Program.StdProgram.t ->
    ( (unit, QuotientTypeCheckingFailure.t) Result.t,
      quotient_typing_error )
    Result.t
end

module MakeZ3 : S
