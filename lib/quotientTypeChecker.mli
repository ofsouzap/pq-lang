open Core

module type S = sig
  module Smt : SmtIntf.S

  type quotient_typing_error =
    | SmtUnknownResult
    | PatternFlatteningError of FlatPattern.flattening_error
    | SmtIntfError of Smt.smt_intf_error
  [@@deriving sexp, equal]

  (** Check if a program's quotient type usage is valid. Returns a result with
      the Ok constructor containing another result describing the actual result
      of the checking *)
  val check_program :
    Smt.tag_program -> ((unit, unit) Result.t, quotient_typing_error) Result.t
end

module MakeZ3 : S
