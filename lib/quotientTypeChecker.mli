open Core

module type S = sig
  module Smt : SmtIntf.S

  type quotient_typing_error =
    | QuotientConstraintCheckFailed
      (* TODO - instead of having an error variant for the constraint check failing, just have this as a returned boolean value *)
    | SmtUnknownResult
    | PatternFlatteningError of FlatPattern.flattening_error
    | SmtIntfError of Smt.smt_intf_error
  [@@deriving sexp, equal]

  val check_program : Smt.tag_program -> (unit, quotient_typing_error) result
end

module MakeZ3 : S
