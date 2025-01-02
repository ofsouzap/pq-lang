open Core

type varname = string [@@deriving sexp, equal]

module Varname = String
module VarnameMap = Map.Make_using_comparator (Varname)
