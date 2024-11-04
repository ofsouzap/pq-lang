open Core

type expr =
  | IntLit of int
  | Add of expr * expr
  | Neg of expr
  | Subtr of expr * expr
  | Mult of expr * expr
  | BoolLit of bool
  | BNot of expr
  | BOr of expr * expr
  | BAnd of expr * expr
  | Eq of expr * expr
  | Gt of expr * expr
  | GtEq of expr * expr
  | Lt of expr * expr
  | LtEq of expr * expr
  | If of expr * expr * expr
  | Var of string
  | Let of string * expr * expr
  | Fun of string * expr
  | App of expr * expr
  | Fix of string * string * expr
[@@deriving sexp, equal]
