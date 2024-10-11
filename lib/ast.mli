type expr =
  | IntLit of int
  | Add of expr * expr
  | Mult of expr * expr
