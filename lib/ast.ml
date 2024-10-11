type expr =
  | IntLit of int
  | Add of expr * expr
  | Mult of expr * expr

let rec show = function
  | IntLit i -> string_of_int i
  | Add (e1, e2) -> Printf.sprintf "(%s + %s)" (show e1) (show e2)
  | Mult (e1, e2) -> Printf.sprintf "(%s * %s)" (show e1) (show e2)
