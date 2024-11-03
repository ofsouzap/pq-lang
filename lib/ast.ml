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
  | Fix

let rec show_ast = function
  | IntLit i -> Printf.sprintf "IntLit %d" i
  | Add (x, y) -> Printf.sprintf "Add (%s, %s)" (show_ast x) (show_ast y)
  | Neg x -> Printf.sprintf "Neg (%s)" (show_ast x)
  | Subtr (x, y) -> Printf.sprintf "Subtr (%s, %s)" (show_ast x) (show_ast y)
  | Mult (x, y) -> Printf.sprintf "Mult (%s, %s)" (show_ast x) (show_ast y)
  | BoolLit b -> Printf.sprintf "BoolLit %b" b
  | BNot x -> Printf.sprintf "BNot (%s)" (show_ast x)
  | BOr (x, y) -> Printf.sprintf "BOr (%s, %s)" (show_ast x) (show_ast y)
  | BAnd (x, y) -> Printf.sprintf "BAnd (%s, %s)" (show_ast x) (show_ast y)
  | Eq (x, y) -> Printf.sprintf "Eq (%s, %s)" (show_ast x) (show_ast y)
  | Gt (x, y) -> Printf.sprintf "Gt (%s, %s)" (show_ast x) (show_ast y)
  | GtEq (x, y) -> Printf.sprintf "GtEq (%s, %s)" (show_ast x) (show_ast y)
  | Lt (x, y) -> Printf.sprintf "Lt (%s, %s)" (show_ast x) (show_ast y)
  | LtEq (x, y) -> Printf.sprintf "LtEq (%s, %s)" (show_ast x) (show_ast y)
  | If (x, y, z) ->
      Printf.sprintf "If (%s, %s, %s)" (show_ast x) (show_ast y) (show_ast z)
  | Var x -> Printf.sprintf "Var %s" x
  | Let (x, z, w) ->
      Printf.sprintf "Let (%s, %s, %s)" x (show_ast z) (show_ast w)
  | Fun (x, z) -> Printf.sprintf "Fun (%s, %s)" x (show_ast z)
  | App (x, y) -> Printf.sprintf "App (%s, %s)" (show_ast x) (show_ast y)
  | Fix -> "Fix"
