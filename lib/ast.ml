type expr =
  (* Integer arithmetic *)
  | IntLit of int
  | Add of expr * expr
  | Subtr of expr * expr
  | Mult of expr * expr
  (* Boolean algebra *)
  | BoolLit of bool
  | BNot of expr
  | BOr of expr * expr
  | BAnd of expr * expr
  (* Comparisons *)
  | Eq of expr * expr
  | Gt of expr * expr
  | GtEq of expr * expr
  | Lt of expr * expr
  | LtEq of expr * expr
  (* Control flow *)
  | If of expr * expr * expr
  (* Variables *)
  | Var of string
  | Let of string * expr * expr

let rec show e =
  match e with
  | IntLit i -> string_of_int i
  | Add (e1, e2) -> Printf.sprintf "(%s) + (%s)" (show e1) (show e2)
  | Subtr (e1, e2) -> Printf.sprintf "(%s) - (%s)" (show e1) (show e2)
  | Mult (e1, e2) -> Printf.sprintf "(%s) * (%s)" (show e1) (show e2)
  | BoolLit b -> string_of_bool b
  | BNot e -> Printf.sprintf "!(%s)" (show e)
  | BOr (e1, e2) -> Printf.sprintf "(%s) || (%s)" (show e1) (show e2)
  | BAnd (e1, e2) -> Printf.sprintf "(%s) || (%s)" (show e1) (show e2)
  | Eq (e1, e2) -> Printf.sprintf "(%s) == (%s)" (show e1) (show e2)
  | Gt (e1, e2) -> Printf.sprintf "(%s) > (%s)" (show e1) (show e2)
  | GtEq (e1, e2) -> Printf.sprintf "(%s) >= (%s)" (show e1) (show e2)
  | Lt (e1, e2) -> Printf.sprintf "(%s) < (%s)" (show e1) (show e2)
  | LtEq (e1, e2) -> Printf.sprintf "(%s) <= (%s)" (show e1) (show e2)
  | If (e1, e2, e3) ->
      Printf.sprintf "if %s then %s else %s" (show e1) (show e2) (show e3)
  | Var x -> x
  | Let (x, e1, e2) -> Printf.sprintf "let %s = %s in %s" x (show e1) (show e2)
