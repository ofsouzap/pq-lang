type vtype = VTypeInt | VTypeBool

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
  | Let of (string * vtype) * expr * expr

let show_vtype vtype =
  match vtype with VTypeInt -> "int" | VTypeBool -> "bool"

let rec show e =
  match e with
  | IntLit i -> string_of_int i
  | Add (e1, e2) -> Printf.sprintf "(%s) + (%s)" (show e1) (show e2)
  | Neg e -> Printf.sprintf "-(%s)" (show e)
  | Subtr (e1, e2) -> Printf.sprintf "(%s) - (%s)" (show e1) (show e2)
  | Mult (e1, e2) -> Printf.sprintf "(%s) * (%s)" (show e1) (show e2)
  | BoolLit b -> string_of_bool b
  | BNot e -> Printf.sprintf "~(%s)" (show e)
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
  | Let ((xname, xtype), e1, e2) ->
      Printf.sprintf "let (%s : %s) = (%s) in (%s)" xname (show_vtype xtype)
        (show e1) (show e2)
