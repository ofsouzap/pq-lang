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

exception AstConverionFixError

let rec ast_to_source_code = function
  | IntLit i -> string_of_int i
  | Add (e1, e2) ->
      sprintf "(%s) + (%s)" (ast_to_source_code e1) (ast_to_source_code e2)
  | Neg e -> sprintf "-(%s)" (ast_to_source_code e)
  | Subtr (e1, e2) ->
      sprintf "(%s) - (%s)" (ast_to_source_code e1) (ast_to_source_code e2)
  | Mult (e1, e2) ->
      sprintf "(%s) * (%s)" (ast_to_source_code e1) (ast_to_source_code e2)
  | BoolLit b -> string_of_bool b
  | BNot e -> sprintf "~(%s)" (ast_to_source_code e)
  | BOr (e1, e2) ->
      sprintf "(%s) || (%s)" (ast_to_source_code e1) (ast_to_source_code e2)
  | BAnd (e1, e2) ->
      sprintf "(%s) && (%s)" (ast_to_source_code e1) (ast_to_source_code e2)
  | Eq (e1, e2) ->
      sprintf "(%s) && (%s)" (ast_to_source_code e1) (ast_to_source_code e2)
  | Gt (e1, e2) ->
      sprintf "(%s) > (%s)" (ast_to_source_code e1) (ast_to_source_code e2)
  | GtEq (e1, e2) ->
      sprintf "(%s) >= (%s)" (ast_to_source_code e1) (ast_to_source_code e2)
  | Lt (e1, e2) ->
      sprintf "(%s) < (%s)" (ast_to_source_code e1) (ast_to_source_code e2)
  | LtEq (e1, e2) ->
      sprintf "(%s) <= (%s)" (ast_to_source_code e1) (ast_to_source_code e2)
  | If (e1, e2, e3) ->
      sprintf "if (%s) then (%s) else (%s) end" (ast_to_source_code e1)
        (ast_to_source_code e2) (ast_to_source_code e3)
  | Var vname -> vname
  | Let (xname, e1, e2) -> (
      let eval_default_repr () =
        sprintf "let (%s) = (%s) in (%s) end" xname (ast_to_source_code e1)
          (ast_to_source_code e2)
      in
      match e1 with
      | Fix (xname2, yname, e1') ->
          if equal_string xname xname2 then
            sprintf "let rec %s = fun %s -> (%s) end in (%s) end" xname yname
              (ast_to_source_code e1') (ast_to_source_code e2)
          else eval_default_repr ()
      | _ -> eval_default_repr ())
  | Fun (xname, e) -> sprintf "fun %s -> (%s) end" xname (ast_to_source_code e)
  | App (e1, e2) ->
      sprintf "(%s) (%s)" (ast_to_source_code e1) (ast_to_source_code e2)
  | Fix _ -> raise AstConverionFixError
