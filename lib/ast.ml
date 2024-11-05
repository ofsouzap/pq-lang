open Core

type 'a expr =
  | IntLit of 'a * int
  | Add of 'a * 'a expr * 'a expr
  | Neg of 'a * 'a expr
  | Subtr of 'a * 'a expr * 'a expr
  | Mult of 'a * 'a expr * 'a expr
  | BoolLit of 'a * bool
  | BNot of 'a * 'a expr
  | BOr of 'a * 'a expr * 'a expr
  | BAnd of 'a * 'a expr * 'a expr
  | Eq of 'a * 'a expr * 'a expr
  | Gt of 'a * 'a expr * 'a expr
  | GtEq of 'a * 'a expr * 'a expr
  | Lt of 'a * 'a expr * 'a expr
  | LtEq of 'a * 'a expr * 'a expr
  | If of 'a * 'a expr * 'a expr * 'a expr
  | Var of 'a * string
  | Let of 'a * string * 'a expr * 'a expr
  | Fun of 'a * string * 'a expr
  | App of 'a * 'a expr * 'a expr
  | Fix of 'a * string * string * 'a expr
[@@deriving sexp, equal]

type plain_expr = unit expr [@@deriving sexp, equal]

let rec expr_to_plain_expr (e : 'a expr) : plain_expr =
  match e with
  | IntLit (_, i) -> IntLit ((), i)
  | Add (_, e1, e2) -> Add ((), expr_to_plain_expr e1, expr_to_plain_expr e2)
  | Neg (_, e) -> Neg ((), expr_to_plain_expr e)
  | Subtr (_, e1, e2) -> Subtr ((), expr_to_plain_expr e1, expr_to_plain_expr e2)
  | Mult (_, e1, e2) -> Mult ((), expr_to_plain_expr e1, expr_to_plain_expr e2)
  | BoolLit (_, b) -> BoolLit ((), b)
  | BNot (_, e) -> BNot ((), expr_to_plain_expr e)
  | BOr (_, e1, e2) -> BOr ((), expr_to_plain_expr e1, expr_to_plain_expr e2)
  | BAnd (_, e1, e2) -> BAnd ((), expr_to_plain_expr e1, expr_to_plain_expr e2)
  | Eq (_, e1, e2) -> Eq ((), expr_to_plain_expr e1, expr_to_plain_expr e2)
  | Gt (_, e1, e2) -> Gt ((), expr_to_plain_expr e1, expr_to_plain_expr e2)
  | GtEq (_, e1, e2) -> GtEq ((), expr_to_plain_expr e1, expr_to_plain_expr e2)
  | Lt (_, e1, e2) -> Lt ((), expr_to_plain_expr e1, expr_to_plain_expr e2)
  | LtEq (_, e1, e2) -> LtEq ((), expr_to_plain_expr e1, expr_to_plain_expr e2)
  | If (_, e1, e2, e3) ->
      If
        ((), expr_to_plain_expr e1, expr_to_plain_expr e2, expr_to_plain_expr e3)
  | Var (_, vname) -> Var ((), vname)
  | Let (_, xname, e1, e2) ->
      Let ((), xname, expr_to_plain_expr e1, expr_to_plain_expr e2)
  | Fun (_, xname, e) -> Fun ((), xname, expr_to_plain_expr e)
  | App (_, e1, e2) -> App ((), expr_to_plain_expr e1, expr_to_plain_expr e2)
  | Fix (_, xname, yname, e) -> Fix ((), xname, yname, expr_to_plain_expr e)

exception AstConverionFixError

let rec ast_to_source_code = function
  | IntLit (_, i) -> string_of_int i
  | Add (_, e1, e2) ->
      sprintf "(%s) + (%s)" (ast_to_source_code e1) (ast_to_source_code e2)
  | Neg (_, e) -> sprintf "-(%s)" (ast_to_source_code e)
  | Subtr (_, e1, e2) ->
      sprintf "(%s) - (%s)" (ast_to_source_code e1) (ast_to_source_code e2)
  | Mult (_, e1, e2) ->
      sprintf "(%s) * (%s)" (ast_to_source_code e1) (ast_to_source_code e2)
  | BoolLit (_, b) -> string_of_bool b
  | BNot (_, e) -> sprintf "~(%s)" (ast_to_source_code e)
  | BOr (_, e1, e2) ->
      sprintf "(%s) || (%s)" (ast_to_source_code e1) (ast_to_source_code e2)
  | BAnd (_, e1, e2) ->
      sprintf "(%s) && (%s)" (ast_to_source_code e1) (ast_to_source_code e2)
  | Eq (_, e1, e2) ->
      sprintf "(%s) == (%s)" (ast_to_source_code e1) (ast_to_source_code e2)
  | Gt (_, e1, e2) ->
      sprintf "(%s) > (%s)" (ast_to_source_code e1) (ast_to_source_code e2)
  | GtEq (_, e1, e2) ->
      sprintf "(%s) >= (%s)" (ast_to_source_code e1) (ast_to_source_code e2)
  | Lt (_, e1, e2) ->
      sprintf "(%s) < (%s)" (ast_to_source_code e1) (ast_to_source_code e2)
  | LtEq (_, e1, e2) ->
      sprintf "(%s) <= (%s)" (ast_to_source_code e1) (ast_to_source_code e2)
  | If (_, e1, e2, e3) ->
      sprintf "if (%s) then (%s) else (%s) end" (ast_to_source_code e1)
        (ast_to_source_code e2) (ast_to_source_code e3)
  | Var (_, vname) -> vname
  | Let (_, xname, e1, e2) -> (
      let eval_default_repr () =
        sprintf "let %s = (%s) in (%s) end" xname (ast_to_source_code e1)
          (ast_to_source_code e2)
      in
      match e1 with
      | Fix (_, xname2, yname, e1') ->
          if equal_string xname xname2 then
            sprintf "let rec %s = fun %s -> (%s) end in (%s) end" xname yname
              (ast_to_source_code e1') (ast_to_source_code e2)
          else eval_default_repr ()
      | _ -> eval_default_repr ())
  | Fun (_, xname, e) ->
      sprintf "fun %s -> (%s) end" xname (ast_to_source_code e)
  | App (_, e1, e2) ->
      sprintf "(%s) (%s)" (ast_to_source_code e1) (ast_to_source_code e2)
  | Fix _ -> raise AstConverionFixError
