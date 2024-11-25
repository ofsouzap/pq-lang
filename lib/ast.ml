open Core
open Vtype

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
  | Fun of 'a * (string * vtype) * 'a expr
  | App of 'a * 'a expr * 'a expr
  | Fix of 'a * (string * vtype * vtype) * (string * vtype) * 'a expr
[@@deriving sexp, equal]

let expr_node_val : 'a expr -> 'a = function
  | IntLit (x, _) -> x
  | Add (x, _, _) -> x
  | Neg (x, _) -> x
  | Subtr (x, _, _) -> x
  | Mult (x, _, _) -> x
  | BoolLit (x, _) -> x
  | BNot (x, _) -> x
  | BOr (x, _, _) -> x
  | BAnd (x, _, _) -> x
  | Eq (x, _, _) -> x
  | Gt (x, _, _) -> x
  | GtEq (x, _, _) -> x
  | Lt (x, _, _) -> x
  | LtEq (x, _, _) -> x
  | If (x, _, _, _) -> x
  | Var (x, _) -> x
  | Let (x, _, _, _) -> x
  | Fun (x, _, _) -> x
  | App (x, _, _) -> x
  | Fix (x, _, _, _) -> x

let rec fmap ~(f : 'a -> 'b) (e : 'a expr) : 'b expr =
  match e with
  | IntLit (a, i) -> IntLit (f a, i)
  | Add (a, e1, e2) -> Add (f a, fmap ~f e1, fmap ~f e2)
  | Neg (a, e) -> Neg (f a, fmap ~f e)
  | Subtr (a, e1, e2) -> Subtr (f a, fmap ~f e1, fmap ~f e2)
  | Mult (a, e1, e2) -> Mult (f a, fmap ~f e1, fmap ~f e2)
  | BoolLit (a, b) -> BoolLit (f a, b)
  | BNot (a, e) -> BNot (f a, fmap ~f e)
  | BOr (a, e1, e2) -> BOr (f a, fmap ~f e1, fmap ~f e2)
  | BAnd (a, e1, e2) -> BAnd (f a, fmap ~f e1, fmap ~f e2)
  | Eq (a, e1, e2) -> Eq (f a, fmap ~f e1, fmap ~f e2)
  | Gt (a, e1, e2) -> Gt (f a, fmap ~f e1, fmap ~f e2)
  | GtEq (a, e1, e2) -> GtEq (f a, fmap ~f e1, fmap ~f e2)
  | Lt (a, e1, e2) -> Lt (f a, fmap ~f e1, fmap ~f e2)
  | LtEq (a, e1, e2) -> LtEq (f a, fmap ~f e1, fmap ~f e2)
  | If (a, e1, e2, e3) -> If (f a, fmap ~f e1, fmap ~f e2, fmap ~f e3)
  | Var (a, vname) -> Var (f a, vname)
  | Let (a, xname, e1, e2) -> Let (f a, xname, fmap ~f e1, fmap ~f e2)
  | Fun (a, (xname, xtype), e) -> Fun (f a, (xname, xtype), fmap ~f e)
  | App (a, e1, e2) -> App (f a, fmap ~f e1, fmap ~f e2)
  | Fix (a, xname, yname, e) -> Fix (f a, xname, yname, fmap ~f e)

let ( >|= ) (e : 'a expr) (f : 'a -> 'b) = fmap ~f e

type plain_expr = unit expr [@@deriving sexp, equal]
type 'a typed_expr = (vtype * 'a) expr [@@deriving sexp, equal]
type plain_typed_expr = unit typed_expr [@@deriving sexp, equal]

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
      | Fix (_, (xname2, x2type1, x2type2), (yname, ytype), e1') ->
          let x2type = VTypeFun (x2type1, x2type2) in
          if equal_string xname xname2 then
            sprintf "let rec (%s : %s) = fun (%s : %s) -> (%s) end in (%s) end"
              xname
              (vtype_to_source_code x2type)
              yname
              (vtype_to_source_code ytype)
              (ast_to_source_code e1') (ast_to_source_code e2)
          else eval_default_repr ()
      | _ -> eval_default_repr ())
  | Fun (_, (xname, xtype), e) ->
      sprintf "fun (%s : %s) -> (%s) end" xname
        (vtype_to_source_code xtype)
        (ast_to_source_code e)
  | App (_, e1, e2) ->
      sprintf "(%s) (%s)" (ast_to_source_code e1) (ast_to_source_code e2)
  | Fix _ -> raise AstConverionFixError
