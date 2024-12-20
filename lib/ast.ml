open Core
open Utils
open Vtype
open Pattern

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
  | Pair of 'a * 'a expr * 'a expr
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
  | Match of 'a * 'a expr * (pattern * 'a expr) Nonempty_list.t
[@@deriving sexp, equal]

let expr_node_map_val_with_result ~(f : 'a -> 'a) : 'a expr -> 'a * 'a expr =
  function
  | IntLit (v, x) -> (f v, IntLit (f v, x))
  | Add (v, e1, e2) -> (f v, Add (f v, e1, e2))
  | Neg (v, e1) -> (f v, Neg (f v, e1))
  | Subtr (v, e1, e2) -> (f v, Subtr (f v, e1, e2))
  | Mult (v, e1, e2) -> (f v, Mult (f v, e1, e2))
  | BoolLit (v, b) -> (f v, BoolLit (f v, b))
  | BNot (v, e1) -> (f v, BNot (f v, e1))
  | BOr (v, e1, e2) -> (f v, BOr (f v, e1, e2))
  | BAnd (v, e1, e2) -> (f v, BAnd (f v, e1, e2))
  | Pair (v, e1, e2) -> (f v, Pair (f v, e1, e2))
  | Eq (v, e1, e2) -> (f v, Eq (f v, e1, e2))
  | Gt (v, e1, e2) -> (f v, Gt (f v, e1, e2))
  | GtEq (v, e1, e2) -> (f v, GtEq (f v, e1, e2))
  | Lt (v, e1, e2) -> (f v, Lt (f v, e1, e2))
  | LtEq (v, e1, e2) -> (f v, LtEq (f v, e1, e2))
  | If (v, e1, e2, e3) -> (f v, If (f v, e1, e2, e3))
  | Var (v, xname) -> (f v, Var (f v, xname))
  | Let (v, e1, e2, e3) -> (f v, Let (f v, e1, e2, e3))
  | Fun (v, xname, e1) -> (f v, Fun (f v, xname, e1))
  | App (v, e1, e2) -> (f v, App (f v, e1, e2))
  | Fix (v, fname, xname, e1) -> (f v, Fix (f v, fname, xname, e1))
  | Match (v, e1, cs) -> (f v, Match (f v, e1, cs))

let expr_node_val (e : 'a expr) : 'a =
  expr_node_map_val_with_result ~f:Fn.id e |> fst

let expr_node_map_val ~(f : 'a -> 'a) : 'a expr -> 'a expr =
  Fn.compose snd (expr_node_map_val_with_result ~f)

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
  | Pair (a, e1, e2) -> Pair (f a, fmap ~f e1, fmap ~f e2)
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
  | Match (a, e, cs) ->
      Match
        ( f a,
          fmap ~f e,
          Nonempty_list.map ~f:(fun (p, c_e) -> (p, fmap ~f c_e)) cs )

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
  | Pair (_, e1, e2) -> Pair ((), expr_to_plain_expr e1, expr_to_plain_expr e2)
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
  | Match (_, e, cs) ->
      Match
        ( (),
          expr_to_plain_expr e,
          Nonempty_list.map ~f:(fun (p, c_e) -> (p, expr_to_plain_expr c_e)) cs
        )

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
  | Pair (_, e1, e2) ->
      sprintf "(%s, %s)" (ast_to_source_code e1) (ast_to_source_code e2)
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
  | Match (_, e, cs) ->
      sprintf "match (%s) with %s end" (ast_to_source_code e)
        (cs
        |> Nonempty_list.map ~f:(fun (p, c_e) ->
               sprintf "| (%s) -> (%s)" (pattern_to_source_code p)
                 (ast_to_source_code c_e))
        |> Nonempty_list.to_list |> String.concat ~sep:" ")
