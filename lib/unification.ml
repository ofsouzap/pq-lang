open Core
open Utils
open Varname
open Pattern
open Ast

type ('tag_e, 'tag_p) unifier = ('tag_e, 'tag_p) expr StringMap.t
[@@deriving sexp, equal]

let find_unifier ~(from_pattern : 'a pattern) ~(to_expr : ('tag_e, 'tag_p) expr)
    : (('tag_e, 'tag_p) unifier, unit) Result.t =
  let open Result in
  let rec aux (acc : ('tag_e, 'tag_p) unifier) :
      'a pattern * ('tag_e, 'tag_p) expr ->
      (('tag_e, 'tag_p) unifier, unit) Result.t = function
    | PatName (_, xname, _), e -> Ok (Map.set acc ~key:xname ~data:e)
    | PatPair (_, p1, p2), Pair (_, e1, e2) ->
        aux acc (p1, e1) >>= fun acc -> aux acc (p2, e2)
    | PatPair _, _ -> Error ()
    | PatConstructor (_, cname, p), Constructor (_, cname', e)
      when equal_string cname cname' ->
        aux acc (p, e)
    | PatConstructor _, _ -> Error ()
  in
  aux StringMap.empty (from_pattern, to_expr)

let rec pattern_to_expr ~(convert_tag : 'tag_p -> 'tag_e) :
    'tag_p pattern -> ('tag_e, 'tag_p) Ast.expr =
  let open Ast in
  function
  | PatName (v, xname, _) -> Var (convert_tag v, xname)
  | PatPair (v, p1, p2) ->
      Pair
        ( convert_tag v,
          pattern_to_expr ~convert_tag p1,
          pattern_to_expr ~convert_tag p2 )
  | PatConstructor (v, cname, p) ->
      Constructor (convert_tag v, cname, pattern_to_expr ~convert_tag p)

let rename_var_in_body ~(old_name : varname) ~(new_name : varname)
    (unifier : ('tag_e, 'tag_p) unifier) : ('tag_e, 'tag_p) unifier =
  Map.map unifier ~f:(Ast.rename_var ~old_name ~new_name)

let rec apply_to_expr ~(unifier : ('tag_e, 'tag_p) unifier) :
    ('tag_e, 'tag_p) expr -> ('tag_e, 'tag_p) expr = function
  | UnitLit v -> UnitLit v
  | IntLit (v, i) -> IntLit (v, i)
  | Add (v, e1, e2) ->
      Add (v, apply_to_expr ~unifier e1, apply_to_expr ~unifier e2)
  | Neg (v, e) -> Neg (v, apply_to_expr ~unifier e)
  | Subtr (v, e1, e2) ->
      Subtr (v, apply_to_expr ~unifier e1, apply_to_expr ~unifier e2)
  | Mult (v, e1, e2) ->
      Mult (v, apply_to_expr ~unifier e1, apply_to_expr ~unifier e2)
  | BoolLit (v, b) -> BoolLit (v, b)
  | BNot (v, e) -> BNot (v, apply_to_expr ~unifier e)
  | BOr (v, e1, e2) ->
      BOr (v, apply_to_expr ~unifier e1, apply_to_expr ~unifier e2)
  | BAnd (v, e1, e2) ->
      BAnd (v, apply_to_expr ~unifier e1, apply_to_expr ~unifier e2)
  | Pair (v, e1, e2) ->
      Pair (v, apply_to_expr ~unifier e1, apply_to_expr ~unifier e2)
  | Eq (v, e1, e2) ->
      Eq (v, apply_to_expr ~unifier e1, apply_to_expr ~unifier e2)
  | Gt (v, e1, e2) ->
      Gt (v, apply_to_expr ~unifier e1, apply_to_expr ~unifier e2)
  | GtEq (v, e1, e2) ->
      GtEq (v, apply_to_expr ~unifier e1, apply_to_expr ~unifier e2)
  | Lt (v, e1, e2) ->
      Lt (v, apply_to_expr ~unifier e1, apply_to_expr ~unifier e2)
  | LtEq (v, e1, e2) ->
      LtEq (v, apply_to_expr ~unifier e1, apply_to_expr ~unifier e2)
  | If (v, e1, e2, e3) ->
      If
        ( v,
          apply_to_expr ~unifier e1,
          apply_to_expr ~unifier e2,
          apply_to_expr ~unifier e3 )
  | Var (v, xname) -> (
      match Map.find unifier xname with
      | Some p_subst -> p_subst
      | None -> Var (v, xname))
  | Let (v, xname, e1, e2) ->
      let unifier = Map.remove unifier xname in
      Let (v, xname, apply_to_expr ~unifier e1, apply_to_expr ~unifier e2)
  | App (v, e1, e2) ->
      App (v, apply_to_expr ~unifier e1, apply_to_expr ~unifier e2)
  | Match (v, e, cases) ->
      let cases' =
        Nonempty_list.map
          ~f:(fun (case_p, case_e) ->
            (* Need to beware of alpha-equivalence considerations:
            if the pattern redefines a variable name, the unifier should remove this variable name from its scope *)
            let case_p_defined_vars = defined_vars case_p in
            let unifier' =
              List.fold ~init:unifier
                ~f:(fun acc (xname, _) -> Map.remove acc xname)
                case_p_defined_vars
            in
            (case_p, apply_to_expr ~unifier:unifier' case_e))
          cases
      in
      Match (v, apply_to_expr ~unifier e, cases')
  | Constructor (v, name, e) -> Constructor (v, name, apply_to_expr ~unifier e)
