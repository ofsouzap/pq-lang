open Core
open Utils
open Varname
open Pattern
open Ast

type ('tag_e, 'tag_p) unifier = ('tag_e, 'tag_p) expr StringMap.t
[@@deriving sexp, equal]

let simply_find_unifier ~(bound_names_in_from : StringSet.t)
    ~(from_expr : ('a, 'b) expr) ~(to_expr : ('tag_e, 'tag_p) expr) :
    (('tag_e, 'tag_p) unifier, unit) Result.t =
  let open Result in
  let rec aux ~(bound_names_in_from : StringSet.t)
      (acc : ('tag_e, 'tag_p) unifier) :
      ('a, 'b) expr * ('tag_e, 'tag_p) expr ->
      (('tag_e, 'tag_p) unifier, unit) Result.t = function
    | Var (_, xname), Var (_, xname')
      when Set.mem bound_names_in_from xname && equal_string xname xname' ->
        Ok acc
    | Var (_, xname), _ when Set.mem bound_names_in_from xname -> Error ()
    | Var (_, xname), e' -> Ok (Map.set acc ~key:xname ~data:e')
    | UnitLit _, UnitLit _ -> Ok acc
    | IntLit (_, x1), IntLit (_, x2) when equal_int x1 x2 -> Ok acc
    | BoolLit (_, b1), BoolLit (_, b2) when equal_bool b1 b2 -> Ok acc
    | Neg (_, e), Neg (_, e') | BNot (_, e), BNot (_, e') ->
        aux ~bound_names_in_from acc (e, e')
    | Add (_, e1, e2), Add (_, e1', e2')
    | Subtr (_, e1, e2), Subtr (_, e1', e2')
    | Mult (_, e1, e2), Mult (_, e1', e2')
    | BOr (_, e1, e2), BOr (_, e1', e2')
    | BAnd (_, e1, e2), BAnd (_, e1', e2')
    | Pair (_, e1, e2), Pair (_, e1', e2')
    | Eq (_, e1, e2), Eq (_, e1', e2')
    | Gt (_, e1, e2), Gt (_, e1', e2')
    | GtEq (_, e1, e2), GtEq (_, e1', e2')
    | Lt (_, e1, e2), Lt (_, e1', e2')
    | LtEq (_, e1, e2), LtEq (_, e1', e2')
    | App (_, e1, e2), App (_, e1', e2') ->
        aux ~bound_names_in_from acc (e1, e1') >>= fun acc ->
        aux ~bound_names_in_from acc (e2, e2')
    | Constructor (_, cname, e1), Constructor (_, cname', e2)
      when equal_string cname cname' ->
        aux ~bound_names_in_from acc (e1, e2)
    | If _, If _ | Let _, Let _ | Match _, Match _ | _, _ -> Error ()
  in
  aux ~bound_names_in_from StringMap.empty (from_expr, to_expr)

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
