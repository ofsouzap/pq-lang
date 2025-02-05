open Core
open Utils
open Varname
open Pattern

type 'tag_p unifier = 'tag_p pattern StringMap.t [@@deriving sexp, equal]

let find_unifier ~(from_pattern : 'tag_p pattern) ~(to_pattern : 'tag_p pattern)
    : ('tag_p unifier, unit) Result.t =
  let open Result in
  let open Pattern in
  let rec aux (acc : 'tag_p unifier) :
      'tag_p pattern * 'tag_p pattern -> ('tag_p unifier, unit) Result.t =
    (* We assume that the patterns are well-typed, meaning that each name only occurs a maximum of once within them *)
    function
    | PatName (_, x1name, _), p2 -> Map.set acc ~key:x1name ~data:p2 |> Ok
    | PatPair (_, x1_p1, x1_p2), PatPair (_, x2_p1, x2_p2) ->
        aux acc (x1_p1, x2_p1) >>= fun acc -> aux acc (x1_p2, x2_p2)
    | PatPair _, _ -> Error ()
    | PatConstructor (_, c1name, x1), PatConstructor (_, c2name, x2) ->
        if equal_string c1name c2name then aux acc (x1, x2) else Error ()
    | PatConstructor _, _ -> Error ()
  in
  aux StringMap.empty (from_pattern, to_pattern)

let rec rename_var_in_body ~(old_name : varname) ~(new_name : varname)
    (unifier : 'tag_p unifier) : 'tag_p unifier =
  Map.map unifier ~f:(Pattern.rename_var ~old_name ~new_name)

let rec apply_to_pattern ~(unifier : 'tag_p unifier) :
    'tag_p pattern -> 'tag_p pattern = function
  | PatName (_, xname, _) as p -> (
      match Map.find unifier xname with Some p_subst -> p_subst | None -> p)
  | PatPair (v, p1, p2) ->
      PatPair (v, apply_to_pattern ~unifier p1, apply_to_pattern ~unifier p2)
  | PatConstructor (v, cname, p) ->
      PatConstructor (v, cname, apply_to_pattern ~unifier p)

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

let rec apply_to_expr ~(convert_tag : 'tag_p -> 'tag_e)
    ~(unifier : 'tag_p unifier) :
    ('tag_e, 'tag_p) Ast.expr -> ('tag_e, 'tag_p) Ast.expr =
  let open Ast in
  function
  | UnitLit v -> UnitLit v
  | IntLit (v, i) -> IntLit (v, i)
  | Add (v, e1, e2) ->
      Add
        ( v,
          apply_to_expr ~convert_tag ~unifier e1,
          apply_to_expr ~convert_tag ~unifier e2 )
  | Neg (v, e) -> Neg (v, apply_to_expr ~convert_tag ~unifier e)
  | Subtr (v, e1, e2) ->
      Subtr
        ( v,
          apply_to_expr ~convert_tag ~unifier e1,
          apply_to_expr ~convert_tag ~unifier e2 )
  | Mult (v, e1, e2) ->
      Mult
        ( v,
          apply_to_expr ~convert_tag ~unifier e1,
          apply_to_expr ~convert_tag ~unifier e2 )
  | BoolLit (v, b) -> BoolLit (v, b)
  | BNot (v, e) -> BNot (v, apply_to_expr ~convert_tag ~unifier e)
  | BOr (v, e1, e2) ->
      BOr
        ( v,
          apply_to_expr ~convert_tag ~unifier e1,
          apply_to_expr ~convert_tag ~unifier e2 )
  | BAnd (v, e1, e2) ->
      BAnd
        ( v,
          apply_to_expr ~convert_tag ~unifier e1,
          apply_to_expr ~convert_tag ~unifier e2 )
  | Pair (v, e1, e2) ->
      Pair
        ( v,
          apply_to_expr ~convert_tag ~unifier e1,
          apply_to_expr ~convert_tag ~unifier e2 )
  | Eq (v, e1, e2) ->
      Eq
        ( v,
          apply_to_expr ~convert_tag ~unifier e1,
          apply_to_expr ~convert_tag ~unifier e2 )
  | Gt (v, e1, e2) ->
      Gt
        ( v,
          apply_to_expr ~convert_tag ~unifier e1,
          apply_to_expr ~convert_tag ~unifier e2 )
  | GtEq (v, e1, e2) ->
      GtEq
        ( v,
          apply_to_expr ~convert_tag ~unifier e1,
          apply_to_expr ~convert_tag ~unifier e2 )
  | Lt (v, e1, e2) ->
      Lt
        ( v,
          apply_to_expr ~convert_tag ~unifier e1,
          apply_to_expr ~convert_tag ~unifier e2 )
  | LtEq (v, e1, e2) ->
      LtEq
        ( v,
          apply_to_expr ~convert_tag ~unifier e1,
          apply_to_expr ~convert_tag ~unifier e2 )
  | If (v, e1, e2, e3) ->
      If
        ( v,
          apply_to_expr ~convert_tag ~unifier e1,
          apply_to_expr ~convert_tag ~unifier e2,
          apply_to_expr ~convert_tag ~unifier e3 )
  | Var (v, xname) -> (
      match Map.find unifier xname with
      | Some p_subst -> pattern_to_expr ~convert_tag p_subst
      | None -> Var (v, xname))
  | Let (v, x, e1, e2) ->
      Let
        ( v,
          x,
          apply_to_expr ~convert_tag ~unifier e1,
          apply_to_expr ~convert_tag ~unifier e2 )
  | App (v, e1, e2) ->
      App
        ( v,
          apply_to_expr ~convert_tag ~unifier e1,
          apply_to_expr ~convert_tag ~unifier e2 )
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
            (case_p, apply_to_expr ~convert_tag ~unifier:unifier' case_e))
          cases
      in
      Match (v, apply_to_expr ~convert_tag ~unifier e, cases')
  | Constructor (v, name, e) ->
      Constructor (v, name, apply_to_expr ~convert_tag ~unifier e)
