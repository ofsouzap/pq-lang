open Core
open Utils

(* TODO - stop using LispBuilder, just use Core.Sexp instead *)

module type LispBuilderSig = sig
  (** The type of a node in the builder *)
  type node =
    | Unit  (** The unit value *)
    | Atom of string  (** An atomic value *)
    | Op of string * node list  (** An operation with argument nodes *)
    | List of node list  (** A list of nodes *)
  [@@deriving sexp, equal]

  (** Build the source to a string *)
  val build : use_newlines:bool -> node list -> string

  (** Build the source to a human-readable string *)
  val build_hum : node list -> string
end

module LispBuilder : LispBuilderSig = struct
  type node =
    | Unit
    | Atom of string
    | Op of string * node list
    | List of node list
  [@@deriving sexp, equal]

  let rec build_node = function
    | Unit -> "()"
    | Atom s -> s
    | Op (op, []) -> sprintf "(%s)" op
    | Op (op, (_ :: _ as xs)) ->
        sprintf "(%s %s)" op
          (String.concat ~sep:" " (List.map ~f:build_node xs))
    | List xs ->
        sprintf "(%s)" (String.concat ~sep:" " (List.map ~f:build_node xs))

  let build ~(use_newlines : bool) (nodes : node list) : string =
    let sep = if use_newlines then "\n" else " " in
    String.concat ~sep (List.map ~f:build_node nodes)

  let rec node_to_sexp : node -> Sexp.t = function
    | Unit -> List []
    | Atom s -> Atom s
    | Op (op, nodes) -> List (Atom op :: List.map ~f:node_to_sexp nodes)
    | List nodes -> List (List.map ~f:node_to_sexp nodes)

  let build_hum (nodes : node list) =
    String.concat ~sep:"\n"
      (List.map ~f:(fun n -> node_to_sexp n |> Sexp.to_string_hum) nodes)
end

type expr_tag = { t : Vtype.t } [@@deriving sexp, equal]
type pattern_tag = { t : Vtype.t } [@@deriving sexp, equal]
type tag_pattern = pattern_tag Pattern.t [@@deriving sexp, equal]
type tag_expr = (expr_tag, pattern_tag) Expr.t [@@deriving sexp, equal]

let pattern_tag_to_expr_tag (v : pattern_tag) : expr_tag =
  ({ t = v.t } : expr_tag)

type tag_unifier = (expr_tag, pattern_tag) Unification.unifier
[@@deriving sexp, equal]

type tag_quotient_type_eqcons = (expr_tag, pattern_tag) QuotientType.eqcons
[@@deriving sexp, equal]

type tag_quotient_type = (expr_tag, pattern_tag) QuotientType.t
[@@deriving sexp, equal]

type tag_custom_type = (expr_tag, pattern_tag) CustomType.t
[@@deriving sexp, equal]

type tag_program = (expr_tag, pattern_tag) Program.t [@@deriving sexp, equal]

type quotient_typing_error =
  | QuotientConstraintCheckFailed
      (** A quotient equality constructor is violated within the code *)
  | SmtUnknownResult
      (** The SMT solver was unable to determine the validity of some of the
          input conditions *)
  | UnexpectedTrivialMatchCasePatternError
      (** The pattern for a case of a match construct was the trivial case
          (matching all variables, equivalent to just renaming a variable)
          unexpectedly and cannot be handled *)
  | PairTypeNotDefinedInState of Vtype.t * Vtype.t
  | UndefinedCustomTypeName of string
      (** A custom type of the given name was referenced but isn't defined *)
[@@deriving sexp, equal]

(** Generate a fresh variable name, given a set of the currently-defined names
*)
let generate_fresh_varname ?(seed_name : string option)
    (existing_names : StringSet.t) : Varname.t * StringSet.t =
  let name_base = Option.value seed_name ~default:"x" in
  let rec loop (i : int) : Varname.t =
    let candidate = sprintf "%s%d" name_base i in
    if Set.mem existing_names candidate then loop (i + 1) else candidate
  in
  let new_name = loop 0 in
  (new_name, Set.add existing_names new_name)

(** Provides functionality for flattened patterns in Expr expressions *)
module FlatPattern = struct
  type t =
    | FlatPatPair of
        pattern_tag
        * (pattern_tag * Varname.t * Vtype.t)
        * (pattern_tag * Varname.t * Vtype.t)
    | FlatPatConstructor of
        pattern_tag * string * (pattern_tag * Varname.t * Vtype.t)
  [@@deriving sexp, equal]

  type flat_pattern = t [@@deriving sexp, equal]

  type flat_expr =
    | UnitLit of expr_tag
    | IntLit of expr_tag * int
    | Add of expr_tag * flat_expr * flat_expr
    | Neg of expr_tag * flat_expr
    | Subtr of expr_tag * flat_expr * flat_expr
    | Mult of expr_tag * flat_expr * flat_expr
    | BoolLit of expr_tag * bool
    | BNot of expr_tag * flat_expr
    | BOr of expr_tag * flat_expr * flat_expr
    | BAnd of expr_tag * flat_expr * flat_expr
    | Pair of expr_tag * flat_expr * flat_expr
    | Eq of expr_tag * flat_expr * flat_expr
    | Gt of expr_tag * flat_expr * flat_expr
    | GtEq of expr_tag * flat_expr * flat_expr
    | Lt of expr_tag * flat_expr * flat_expr
    | LtEq of expr_tag * flat_expr * flat_expr
    | If of expr_tag * flat_expr * flat_expr * flat_expr
    | Var of expr_tag * string
    | Let of expr_tag * Varname.t * flat_expr * flat_expr
    | App of expr_tag * flat_expr * flat_expr
    | Match of
        expr_tag
        * flat_expr
        * Vtype.t
        * (flat_pattern * flat_expr) Nonempty_list.t
    | Constructor of expr_tag * string * flat_expr
  [@@deriving sexp, equal]

  type flat_top_level_defn = {
    recursive : bool;
    name : string;
    param : Varname.t * Vtype.t;
    return_t : Vtype.t;
    body : flat_expr;
  }
  [@@deriving sexp, equal]

  type flat_program = {
    custom_types : tag_custom_type list;
    top_level_defns : flat_top_level_defn list;
    e : flat_expr;
  }
  [@@deriving sexp, equal]

  let flat_pattern_node_val : flat_pattern -> pattern_tag = function
    | FlatPatPair (v, _, _) -> v
    | FlatPatConstructor (v, _, _) -> v

  let flat_node_val : flat_expr -> expr_tag = function
    | UnitLit v -> v
    | IntLit (v, _) -> v
    | Add (v, _, _) -> v
    | Neg (v, _) -> v
    | Subtr (v, _, _) -> v
    | Mult (v, _, _) -> v
    | BoolLit (v, _) -> v
    | BNot (v, _) -> v
    | BOr (v, _, _) -> v
    | BAnd (v, _, _) -> v
    | Pair (v, _, _) -> v
    | Eq (v, _, _) -> v
    | Gt (v, _, _) -> v
    | GtEq (v, _, _) -> v
    | Lt (v, _, _) -> v
    | LtEq (v, _, _) -> v
    | If (v, _, _, _) -> v
    | Var (v, _) -> v
    | Let (v, _, _, _) -> v
    | App (v, _, _) -> v
    | Match (v, _, _, _) -> v
    | Constructor (v, _, _) -> v

  (** Get the list of variables and their types that this pattern introduces to
      its case expression's variable context *)
  let defined_vars : t -> (Varname.t * Vtype.t) list = function
    | FlatPatPair (_, (_, x1name, x1type), (_, x2name, x2type)) ->
        [ (x1name, x1type); (x2name, x2type) ]
    | FlatPatConstructor (_, _, (_, xname, xtype)) -> [ (xname, xtype) ]

  let rec expr_rename_var ~(old_name : Varname.t) ~(new_name : Varname.t) =
    function
    | UnitLit _ as e -> e
    | IntLit _ as e -> e
    | BoolLit _ as e -> e
    | Add (v, e1, e2) ->
        Add
          ( v,
            expr_rename_var ~old_name ~new_name e1,
            expr_rename_var ~old_name ~new_name e2 )
    | Neg (v, e) -> Neg (v, expr_rename_var ~old_name ~new_name e)
    | Subtr (v, e1, e2) ->
        Subtr
          ( v,
            expr_rename_var ~old_name ~new_name e1,
            expr_rename_var ~old_name ~new_name e2 )
    | Mult (v, e1, e2) ->
        Mult
          ( v,
            expr_rename_var ~old_name ~new_name e1,
            expr_rename_var ~old_name ~new_name e2 )
    | BNot (v, e) -> BNot (v, expr_rename_var ~old_name ~new_name e)
    | BOr (v, e1, e2) ->
        BOr
          ( v,
            expr_rename_var ~old_name ~new_name e1,
            expr_rename_var ~old_name ~new_name e2 )
    | BAnd (v, e1, e2) ->
        BAnd
          ( v,
            expr_rename_var ~old_name ~new_name e1,
            expr_rename_var ~old_name ~new_name e2 )
    | Pair (v, e1, e2) ->
        Pair
          ( v,
            expr_rename_var ~old_name ~new_name e1,
            expr_rename_var ~old_name ~new_name e2 )
    | Eq (v, e1, e2) ->
        Eq
          ( v,
            expr_rename_var ~old_name ~new_name e1,
            expr_rename_var ~old_name ~new_name e2 )
    | Gt (v, e1, e2) ->
        Gt
          ( v,
            expr_rename_var ~old_name ~new_name e1,
            expr_rename_var ~old_name ~new_name e2 )
    | GtEq (v, e1, e2) ->
        GtEq
          ( v,
            expr_rename_var ~old_name ~new_name e1,
            expr_rename_var ~old_name ~new_name e2 )
    | Lt (v, e1, e2) ->
        Lt
          ( v,
            expr_rename_var ~old_name ~new_name e1,
            expr_rename_var ~old_name ~new_name e2 )
    | LtEq (v, e1, e2) ->
        LtEq
          ( v,
            expr_rename_var ~old_name ~new_name e1,
            expr_rename_var ~old_name ~new_name e2 )
    | If (v, e1, e2, e3) ->
        If
          ( v,
            expr_rename_var ~old_name ~new_name e1,
            expr_rename_var ~old_name ~new_name e2,
            expr_rename_var ~old_name ~new_name e3 )
    | Var (v, xname) ->
        if equal_string old_name xname then Var (v, new_name) else Var (v, xname)
    | Let (v, xname, e1, e2) ->
        Let
          ( v,
            xname,
            expr_rename_var ~old_name ~new_name e1,
            if equal_string xname old_name then e2
            else expr_rename_var ~old_name ~new_name e2 )
    | App (v, e1, e2) ->
        App
          ( v,
            expr_rename_var ~old_name ~new_name e1,
            expr_rename_var ~old_name ~new_name e2 )
    | Match (v, e, t2, cases) ->
        Match
          ( v,
            expr_rename_var ~old_name ~new_name e,
            t2,
            Nonempty_list.map cases ~f:(fun (case_p, case_e) ->
                ( case_p,
                  if
                    List.exists ~f:(equal_string old_name)
                      (defined_vars case_p |> List.map ~f:fst)
                  then case_e
                  else expr_rename_var ~old_name ~new_name case_e )) )
    | Constructor (v, name, e) ->
        Constructor (v, name, expr_rename_var ~old_name ~new_name e)

  (** Convert a flat pattern to a regular pattern *)
  let to_non_flat_pattern : t -> tag_pattern = function
    | FlatPatPair (v, (x1_v, x1_name, x1_t), (x2_v, x2_name, x2_t)) ->
        Pattern.PatPair
          (v, PatName (x1_v, x1_name, x1_t), PatName (x2_v, x2_name, x2_t))
    | FlatPatConstructor (v, c_name, (x1_v, x1_name, x1_t)) ->
        PatConstructor (v, c_name, PatName (x1_v, x1_name, x1_t))

  (** Flatten a single case of a Match node so that the pattern is a flat
      pattern, and modify the case expression to perform any subsequent matching
      as needed *)
  let flatten_case_pattern ~(existing_names : StringSet.t)
      ((p : tag_pattern), (e : flat_expr)) :
      (StringSet.t * flat_pattern * flat_expr, quotient_typing_error) Result.t =
    let open Result in
    match p with
    | PatName _ -> Error UnexpectedTrivialMatchCasePatternError
    | PatPair
        (v_pair, PatName (x1_v, x1_name, x1_t), PatName (x2_v, x2_name, x2_t))
      ->
        (* A flat pair pattern *)
        ( existing_names,
          FlatPatPair (v_pair, (x1_v, x1_name, x1_t), (x2_v, x2_name, x2_t)),
          e )
        |> Ok
    | PatConstructor (v_constructor, c_name, PatName (x_v, x_name, x_t)) ->
        (* A flat constructor pattern *)
        ( existing_names,
          FlatPatConstructor (v_constructor, c_name, (x_v, x_name, x_t)),
          e )
        |> Ok
    | _ ->
        failwith
          "TODO - the compound cases for flattening patterns haven't yet been \
           implemented"
  (*
    let outer_expr_type : Vtype.t = (flat_node_val e).t in
    match p with
    | PatName _ -> Error UnexpectedTrivialMatchCasePatternError
    | PatPair
        (v_pair, PatName (x1_v, x1_name, x1_t), PatName (x2_v, x2_name, x2_t))
      ->
        (* A flat pair pattern *)
        ( existing_names,
          FlatPatPair (v_pair, (x1_v, x1_name, x1_t), (x2_v, x2_name, x2_t)),
          e )
        |> Ok
    | PatPair (v, p1, p2) ->
        (* A compound pair pattern.

            ```
            match orig_e with
            | ({p1}, {p2}) -> e

            becomes

            match orig_e with
              | (x1, x2) ->
                ( match x1 with
                  | {flattened p1} ->
                    ( match x2 with
                      | {flattened p2} -> e
                    )
                )
            ``` *)
        let new_binding_name_1, existing_names =
          generate_fresh_varname ~seed_name:"fst" existing_names
        in
        let new_binding_name_2, existing_names =
          generate_fresh_varname ~seed_name:"snd" existing_names
        in
        let p1_t = (Pattern.Pattern.node_val p1).t in
        let p2_t = (Pattern.Pattern.node_val p2).t in
        flatten_case_pattern ~existing_names (p2, e)
        >>= fun (existing_names, flattened_p2_case_p, flattened_p2_case_e) ->
        flatten_case_pattern ~existing_names
          ( p1,
            Match
              ( { t = outer_expr_type },
                Var ({ t = p2_t }, new_binding_name_2),
                Nonempty_list.singleton
                  (flattened_p2_case_p, flattened_p2_case_e) ) )
        >>| fun (existing_names, flattened_p1_case_p, flattened_p1_case_e) ->
        ( existing_names,
          FlatPatPair
            ( v,
              ({ t = p1_t }, new_binding_name_1, p1_t),
              ({ t = p2_t }, new_binding_name_2, p2_t) ),
          Match
            ( { t = outer_expr_type },
              Var ({ t = p1_t }, new_binding_name_1),
              Nonempty_list.singleton (flattened_p1_case_p, flattened_p1_case_e)
            ) )
    | PatConstructor (v_constructor, c_name, PatName (x_v, x_name, x_t)) ->
        (* A flat constructor pattern *)
        ( existing_names,
          FlatPatConstructor (v_constructor, c_name, (x_v, x_name, x_t)),
          e )
        |> Ok
    | PatConstructor (v, c_name, p1) ->
        (* A compound constructor pattern

            ```
            match orig_e with
            | C ({p1}) -> e

            becomes

            match orig_e with
              | C x ->
                ( match x with
                  | {flattened p1} -> e
                )
            ``` *)
        let new_binding_name, existing_names =
          generate_fresh_varname ~seed_name:"val" existing_names
        in
        let p1_t = (Pattern.Pattern.node_val p1).t in
        flatten_case_pattern ~existing_names (p1, e)
        >>| fun (existing_names, flattened_p1_case_p, flattened_p1_case_e) ->
        ( existing_names,
          FlatPatConstructor (v, c_name, ({ t = p1_t }, new_binding_name, p1_t)),
          Match
            ( { t = outer_expr_type },
              Var ({ t = p1_t }, new_binding_name),
              Nonempty_list.singleton (flattened_p1_case_p, flattened_p1_case_e)
            ) ) *)

  (** Convert an Expr expression to a flat expression *)
  let rec of_expr ~(existing_names : StringSet.t) :
      tag_expr -> (StringSet.t * flat_expr, quotient_typing_error) Result.t =
    let open Result in
    let unop (recomb : StringSet.t -> flat_expr -> StringSet.t * flat_expr)
        ~(existing_names : StringSet.t) (e1 : tag_expr) :
        (StringSet.t * flat_expr, quotient_typing_error) Result.t =
      of_expr ~existing_names e1 >>= fun (existing_names, e1') ->
      Ok (recomb existing_names e1')
    in
    let binop
        (recomb :
          StringSet.t -> flat_expr -> flat_expr -> StringSet.t * flat_expr)
        ~(existing_names : StringSet.t) (e1 : tag_expr) (e2 : tag_expr) :
        (StringSet.t * flat_expr, quotient_typing_error) Result.t =
      of_expr ~existing_names e1 >>= fun (existing_names, e1') ->
      of_expr ~existing_names e2 >>= fun (existing_names, e2') ->
      Ok (recomb existing_names e1' e2')
    in
    function
    | UnitLit v -> Ok (existing_names, UnitLit v)
    | IntLit (v, x) -> Ok (existing_names, IntLit (v, x))
    | Add (v, e1, e2) ->
        binop
          (fun existing_names e1' e2' -> (existing_names, Add (v, e1', e2')))
          ~existing_names e1 e2
    | Neg (v, e) ->
        unop
          (fun existing_names e' -> (existing_names, Neg (v, e')))
          ~existing_names e
    | Subtr (v, e1, e2) ->
        binop
          (fun existing_names e1' e2' -> (existing_names, Subtr (v, e1', e2')))
          ~existing_names e1 e2
    | Mult (v, e1, e2) ->
        binop
          (fun existing_names e1' e2' -> (existing_names, Mult (v, e1', e2')))
          ~existing_names e1 e2
    | BoolLit (v, b) -> Ok (existing_names, BoolLit (v, b))
    | BNot (v, e) ->
        unop
          (fun existing_names e' -> (existing_names, BNot (v, e')))
          ~existing_names e
    | BOr (v, e1, e2) ->
        binop
          (fun existing_names e1' e2' -> (existing_names, BOr (v, e1', e2')))
          ~existing_names e1 e2
    | BAnd (v, e1, e2) ->
        binop
          (fun existing_names e1' e2' -> (existing_names, BAnd (v, e1', e2')))
          ~existing_names e1 e2
    | Pair (v, e1, e2) ->
        binop
          (fun existing_names e1' e2' -> (existing_names, Pair (v, e1', e2')))
          ~existing_names e1 e2
    | Eq (v, e1, e2) ->
        binop
          (fun existing_names e1' e2' -> (existing_names, Eq (v, e1', e2')))
          ~existing_names e1 e2
    | Gt (v, e1, e2) ->
        binop
          (fun existing_names e1' e2' -> (existing_names, Gt (v, e1', e2')))
          ~existing_names e1 e2
    | GtEq (v, e1, e2) ->
        binop
          (fun existing_names e1' e2' -> (existing_names, GtEq (v, e1', e2')))
          ~existing_names e1 e2
    | Lt (v, e1, e2) ->
        binop
          (fun existing_names e1' e2' -> (existing_names, Lt (v, e1', e2')))
          ~existing_names e1 e2
    | LtEq (v, e1, e2) ->
        binop
          (fun existing_names e1' e2' -> (existing_names, LtEq (v, e1', e2')))
          ~existing_names e1 e2
    | If (v, e1, e2, e3) ->
        of_expr ~existing_names e1 >>= fun (existing_names, e1') ->
        of_expr ~existing_names e2 >>= fun (existing_names, e2') ->
        of_expr ~existing_names e3 >>| fun (existing_names, e3') ->
        (existing_names, If (v, e1', e2', e3'))
    | Var (v, name) -> Ok (existing_names, Var (v, name))
    | Let (v, xname, e1, e2) ->
        binop
          (fun existing_names e1' e2' ->
            (existing_names, Let (v, xname, e1', e2')))
          ~existing_names e1 e2
    | App (v, e1, e2) ->
        binop
          (fun existing_names e1' e2' -> (existing_names, App (v, e1', e2')))
          ~existing_names e1 e2
    | Match (v, e, t2, cs) ->
        of_expr ~existing_names e >>= fun (existing_names, e') ->
        Nonempty_list.fold_result_consume_init ~init:existing_names
          ~f:(fun acc (p, e) ->
            let existing_names =
              match acc with
              | First existing_names -> existing_names
              | Second (existing_names, _) -> existing_names
            in
            of_expr ~existing_names e >>= fun (existing_names, e') ->
            flatten_case_pattern ~existing_names (p, e')
            >>= fun (existing_names, flat_p, flat_e) ->
            match acc with
            | First _ ->
                (existing_names, Nonempty_list.singleton (flat_p, flat_e)) |> Ok
            | Second (_, acc) ->
                (existing_names, Nonempty_list.cons (flat_p, flat_e) acc) |> Ok)
          cs
        >>= fun (existing_names, flat_cases_rev) ->
        (existing_names, Match (v, e', t2, Nonempty_list.rev flat_cases_rev))
        |> Ok
    | Constructor (v, name, e) ->
        unop
          (fun existing_names e' -> (existing_names, Constructor (v, name, e')))
          ~existing_names e

  (** Convert a flat Expr expression to a non-flat Expr expression *)
  let rec to_non_flat_expr : flat_expr -> tag_expr = function
    | UnitLit v -> UnitLit v
    | IntLit (v, n) -> IntLit (v, n)
    | Add (v, e1, e2) -> Add (v, to_non_flat_expr e1, to_non_flat_expr e2)
    | Neg (v, e) -> Neg (v, to_non_flat_expr e)
    | Subtr (v, e1, e2) -> Subtr (v, to_non_flat_expr e1, to_non_flat_expr e2)
    | Mult (v, e1, e2) -> Mult (v, to_non_flat_expr e1, to_non_flat_expr e2)
    | BoolLit (v, b) -> BoolLit (v, b)
    | BNot (v, e) -> BNot (v, to_non_flat_expr e)
    | BOr (v, e1, e2) -> BOr (v, to_non_flat_expr e1, to_non_flat_expr e2)
    | BAnd (v, e1, e2) -> BAnd (v, to_non_flat_expr e1, to_non_flat_expr e2)
    | Pair (v, e1, e2) -> Pair (v, to_non_flat_expr e1, to_non_flat_expr e2)
    | Eq (v, e1, e2) -> Eq (v, to_non_flat_expr e1, to_non_flat_expr e2)
    | Gt (v, e1, e2) -> Gt (v, to_non_flat_expr e1, to_non_flat_expr e2)
    | GtEq (v, e1, e2) -> GtEq (v, to_non_flat_expr e1, to_non_flat_expr e2)
    | Lt (v, e1, e2) -> Lt (v, to_non_flat_expr e1, to_non_flat_expr e2)
    | LtEq (v, e1, e2) -> LtEq (v, to_non_flat_expr e1, to_non_flat_expr e2)
    | If (v, e1, e2, e3) ->
        If (v, to_non_flat_expr e1, to_non_flat_expr e2, to_non_flat_expr e3)
    | Var (v, name) -> Var (v, name)
    | Let (v, xname, e1, e2) ->
        Let (v, xname, to_non_flat_expr e1, to_non_flat_expr e2)
    | App (v, e1, e2) -> App (v, to_non_flat_expr e1, to_non_flat_expr e2)
    | Match (v, e, t2, cases) ->
        Match
          ( v,
            to_non_flat_expr e,
            t2,
            Nonempty_list.map
              ~f:(fun (p, e) -> (to_non_flat_pattern p, to_non_flat_expr e))
              cases )
    | Constructor (v, name, e) -> Constructor (v, name, to_non_flat_expr e)

  (** Convert a program to a flat program *)
  let of_program ~(existing_names : StringSet.t) (prog : tag_program) :
      (StringSet.t * flat_program, quotient_typing_error) Result.t =
    let open Result in
    List.fold_result ~init:(existing_names, [])
      ~f:(fun
          (existing_names, acc_defns_rev)
          (defn : (expr_tag, pattern_tag) Program.top_level_defn)
        ->
        of_expr ~existing_names defn.body >>| fun (existing_names, body') ->
        ( existing_names,
          {
            recursive = defn.recursive;
            name = defn.name;
            param = defn.param;
            return_t = defn.return_t;
            body = body';
          }
          :: acc_defns_rev ))
      prog.top_level_defns
    >>= fun (existing_names, flat_defns_rev) ->
    of_expr ~existing_names prog.e >>| fun (existing_names, e') ->
    ( existing_names,
      {
        custom_types = prog.custom_types;
        top_level_defns = List.rev flat_defns_rev;
        e = e';
      } )
end

(** Provides integration with the SMT solver *)
module Smt = struct
  let custom_special_name x =
    "PQ-"
    ^ (function
        | `Var _ -> "Var-"
        | `QuotientEqBinding _ -> "QVar-"
        | `VariantType _ -> "VT-"
        | `VariantTypeConstructor _ -> "VTC-"
        | `VariantTypeConstructorAccessor _ -> "VTCA-"
        | `Function _ -> "Fun-"
        | `EqualityFunction _ -> "Eq-")
        x
    ^
    match x with
    | `Var name -> name
    | `QuotientEqBinding ((name : string), (side : [ `Pattern | `Expr ])) ->
        sprintf "%s-%s" name
          (match side with `Pattern -> "pat" | `Expr -> "expr")
    | `VariantType (vt_name : string) -> vt_name
    | `VariantTypeConstructor ((vt_name : string), (c_name : string)) ->
        vt_name ^ "-" ^ c_name
    | `VariantTypeConstructorAccessor ((vt_name : string), (c_name : string)) ->
        vt_name ^ "-" ^ c_name ^ "-val"
    | `Function (f_name : string) -> f_name
    | `EqualityFunction (qt_name : string) -> qt_name

  (** Provides a representation of a state that can be built into input for the
      solver *)
  module State = struct
    type var_defn = {
      name : Varname.t;
      kind :
        [ `NonRec of (Varname.t * Vtype.t) option | `Rec of Varname.t * Vtype.t ];
      return_t : Vtype.t;
      body : FlatPattern.flat_expr;
    }
    [@@deriving sexp, equal]

    type top_level_elem = VarDecl of Varname.t * Vtype.t | VarDefn of var_defn
    [@@deriving sexp, equal]

    module VtypePairMap = Map.Make (struct
      type t = Vtype.t * Vtype.t

      let compare ((t1_1, t1_2) : Vtype.t * Vtype.t)
          ((t2_1, t2_2) : Vtype.t * Vtype.t) : int =
        let first_comp = Vtype.compare t1_1 t2_1 in
        if first_comp = 0 then Vtype.compare t1_2 t2_2 else first_comp

      let t_of_sexp = pair_of_sexp Vtype.t_of_sexp Vtype.t_of_sexp
      let sexp_of_t = sexp_of_pair Vtype.sexp_of_t Vtype.sexp_of_t
    end)

    type variant_type_constructor_info = {
      name : string;
      accessor_name : string;
      t : Vtype.t;
    }
    [@@deriving sexp, equal]

    type variant_type_info = {
      name : string;
      constructors : variant_type_constructor_info list;
    }
    [@@deriving sexp, equal]

    type pair_type_info = {
      name : string;
      t : Vtype.t * Vtype.t;
      constructor_name : string;
      fst_accessor_name : string;
      snd_accessor_name : string;
    }
    [@@deriving sexp, equal]

    type t = {
      custom_types : tag_custom_type list;
          (** A list of the custom types defined. This isn't used for building
              the SMT formula, but is used for e.g. mapping quotient types to
              their root base type *)
      pair_types_defined : pair_type_info VtypePairMap.t;
          (** A mapping from all pair types that have been defined to info about
              them. This used to decide which possible instantiations of the
              pair type to create in the initial datatype declarations. Added
              automatically by module *)
      special_variant_types : LispBuilder.node list;
          (** These should be translated by: x -> "(declare-datatypes () x)".
              Created automatically in state initialization *)
      variant_types : variant_type_info list;
          (** The custom variant types defined. Added by module user *)
      declared_consts : (string * Vtype.t) list;
          (** Translated by: (xname, xtype) -> (declare-const xname xtype ).
              Created automatically in state initialization *)
      top_level_rev : top_level_elem list;
          (** The reversed list of top-level definitions of the program. Added
              by module user *)
    }
    [@@deriving sexp, equal]

    type state = t

    let vt_unit_name : string = custom_special_name (`VariantType "unit")

    let vt_unit_constructor_name : string =
      custom_special_name (`VariantTypeConstructor ("unit", "unit"))

    let vt_unit_val : string = custom_special_name (`Var "unit")

    let state_init (custom_types : tag_custom_type list) : t =
      {
        custom_types;
        pair_types_defined =
          (VtypePairMap.empty : pair_type_info VtypePairMap.t);
        special_variant_types =
          LispBuilder.
            [
              (* Unit type *)
              Op (vt_unit_name, [ Atom vt_unit_constructor_name ]);
            ];
        variant_types = [];
        declared_consts = [ (vt_unit_val, VTypeUnit) ];
        top_level_rev = [];
      }

    let rec find_root_base_type (state : t) (t : Vtype.t) :
        (Vtype.t, quotient_typing_error) Result.t =
      let open Result in
      match t with
      | VTypeCustom ct_name -> (
          List.find state.custom_types ~f:(fun x_ct ->
              equal_string (CustomType.name x_ct) ct_name)
          |> Result.of_option ~error:(UndefinedCustomTypeName ct_name)
          >>= function
          | VariantType (vt_name, _) -> Ok (Vtype.VTypeCustom vt_name)
          | CustomType.QuotientType qt ->
              find_root_base_type state (VTypeCustom qt.base_type_name))
      | _ -> Ok t

    let state_add_pair_type ((t1 : Vtype.t), (t2 : Vtype.t)) (state : t) :
        (t, quotient_typing_error) Result.t =
      let open Result in
      (* Make sure to only be using types for base variant types, not quotient types *)
      find_root_base_type state t1 >>= fun t1 ->
      find_root_base_type state t2 >>| fun t2 ->
      let pair_type_vt_name : Vtype.t * Vtype.t -> string =
        (* Generates an implicitly-unique name for the instantiation of the
            pair type for the specified two parameter types *)
        let rec aux ((t1 : Vtype.t), (t2 : Vtype.t)) : string =
          custom_special_name (`VariantType "pair")
          ^ sprintf "-+%s++%s+" (vtype_name t1) (vtype_name t2)
        and vtype_name : Vtype.t -> string = function
          | VTypeUnit -> "Unit"
          | VTypeInt -> "Int"
          | VTypeBool -> "Bool"
          | VTypePair (t1, t2) -> aux (t1, t2)
          | VTypeFun (t1, t2) ->
              sprintf "%s->%s" (vtype_name t1) (vtype_name t2)
          | VTypeCustom ct_name -> ct_name
        in
        aux
      in
      let vt_name : string = pair_type_vt_name (t1, t2) in
      let constructor_name : string =
        custom_special_name (`VariantTypeConstructor (vt_name, "pair"))
      in
      let accessor_name (side : [ `Fst | `Snd ]) : string =
        custom_special_name (`VariantTypeConstructorAccessor (vt_name, "pair"))
        ^ match side with `Fst -> "-fst" | `Snd -> "-snd"
      in
      let info =
        {
          name = vt_name;
          t = (t1, t2);
          constructor_name;
          fst_accessor_name = accessor_name `Fst;
          snd_accessor_name = accessor_name `Snd;
        }
      in
      if Map.mem state.pair_types_defined (t1, t2) then state
      else
        {
          state with
          pair_types_defined =
            Map.set state.pair_types_defined ~key:(t1, t2) ~data:info;
        }

    let state_get_pair_type_info ((t1 : Vtype.t), (t2 : Vtype.t)) (state : t) :
        (pair_type_info, quotient_typing_error) Result.t =
      let open Result in
      find_root_base_type state t1 >>= fun t1 ->
      find_root_base_type state t2 >>= fun t2 ->
      match Map.find state.pair_types_defined (t1, t2) with
      | Some info -> Ok info
      | None -> Error (PairTypeNotDefinedInState (t1, t2))

    (** Get the name of the special function that should be defined to compare
        two values of the specified type. This is only for quotient types *)
    let state_get_vtype_special_eq_fun_name (state : t) :
        Vtype.t -> string option = function
      | VTypeCustom ct_name ->
          if
            List.exists state.custom_types ~f:(function
              | VariantType _ -> false
              | CustomType.QuotientType qt -> equal_string qt.name ct_name)
          then Some (custom_special_name (`EqualityFunction ct_name))
          else None
      | _ -> None

    (** Add a variant type definition to the state *)
    let state_add_variant_type ((vt_name, vt_cs) : VariantType.t) (state : t) :
        t =
      let constructor_info ((c_name, c_t) : VariantType.constructor) :
          variant_type_constructor_info =
        {
          name = c_name;
          accessor_name =
            custom_special_name
              (`VariantTypeConstructorAccessor (vt_name, c_name));
          t = c_t;
        }
      in
      {
        state with
        variant_types =
          { name = vt_name; constructors = List.map ~f:constructor_info vt_cs }
          :: state.variant_types;
      }

    (** Add a variable declaration to the state *)
    let state_add_var_decl ((xname : Varname.t), (xtype : Vtype.t)) (state : t)
        : t =
      {
        state with
        top_level_rev = VarDecl (xname, xtype) :: state.top_level_rev;
      }

    (** Add a variable definition to the state *)
    let state_add_var_defn (defn : var_defn) (state : t) :
        (t, quotient_typing_error) Result.t =
      let open Result in
      let rec add_pair_types_used (state : state) :
          Vtype.t -> (state, quotient_typing_error) Result.t = function
        | VTypeUnit | VTypeInt | VTypeBool | VTypeCustom _ -> Ok state
        | VTypePair (t1, t2) ->
            state_add_pair_type (t1, t2) state >>= fun state ->
            add_pair_types_used state t1 >>= fun state ->
            add_pair_types_used state t2
        | VTypeFun (t1, t2) ->
            add_pair_types_used state t1 >>= fun state ->
            add_pair_types_used state t2
      in
      let rec search_add_pair_types_used (state : state) :
          FlatPattern.flat_expr -> (state, quotient_typing_error) Result.t =
        function
        | UnitLit v -> add_pair_types_used state v.t
        | IntLit (v, _) -> add_pair_types_used state v.t
        | Add (v, e1, e2) ->
            add_pair_types_used state v.t >>= fun state ->
            search_add_pair_types_used state e1 >>= fun state ->
            search_add_pair_types_used state e2
        | Neg (v, e1) ->
            add_pair_types_used state v.t >>= fun state ->
            search_add_pair_types_used state e1
        | Subtr (v, e1, e2) ->
            add_pair_types_used state v.t >>= fun state ->
            search_add_pair_types_used state e1 >>= fun state ->
            search_add_pair_types_used state e2
        | Mult (v, e1, e2) ->
            add_pair_types_used state v.t >>= fun state ->
            search_add_pair_types_used state e1 >>= fun state ->
            search_add_pair_types_used state e2
        | BoolLit (v, _) -> add_pair_types_used state v.t
        | BNot (v, e1) ->
            add_pair_types_used state v.t >>= fun state ->
            search_add_pair_types_used state e1
        | BOr (v, e1, e2) ->
            add_pair_types_used state v.t >>= fun state ->
            search_add_pair_types_used state e1 >>= fun state ->
            search_add_pair_types_used state e2
        | BAnd (v, e1, e2) ->
            add_pair_types_used state v.t >>= fun state ->
            search_add_pair_types_used state e1 >>= fun state ->
            search_add_pair_types_used state e2
        | Pair (v, e1, e2) ->
            add_pair_types_used state v.t >>= fun state ->
            search_add_pair_types_used state e1 >>= fun state ->
            search_add_pair_types_used state e2
        | Eq (v, e1, e2) ->
            add_pair_types_used state v.t >>= fun state ->
            search_add_pair_types_used state e1 >>= fun state ->
            search_add_pair_types_used state e2
        | Gt (v, e1, e2) ->
            add_pair_types_used state v.t >>= fun state ->
            search_add_pair_types_used state e1 >>= fun state ->
            search_add_pair_types_used state e2
        | GtEq (v, e1, e2) ->
            add_pair_types_used state v.t >>= fun state ->
            search_add_pair_types_used state e1 >>= fun state ->
            search_add_pair_types_used state e2
        | Lt (v, e1, e2) ->
            add_pair_types_used state v.t >>= fun state ->
            search_add_pair_types_used state e1 >>= fun state ->
            search_add_pair_types_used state e2
        | LtEq (v, e1, e2) ->
            add_pair_types_used state v.t >>= fun state ->
            search_add_pair_types_used state e1 >>= fun state ->
            search_add_pair_types_used state e2
        | If (v, e1, e2, e3) ->
            add_pair_types_used state v.t >>= fun state ->
            search_add_pair_types_used state e1 >>= fun state ->
            search_add_pair_types_used state e2 >>= fun state ->
            search_add_pair_types_used state e3
        | Var (v, _) -> add_pair_types_used state v.t
        | Let (v, _, e1, e2) ->
            add_pair_types_used state v.t >>= fun state ->
            search_add_pair_types_used state e1 >>= fun state ->
            search_add_pair_types_used state e2
        | App (v, e1, e2) ->
            add_pair_types_used state v.t >>= fun state ->
            search_add_pair_types_used state e1 >>= fun state ->
            search_add_pair_types_used state e2
        | Match (v, e1, t2, cases) ->
            add_pair_types_used state t2 >>= fun state ->
            add_pair_types_used state v.t >>= fun state ->
            search_add_pair_types_used state e1 >>= fun state ->
            Nonempty_list.fold_result
              ~f:(fun state (pat, e) ->
                let pat_t = FlatPattern.flat_pattern_node_val pat in
                add_pair_types_used state pat_t.t >>= fun state ->
                search_add_pair_types_used state e)
              ~init:state cases
        | Constructor (v, _, e1) ->
            add_pair_types_used state v.t >>= fun state ->
            search_add_pair_types_used state e1
      in
      (* Add all used pair types to the state *)
      search_add_pair_types_used state defn.body >>| fun state ->
      { state with top_level_rev = VarDefn defn :: state.top_level_rev }
  end

  module Assertion = struct
    type t =
      | Not of t
      | Eq of Vtype.t option * FlatPattern.flat_expr * FlatPattern.flat_expr
          (** Equality node. Optionally the types of the two expressions can be
              specified. This allows for equality lifting for quotient types *)

    type assertion = t

    module Builder = struct end
  end

  (** Provides building of components of an SMT formula into input for the
      solver *)
  module Builder = struct
    open State
    open Assertion

    let rec build_vtype (state : State.t) (t : Vtype.t) :
        (LispBuilder.node, quotient_typing_error) Result.t =
      let open Result in
      let open LispBuilder in
      (* Make sure to map the type to its root base type, in case it is a quotient type *)
      State.find_root_base_type state t >>= fun t ->
      match t with
      | VTypeUnit -> Ok (Atom vt_unit_name)
      | VTypeInt -> Ok (Atom "Int")
      | VTypeBool -> Ok (Atom "Bool")
      | VTypePair (t1, t2) ->
          state_get_pair_type_info (t1, t2) state >>| fun pair_type_info ->
          Atom pair_type_info.name
      | VTypeFun (t1, t2) ->
          build_vtype state t1 >>= fun t1_node ->
          build_vtype state t2 >>= fun t2_node ->
          Ok (Op ("Array", [ t1_node; t2_node ]))
      | VTypeCustom ct_name -> Ok (Atom ct_name)

    let rec build_expr ~(directly_callable_fun_names : StringSet.t)
        (state : State.t) :
        FlatPattern.flat_expr ->
        (LispBuilder.node, quotient_typing_error) Result.t =
      let open Result in
      let open LispBuilder in
      function
      | UnitLit _ -> Ok (Atom vt_unit_val)
      | IntLit (_, n) -> Ok (Atom (Int.to_string n))
      | BoolLit (_, b) -> Ok (Atom (Bool.to_string b))
      | Var (_, name) -> Ok (Atom name)
      | Add (_, e1, e2) ->
          build_expr ~directly_callable_fun_names state e1 >>= fun node1 ->
          build_expr ~directly_callable_fun_names state e2 >>= fun node2 ->
          Ok (Op ("+", [ node1; node2 ]))
      | Neg (_, e) ->
          build_expr ~directly_callable_fun_names state e >>= fun node ->
          Ok (Op ("-", [ node ]))
      | Subtr (_, e1, e2) ->
          build_expr ~directly_callable_fun_names state e1 >>= fun node1 ->
          build_expr ~directly_callable_fun_names state e2 >>= fun node2 ->
          Ok (Op ("-", [ node1; node2 ]))
      | Mult (_, e1, e2) ->
          build_expr ~directly_callable_fun_names state e1 >>= fun node1 ->
          build_expr ~directly_callable_fun_names state e2 >>= fun node2 ->
          Ok (Op ("*", [ node1; node2 ]))
      | BNot (_, e) ->
          build_expr ~directly_callable_fun_names state e >>= fun node ->
          Ok (Op ("not", [ node ]))
      | BOr (_, e1, e2) ->
          build_expr ~directly_callable_fun_names state e1 >>= fun node1 ->
          build_expr ~directly_callable_fun_names state e2 >>= fun node2 ->
          Ok (Op ("or", [ node1; node2 ]))
      | BAnd (_, e1, e2) ->
          build_expr ~directly_callable_fun_names state e1 >>= fun node1 ->
          build_expr ~directly_callable_fun_names state e2 >>= fun node2 ->
          Ok (Op ("and", [ node1; node2 ]))
      | Eq (_, e1, e2) ->
          build_expr ~directly_callable_fun_names state e1 >>= fun node1 ->
          build_expr ~directly_callable_fun_names state e2 >>= fun node2 ->
          Ok (Op ("=", [ node1; node2 ]))
      | Gt (_, e1, e2) ->
          build_expr ~directly_callable_fun_names state e1 >>= fun node1 ->
          build_expr ~directly_callable_fun_names state e2 >>= fun node2 ->
          Ok (Op (">", [ node1; node2 ]))
      | GtEq (_, e1, e2) ->
          build_expr ~directly_callable_fun_names state e1 >>= fun node1 ->
          build_expr ~directly_callable_fun_names state e2 >>= fun node2 ->
          Ok (Op (">=", [ node1; node2 ]))
      | Lt (_, e1, e2) ->
          build_expr ~directly_callable_fun_names state e1 >>= fun node1 ->
          build_expr ~directly_callable_fun_names state e2 >>= fun node2 ->
          Ok (Op ("<", [ node1; node2 ]))
      | LtEq (_, e1, e2) ->
          build_expr ~directly_callable_fun_names state e1 >>= fun node1 ->
          build_expr ~directly_callable_fun_names state e2 >>= fun node2 ->
          Ok (Op ("<=", [ node1; node2 ]))
      | If (_, e1, e2, e3) ->
          build_expr ~directly_callable_fun_names state e1 >>= fun node1 ->
          build_expr ~directly_callable_fun_names state e2 >>= fun node2 ->
          build_expr ~directly_callable_fun_names state e3 >>= fun node3 ->
          Ok (Op ("ite", [ node1; node2; node3 ]))
      | Let (_, xname, e1, e2) ->
          build_expr ~directly_callable_fun_names state e1 >>= fun node1 ->
          build_expr ~directly_callable_fun_names state e2 >>= fun node2 ->
          Ok (Op ("let", [ List [ List [ Atom xname; node1 ] ]; node2 ]))
      | Pair (_, e1, e2) ->
          let e1_t = (FlatPattern.flat_node_val e1).t in
          let e2_t = (FlatPattern.flat_node_val e2).t in
          state_get_pair_type_info (e1_t, e2_t) state >>= fun pair_type_info ->
          build_expr ~directly_callable_fun_names state e1 >>= fun e1' ->
          build_expr ~directly_callable_fun_names state e2 >>| fun e2' ->
          Op (pair_type_info.constructor_name, [ e1'; e2' ])
      | App (_, e1, e2) -> (
          build_expr ~directly_callable_fun_names state e1 >>= fun e1_node ->
          build_expr ~directly_callable_fun_names state e2 >>| fun e2_node ->
          let get_default_repr () = Op ("select", [ e1_node; e2_node ]) in
          match e1 with
          | Var (_, fname) ->
              if Set.mem directly_callable_fun_names fname then
                Op (fname, [ e2_node ])
              else get_default_repr ()
          | _ -> get_default_repr ())
      | Constructor (_, name, e) ->
          build_expr ~directly_callable_fun_names state e >>= fun node ->
          Ok (Op (name, [ node ]))
      | Match (_, e, _, cases) ->
          build_expr ~directly_callable_fun_names state e >>= fun e_node ->
          let build_case
              ((pat : FlatPattern.flat_pattern), (body : FlatPattern.flat_expr))
              : (node, quotient_typing_error) Result.t =
            build_expr ~directly_callable_fun_names state body
            >>= fun body_node ->
            match pat with
            | FlatPattern.FlatPatPair (_, (_, x1name, x1t), (_, x2name, x2t)) ->
                state_get_pair_type_info (x1t, x2t) state
                >>| fun pair_type_info ->
                List
                  [
                    Op
                      ( pair_type_info.constructor_name,
                        [ Atom x1name; Atom x2name ] );
                    body_node;
                  ]
            | FlatPattern.FlatPatConstructor (_, c_name, (_, x, _)) ->
                Ok (List [ List [ Atom c_name; Atom x ]; body_node ])
          in
          Nonempty_list.fold_result ~init:[]
            ~f:(fun acc case ->
              build_case case >>| fun case_node -> case_node :: acc)
            cases
          >>| List.rev
          >>| fun case_nodes -> Op ("match", e_node :: [ List case_nodes ])

    let build_top_level_elem (state : State.t) :
        top_level_elem -> (LispBuilder.node, quotient_typing_error) Result.t =
      let open Result in
      let open LispBuilder in
      function
      | VarDecl (xname, xtype) ->
          build_vtype state xtype >>= fun type_node ->
          Ok (Op ("declare-const", [ Atom xname; type_node ]))
      | VarDefn { name; kind; return_t; body } -> (
          match kind with
          | `NonRec None ->
              build_vtype state return_t >>= fun type_node ->
              Ok (Op ("declare-const", [ Atom name; type_node ]))
          | `NonRec (Some (param_name, param_t)) ->
              build_vtype state param_t >>= fun param_type_node ->
              build_vtype state return_t >>= fun return_type_node ->
              build_expr ~directly_callable_fun_names:StringSet.empty state body
              >>= fun body_node ->
              Ok
                (Op
                   ( "define-fun",
                     [
                       Atom name;
                       List [ List [ Atom param_name; param_type_node ] ];
                       return_type_node;
                       body_node;
                     ] ))
          | `Rec (param_name, param_t) ->
              build_vtype state param_t >>= fun param_type_node ->
              build_vtype state return_t >>= fun return_type_node ->
              build_expr ~directly_callable_fun_names:(StringSet.singleton name)
                state body
              >>= fun body_node ->
              Ok
                (Op
                   ( "define-fun-rec",
                     [
                       Atom name;
                       List [ List [ Atom param_name; param_type_node ] ];
                       return_type_node;
                       body_node;
                     ] )))

    let build_state ~(existing_names : StringSet.t) (state : State.t) :
        (StringSet.t * LispBuilder.node list, quotient_typing_error) Result.t =
      let open Result in
      let open LispBuilder in
      let build_datatype_decl () : (node, quotient_typing_error) Result.t =
        (* Pair type declarations *)
        List.map (Map.to_alist state.pair_types_defined)
          ~f:(fun ((t1, t2), pair_type_info) ->
            build_vtype state t1 >>= fun t1_node ->
            build_vtype state t2 >>| fun t2_node ->
            Op
              ( pair_type_info.name,
                [
                  Op
                    ( pair_type_info.constructor_name,
                      [
                        Op (pair_type_info.fst_accessor_name, [ t1_node ]);
                        Op (pair_type_info.snd_accessor_name, [ t2_node ]);
                      ] );
                ] ))
        |> Result.all
        >>= fun (pair_type_decls : node list) ->
        (* Special variant type declarations *)
        List.map state.special_variant_types ~f:(fun special_variant_type ->
            Ok special_variant_type)
        |> Result.all
        >>= fun (special_variant_type_decls : node list) ->
        List.map state.variant_types ~f:(fun vt_info ->
            (* Create the nodes for the constructors *)
            List.map vt_info.constructors ~f:(fun c_info ->
                build_vtype state c_info.t >>| fun t_node ->
                Op (c_info.name, [ Op (c_info.accessor_name, [ t_node ]) ]))
            |> Result.all
            >>| fun constructor_nodes ->
            (* Create the main variant type definition node *)
            Op (vt_info.name, constructor_nodes))
        |> Result.all
        >>| fun (variant_type_decls : node list) ->
        (* Combine all the datatype declarations into the single declare-datatypes node *)
        let datatype_decls =
          pair_type_decls @ special_variant_type_decls @ variant_type_decls
        in
        Op ("declare-datatypes", [ Unit; List datatype_decls ])
      in
      let build_consts_nodes () : (node list, quotient_typing_error) Result.t =
        List.fold_result ~init:[]
          ~f:(fun acc (xname, xtype) ->
            build_vtype state xtype >>| fun xtype_node ->
            Op ("declare-const", [ Atom xname; xtype_node ]) :: acc)
          state.declared_consts
        >>| List.rev
      in
      let build_lifted_eq_fun_nodes ~(existing_names : StringSet.t) () :
          (StringSet.t * node list, quotient_typing_error) Result.t =
        List.fold_result state.custom_types ~init:(existing_names, [])
          ~f:(fun (existing_names, (acc_rev : node list list)) -> function
          | VariantType _ -> Ok (existing_names, acc_rev)
          | CustomType.QuotientType qt ->
              let t = Vtype.VTypeCustom qt.name in
              let eq_fun_name =
                State.state_get_vtype_special_eq_fun_name state t
                |> Option.value_exn
                     ~message:"Quotient type has no special equality function"
              in
              build_vtype state t >>= fun t_node ->
              build_vtype state VTypeBool >>= fun bool_node ->
              (* Equality function definition node *)
              Op
                ( "declare-fun",
                  [ Atom eq_fun_name; List [ t_node; t_node ]; bool_node ] )
              |> fun decl_node ->
              (* Assertion nodes *)
              let create_assert_node (bindings : (Varname.t * Vtype.t) list)
                  (l : FlatPattern.flat_expr) (r : FlatPattern.flat_expr) :
                  (node, quotient_typing_error) Result.t =
                List.fold_result bindings ~init:([], [])
                  ~f:(fun (qt_names, acc_rev) (xname, xtype) ->
                    (match xtype with
                    | VTypeCustom ct_name ->
                        List.find_map state.custom_types ~f:(function
                          | VariantType _ -> None
                          | CustomType.QuotientType qt ->
                              if equal_string ct_name qt.name then Some qt
                              else None)
                    | _ -> None)
                    |> fun x_qt ->
                    build_vtype state xtype >>| fun xtype_node ->
                    match x_qt with
                    | Some x_qt ->
                        let xname_l =
                          custom_special_name
                            (`QuotientEqBinding (xname, `Pattern))
                        in
                        let xname_r =
                          custom_special_name
                            (`QuotientEqBinding (xname, `Expr))
                        in
                        ( (xname, x_qt) :: qt_names,
                          Op (xname_l, [ xtype_node ])
                          :: Op (xname_r, [ xtype_node ])
                          :: acc_rev )
                    | None -> (qt_names, Op (xname, [ xtype_node ]) :: acc_rev))
                >>=
                fun ( (qt_binding_names : (string * tag_quotient_type) list),
                      bindings_nodes_rev )
                ->
                let bindings_nodes = List.rev bindings_nodes_rev in
                let l, r =
                  List.fold qt_binding_names ~init:(l, r)
                    ~f:(fun (l, r) (xname, _) ->
                      ( FlatPattern.expr_rename_var ~old_name:xname
                          ~new_name:
                            (custom_special_name
                               (`QuotientEqBinding (xname, `Pattern)))
                          l,
                        FlatPattern.expr_rename_var ~old_name:xname
                          ~new_name:
                            (custom_special_name
                               (`QuotientEqBinding (xname, `Expr)))
                          r ))
                in
                build_expr ~directly_callable_fun_names:StringSet.empty state l
                >>= fun l_node ->
                build_expr ~directly_callable_fun_names:StringSet.empty state r
                >>= fun r_node ->
                (let eq_node = Op (eq_fun_name, [ l_node; r_node ]) in
                 List.fold qt_binding_names ~init:eq_node
                   ~f:(fun acc_node (xname, x_qt) ->
                     let xname_l =
                       custom_special_name
                         (`QuotientEqBinding (xname, `Pattern))
                     in
                     let xname_r =
                       custom_special_name (`QuotientEqBinding (xname, `Expr))
                     in
                     Op
                       ( "=>",
                         [
                           Op
                             ( State.state_get_vtype_special_eq_fun_name state
                                 (VTypeCustom x_qt.name)
                               |> Option.value_exn
                                    ~message:
                                      "Quotient type didn't have custom secial \
                                       equality function name",
                               [ Atom xname_l; Atom xname_r ] );
                           acc_node;
                         ] )))
                |> fun body_node ->
                Ok
                  (Op
                     ( "assert",
                       [ Op ("forall", [ List bindings_nodes; body_node ]) ] ))
              in
              (let arg_name = custom_special_name (`Var "x") in
               Op
                 ( "assert",
                   [
                     Op
                       ( "forall",
                         [
                           List [ List [ Atom arg_name; t_node ] ];
                           Op (eq_fun_name, [ Atom arg_name; Atom arg_name ]);
                         ] );
                   ] ))
              |> fun assert_node_base ->
              List.fold_result qt.eqconss ~init:(existing_names, [])
                ~f:(fun (existing_names, acc_rev) eqcons ->
                  fst eqcons.body
                  |> Expr.of_pattern ~convert_tag:pattern_tag_to_expr_tag
                  |> FlatPattern.of_expr ~existing_names
                  >>= fun (existing_names, flat_l) ->
                  snd eqcons.body |> FlatPattern.of_expr ~existing_names
                  >>= fun (existing_names, flat_r) ->
                  create_assert_node eqcons.bindings flat_l flat_r
                  >>| fun node -> (existing_names, node :: acc_rev))
              >>= fun (existing_names, assert_nodes_main_rev) ->
              let assert_nodes_main = List.rev assert_nodes_main_rev in
              (* Output *)
              [ decl_node; assert_node_base ] @ assert_nodes_main |> Ok
              >>= fun node -> Ok (existing_names, node :: acc_rev))
        >>| fun (existing_names, nodes_deep_rev) ->
        (existing_names, nodes_deep_rev |> List.rev |> List.concat)
      in
      let build_top_level_nodes () : (node list, quotient_typing_error) Result.t
          =
        (* Note that top_level_rev is reversed, but the folding here un-reverses it *)
        List.fold_result ~init:[]
          ~f:(fun acc elem ->
            build_top_level_elem state elem >>| fun elem_node ->
            elem_node :: acc)
          state.top_level_rev
      in
      build_datatype_decl () >>= fun (datatype_decl : node) ->
      build_lifted_eq_fun_nodes ~existing_names ()
      >>= fun (existing_names, (lifted_eq_fun_nodes : node list)) ->
      build_consts_nodes () >>= fun (declared_consts_nodes : node list) ->
      build_top_level_nodes () >>| fun (top_level_nodes : node list) ->
      ( existing_names,
        (datatype_decl :: declared_consts_nodes)
        @ lifted_eq_fun_nodes @ top_level_nodes )

    let build_assertion ~(state : State.t) :
        assertion -> (LispBuilder.node, quotient_typing_error) Result.t =
      let open Result in
      let open LispBuilder in
      let rec aux :
          assertion -> (LispBuilder.node, quotient_typing_error) Result.t =
        function
        | Not x -> aux x >>| fun x_node -> Op ("not", [ x_node ])
        | Eq (t_opt, e1, e2) ->
            build_expr ~directly_callable_fun_names:StringSet.empty state e1
            >>= fun e1_node ->
            build_expr ~directly_callable_fun_names:StringSet.empty state e2
            >>|
            let eq_fun_name =
              let default = "=" in
              match t_opt with
              | None -> default
              | Some t -> (
                  match State.state_get_vtype_special_eq_fun_name state t with
                  | None -> default
                  | Some eq_fun_name -> eq_fun_name)
            in
            fun e2_node -> Op (eq_fun_name, [ e1_node; e2_node ])
      in
      fun assertion ->
        aux assertion >>| fun assertion_node -> Op ("assert", [ assertion_node ])
  end

  type formula = LispBuilder.node list

  let create_formula ~(existing_names : StringSet.t) (state : State.t)
      (assertions : Assertion.t list) :
      (StringSet.t * formula, quotient_typing_error) Result.t =
    let open Result in
    Builder.build_state ~existing_names state
    >>= fun (existing_names, state_nodes) ->
    List.map assertions ~f:(Builder.build_assertion ~state) |> Result.all
    >>| fun assertions_nodes -> (existing_names, state_nodes @ assertions_nodes)

  (** Check the satisifability of a given formula *)
  let check_satisfiability (formula : formula) : [ `Sat | `Unsat | `Unknown ] =
    let smtlib_string = LispBuilder.build ~use_newlines:true formula in
    let ctx = Z3.mk_context [ ("model", "false") ] in
    let ast_vec = Z3.SMT.parse_smtlib2_string ctx smtlib_string [] [] [] [] in
    let solver = Z3.Solver.mk_solver ctx None in
    (* Add the parsed formula into the solver *)
    Z3.Solver.add solver (Z3.AST.ASTVector.to_expr_list ast_vec);
    match Z3.Solver.check solver [] with
    | Z3.Solver.SATISFIABLE -> `Sat
    | Z3.Solver.UNSATISFIABLE -> `Unsat
    | Z3.Solver.UNKNOWN -> `Unknown
end

let use_fresh_names_for_eqcons ~(existing_names : StringSet.t)
    (eqcons : tag_quotient_type_eqcons) : StringSet.t * tag_quotient_type_eqcons
    =
  List.fold ~init:(existing_names, [], [])
    ~f:(fun (existing_names, acc_map, acc_bindings_rev) (xname, xtype) ->
      let xname', existing_names =
        generate_fresh_varname ~seed_name:xname existing_names
      in
      ( Set.add existing_names xname',
        (xname, xname') :: acc_map,
        (xname', xtype) :: acc_bindings_rev ))
    eqcons.bindings
  |> fun (existing_names, renames_list, acc_bindings_rev) ->
  ( existing_names,
    QuotientType.
      {
        bindings = List.rev acc_bindings_rev;
        body =
          List.fold ~init:eqcons.body
            ~f:(fun (p, e) (old_name, new_name) ->
              ( Pattern.rename_var ~old_name ~new_name p,
                Expr.rename_var ~old_name ~new_name e ))
            renames_list;
      } )

let perform_quotient_match_check ~(existing_names : StringSet.t)
    ~(quotient_type : tag_quotient_type) ~(match_node_v : expr_tag)
    ~(match_t_out : Vtype.t) ~(cases : (tag_pattern * tag_expr) Nonempty_list.t)
    (state : Smt.State.t) : (StringSet.t, quotient_typing_error) Result.t =
  let open Result in
  let reform_match_with_arg (e1 : tag_expr) : tag_expr =
    Match (match_node_v, e1, match_t_out, cases)
  in
  Nonempty_list.fold_result cases ~init:existing_names
    ~f:(fun existing_names (case_p, case_e) ->
      (* Use fresh names for each of the eqconss (before finding unifiers) *)
      let existing_names, fresh_name_eqconss =
        List.fold ~init:(existing_names, [])
          ~f:(fun (existing_names, acc) eqcons ->
            let existing_names, eqcons' =
              use_fresh_names_for_eqcons ~existing_names eqcons
            in
            (existing_names, eqcons' :: acc))
          quotient_type.eqconss
        |> fun (existing_names, fresh_name_eqconss_rev) ->
        (existing_names, List.rev fresh_name_eqconss_rev)
      in
      (* Iterating through each case of the match *)
      List.fold_result ~init:existing_names
        (List.filter_map fresh_name_eqconss ~f:(fun eqcons ->
             Unification.simply_find_unifier
               ~bound_names_in_from:StringSet.empty
               ~from_expr:
                 (case_p |> Expr.of_pattern ~convert_tag:pattern_tag_to_expr_tag)
               ~to_expr:
                 (fst eqcons.body
                 |> Expr.of_pattern ~convert_tag:pattern_tag_to_expr_tag)
             |> function
             | Error () -> None
             | Ok unifier -> Some (unifier, eqcons)))
        ~f:(fun existing_names (expr_to_eqcons_unifier, eqcons) ->
          (* Iterating through matching eqconss of the case *)
          let state =
            (* Add the eqcons' bindings to the state *)
            List.fold ~init:state
              ~f:(fun state (xname, xtype) ->
                Smt.State.state_add_var_decl (xname, xtype) state)
              eqcons.bindings
          in
          let l =
            Unification.apply_to_expr ~unifier:expr_to_eqcons_unifier case_e
          in
          let r = reform_match_with_arg (snd eqcons.body) in
          FlatPattern.of_expr ~existing_names l
          >>= fun (existing_names, l_flat) ->
          FlatPattern.of_expr ~existing_names r
          >>= fun (existing_names, r_flat) ->
          (* Form the main assertion of the SMT check *)
          let assertion : Smt.Assertion.t =
            let open Smt.Assertion in
            (* Note that I use the negation as we are looking or validity, not satisifability:
                the condition must be necessarily true, so we check that the negation is unsatisfiable *)
            Not (Eq (Some match_t_out, l_flat, r_flat))
          in
          Smt.create_formula ~existing_names state [ assertion ]
          >>= fun (existing_names, formula) ->
          match Smt.check_satisfiability formula with
          | `Unsat -> Ok existing_names
          | `Sat -> Error QuotientConstraintCheckFailed
          | `Unknown -> Error SmtUnknownResult))

let rec check_expr ~(existing_names : StringSet.t)
    ~(quotient_types : tag_quotient_type list) (state : Smt.State.t)
    (orig_e : FlatPattern.flat_expr) :
    (StringSet.t, quotient_typing_error) Result.t =
  let open Result in
  let open Smt.State in
  match orig_e with
  | UnitLit _ | IntLit _ | BoolLit _ | Var _ -> Ok existing_names
  | Neg (_, e) -> check_expr ~existing_names ~quotient_types state e
  | BNot (_, e) -> check_expr ~existing_names ~quotient_types state e
  | Add (_, e1, e2)
  | Subtr (_, e1, e2)
  | Mult (_, e1, e2)
  | BOr (_, e1, e2)
  | BAnd (_, e1, e2)
  | Pair (_, e1, e2)
  | Eq (_, e1, e2)
  | Gt (_, e1, e2)
  | GtEq (_, e1, e2)
  | Lt (_, e1, e2)
  | LtEq (_, e1, e2) ->
      check_expr ~existing_names ~quotient_types state e1
      >>= fun existing_names ->
      check_expr ~existing_names ~quotient_types state e2
  | If (_, e1, e2, e3) ->
      check_expr ~existing_names ~quotient_types state e1
      >>= fun existing_names ->
      check_expr ~existing_names ~quotient_types state e2
      >>= fun existing_names ->
      check_expr ~existing_names ~quotient_types state e3
  | Let (_, xname, e1, e2) ->
      check_expr ~existing_names ~quotient_types state e1
      >>= fun existing_names ->
      let e1_type = (FlatPattern.flat_node_val e1).t in

      state_add_var_defn
        { name = xname; kind = `NonRec None; return_t = e1_type; body = e1 }
        state
      >>= fun state -> check_expr ~existing_names ~quotient_types state e2
  | App (_, e1, e2) ->
      check_expr ~existing_names ~quotient_types state e1
      >>= fun existing_names ->
      check_expr ~existing_names ~quotient_types state e2
  | Match (v, e1, t_out, cases) ->
      check_expr ~existing_names ~quotient_types state e1
      >>= fun existing_names ->
      let e1_t = (FlatPattern.flat_node_val e1).t in
      (match e1_t with
      | VTypeCustom ct_name -> (
          match
            List.find quotient_types ~f:(fun qt -> equal_string qt.name ct_name)
          with
          | Some qt ->
              let non_flat_cases =
                Nonempty_list.map
                  ~f:(fun (flat_p, flat_e) ->
                    FlatPattern.
                      (to_non_flat_pattern flat_p, to_non_flat_expr flat_e))
                  cases
              in
              perform_quotient_match_check ~existing_names ~quotient_type:qt
                ~match_node_v:v ~match_t_out:t_out ~cases:non_flat_cases state
          | None -> Ok existing_names)
      | _ -> Ok existing_names)
      >>= fun existing_names ->
      (* Check each case in turn *)
      Nonempty_list.fold_result ~init:existing_names
        ~f:(fun existing_names (p, e) ->
          let state' : Smt.State.t =
            (* Add the variables declared by the case pattern *)
            List.fold ~init:state
              ~f:(fun acc (xname, xtype) ->
                state_add_var_decl (xname, xtype) acc)
              (FlatPattern.defined_vars p)
          in
          (* Check the case expression, with the updated state *)
          check_expr ~existing_names ~quotient_types state' e)
        cases
  | Constructor (_, _, e) -> check_expr ~existing_names ~quotient_types state e

let check_program (prog : tag_program) : (unit, quotient_typing_error) Result.t
    =
  (* TODO - make this take a checked program (from the Typing module's TypeChecker functor) as parameter instead of just a program *)
  let open Result in
  let open Smt.State in
  let existing_names = Program.existing_names prog in
  FlatPattern.of_program ~existing_names prog
  >>= fun (existing_names, flat_prog) ->
  let quotient_types : tag_quotient_type list =
    List.filter_map
      ~f:(function QuotientType qt -> Some qt | _ -> None)
      flat_prog.custom_types
  in
  let state = Smt.State.state_init prog.custom_types in
  let state =
    (* Add the variant type definitions to the state *)
    List.fold ~init:state
      ~f:(fun state -> function
        | VariantType vt -> state_add_variant_type vt state
        | CustomType.QuotientType _ -> state)
      flat_prog.custom_types
  in
  (* Check the top-level definitions and add them to the state *)
  List.fold_result ~init:(existing_names, state)
    ~f:(fun (existing_names, state) defn ->
      (* Create the state used when checking the body of the function *)
      let defn_checking_state = state_add_var_decl defn.param state in
      (if defn.recursive then
         state_add_var_defn
           {
             name = defn.name;
             kind = `Rec defn.param;
             return_t = defn.return_t;
             body = defn.body;
           }
           defn_checking_state
       else Ok state)
      >>= fun defn_checking_state ->
      (* Check the body of the TLD *)
      check_expr ~existing_names ~quotient_types defn_checking_state defn.body
      >>= fun existing_names ->
      (* Return the resulting state after defining the TLD *)
      state_add_var_defn
        {
          name = defn.name;
          kind =
            (if defn.recursive then `Rec defn.param
             else `NonRec (Some defn.param));
          return_t = defn.return_t;
          body = defn.body;
        }
        state
      >>| fun state -> (existing_names, state))
    flat_prog.top_level_defns
  >>= fun (existing_names, state) ->
  (* Check the main body of the program with the accumulated state *)
  check_expr ~existing_names ~quotient_types state flat_prog.e >>| fun _ -> ()
