open Core
open Utils
module StdPattern = Pattern.StdPattern
module StdExpr = Expr.StdExpr
module StdProgram = Program.StdProgram

type 'a flat_pattern =
  | FlatPatName of 'a * string * Vtype.t
  | FlatPatPair of 'a * ('a * string * Vtype.t) * ('a * string * Vtype.t)
  | FlatPatConstructor of 'a * string * ('a * string * Vtype.t)
[@@deriving sexp, equal]

module M : Pattern.S with type 'a t = 'a flat_pattern = struct
  type 'a t = 'a flat_pattern [@@deriving sexp, equal]
  type 'a typed_t = (Vtype.t * 'a) t [@@deriving sexp, equal]
  type plain_t = unit t [@@deriving sexp, equal]

  let to_plain_t : 'a t -> plain_t = function
    | FlatPatName (_, name, t) -> FlatPatName ((), name, t)
    | FlatPatPair (_, (_, p1name, p1t), (_, p2name, p2t)) ->
        FlatPatPair ((), ((), p1name, p1t), ((), p2name, p2t))
    | FlatPatConstructor (_, cname, (_, p1name, p1t)) ->
        FlatPatConstructor ((), cname, ((), p1name, p1t))

  let node_val = function
    | FlatPatName (v, _, _) -> v
    | FlatPatPair (v, _, _) -> v
    | FlatPatConstructor (v, _, _) -> v

  let fmap ~(f : 'a -> 'b) : 'a t -> 'b t = function
    | FlatPatName (v, name, t) -> FlatPatName (f v, name, t)
    | FlatPatPair (v, (v1, p1name, p1t), (v2, p2name, p2t)) ->
        FlatPatPair (f v, (f v1, p1name, p1t), (f v2, p2name, p2t))
    | FlatPatConstructor (v, cname, (v1, p1name, p1t)) ->
        FlatPatConstructor (f v, cname, (f v1, p1name, p1t))

  let rename_var ~(old_name : Varname.t) ~(new_name : Varname.t) = function
    | FlatPatName (v, name, t) ->
        FlatPatName
          (v, (if equal_string name old_name then new_name else name), t)
    | FlatPatPair (v, (v1, p1name, p1t), (v2, p2name, p2t)) ->
        FlatPatPair
          ( v,
            ( v1,
              (if equal_string p1name old_name then new_name else p1name),
              p1t ),
            ( v2,
              (if equal_string p2name old_name then new_name else p2name),
              p2t ) )
    | FlatPatConstructor (v, cname, (v1, p1name, p1t)) ->
        FlatPatConstructor
          ( v,
            cname,
            ( v1,
              (if equal_string p1name old_name then new_name else p1name),
              p1t ) )

  let existing_names : 'a t -> StringSet.t = function
    | FlatPatName (_, name, _) -> StringSet.singleton name
    | FlatPatPair (_, (_, p1name, _), (_, p2name, _)) ->
        StringSet.of_list [ p1name; p2name ]
    | FlatPatConstructor (_, c_name, (_, p1name, _)) ->
        StringSet.of_list [ c_name; p1name ]

  let defined_vars : 'a t -> (Varname.t * Vtype.t) list = function
    | FlatPatName (_, name, t) -> [ (name, t) ]
    | FlatPatPair (_, (_, p1name, p1t), (_, p2name, p2t)) ->
        [ (p1name, p1t); (p2name, p2t) ]
    | FlatPatConstructor (_, _, (_, p1name, p1t)) -> [ (p1name, p1t) ]

  let to_source_code : 'a t -> string = function
    | FlatPatName (_, name, t) ->
        sprintf "%s : (%s)" name (Vtype.to_source_code t)
    | FlatPatPair (_, (_, p1name, p1t), (_, p2name, p2t)) ->
        sprintf "(%s : (%s)), (%s : (%s))" p1name (Vtype.to_source_code p1t)
          p2name (Vtype.to_source_code p2t)
    | FlatPatConstructor (_, cname, (_, p1name, p1t)) ->
        sprintf "%s (%s : (%s))" cname p1name (Vtype.to_source_code p1t)

  module QCheck_testing =
  functor
    (Tag : sig
       type t
     end)
    ->
    struct
      type this_t = Tag.t t * (string * Vtype.t) list
      type t = this_t

      type gen_options = {
        get_variant_type_constructors : string -> VariantType.constructor list;
        v_gen : Tag.t QCheck.Gen.t;
        t : Vtype.t;
      }

      type print_options = unit
      type shrink_options = unit
      type arb_options = gen_options

      let gen = failwith "Not implemented"
      let print = failwith "Not implemented"
      let shrink = failwith "Not implemented"
      let arbitrary = failwith "Not implemented"
    end
end

module FlatExpr : Expr.S with module Pattern = M = Expr.Make (M)

module FlatProgram :
  Program.S
    with module Pattern = M
     and module Expr = FlatExpr
     and module QuotientType = QuotientType.StdQuotientType
     and module CustomType = CustomType.StdCustomType =
  Program.Make (FlatExpr) (CustomType.StdCustomType)

let std_program_private_flag_to_flat_program_private_flag :
    StdProgram.private_flag -> FlatProgram.private_flag = function
  | StdProgram.Public -> Public
  | Private -> Private

type flattening_error =
  | UnexpectedTrivialMatchCasePatternError
  | CasePatternTypingMismatch
[@@deriving sexp, equal]

let to_std_pattern : 'a M.t -> 'a StdPattern.t = function
  | FlatPatName (v, name, t) -> PatName (v, name, t)
  | FlatPatPair (v, (x1_v, x1_name, x1_t), (x2_v, x2_name, x2_t)) ->
      PatPair (v, PatName (x1_v, x1_name, x1_t), PatName (x2_v, x2_name, x2_t))
  | FlatPatConstructor (v, c_name, (x1_v, x1_name, x1_t)) ->
      PatConstructor (v, c_name, PatName (x1_v, x1_name, x1_t))

(** Provides a representation a standard match construct that can be compmiled
    to a flat match construct *)
module MatchTree : sig
  type ('tag_e, 'tag_p) t

  val of_cases :
    ('tag_p StdPattern.t * ('tag_e, 'tag_p) FlatExpr.t) Nonempty_list.t ->
    (('tag_e, 'tag_p) t, flattening_error) Result.t

  (* val to_flat_cases :
      ('tag_e, 'tag_p) t ->
      ( ('tag_p M.t * ('tag_e, 'tag_p) FlatExpr.t) Nonempty_list.t,
        flattening_error )
      Result.t *)
end = struct
  type ('tag_e, 'tag_p) t =
    | Leaf of 'tag_p * string * Vtype.t * ('tag_e, 'tag_p) FlatExpr.t
        (** The leaf node of the tree, where we don't need to perform a match
            deconstruction *)
    | Pair of 'tag_p * string option * ('tag_e, 'tag_p) t * ('tag_e, 'tag_p) t
        (** Destructing a pair type into its constituent values *)
    | Variant of
        'tag_p
        * string option
        * ('tag_e, 'tag_p) FlatExpr.t option
        * ('tag_p * ('tag_e, 'tag_p) t) StringMap.t
        (** Destructing a variant type, where we must have a case for each of
            the constructors of the variant type. Holds a default expression and
            a mapping from constructors to sub-match trees *)
  [@@deriving sexp, equal]

  (** Add a case pattern to a match tree, possibly overwriting existing data for
      overlapping pattern data *)
  let rec add_case_pattern ~(case_e : ('tag_e, 'tag_p) FlatExpr.t)
      (case_p : 'tag_p StdPattern.t) (orig_tree : ('tag_e, 'tag_p) t option) :
      (('tag_e, 'tag_p) t, flattening_error) Result.t =
    let open Result in
    match (case_p, orig_tree) with
    | PatName (v, xname, xt), _ ->
        (* A named variable pattern overwrites existing subtrees *)
        Leaf (v, xname, xt, case_e) |> Ok
    | PatPair (v, p1, p2), None ->
        add_case_pattern ~case_e p1 None >>= fun t1 ->
        add_case_pattern ~case_e p2 None >>| fun t2 -> Pair (v, None, t1, t2)
    | PatPair (v, p1, p2), Some (Leaf (_, t_name, _, _)) ->
        (* A pair pattern can transform a leaf into a pair node *)
        add_case_pattern ~case_e p1 None >>= fun t1 ->
        add_case_pattern ~case_e p2 None >>| fun t2 ->
        Pair (v, Some t_name, t1, t2)
    | PatPair (v, p1, p2), Some (Pair (_, t_name_opt, t1, t2)) ->
        (* A pir pattern recurses into the two subtrees with the two subpatterns *)
        add_case_pattern ~case_e p1 (Some t1) >>= fun t1' ->
        add_case_pattern ~case_e p2 (Some t2) >>| fun t2' ->
        Pair (v, t_name_opt, t1', t2')
    | PatPair _, Some (Variant _) -> Error CasePatternTypingMismatch
    | PatConstructor (v, c_name, p1), None ->
        (* A constructor pattern can transform a leaf into a variant node *)
        add_case_pattern ~case_e p1 None >>| fun t1 ->
        Variant
          ( v,
            None,
            None,
            StringMap.singleton c_name (StdPattern.node_val p1, t1) )
    | PatConstructor (v, c_name, p1), Some (Leaf (_, t_name, _, t_e)) ->
        (* A constructor pattern can transform a leaf into a variant node *)
        add_case_pattern ~case_e p1 None >>| fun t1 ->
        Variant
          ( v,
            Some t_name,
            Some t_e,
            StringMap.singleton c_name (StdPattern.node_val p1, t1) )
    | PatConstructor _, Some (Pair _) -> Error CasePatternTypingMismatch
    | ( PatConstructor (v, c_name, p1),
        Some (Variant (_, t_name_opt, t_e_opt, t_map)) ) ->
        (* A constructor pattern can add a new case to an existing variant node *)
        let curr_subtree = Map.find t_map c_name |> Option.map ~f:snd in
        add_case_pattern ~case_e p1 curr_subtree >>| fun t1 ->
        Variant
          ( v,
            t_name_opt,
            t_e_opt,
            Map.set t_map ~key:c_name ~data:(StdPattern.node_val p1, t1) )

  let of_cases (cases : ('tag_p StdPattern.t * 'a) Nonempty_list.t) :
      (('tag_e, 'tag_p) t, flattening_error) Result.t =
    cases
    |>
    (* We reverse the cases so that earlier cases can overwrite later ones *)
    Nonempty_list.rev
    |>
    (* Then we fold to create a match tree *)
    Nonempty_list.fold_result_consume_init ~init:()
      ~f:(fun tree_either (case_p, case_e) ->
        let tree_opt =
          match tree_either with First () -> None | Second tree -> Some tree
        in
        add_case_pattern ~case_e case_p tree_opt)

  let rec to_flat_cases (match_tree : ('tag_e, 'tag_p) t) :
      ( ('tag_p M.t * ('tag_e, 'tag_p) FlatExpr.t) Nonempty_list.t,
        flattening_error )
      Result.t =
    let open Result in
    match match_tree with
    | Leaf (v, name, t, e) ->
        (FlatPatName (v, name, t), e) |> Nonempty_list.singleton |> Ok
    | Pair (v, t_name_opt, t1, t2) ->
        to_flat_cases t1 >>= fun t1' ->
        to_flat_cases t2 >>| fun t2' ->
        let t1_expr = FlatExpr.Match (v, _, _, t1') in
        let t2_expr = FlatExpr.Match (v, _, _, t2') in
        _
    | Variant (v, t_name_opt, default_e, t_map) -> _
end

(** Convert an Expr expression to a flat expression *)
let rec of_expr ~(existing_names : StringSet.t) :
    ('tag_e, 'tag_p) StdExpr.t ->
    (StringSet.t * ('tag_e, 'tag_p) FlatExpr.t, flattening_error) Result.t =
  let open Result in
  let unop
      (recomb :
        StringSet.t ->
        ('tag_e, 'tag_p) FlatExpr.t ->
        StringSet.t * ('tag_e, 'tag_p) FlatExpr.t)
      ~(existing_names : StringSet.t) (e1 : ('tag_e, 'tag_p) StdExpr.t) :
      (StringSet.t * ('tag_e, 'tag_p) FlatExpr.t, flattening_error) Result.t =
    of_expr ~existing_names e1 >>= fun (existing_names, e1') ->
    Ok (recomb existing_names e1')
  in
  let binop
      (recomb :
        StringSet.t ->
        ('tag_e, 'tag_p) FlatExpr.t ->
        ('tag_e, 'tag_p) FlatExpr.t ->
        StringSet.t * ('tag_e, 'tag_p) FlatExpr.t)
      ~(existing_names : StringSet.t) (e1 : ('tag_e, 'tag_p) StdExpr.t)
      (e2 : ('tag_e, 'tag_p) StdExpr.t) :
      (StringSet.t * ('tag_e, 'tag_p) FlatExpr.t, flattening_error) Result.t =
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
      (existing_names, FlatExpr.If (v, e1', e2', e3'))
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
  | Match (v, e, t2, cases) ->
      of_expr ~existing_names e >>= fun (existing_names, e') ->
      Nonempty_list.fold_result_consume_init cases ~init:existing_names
        ~f:(fun acc (p, e) ->
          let existing_names =
            match acc with
            | First existing_names -> existing_names
            | Second (existing_names, _) -> existing_names
          in
          of_expr ~existing_names e >>= fun (existing_names, e') ->
          match acc with
          | First _ -> Ok (existing_names, Nonempty_list.singleton (p, e'))
          | Second (_, acc_rev) ->
              Ok (existing_names, Nonempty_list.cons (p, e') acc_rev))
      >>= fun (existing_names, flat_expr_cases_rev) ->
      MatchTree.of_cases (Nonempty_list.rev flat_expr_cases_rev)
      |> Result.map_error ~f:_
      >>= fun match_tree ->
      MatchTree.to_flat_cases match_tree >>| fun flat_cases_rev ->
      ( existing_names,
        FlatExpr.Match (v, e', t2, Nonempty_list.rev flat_cases_rev) )
  | Constructor (v, name, e) ->
      unop
        (fun existing_names e' -> (existing_names, Constructor (v, name, e')))
        ~existing_names e

(** Convert a flat Expr expression to a non-flat Expr expression *)
let rec to_std_expr : ('tag_e, 'tag_p) FlatExpr.t -> ('tag_e, 'tag_p) StdExpr.t
    = function
  | UnitLit v -> UnitLit v
  | IntLit (v, n) -> IntLit (v, n)
  | Add (v, e1, e2) -> Add (v, to_std_expr e1, to_std_expr e2)
  | Neg (v, e) -> Neg (v, to_std_expr e)
  | Subtr (v, e1, e2) -> Subtr (v, to_std_expr e1, to_std_expr e2)
  | Mult (v, e1, e2) -> Mult (v, to_std_expr e1, to_std_expr e2)
  | BoolLit (v, b) -> BoolLit (v, b)
  | BNot (v, e) -> BNot (v, to_std_expr e)
  | BOr (v, e1, e2) -> BOr (v, to_std_expr e1, to_std_expr e2)
  | BAnd (v, e1, e2) -> BAnd (v, to_std_expr e1, to_std_expr e2)
  | Pair (v, e1, e2) -> Pair (v, to_std_expr e1, to_std_expr e2)
  | Eq (v, e1, e2) -> Eq (v, to_std_expr e1, to_std_expr e2)
  | Gt (v, e1, e2) -> Gt (v, to_std_expr e1, to_std_expr e2)
  | GtEq (v, e1, e2) -> GtEq (v, to_std_expr e1, to_std_expr e2)
  | Lt (v, e1, e2) -> Lt (v, to_std_expr e1, to_std_expr e2)
  | LtEq (v, e1, e2) -> LtEq (v, to_std_expr e1, to_std_expr e2)
  | If (v, e1, e2, e3) -> If (v, to_std_expr e1, to_std_expr e2, to_std_expr e3)
  | Var (v, name) -> Var (v, name)
  | Let (v, xname, e1, e2) -> Let (v, xname, to_std_expr e1, to_std_expr e2)
  | App (v, e1, e2) -> App (v, to_std_expr e1, to_std_expr e2)
  | Match (v, e, t2, cases) ->
      Match
        ( v,
          to_std_expr e,
          t2,
          Nonempty_list.map
            ~f:(fun (p, e) -> (to_std_pattern p, to_std_expr e))
            cases )
  | Constructor (v, name, e) -> Constructor (v, name, to_std_expr e)

(** Convert a program to a flat program *)
let of_program ~(existing_names : StringSet.t)
    (prog : ('tag_e, 'tag_p) StdProgram.t) :
    (StringSet.t * ('tag_e, 'tag_p) FlatProgram.t, flattening_error) Result.t =
  let open Result in
  List.fold_result ~init:(existing_names, [])
    ~f:(fun
        (existing_names, acc_defns_rev)
        (defn : ('tag_e, 'tag_p) StdProgram.top_level_defn)
      ->
      of_expr ~existing_names defn.body >>| fun (existing_names, body') ->
      ( existing_names,
        FlatProgram.
          {
            private_flag =
              defn.private_flag
              |> std_program_private_flag_to_flat_program_private_flag;
            recursive = defn.recursive;
            name = defn.name;
            param = defn.param;
            return_t = defn.return_t;
            body = body';
          }
        :: acc_defns_rev ))
    prog.top_level_defns
  >>= fun (existing_names, flat_defns_rev) ->
  (match prog.body with
  | None -> Ok (existing_names, None)
  | Some prog_body ->
      of_expr ~existing_names prog_body >>| fun (existing_names, body') ->
      (existing_names, Some body'))
  >>| fun (existing_names, prog_body') ->
  ( existing_names,
    FlatProgram.
      {
        custom_types =
          prog.custom_types
          |> List.map ~f:(fun { private_flag; ct } ->
                 {
                   private_flag =
                     std_program_private_flag_to_flat_program_private_flag
                       private_flag;
                   ct;
                 });
        top_level_defns = List.rev flat_defns_rev;
        body = prog_body';
      } )
