open Core
open Utils
module StdPattern = Pattern.StdPattern
module StdExpr = Expr.StdExpr
module StdProgram = Program.StdProgram

type 'a flat_pattern =
  | FlatPatPair of 'a * ('a * string * Vtype.t) * ('a * string * Vtype.t)
  | FlatPatConstructor of 'a * string * ('a * string * Vtype.t)
[@@deriving sexp, equal]

module M : Pattern.S with type 'a t = 'a flat_pattern = struct
  type 'a t = 'a flat_pattern [@@deriving sexp, equal]
  type 'a typed_t = (Vtype.t * 'a) t [@@deriving sexp, equal]
  type plain_t = unit t [@@deriving sexp, equal]

  let to_plain_pattern : 'a t -> plain_t = function
    | FlatPatPair (_, (_, p1name, p1t), (_, p2name, p2t)) ->
        FlatPatPair ((), ((), p1name, p1t), ((), p2name, p2t))
    | FlatPatConstructor (_, cname, (_, p1name, p1t)) ->
        FlatPatConstructor ((), cname, ((), p1name, p1t))

  let node_val = function
    | FlatPatPair (v, _, _) -> v
    | FlatPatConstructor (v, _, _) -> v

  let fmap ~(f : 'a -> 'b) : 'a t -> 'b t = function
    | FlatPatPair (v, (v1, p1name, p1t), (v2, p2name, p2t)) ->
        FlatPatPair (f v, (f v1, p1name, p1t), (f v2, p2name, p2t))
    | FlatPatConstructor (v, cname, (v1, p1name, p1t)) ->
        FlatPatConstructor (f v, cname, (f v1, p1name, p1t))

  let rename_var ~(old_name : Varname.t) ~(new_name : Varname.t) = function
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
    | FlatPatPair (_, (_, p1name, _), (_, p2name, _)) ->
        StringSet.of_list [ p1name; p2name ]
    | FlatPatConstructor (_, c_name, (_, p1name, _)) ->
        StringSet.of_list [ c_name; p1name ]

  let defined_vars : 'a t -> (Varname.t * Vtype.t) list = function
    | FlatPatPair (_, (_, p1name, p1t), (_, p2name, p2t)) ->
        [ (p1name, p1t); (p2name, p2t) ]
    | FlatPatConstructor (_, _, (_, p1name, p1t)) -> [ (p1name, p1t) ]

  let to_source_code : 'a t -> string = function
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

type flattening_error = UnexpectedTrivialMatchCasePatternError
[@@deriving sexp, equal]

let to_std_pattern : 'a M.t -> 'a StdPattern.t = function
  | FlatPatPair (v, (x1_v, x1_name, x1_t), (x2_v, x2_name, x2_t)) ->
      PatPair (v, PatName (x1_v, x1_name, x1_t), PatName (x2_v, x2_name, x2_t))
  | FlatPatConstructor (v, c_name, (x1_v, x1_name, x1_t)) ->
      PatConstructor (v, c_name, PatName (x1_v, x1_name, x1_t))

(** Flatten a single case of a Match node so that the pattern is a flat pattern,
    and modify the case expression to perform any subsequent matching as needed
*)
let flatten_case_pattern ~(existing_names : StringSet.t)
    ((p : 'tag_p StdPattern.t), (e : ('tag_e, 'tag_p) FlatExpr.t)) :
    ( StringSet.t * 'tag_p M.t * ('tag_e, 'tag_p) FlatExpr.t,
      flattening_error )
    Result.t =
  let open Result in
  match p with
  | PatName _ -> Error UnexpectedTrivialMatchCasePatternError
  | PatPair
      (v_pair, PatName (x1_v, x1_name, x1_t), PatName (x2_v, x2_name, x2_t)) ->
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
        let p1_t = (Pattern.node_val p1).t in
        let p2_t = (Pattern.node_val p2).t in
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
        let p1_t = (Pattern.node_val p1).t in
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
      ( existing_names,
        FlatExpr.Match (v, e', t2, Nonempty_list.rev flat_cases_rev) )
      |> Ok
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
  of_expr ~existing_names prog.e >>| fun (existing_names, e') ->
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
        e = e';
      } )
