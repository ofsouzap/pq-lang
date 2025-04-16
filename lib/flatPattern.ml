open Core
open Utils
module StdPattern = Pattern.StdPattern
module StdExpr = Expr.StdExpr
module StdProgram = Program.StdProgram
module StdTypeCtx = TypeChecker.TypeContext.StdSetTypeContext

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

type flattening_error = UnknownVariantConstructor of string
[@@deriving sexp, equal]

let to_std_pattern : 'a M.t -> 'a StdPattern.t = function
  | FlatPatName (v, name, t) -> PatName (v, name, t)
  | FlatPatPair (v, (x1_v, x1_name, x1_t), (x2_v, x2_name, x2_t)) ->
      PatPair (v, PatName (x1_v, x1_name, x1_t), PatName (x2_v, x2_name, x2_t))
  | FlatPatConstructor (v, c_name, (x1_v, x1_name, x1_t)) ->
      PatConstructor (v, c_name, PatName (x1_v, x1_name, x1_t))

(*** Provides functionality for flattening standard expressions to flat expressions *)
module ExprFlattener : sig
  val flatten_expr :
    existing_names:StringSet.t ->
    type_ctx:StdTypeCtx.t ->
    StdExpr.plain_typed_t ->
    (StringSet.t * FlatExpr.plain_typed_t, flattening_error) Result.t
end = struct
  (* The algorithm used is based on that described in Chapter 5 of Simon Peyton Jones' "The implementation of functional programming languages".
  Annotations are sometimes provided in the code for how each part relates the the algorithm described in the book *)

  type ('tag_e, 'tag_p) cases =
    | Names of
        ((('tag_p * Varname.t * Vtype.t) * 'tag_p StdPattern.t list)
        * ('tag_e, 'tag_p) StdExpr.t)
        Nonempty_list.t
    | Pairs of
        ((('tag_p * 'tag_p StdPattern.t * 'tag_p StdPattern.t)
         * 'tag_p StdPattern.t list)
        * ('tag_e, 'tag_p) StdExpr.t)
        Nonempty_list.t
    | Constructors of
        ((('tag_p * string * 'tag_p StdPattern.t) * 'tag_p StdPattern.t list)
        * ('tag_e, 'tag_p) StdExpr.t)
        Nonempty_list.t

  let partition_cases_rev (type tag_e tag_p)
      (case_queues : (tag_p StdPattern.t list * (tag_e, tag_p) StdExpr.t) list)
      : (tag_e, tag_p) cases list =
    (* This corresponds to the `partition` function in the book, but returns a reversed output as I am using the opposite-direction fold operation *)
    List.fold case_queues ~init:[] ~f:(fun acc (ps, e) ->
        let add_new = function
          | (Pattern.PatName (v, xname, xt), ps_ts), e ->
              Names (Nonempty_list.singleton (((v, xname, xt), ps_ts), e))
              :: acc
          | (Pattern.PatPair (v, p1, p2), ps_ts), e ->
              Pairs (Nonempty_list.singleton (((v, p1, p2), ps_ts), e)) :: acc
          | (Pattern.PatConstructor (v, cname, p1), ps_ts), e ->
              Constructors
                (Nonempty_list.singleton (((v, cname, p1), ps_ts), e))
              :: acc
        in
        match (ps, acc) with
        | Pattern.PatName (v, xname, xt) :: ps_ts, Names acc_h :: acc_ts ->
            Names (Nonempty_list.cons (((v, xname, xt), ps_ts), e) acc_h)
            :: acc_ts
        | Pattern.PatPair (v, p1, p2) :: ps_ts, Pairs acc_h :: acc_ts ->
            Pairs (Nonempty_list.cons (((v, p1, p2), ps_ts), e) acc_h) :: acc_ts
        | ( Pattern.PatConstructor (v, cname, p1) :: ps_ts,
            Constructors acc_h :: acc_ts ) ->
            Constructors (Nonempty_list.cons (((v, cname, p1), ps_ts), e) acc_h)
            :: acc_ts
        | [], _ ->
            (* This case shouldn't occur as this function should only be called with remaining arguments,
        and if there are remaining arguments then all case queues must be non-empty *)
            failwith "Erroneous empty case queue"
        | p_h :: ps_ts, _ -> add_new ((p_h, ps_ts), e))

  let flatten_expr ~(existing_names : StringSet.t) ~(type_ctx : StdTypeCtx.t) =
    let existing_names = ref existing_names in
    let generate_fresh_varname ?(seed_name : string option) () : Varname.t =
      let name_base = Option.value seed_name ~default:"x" in
      let rec loop (i : int) : Varname.t =
        let candidate = sprintf "%s%d" name_base i in
        if Set.mem !existing_names candidate then loop (i + 1) else candidate
      in
      let new_name = loop 0 in
      existing_names := Set.add !existing_names new_name;
      new_name
    in
    let rec compile_match ~(return_t : Vtype.t)
        (args : StdExpr.plain_typed_t list)
        (case_queues :
          (unit StdPattern.typed_t list * StdExpr.plain_typed_t) list)
        (def : FlatExpr.plain_typed_t) :
        (FlatExpr.plain_typed_t, flattening_error) Result.t =
      (* This corresponds to the `match` function in the book *)
      match args with
      | [] -> (
          match case_queues with
          | [] -> Ok def
          | (_, case_e) :: _ -> flatten_expr case_e)
      | args_h :: args_ts ->
          let args_nonempty = Nonempty_list.make (args_h, args_ts) in
          partition_cases_rev case_queues
          |> List.fold_result ~init:def ~f:(fun def -> function
               | Names case_queues ->
                   compile_names ~return_t args_nonempty case_queues def
               | Pairs case_queues ->
                   compile_pairs ~return_t args_nonempty case_queues def
               | Constructors case_queues ->
                   compile_constructors ~return_t args_nonempty case_queues def)
    and compile_names ~(return_t : Vtype.t) args case_queues def =
      (* This corresponds to the `matchVar` function in the book *)
      compile_match ~return_t (Nonempty_list.tail args)
        (List.map (Nonempty_list.to_list case_queues)
           ~f:(fun (((_, xname, _), case_queues_ts), case_e) ->
             ( case_queues_ts,
               StdExpr.subst ~varname:xname
                 ~sub:(fun _ -> Nonempty_list.head args)
                 case_e )))
        def
    and compile_pairs ~(return_t : Vtype.t) args case_queues def =
      (* This is inspired by the `matchVar` and `matchCon` functions in the book,
        but is adapted for my language where pair values are used instead of constructors of higher arities. *)
      _
    and compile_constructors ~(return_t : Vtype.t) args case_queues def =
      (* This corresponds to the `matchCon` function in the book *)
      let open Result in
      (* Find which variant type we are looking at *)
      case_queues |> Nonempty_list.head |> fst |> fun ((_, cname, _), _) ->
      StdTypeCtx.find_variant_type_with_constructor type_ctx cname
      |> Result.of_option ~error:(UnknownVariantConstructor cname)
      >>= fun ((vt_name, vt_cs), _) ->
      (* For each possible constructor of the variant type, create a case for the match *)
      let create_case_for_constructor ~variant_vtype ~cname ~ct
          ~(case_queues :
             ((unit StdPattern.typed_t * unit StdPattern.typed_t list)
             * StdExpr.plain_typed_t)
             list) :
          (unit M.typed_t * FlatExpr.plain_typed_t, flattening_error) Result.t =
        (* Create the fresh name for the argument, and the corresponding expr nodes *)
        let fresh_arg_name : Varname.t =
          generate_fresh_varname ~seed_name:(sprintf "%s_arg" vt_name) ()
        in
        let new_arg_std_node : StdExpr.plain_typed_t =
          StdExpr.Var ((ct, ()), fresh_arg_name)
        in
        (* Create the case expression for this case *)
        compile_match ~return_t
          (new_arg_std_node :: Nonempty_list.tail args)
          (List.map case_queues ~f:(fun ((p', ps_ts), case_e) ->
               (p' :: ps_ts, case_e)))
          def
        >>| fun flat_case_e ->
        (* Create the output case, consisting of the flat pattern and flat expression *)
        ( FlatPatConstructor
            ((variant_vtype, ()), cname, ((ct, ()), fresh_arg_name, ct)),
          flat_case_e )
      in
      Nonempty_list.map
        (vt_cs
       |>
       (* TODO - a variant type must have constructors. Maybe I should change in VariantType.t to disallow no constructors so this isn't necessary *)
       Nonempty_list.from_list_unsafe) ~f:(fun (cname, ct) ->
          create_case_for_constructor ~variant_vtype:(VTypeCustom vt_name)
            ~cname ~ct
            ~case_queues:
              (case_queues |> Nonempty_list.to_list
              |> List.filter_map ~f:(fun (((_, cname2, p1), ps), case_e) ->
                     if not (equal_string cname cname2) then None
                     else Some ((p1, ps), case_e))))
      |> Nonempty_list.result_all
      >>= fun flat_cases ->
      (* Flatten the argument *)
      flatten_expr (Nonempty_list.head args) >>= fun flat_arg ->
      (* Create the output flat match expression *)
      FlatExpr.Match ((return_t, ()), flat_arg, return_t, flat_cases) |> Ok
    and flatten_expr :
        StdExpr.plain_typed_t ->
        (FlatExpr.plain_typed_t, flattening_error) Result.t =
      let open Result in
      let unop
          (recomb : ('tag_e, 'tag_p) FlatExpr.t -> ('tag_e, 'tag_p) FlatExpr.t)
          (e1 : ('tag_e, 'tag_p) StdExpr.t) :
          (('tag_e, 'tag_p) FlatExpr.t, flattening_error) Result.t =
        flatten_expr e1 >>= fun e1' -> Ok (recomb e1')
      in
      let binop
          (recomb :
            ('tag_e, 'tag_p) FlatExpr.t ->
            ('tag_e, 'tag_p) FlatExpr.t ->
            ('tag_e, 'tag_p) FlatExpr.t) (e1 : ('tag_e, 'tag_p) StdExpr.t)
          (e2 : ('tag_e, 'tag_p) StdExpr.t) :
          (('tag_e, 'tag_p) FlatExpr.t, flattening_error) Result.t =
        flatten_expr e1 >>= fun e1' ->
        flatten_expr e2 >>= fun e2' -> Ok (recomb e1' e2')
      in
      function
      | UnitLit v -> Ok (UnitLit v)
      | IntLit (v, x) -> Ok (IntLit (v, x))
      | Add (v, e1, e2) -> binop (fun e1' e2' -> Add (v, e1', e2')) e1 e2
      | Neg (v, e) -> unop (fun e' -> Neg (v, e')) e
      | Subtr (v, e1, e2) -> binop (fun e1' e2' -> Subtr (v, e1', e2')) e1 e2
      | Mult (v, e1, e2) -> binop (fun e1' e2' -> Mult (v, e1', e2')) e1 e2
      | BoolLit (v, b) -> Ok (BoolLit (v, b))
      | BNot (v, e) -> unop (fun e' -> BNot (v, e')) e
      | BOr (v, e1, e2) -> binop (fun e1' e2' -> BOr (v, e1', e2')) e1 e2
      | BAnd (v, e1, e2) -> binop (fun e1' e2' -> BAnd (v, e1', e2')) e1 e2
      | Pair (v, e1, e2) -> binop (fun e1' e2' -> Pair (v, e1', e2')) e1 e2
      | Eq (v, e1, e2) -> binop (fun e1' e2' -> Eq (v, e1', e2')) e1 e2
      | Gt (v, e1, e2) -> binop (fun e1' e2' -> Gt (v, e1', e2')) e1 e2
      | GtEq (v, e1, e2) -> binop (fun e1' e2' -> GtEq (v, e1', e2')) e1 e2
      | Lt (v, e1, e2) -> binop (fun e1' e2' -> Lt (v, e1', e2')) e1 e2
      | LtEq (v, e1, e2) -> binop (fun e1' e2' -> LtEq (v, e1', e2')) e1 e2
      | If (v, e1, e2, e3) ->
          flatten_expr e1 >>= fun e1' ->
          flatten_expr e2 >>= fun e2' ->
          flatten_expr e3 >>| fun e3' -> FlatExpr.If (v, e1', e2', e3')
      | Var (v, name) -> Ok (Var (v, name))
      | Let (v, xname, e1, e2) ->
          binop (fun e1' e2' -> Let (v, xname, e1', e2')) e1 e2
      | App (v, e1, e2) -> binop (fun e1' e2' -> App (v, e1', e2')) e1 e2
      | Match (v, e, t2, cases) ->
          flatten_expr e >>= fun e' ->
          Nonempty_list.fold_result_consume_init cases ~init:()
            ~f:(fun acc (p, e) ->
              flatten_expr e >>= fun e' ->
              match acc with
              | First () -> Ok (Nonempty_list.singleton (p, e'))
              | Second acc_rev -> Ok (Nonempty_list.cons (p, e') acc_rev))
          >>= fun flat_expr_cases_rev ->
          (fun _ -> failwith "TODO") (Nonempty_list.rev flat_expr_cases_rev)
          |> Result.map_error ~f:(failwith "TODO")
          >>= fun match_tree ->
          (fun _ -> failwith "TODO") match_tree >>| fun flat_cases_rev ->
          FlatExpr.Match (v, e', t2, Nonempty_list.rev flat_cases_rev)
      | Constructor (v, name, e) -> unop (fun e' -> Constructor (v, name, e')) e
    in
    fun orig_e ->
      Result.(flatten_expr orig_e >>| fun res -> (!existing_names, res))
end

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
      ExprFlattener.flatten_expr ~existing_names ~type_ctx:_ defn.body
      >>| fun (existing_names, body') ->
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
      ExprFlattener.flatten_expr ~existing_names ~type_ctx:_ prog_body
      >>| fun (existing_names, body') -> (existing_names, Some body'))
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
