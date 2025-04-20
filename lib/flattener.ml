open Core
open Utils
module StdPattern = Pattern.StdPattern
module StdExpr = Expr.StdExpr
module StdProgram = Program.StdProgram
module FlatExpr = FlatPattern.FlatExpr
module FlatProgram = FlatPattern.FlatProgram

let std_program_private_flag_to_flat_program_private_flag :
    StdProgram.private_flag -> FlatProgram.private_flag = function
  | StdProgram.Public -> Public
  | Private -> Private

type flattening_error =
  | UnknownVariantConstructor of string
  | NoDefaultCaseForMatchBranch of StdExpr.plain_t * (Varname.t * Vtype.t) list
[@@deriving sexp, equal]

let print_flattening_error : flattening_error -> string = function
  | UnknownVariantConstructor cname ->
      sprintf "Unknown variant constructor: %s" cname
  | NoDefaultCaseForMatchBranch (match_expr, state) ->
      sprintf
        "No default case for match branch for variables, [%s], for match \
         statement:\n\
         %s"
        (state
        |> List.map ~f:(fun (xname, xt) ->
               sprintf "(%s : %s)" xname (Vtype.to_source_code xt))
        |> String.concat ~sep:", ")
        (StdExpr.to_source_code ~use_newlines:true match_expr)

module type S = sig
  module TypeChecker :
    TypeChecker.S
      with module Pattern = Pattern.StdPattern
       and module Expr = Expr.StdExpr
       and module Program = Program.StdProgram

  val flatten_expr :
    existing_names:StringSet.t ->
    type_ctx:TypeChecker.TypeCtx.t ->
    ('tag, 'tag) Expr.StdExpr.typed_t ->
    ( StringSet.t * ('tag, 'tag) FlatPattern.FlatExpr.typed_t,
      flattening_error )
    Result.t

  val flatten_program :
    existing_names:StringSet.t ->
    type_ctx:TypeChecker.TypeCtx.t ->
    ('tag, 'tag) Program.StdProgram.typed_t ->
    ( StringSet.t * ('tag, 'tag) FlatPattern.FlatProgram.typed_t,
      flattening_error )
    Result.t

  val flatten_typed_program :
    existing_names:StringSet.t ->
    ('tag, 'tag) TypeChecker.typed_program ->
    ( StringSet.t * ('tag, 'tag) FlatPattern.FlatProgram.typed_t,
      flattening_error )
    Result.t
end

module Make
    (TypeChecker :
      TypeChecker.S
        with module Pattern = Pattern.StdPattern
         and module Expr = Expr.StdExpr
         and module Program = Program.StdProgram) :
  S with module TypeChecker = TypeChecker = struct
  module TypeChecker = TypeChecker
  module TypeCtx = TypeChecker.TypeCtx

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

  let partition_cases_rev (type tag)
      (case_queues : (tag StdPattern.t list * (tag, tag) StdExpr.t) list) :
      (tag, tag) cases list =
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
            Names (Nonempty_list.append_one acc_h (((v, xname, xt), ps_ts), e))
            :: acc_ts
        | Pattern.PatPair (v, p1, p2) :: ps_ts, Pairs acc_h :: acc_ts ->
            Pairs (Nonempty_list.append_one acc_h (((v, p1, p2), ps_ts), e))
            :: acc_ts
        | ( Pattern.PatConstructor (v, cname, p1) :: ps_ts,
            Constructors acc_h :: acc_ts ) ->
            Constructors
              (Nonempty_list.append_one acc_h (((v, cname, p1), ps_ts), e))
            :: acc_ts
        | [], _ ->
            (* This case shouldn't occur as this function should only be called with remaining arguments,
        and if there are remaining arguments then all case queues must be non-empty *)
            failwith "Erroneous empty case queue"
        | p_h :: ps_ts, _ -> add_new ((p_h, ps_ts), e))

  type ('tag_e, 'tag_p) holed_flat_match_case_val =
    | Hole
    | Leaf of ('tag_e, 'tag_p) StdExpr.typed_t
    | Next of ('tag_e, 'tag_p) holed_flat_match_repr

  and ('tag_e, 'tag_p) holed_flat_match_repr =
    'tag_e
    * ('tag_e * Varname.t * Vtype.t)
    * Vtype.t
    * ('tag_p FlatPattern.M.typed_t
      * ('tag_e, 'tag_p) holed_flat_match_case_val)
      Nonempty_list.t

  type ('tag_e, 'tag_p) filled_flat_match_case_val =
    | Leaf of ('tag_e, 'tag_p) StdExpr.typed_t
    | Next of ('tag_e, 'tag_p) filled_flat_match_repr

  and ('tag_e, 'tag_p) filled_flat_match_repr =
    'tag_e
    * ('tag_e * Varname.t * Vtype.t)
    * Vtype.t
    * ('tag_p FlatPattern.M.typed_t
      * ('tag_e, 'tag_p) filled_flat_match_case_val)
      Nonempty_list.t

  let generate_fresh_varname_from ?(seed_name : string option)
      (existing_names : StringSet.t) : StringSet.t * Varname.t =
    let name_base = Option.value seed_name ~default:"x" in
    let rec loop (i : int) : Varname.t =
      let candidate = sprintf "%s%d" name_base i in
      if Set.mem existing_names candidate then loop (i + 1) else candidate
    in
    let new_name = loop 0 in
    let existing_names = Set.add existing_names new_name in
    (existing_names, new_name)

  let compile_match_to_repr (type tag) ~(existing_names : StringSet.t)
      ~(type_ctx : TypeCtx.t) :
      return_t:Vtype.t ->
      (Varname.t * Vtype.t) list ->
      (tag StdPattern.typed_t list * (tag, tag) StdExpr.typed_t) list ->
      ( StringSet.t * (tag, tag) holed_flat_match_case_val,
        flattening_error )
      Result.t =
    let existing_names = ref existing_names in
    let generate_fresh_varname ?(seed_name : string option) () : Varname.t =
      let new_existing_names, new_name =
        generate_fresh_varname_from ?seed_name !existing_names
      in
      existing_names := new_existing_names;
      new_name
    in
    let create_std_arg_node ~tag ((xname : Varname.t), (xt : Vtype.t)) :
        (tag, tag) StdExpr.typed_t =
      StdExpr.Var ((xt, tag), xname)
    in
    let rec compile_match ~(return_t : Vtype.t)
        (args : (Varname.t * Vtype.t) list)
        (case_queues :
          (tag StdPattern.typed_t list * (tag, tag) StdExpr.typed_t) list)
        (def : (tag, tag) holed_flat_match_case_val option) :
        ((tag, tag) holed_flat_match_case_val, flattening_error) Result.t =
      (* This corresponds to the `match` function in the book *)
      match args with
      | [] -> (
          match (case_queues, def) with
          | (_, case_e) :: _, _ -> Leaf case_e |> Ok
          | [], None -> Hole |> Ok
          | [], Some def -> Ok def)
      | args_h :: args_ts -> (
          let args_nonempty = Nonempty_list.make (args_h, args_ts) in
          match (partition_cases_rev case_queues, def) with
          | [], Some def -> Ok def
          | [], None -> Hole |> Ok
          | partitions_rev_h :: partitions_rev_ts, _ ->
              Nonempty_list.fold_result_consume_init
                (Nonempty_list.make (partitions_rev_h, partitions_rev_ts))
                ~init:def
                ~f:(fun def ->
                  let def =
                    match def with
                    | First def_opt -> def_opt
                    | Second def -> Some def
                  in
                  function
                  | Names case_queues ->
                      compile_names ~return_t args_nonempty case_queues def
                  | Pairs case_queues ->
                      compile_pairs ~return_t args_nonempty case_queues def
                  | Constructors case_queues ->
                      compile_constructors ~return_t args_nonempty case_queues
                        def))
    and compile_names ~(return_t : Vtype.t) args case_queues def =
      (* This corresponds to the `matchVar` function in the book *)
      compile_match ~return_t (Nonempty_list.tail args)
        (List.map (Nonempty_list.to_list case_queues)
           ~f:(fun ((((_, tag), xname, _), case_queues_ts), case_e) ->
             ( case_queues_ts,
               StdExpr.subst ~varname:xname
                 ~sub:(fun _ ->
                   Nonempty_list.head args |> create_std_arg_node ~tag)
                 case_e )))
        def
    and compile_pairs ~(return_t : Vtype.t) (curr_arg, arg_ts) case_queues def =
      (* This is inspired by the `matchVar` and `matchCon` functions in the book,
        but is adapted for my language where pair values are used instead of constructors of higher arities. *)
      let open Result in
      let tag, (t1, t2) =
        case_queues |> Nonempty_list.head |> fst |> fun (((t, tag), _, _), _) ->
        ( tag,
          match t with
          | Vtype.VTypePair (t1, t2) -> (t1, t2)
          | _ -> failwith "Expected a pair type" )
      in
      let pair_t = Vtype.VTypePair (t1, t2) in
      (* Create the case matching on the pair *)
      let create_case ~(tag : tag)
          ~(case_queues :
             ((((Vtype.t * tag)
               * tag StdPattern.typed_t
               * tag StdPattern.typed_t)
              * tag StdPattern.typed_t list)
             * (tag, tag) StdExpr.typed_t)
             list) :
          ( tag FlatPattern.M.typed_t * (tag, tag) holed_flat_match_case_val,
            flattening_error )
          Result.t =
        let fresh_arg_names : Varname.t * Varname.t =
          ( generate_fresh_varname ~seed_name:"l" (),
            generate_fresh_varname ~seed_name:"r" () )
        in
        compile_match ~return_t
          ((fst fresh_arg_names, t1) :: (snd fresh_arg_names, t2) :: arg_ts)
          (List.map case_queues ~f:(fun (((_, p1, p2), ps_ts), case_e) ->
               (p1 :: p2 :: ps_ts, case_e)))
          def
        >>= fun flat_case_val ->
        ( FlatPattern.FlatPatPair
            ( (pair_t, tag),
              ((t1, tag), fst fresh_arg_names, t1),
              ((t2, tag), snd fresh_arg_names, t2) ),
          flat_case_val )
        |> Ok
      in
      create_case ~tag ~case_queues:(Nonempty_list.to_list case_queues)
      >>= fun flat_case ->
      flat_case |> Nonempty_list.singleton |> fun flat_cases ->
      (* Create the output flat match expression *)
      (Next (tag, (tag, fst curr_arg, snd curr_arg), return_t, flat_cases)
        : (tag, tag) holed_flat_match_case_val)
      |> Ok
    and compile_constructors ~(return_t : Vtype.t) (curr_arg, arg_ts)
        case_queues def =
      (* This corresponds to the `matchCon` function in the book *)
      let open Result in
      (* Find which variant type we are looking at *)
      case_queues |> Nonempty_list.head |> fst
      |> fun (((_, tag), cname, _), _) ->
      TypeCtx.find_variant_type_with_constructor type_ctx cname
      |> Result.of_option ~error:(UnknownVariantConstructor cname)
      >>= fun ((vt_name, vt_cs), _) ->
      (* For each possible constructor of the variant type, create a case for the match *)
      let create_case_for_constructor ~variant_vtype ~cname ~ct
          ~(case_queues :
             ((((Vtype.t * tag) * tag StdPattern.typed_t)
              * tag StdPattern.typed_t list)
             * (tag, tag) StdExpr.typed_t)
             list) :
          ( tag FlatPattern.M.typed_t * (tag, tag) holed_flat_match_case_val,
            flattening_error )
          Result.t =
        (* Create the fresh name for the argument, and the corresponding expr nodes *)
        let fresh_arg_name : Varname.t =
          generate_fresh_varname ~seed_name:(sprintf "%s_arg" vt_name) ()
        in
        (* Create the case expression for this case *)
        compile_match ~return_t
          ((fresh_arg_name, ct) :: arg_ts)
          (List.map case_queues ~f:(fun (((_, p'), ps_ts), case_e) ->
               (p' :: ps_ts, case_e)))
          def
        >>= fun flat_case_e ->
        (* Create the output case, consisting of the flat pattern and flat expression *)
        ( FlatPattern.FlatPatConstructor
            ((variant_vtype, tag), cname, ((ct, tag), fresh_arg_name, ct)),
          flat_case_e )
        |> Ok
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
              |> List.filter_map ~f:(fun (((v, cname2, p1), ps_ts), case_e) ->
                     if not (equal_string cname cname2) then None
                     else Some (((v, p1), ps_ts), case_e))))
      |> Nonempty_list.result_all
      >>= fun flat_cases ->
      (* Create the output flat match expression *)
      (Next (tag, (tag, fst curr_arg, snd curr_arg), return_t, flat_cases)
        : (tag, tag) holed_flat_match_case_val)
      |> Ok
    in
    fun ~return_t args case_queues ->
      let open Result in
      compile_match ~return_t args case_queues None >>= fun res ->
      (!existing_names, res) |> Ok

  let fill_flat_match_case_val_holes (type tag_e tag_p)
      ~(orig_std_match : ('a, 'b) StdExpr.t)
      (orig_case_val : (tag_e, tag_p) holed_flat_match_case_val) :
      ((tag_e, tag_p) filled_flat_match_case_val, flattening_error) Result.t =
    let try_find_hole_filler :
        _ FlatPattern.M.t StringMap.t ->
        (tag_e, tag_p) holed_flat_match_case_val ->
        (tag_e, tag_p) filled_flat_match_case_val option =
      let rec fill_case_val (state : _ FlatPattern.M.t StringMap.t) :
          (tag_e, tag_p) holed_flat_match_case_val ->
          (tag_e, tag_p) filled_flat_match_case_val option = function
        | Hole -> None
        | Leaf x -> Some (Leaf x)
        | Next next -> fill_next state next
      and fill_next (state : _ FlatPattern.M.t StringMap.t)
          ((_, (_, arg_name, _), _, cases) :
            (tag_e, tag_p) holed_flat_match_repr) :
          (tag_e, tag_p) filled_flat_match_case_val option =
        let match_state_to_flat_pat (pat : _ FlatPattern.M.t) :
            _ FlatPattern.M.t StringMap.t option =
          Map.find state arg_name |> function
          | None ->
              failwith
                "TODO - some error. I think this shouldn't happen but I'm not \
                 sure"
          | Some arg_pat -> (
              let open FlatPattern in
              let set_if_exists (pat_xname : string) (arg_xname : string)
                  (state : _ FlatPattern.M.t StringMap.t) :
                  _ FlatPattern.M.t StringMap.t =
                Map.find state arg_xname
                |> Option.value_map ~default:state ~f:(fun arg_xpat ->
                       Map.set state ~key:pat_xname ~data:arg_xpat)
              in
              match (pat, arg_pat) with
              (* Note, since everything is already type-checked, we ignore types here *)
              | FlatPatName (_, pat_xname, _), FlatPatName (_, arg_xname, _) ->
                  set_if_exists pat_xname arg_xname state |> Some
              | ( FlatPatPair (_, (_, pat_x1name, _), (_, pat_x2name, _)),
                  FlatPatPair (_, (_, arg_x1name, _), (_, arg_x2name, _)) ) ->
                  state
                  |> set_if_exists pat_x1name arg_x1name
                  |> set_if_exists pat_x2name arg_x2name
                  |> Some
              | ( FlatPatConstructor (_, pat_cname, (_, pat_xname, _)),
                  FlatPatConstructor (_, arg_cname, (_, arg_xname, _)) ) ->
                  if equal_string pat_cname arg_cname then
                    state |> set_if_exists pat_xname arg_xname |> Some
                  else (* When the constructor names are a mismatch *) None
              | FlatPatName _, _ | FlatPatPair _, _ | FlatPatConstructor _, _ ->
                  (* When it is a mismatch *)
                  None)
        in
        Nonempty_list.fold_result cases ~init:()
          ~f:(fun () (case_p, case_val) ->
            (* Note, the "Result.t" type is here just used for shortcircuiting, and doesn't actually represent an "Ok" or "Error" result *)
            match match_state_to_flat_pat case_p with
            | None -> Ok ()
            | Some state' -> fill_case_val state' case_val |> Error)
        |> function
        | Error res -> res
        | Ok () -> None
      in
      fill_case_val
    in
    let rec translate_case_val ~(acc_state : _ FlatPattern.M.t StringMap.t) :
        (tag_e, tag_p) holed_flat_match_case_val ->
        ((tag_e, tag_p) filled_flat_match_case_val, flattening_error) Result.t =
      let open Result in
      function
      | Hole ->
          try_find_hole_filler acc_state orig_case_val
          |> Result.of_option
               ~error:
                 (NoDefaultCaseForMatchBranch
                    ( orig_std_match
                      |> StdExpr.fmap ~f:(Fn.const ())
                      |> StdExpr.fmap_pattern ~f:(Fn.const ()),
                      failwith "TODO" ))
      | Leaf e -> Leaf e |> Ok
      | Next next ->
          translate_next ~acc_state next >>= fun next_repr ->
          Next next_repr |> Ok
    and translate_next ~(acc_state : _ FlatPattern.M.t StringMap.t)
        ((v, ((_, arg_name, _) as arg), return_t, cases) :
          (tag_e, tag_p) holed_flat_match_repr) :
        ((tag_e, tag_p) filled_flat_match_repr, flattening_error) Result.t =
      let open Result in
      Nonempty_list.map cases ~f:(fun (case_p, case_val) ->
          let acc_state : _ FlatPattern.M.t StringMap.t =
            Map.set acc_state ~key:arg_name ~data:case_p
          in
          translate_case_val ~acc_state case_val >>= fun case_e ->
          (case_p, case_e) |> Ok)
      |> Nonempty_list.result_all
      >>= fun filled_cases -> (v, arg, return_t, filled_cases) |> Ok
    in

    translate_case_val ~acc_state:StringMap.empty orig_case_val

  let compile_flat_match_repr (type tag_e tag_p)
      ~(flatten_expr :
         (tag_e, tag_p) StdExpr.typed_t ->
         ((tag_e, tag_p) FlatExpr.typed_t, flattening_error) Result.t) :
      (tag_e, tag_p) filled_flat_match_case_val ->
      ((tag_e, tag_p) FlatExpr.typed_t, flattening_error) Result.t =
    let rec compile_case_val :
        (tag_e, tag_p) filled_flat_match_case_val ->
        ((tag_e, tag_p) FlatExpr.typed_t, _) Result.t = function
      | Leaf e -> flatten_expr e
      | Next next -> compile_next next
    and compile_next
        ((v, (arg_v, arg_name, arg_t), return_t, cases) :
          (tag_e, tag_p) filled_flat_match_repr) :
        ((tag_e, tag_p) FlatExpr.typed_t, _) Result.t =
      let open Result in
      let open FlatExpr in
      Nonempty_list.map cases ~f:(fun (case_p, case_val) ->
          compile_case_val case_val >>= fun case_e -> (case_p, case_e) |> Ok)
      |> Nonempty_list.result_all
      >>= fun cases ->
      let arg_node = Var ((arg_t, arg_v), arg_name) in
      Match ((return_t, v), arg_node, return_t, cases) |> Ok
    in
    compile_case_val

  let flatten_expr (type tag) ~(existing_names : StringSet.t)
      ~(type_ctx : TypeCtx.t) =
    let existing_names = ref existing_names in
    let generate_fresh_varname ?(seed_name : string option) () : Varname.t =
      let new_existing_names, new_name =
        generate_fresh_varname_from ?seed_name !existing_names
      in
      existing_names := new_existing_names;
      new_name
    in
    let rec flatten_expr :
        (tag, tag) StdExpr.typed_t ->
        ((tag, tag) FlatExpr.typed_t, flattening_error) Result.t =
      let open Result in
      let unop
          (recomb : (tag, tag) FlatExpr.typed_t -> (tag, tag) FlatExpr.typed_t)
          (e1 : (tag, tag) StdExpr.typed_t) :
          ((tag, tag) FlatExpr.typed_t, flattening_error) Result.t =
        flatten_expr e1 >>= fun e1' -> Ok (recomb e1')
      in
      let binop
          (recomb :
            (tag, tag) FlatExpr.typed_t ->
            (tag, tag) FlatExpr.typed_t ->
            (tag, tag) FlatExpr.typed_t) (e1 : (tag, tag) StdExpr.typed_t)
          (e2 : (tag, tag) StdExpr.typed_t) :
          ((tag, tag) FlatExpr.typed_t, flattening_error) Result.t =
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
      | Match ((v_t, v_val), e, return_t, cases) as orig_match ->
          let arg_t = e |> StdExpr.node_val |> fst in
          let new_arg_name = generate_fresh_varname ~seed_name:"arg" () in
          flatten_expr e >>= fun flattened_arg ->
          let prepared_cases =
            Nonempty_list.map cases ~f:(fun (case_p, case_e) ->
                (List.singleton case_p, case_e))
            |> Nonempty_list.to_list
          in
          compile_match_to_repr ~existing_names:!existing_names ~type_ctx
            ~return_t
            (List.singleton (new_arg_name, arg_t))
            prepared_cases
          >>= fun (new_existing_names, holed_match_case_val) ->
          existing_names := new_existing_names;
          fill_flat_match_case_val_holes ~orig_std_match:orig_match
            holed_match_case_val
          >>= fun filled_match_case_val ->
          compile_flat_match_repr ~flatten_expr filled_match_case_val
          >>= fun generated_match_expr ->
          FlatExpr.Let
            ((v_t, v_val), new_arg_name, flattened_arg, generated_match_expr)
          |> Ok
      | Constructor (v, name, e) -> unop (fun e' -> Constructor (v, name, e')) e
    in
    fun orig_e ->
      Result.(flatten_expr orig_e >>| fun res -> (!existing_names, res))

  let flatten_program (type tag) ~(existing_names : StringSet.t)
      ~(type_ctx : TypeCtx.t) (prog : (tag, tag) StdProgram.typed_t) :
      (StringSet.t * (tag, tag) FlatProgram.typed_t, flattening_error) Result.t
      =
    let open Result in
    List.fold_result ~init:(existing_names, [])
      ~f:(fun
          (existing_names, acc_defns_rev)
          (defn : (Vtype.t * tag, Vtype.t * tag) StdProgram.top_level_defn)
        ->
        flatten_expr ~existing_names ~type_ctx defn.body
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
        flatten_expr ~existing_names ~type_ctx prog_body
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

  let flatten_typed_program (type tag) ~(existing_names : StringSet.t)
      (prog : (tag, tag) TypeChecker.typed_program) :
      ( StringSet.t * (tag, tag) FlatPattern.FlatProgram.typed_t,
        flattening_error )
      Result.t =
    flatten_program ~existing_names
      ~type_ctx:
        (TypeChecker.typed_program_get_type_ctx_checked prog
        |> TypeChecker.checked_type_ctx_get_type_ctx)
      (TypeChecker.typed_program_get_program prog)
end
