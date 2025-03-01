open Core
open Utils

module type S = sig
  module Smt : SmtIntf.S

  type node_tag = { source_pos : Frontend.source_position; t : Vtype.t }

  type quotient_typing_error =
    | SmtUnknownResult
    | PatternFlatteningError of FlatPattern.flattening_error
    | SmtIntfError of Smt.smt_intf_error
  [@@deriving sexp, equal]

  module QuotientTypeCheckingFailure : sig
    type t [@@deriving sexp, equal]

    (** Get the source position at which the problematic match case is *)
    val get_match_case_source_pos : t -> Frontend.source_position

    (** Print a quotient type checking failure for a human reader *)
    val print : t -> string
  end

  (** Check if a program's quotient type usage is valid. Returns a result with
      the Ok constructor containing another result describing the actual result
      of the checking *)
  val check_program :
    (node_tag, node_tag) Program.StdProgram.t ->
    ( (unit, QuotientTypeCheckingFailure.t) Result.t,
      quotient_typing_error )
    Result.t
end

module MakeZ3 : S = struct
  module Smt = SmtIntf.Z3Intf

  (* Required for exposing Expr.std_expr_of_std_pattern *)
  open Expr
  module Pattern = Pattern.StdPattern
  module Expr = Expr.StdExpr
  module Unifier = Unifier.StdUnifier
  module QuotientType = QuotientType.StdQuotientType
  module CustomType = CustomType.StdCustomType
  module Program = Program.StdProgram

  type node_tag = { source_pos : Frontend.source_position; t : Vtype.t }
  [@@deriving sexp, equal]

  let node_tag_to_smt_pattern_tag { source_pos = _; t } : Smt.pattern_tag =
    { t }

  let node_tag_to_smt_expr_tag { source_pos = _; t } : Smt.expr_tag = { t }

  type tag_pattern = node_tag Pattern.t [@@deriving sexp, equal]
  type tag_expr = (node_tag, node_tag) Expr.t [@@deriving sexp, equal]
  type tag_unifier = (node_tag, node_tag) Unifier.t [@@deriving sexp, equal]

  type tag_quotient_type = (node_tag, node_tag) QuotientType.t
  [@@deriving sexp, equal]

  type tag_quotient_type_eqcons = (node_tag, node_tag) QuotientType.eqcons
  [@@deriving sexp, equal]

  type tag_program = (node_tag, node_tag) Program.t [@@deriving sexp, equal]

  type tag_flat_expr = (node_tag, node_tag) FlatPattern.FlatExpr.t
  [@@deriving sexp, equal]

  let tag_flat_expr_to_smt_flat_expr (e : tag_flat_expr) :
      (Smt.expr_tag, Smt.pattern_tag) FlatPattern.FlatExpr.t =
    e
    |> FlatPattern.FlatExpr.fmap ~f:node_tag_to_smt_expr_tag
    |> FlatPattern.FlatExpr.fmap_pattern ~f:node_tag_to_smt_pattern_tag

  type quotient_typing_error =
    | SmtUnknownResult
    | PatternFlatteningError of FlatPattern.flattening_error
    | SmtIntfError of Smt.smt_intf_error
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

  let use_fresh_names_for_eqcons ~(existing_names : StringSet.t)
      (eqcons : tag_quotient_type_eqcons) :
      StringSet.t * tag_quotient_type_eqcons =
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

  module QuotientTypeCheckingFailure = struct
    type t = {
      eqcons : tag_quotient_type_eqcons;
      case_p : tag_pattern;
      l : tag_expr;
      r : tag_expr;
      model_mapping : (string * string) list;
    }
    [@@deriving sexp, equal]

    let get_match_case_source_pos ({ case_p; _ } : t) : Frontend.source_position
        =
      (Pattern.node_val case_p).source_pos

    let print ({ eqcons; case_p; l; r; model_mapping } as x : t) : string =
      let match_case_source_pos = get_match_case_source_pos x in
      sprintf
        {|Considering the given equality constructor on the match case with the given pattern, we should have that the below "Side L" and "Side R" are equivalent, but they are not, as shown by the example interpretation below.

====================
Equality Constructor
====================

%s

==================
Match Case Pattern
==================
(from line %d)

%s

======
Side L
======

%s

======
Side R
======

%s

=========================
Falsifying Interpretation
=========================

%s
|}
        (eqcons |> QuotientType.eqcons_to_source_code)
        match_case_source_pos.lnum
        (case_p |> Pattern.to_source_code)
        (l |> Expr.to_source_code) (r |> Expr.to_source_code)
        (model_mapping
        |> List.map ~f:(fun (xname, xe) -> sprintf "%s = %s" xname xe)
        |> String.concat ~sep:"\n")
  end

  type check_result =
    | Err of quotient_typing_error
    | Failure of QuotientTypeCheckingFailure.t
  [@@deriving sexp, equal]

  let perform_quotient_match_check ~(existing_names : StringSet.t)
      ~(quotient_type : tag_quotient_type) ~(match_node_v : node_tag)
      ~(match_t_out : Vtype.t)
      ~(cases : (tag_pattern * tag_expr) Nonempty_list.t) (state : Smt.State.t)
      : (StringSet.t, check_result) Result.t =
    let open Result in
    let reform_match_with_arg (e1 : tag_expr) : tag_expr =
      Match (match_node_v, e1, match_t_out, cases)
    in
    (* Iterating through each case of the match *)
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
        (* Iterating through matching eqconss of the case *)
        List.fold_result ~init:existing_names
          (List.filter_map fresh_name_eqconss ~f:(fun eqcons ->
               Unifier.simply_find_unifier ~bound_names_in_from:StringSet.empty
                 ~from_expr:
                   (case_p |> std_expr_of_std_pattern ~convert_tag:Fn.id)
                 ~to_expr:
                   (fst eqcons.body
                   |> std_expr_of_std_pattern ~convert_tag:Fn.id)
               |> function
               | Error () -> None
               | Ok unifier -> Some (unifier, eqcons)))
          ~f:(fun existing_names (expr_to_eqcons_unifier, eqcons) ->
            let state =
              (* Add the eqcons' bindings to the state *)
              List.fold ~init:state
                ~f:(fun state (xname, xtype) ->
                  Smt.State.state_add_var_decl (xname, xtype) state)
                eqcons.bindings
            in
            let l =
              Unifier.apply_to_expr ~unifier:expr_to_eqcons_unifier case_e
            in
            let r = reform_match_with_arg (snd eqcons.body) in
            FlatPattern.of_expr ~existing_names l
            |> Result.map_error ~f:(fun err -> Err (PatternFlatteningError err))
            >>= fun (existing_names, l_flat) ->
            FlatPattern.of_expr ~existing_names r
            |> Result.map_error ~f:(fun err -> Err (PatternFlatteningError err))
            >>= fun (existing_names, r_flat) ->
            (* Form the main assertion of the SMT check *)
            let assertion : Smt.Assertion.t =
              let open Smt.Assertion in
              (* Note that I use the negation as we are looking or validity, not satisifability:
                the condition must be necessarily true, so we check that the negation is unsatisfiable *)
              Not
                (Eq
                   ( Some match_t_out,
                     tag_flat_expr_to_smt_flat_expr l_flat,
                     tag_flat_expr_to_smt_flat_expr r_flat ))
            in
            Smt.create_formula ~existing_names state [ assertion ]
            |> Result.map_error ~f:(fun err -> Err (SmtIntfError err))
            >>= fun (existing_names, formula) ->
            match Smt.check_satisfiability formula with
            | `Unsat -> Ok existing_names
            | `Sat model ->
                let model_mapping = model |> Option.value_exn |> Map.to_alist in
                Error (Failure { eqcons; case_p; l; r; model_mapping })
            | `Unknown -> Error (Err SmtUnknownResult)))

  let rec check_expr ~(existing_names : StringSet.t)
      ~(quotient_types : tag_quotient_type list) (state : Smt.State.t)
      (orig_e : tag_flat_expr) : (StringSet.t, check_result) Result.t =
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
        let e1_type = (FlatPattern.FlatExpr.node_val e1).t in

        state_add_var_defn
          {
            name = xname;
            kind = `NonRec None;
            return_t = e1_type;
            body = tag_flat_expr_to_smt_flat_expr e1;
          }
          state
        |> Result.map_error ~f:(fun err -> Err (SmtIntfError err))
        >>= fun state -> check_expr ~existing_names ~quotient_types state e2
    | App (_, e1, e2) ->
        check_expr ~existing_names ~quotient_types state e1
        >>= fun existing_names ->
        check_expr ~existing_names ~quotient_types state e2
    | Match (v, e1, t_out, cases) ->
        check_expr ~existing_names ~quotient_types state e1
        >>= fun existing_names ->
        let e1_t = (FlatPattern.FlatExpr.node_val e1).t in
        (match e1_t with
        | VTypeCustom ct_name -> (
            match
              List.find quotient_types ~f:(fun qt ->
                  equal_string qt.name ct_name)
            with
            | Some qt ->
                let non_flat_cases =
                  Nonempty_list.map
                    ~f:(fun (flat_p, flat_e) ->
                      FlatPattern.(to_std_pattern flat_p, to_std_expr flat_e))
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
                (FlatPattern.M.defined_vars p)
            in
            (* Check the case expression, with the updated state *)
            check_expr ~existing_names ~quotient_types state' e)
          cases
    | Constructor (_, _, e) ->
        check_expr ~existing_names ~quotient_types state e

  let check_program (prog : tag_program) :
      ( (unit, QuotientTypeCheckingFailure.t) Result.t,
        quotient_typing_error )
      Result.t =
    (* TODO - make this take a checked program (from the Typing module's TypeChecker functor) as parameter instead of just a program *)
    let open Result in
    let open Smt.State in
    let existing_names = Program.existing_names prog in
    FlatPattern.of_program ~existing_names prog
    |> Result.map_error ~f:(fun err -> PatternFlatteningError err)
    >>= fun (existing_names, flat_prog) ->
    let quotient_types : tag_quotient_type list =
      List.filter_map
        ~f:(function
          | { private_flag = _; ct = QuotientType qt } -> Some qt | _ -> None)
        flat_prog.custom_types
    in
    let state =
      Smt.State.state_init
        (prog.custom_types
        |> List.map ~f:(fun { private_flag = _; ct } ->
               ct
               |> CustomType.fmap_expr ~f:node_tag_to_smt_expr_tag
               |> CustomType.fmap_pattern ~f:node_tag_to_smt_pattern_tag))
    in
    let state =
      (* Add the variant type definitions to the state *)
      List.fold ~init:state
        ~f:(fun state { private_flag = _; ct } ->
          match ct with
          | VariantType vt -> state_add_variant_type vt state
          | CustomType.QuotientType _ -> state)
        flat_prog.custom_types
    in
    (* Check the top-level definitions and add them to the state *)
    ( List.fold_result ~init:(existing_names, state)
        ~f:(fun (existing_names, state) defn ->
          (* Create the state used when checking the body of the function *)
          let defn_checking_state = state_add_var_decl defn.param state in
          (if defn.recursive then
             state_add_var_defn
               {
                 name = defn.name;
                 kind = `Rec defn.param;
                 return_t = defn.return_t;
                 body = tag_flat_expr_to_smt_flat_expr defn.body;
               }
               defn_checking_state
           else Ok state)
          |> Result.map_error ~f:(fun err -> Err (SmtIntfError err))
          >>= fun defn_checking_state ->
          (* Check the body of the TLD *)
          check_expr ~existing_names ~quotient_types defn_checking_state
            defn.body
          >>= fun existing_names ->
          (* Return the resulting state after defining the TLD *)
          state_add_var_defn
            {
              name = defn.name;
              kind =
                (if defn.recursive then `Rec defn.param
                 else `NonRec (Some defn.param));
              return_t = defn.return_t;
              body = tag_flat_expr_to_smt_flat_expr defn.body;
            }
            state
          |> Result.map_error ~f:(fun err -> Err (SmtIntfError err))
          >>| fun state -> (existing_names, state))
        flat_prog.top_level_defns
    >>= fun (existing_names, state) ->
      (* Check the main body of the program with the accumulated state *)
      check_expr ~existing_names ~quotient_types state flat_prog.e )
    |> function
    | Ok _ -> Ok (Ok ())
    | Error (Failure failure) -> Ok (Error failure)
    | Error (Err err) -> Error err
end
