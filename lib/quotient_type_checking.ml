open Core
open Utils
open Vtype
open Varname
open Pattern
open Ast
open Typing

module type LispBuilderSig = sig
  (** The type of a node in the builder *)
  type node =
    | Unit  (** The unit value *)
    | Atom of string  (** An atomic value *)
    | Op of string * node list  (** An operation with argument nodes *)
    | List of node list  (** A list of nodes *)

  (** Build the source to a string *)
  val build : use_newlines:bool -> node list -> string
end

module LispBuilder : LispBuilderSig = struct
  type node =
    | Unit
    | Atom of string
    | Op of string * node list
    | List of node list

  let rec build_node = function
    | Unit -> "()"
    | Atom s -> s
    | Op (op, []) -> op
    | Op (op, (_ :: _ as xs)) ->
        sprintf "(%s %s)" op
          (String.concat ~sep:" " (List.map ~f:build_node xs))
    | List xs ->
        sprintf "(%s)" (String.concat ~sep:" " (List.map ~f:build_node xs))

  let build ~(use_newlines : bool) (nodes : node list) : string =
    let sep = if use_newlines then "\n" else " " in
    String.concat ~sep (List.map ~f:build_node nodes)
end

module QuotientTypeChecker =
functor
  (TypeCtx : TypingTypeContext)
  (VarCtx : TypingVarContext)
  ->
  struct
    module TypeChecker = TypeChecker (TypeCtx) (VarCtx)

    type ast_tag = { t : vtype }
    type pattern_tag = { t : vtype }
    type tag_expr = (ast_tag, pattern_tag) expr
    type tag_pattern = pattern_tag pattern

    type quotient_typing_error =
      | MisplacedFixNode
      | ProgramTypingError of typing_error
      | MultipleVariableDefinitions of varname
      | VariantTypeConstructorDoesNotExist of string
      | PatternTypeMismatch of vtype * vtype

    (** A special prefix to be prepended to names defined by this code , so that
        they can't interfere with user-defined names. Therefore, this contains a
        character not usable by the user when defining variable names *)
    let custom_special_prefix : string -> string = String.append "PQ-"

    (** Generate a fresh variable name, given a set of the currently-defined
        names *)
    let generate_fresh_varname (existing_names : StringSet.t) :
        varname * StringSet.t =
      let rec loop (i : int) : varname =
        let candidate = sprintf "x%d" i in
        if Set.mem existing_names candidate then loop (i + 1) else candidate
      in
      let new_name = loop 0 in
      (new_name, Set.add existing_names new_name)

    (** Provides the representation of an AST for quotient analysis.

        We convert Fix nodes in let-bindings to let-rec bindings, for easier
        analysis.

        We also use flattened patterns.

        Also, we apply the "unscoping" transformation to let-rec bindings, where
        functions don't capture variables, and instead will be provided all
        values as parameters. To help with this, the function application node
        contains a list of arguments to provide, instead of only allowing for a
        single argument *)
    module QuotientAnalysisRepr = struct
      type flat_pattern =
        | FlatPatName of pattern_tag * varname * vtype
        | FlatPatPair of
            pattern_tag
            * (pattern_tag * varname * vtype)
            * (pattern_tag * varname * vtype)
        | FlatPatConstructor of
            pattern_tag * string * (pattern_tag * varname * vtype)

      type t =
        | UnitLit of ast_tag
        | IntLit of ast_tag * int
        | Add of ast_tag * t * t
        | Neg of ast_tag * t
        | Subtr of ast_tag * t * t
        | Mult of ast_tag * t * t
        | BoolLit of ast_tag * bool
        | BNot of ast_tag * t
        | BOr of ast_tag * t * t
        | BAnd of ast_tag * t * t
        | Pair of ast_tag * t * t
        | Eq of ast_tag * t * t
        | Gt of ast_tag * t * t
        | GtEq of ast_tag * t * t
        | Lt of ast_tag * t * t
        | LtEq of ast_tag * t * t
        | If of ast_tag * t * t * t
        | Var of ast_tag * string
        | LetNoRec of ast_tag * string * t * t
        | LetRec of ast_tag * string * (string * vtype) Nonempty_list.t * t * t
        | Fun of ast_tag * (string * vtype) * t
        | App of ast_tag * t * t list
        | Match of ast_tag * t * (flat_pattern * t) Nonempty_list.t
        | Constructor of ast_tag * string * t

      type quotient_analysis_repr = t

      let flat_pattern_node_val : flat_pattern -> pattern_tag = function
        | FlatPatName (v, _, _) -> v
        | FlatPatPair (v, _, _) -> v
        | FlatPatConstructor (v, _, _) -> v

      let node_val : t -> ast_tag = function
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
        | LetNoRec (v, _, _, _) -> v
        | LetRec (v, _, _, _, _) -> v
        | Fun (v, _, _) -> v
        | App (v, _, _) -> v
        | Match (v, _, _) -> v
        | Constructor (v, _, _) -> v

      type of_ast_state = { var_scope : (varname * vtype) list }

      (** Flatten a single case of a Match node so that the pattern is a flat
          pattern, and modify the case expression to perform any subsequent
          matching as needed *)
      let rec flatten_case_pattern ~(existing_names : StringSet.t)
          ((p : tag_pattern), (e : quotient_analysis_repr)) :
          ( StringSet.t * flat_pattern * quotient_analysis_repr,
            quotient_typing_error )
          Result.t =
        let open Result in
        let outer_expr_type : vtype = (node_val e).t in
        match p with
        | PatName (v, x_name, x_t) ->
            (* A named variable pattern *)
            (existing_names, FlatPatName (v, x_name, x_t), e) |> Ok
        | PatPair
            ( v_pair,
              PatName (x1_v, x1_name, x1_t),
              PatName (x2_v, x2_name, x2_t) ) ->
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
              generate_fresh_varname existing_names
            in
            let new_binding_name_2, existing_names =
              generate_fresh_varname existing_names
            in
            let p1_t = (pattern_node_val p1).t in
            let p2_t = (pattern_node_val p2).t in
            flatten_case_pattern ~existing_names (p2, e)
            >>=
            fun (existing_names, flattened_p2_case_p, flattened_p2_case_e) ->
            flatten_case_pattern ~existing_names
              ( p1,
                Match
                  ( { t = outer_expr_type },
                    Var ({ t = p2_t }, new_binding_name_2),
                    Nonempty_list.singleton
                      (flattened_p2_case_p, flattened_p2_case_e) ) )
            >>|
            fun (existing_names, flattened_p1_case_p, flattened_p1_case_e) ->
            ( existing_names,
              FlatPatPair
                ( v,
                  ({ t = p1_t }, new_binding_name_1, p1_t),
                  ({ t = p2_t }, new_binding_name_2, p2_t) ),
              Match
                ( { t = outer_expr_type },
                  Var ({ t = p1_t }, new_binding_name_1),
                  Nonempty_list.singleton
                    (flattened_p1_case_p, flattened_p1_case_e) ) )
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
              generate_fresh_varname existing_names
            in
            let p1_t = (pattern_node_val p1).t in
            flatten_case_pattern ~existing_names (p1, e)
            >>|
            fun (existing_names, flattened_p1_case_p, flattened_p1_case_e) ->
            ( existing_names,
              FlatPatConstructor
                (v, c_name, ({ t = p1_t }, new_binding_name, p1_t)),
              Match
                ( { t = outer_expr_type },
                  Var ({ t = p1_t }, new_binding_name),
                  Nonempty_list.singleton
                    (flattened_p1_case_p, flattened_p1_case_e) ) )

      (** Convert an AST node to its quotient-analysis representation *)
      let rec of_ast ~(existing_names : StringSet.t) (state : of_ast_state) :
          tag_expr -> (StringSet.t * t, quotient_typing_error) Result.t =
        let open Result in
        let unop
            (recomb :
              StringSet.t ->
              quotient_analysis_repr ->
              StringSet.t * quotient_analysis_repr)
            ~(existing_names : StringSet.t) (state : of_ast_state)
            (e1 : tag_expr) :
            ( StringSet.t * quotient_analysis_repr,
              quotient_typing_error )
            Result.t =
          of_ast ~existing_names state e1 >>= fun (existing_names, e1') ->
          Ok (recomb existing_names e1')
        in
        let binop
            (recomb :
              StringSet.t ->
              quotient_analysis_repr ->
              quotient_analysis_repr ->
              StringSet.t * quotient_analysis_repr)
            ~(existing_names : StringSet.t) (state : of_ast_state)
            (e1 : tag_expr) (e2 : tag_expr) :
            ( StringSet.t * quotient_analysis_repr,
              quotient_typing_error )
            Result.t =
          of_ast ~existing_names state e1 >>= fun (existing_names, e1') ->
          of_ast ~existing_names state e2 >>= fun (existing_names, e2') ->
          Ok (recomb existing_names e1' e2')
        in
        function
        | UnitLit v -> Ok (existing_names, UnitLit v)
        | IntLit (v, x) -> Ok (existing_names, IntLit (v, x))
        | Add (v, e1, e2) ->
            binop
              (fun existing_names e1' e2' ->
                (existing_names, Add (v, e1', e2')))
              ~existing_names state e1 e2
        | Neg (v, e) ->
            unop
              (fun existing_names e' -> (existing_names, Neg (v, e')))
              ~existing_names state e
        | Subtr (v, e1, e2) ->
            binop
              (fun existing_names e1' e2' ->
                (existing_names, Subtr (v, e1', e2')))
              ~existing_names state e1 e2
        | Mult (v, e1, e2) ->
            binop
              (fun existing_names e1' e2' ->
                (existing_names, Mult (v, e1', e2')))
              ~existing_names state e1 e2
        | BoolLit (v, b) -> Ok (existing_names, BoolLit (v, b))
        | BNot (v, e) ->
            unop
              (fun existing_names e' -> (existing_names, BNot (v, e')))
              ~existing_names state e
        | BOr (v, e1, e2) ->
            binop
              (fun existing_names e1' e2' ->
                (existing_names, BOr (v, e1', e2')))
              ~existing_names state e1 e2
        | BAnd (v, e1, e2) ->
            binop
              (fun existing_names e1' e2' ->
                (existing_names, BAnd (v, e1', e2')))
              ~existing_names state e1 e2
        | Pair (v, e1, e2) ->
            binop
              (fun existing_names e1' e2' ->
                (existing_names, Pair (v, e1', e2')))
              ~existing_names state e1 e2
        | Eq (v, e1, e2) ->
            binop
              (fun existing_names e1' e2' -> (existing_names, Eq (v, e1', e2')))
              ~existing_names state e1 e2
        | Gt (v, e1, e2) ->
            binop
              (fun existing_names e1' e2' -> (existing_names, Gt (v, e1', e2')))
              ~existing_names state e1 e2
        | GtEq (v, e1, e2) ->
            binop
              (fun existing_names e1' e2' ->
                (existing_names, GtEq (v, e1', e2')))
              ~existing_names state e1 e2
        | Lt (v, e1, e2) ->
            binop
              (fun existing_names e1' e2' -> (existing_names, Lt (v, e1', e2')))
              ~existing_names state e1 e2
        | LtEq (v, e1, e2) ->
            binop
              (fun existing_names e1' e2' ->
                (existing_names, LtEq (v, e1', e2')))
              ~existing_names state e1 e2
        | If (v, e1, e2, e3) ->
            of_ast ~existing_names state e1 >>= fun (existing_names, e1') ->
            of_ast ~existing_names state e2 >>= fun (existing_names, e2') ->
            of_ast ~existing_names state e3 >>| fun (existing_names, e3') ->
            (existing_names, If (v, e1', e2', e3'))
        | Var (v, name) -> Ok (existing_names, Var (v, name))
        | Let (v, xname, e1, e2) -> (
            let get_default_repr :
                unit ->
                ( StringSet.t * quotient_analysis_repr,
                  quotient_typing_error )
                Result.t =
             fun () ->
              binop
                (fun existing_names e1' e2' ->
                  (existing_names, LetNoRec (v, xname, e1', e2')))
                ~existing_names
                {
                  var_scope =
                    List.Assoc.add state.var_scope ~equal:equal_varname xname
                      (expr_node_val e2).t;
                }
                e1 e2
            in
            match e1 with
            | Fix (_, (xname2, x2type1, x2type2), (yname, ytype), e1') ->
                (* TODO - this also needs to consider the variable scope *)
                let x2type = VTypeFun (x2type1, x2type2) in
                if equal_string xname xname2 then
                  binop
                    (fun existing_names e1'_converted e2_converted ->
                      ( existing_names,
                        LetRec
                          ( v,
                            xname,
                            Nonempty_list.singleton (yname, ytype),
                            e1'_converted,
                            e2_converted ) ))
                    ~existing_names
                    {
                      var_scope =
                        List.Assoc.add
                          (List.Assoc.add state.var_scope ~equal:equal_varname
                             xname x2type)
                          ~equal:equal_varname yname ytype;
                    }
                    e1' e2
                else get_default_repr ()
            | _ -> get_default_repr ())
        | Fun (v, xname, e) ->
            unop
              (fun existing_names e' -> (existing_names, Fun (v, xname, e')))
              ~existing_names state e
        | App (v, e1, e2) ->
            binop
              (fun existing_names e1' e2' ->
                (existing_names, App (v, e1', [ e2' ])))
              ~existing_names state e1 e2
        | Fix _ -> Error MisplacedFixNode
        | Match (v, e, cs) ->
            (* TODO - this also must alter the variable scope *)
            of_ast ~existing_names state e >>= fun (existing_names, e') ->
            Nonempty_list.fold_result_consume_init ~init:existing_names
              ~f:(fun acc (p, e) ->
                let existing_names =
                  match acc with
                  | First existing_names -> existing_names
                  | Second (existing_names, _) -> existing_names
                in
                of_ast ~existing_names (failwith "TODO - state") e
                >>= fun (existing_names, e') ->
                flatten_case_pattern ~existing_names (p, e')
                >>= fun (existing_names, flat_p, flat_e) ->
                match acc with
                | First _ ->
                    (existing_names, Nonempty_list.singleton (flat_p, flat_e))
                    |> Ok
                | Second (_, acc) ->
                    (existing_names, Nonempty_list.cons (flat_p, flat_e) acc)
                    |> Ok)
              cs
            >>= fun (existing_names, flat_cases) ->
            (existing_names, Match (v, e', flat_cases)) |> Ok
        | Constructor (v, name, e) ->
            unop
              (fun existing_names e' ->
                (existing_names, Constructor (v, name, e')))
              ~existing_names state e
    end
  end
