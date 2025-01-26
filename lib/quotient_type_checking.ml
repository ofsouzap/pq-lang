open Core
open Utils
open Vtype
open Variant_types
open Varname
open Pattern
open Ast
open Program
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

    (** Preprocessing for arbitrary expressions, for usage in quotient type
        checking *)
    module ExprPreprocessor = struct
      type flat_pattern =
        | FlatPatName of pattern_tag * varname * vtype
        | FlatPatPair of
            pattern_tag
            * (pattern_tag * varname * vtype)
            * (pattern_tag * varname * vtype)
        | FlatPatConstructor of
            pattern_tag * string * (pattern_tag * varname * vtype)

      let flat_pattern_node_val : flat_pattern -> pattern_tag = function
        | FlatPatName (v, _, _) -> v
        | FlatPatPair (v, _, _) -> v
        | FlatPatConstructor (v, _, _) -> v

      type preprocessed_tag_expr =
        | UnitLit of ast_tag
        | IntLit of ast_tag * int
        | Add of ast_tag * preprocessed_tag_expr * preprocessed_tag_expr
        | Neg of ast_tag * preprocessed_tag_expr
        | Subtr of ast_tag * preprocessed_tag_expr * preprocessed_tag_expr
        | Mult of ast_tag * preprocessed_tag_expr * preprocessed_tag_expr
        | BoolLit of ast_tag * bool
        | BNot of ast_tag * preprocessed_tag_expr
        | BOr of ast_tag * preprocessed_tag_expr * preprocessed_tag_expr
        | BAnd of ast_tag * preprocessed_tag_expr * preprocessed_tag_expr
        | Pair of ast_tag * preprocessed_tag_expr * preprocessed_tag_expr
        | Eq of ast_tag * preprocessed_tag_expr * preprocessed_tag_expr
        | Gt of ast_tag * preprocessed_tag_expr * preprocessed_tag_expr
        | GtEq of ast_tag * preprocessed_tag_expr * preprocessed_tag_expr
        | Lt of ast_tag * preprocessed_tag_expr * preprocessed_tag_expr
        | LtEq of ast_tag * preprocessed_tag_expr * preprocessed_tag_expr
        | If of
            ast_tag
            * preprocessed_tag_expr
            * preprocessed_tag_expr
            * preprocessed_tag_expr
        | Var of ast_tag * varname
        | LetNoRec of
            ast_tag * varname * preprocessed_tag_expr * preprocessed_tag_expr
        | LetRec of
            ast_tag
            * varname
            * (varname * vtype) Nonempty_list.t
            * preprocessed_tag_expr
            * preprocessed_tag_expr
            (** LetRec (tag, xname, params, e1, e2) == "let rec xname
                ...params... = e1 in e2" *)
        | Fun of ast_tag * (string * vtype) * preprocessed_tag_expr
        | App of ast_tag * preprocessed_tag_expr * preprocessed_tag_expr
        | Match of
            ast_tag
            * preprocessed_tag_expr
            * (flat_pattern * preprocessed_tag_expr) Nonempty_list.t
        | Constructor of ast_tag * string * preprocessed_tag_expr

      let preprocessed_expr_node_val : preprocessed_tag_expr -> ast_tag =
        function
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

      let generate_fresh_varname (existing_names : StringSet.t) :
          varname * StringSet.t =
        let rec loop (i : int) : varname =
          let candidate = sprintf "x%d" i in
          if Set.mem existing_names candidate then loop (i + 1) else candidate
        in
        let new_name = loop 0 in
        (new_name, Set.add existing_names new_name)

      let rec flatten_case_pattern
          ( (existing_names : StringSet.t),
            (p : tag_pattern),
            (e : preprocessed_tag_expr) ) :
          ( StringSet.t * flat_pattern * preprocessed_tag_expr,
            quotient_typing_error )
          Result.t =
        let open Result in
        let outer_expr_type : vtype = (preprocessed_expr_node_val e).t in
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
            let p1_t, p2_t =
              failwith "TODO - once pattern tagging implemented"
            in
            flatten_case_pattern (existing_names, p2, e)
            >>=
            fun (existing_names, flattened_p2_case_p, flattened_p2_case_e) ->
            flatten_case_pattern
              ( existing_names,
                p1,
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
            let p1_t = failwith "TODO - once pattern tagging implemented" in
            flatten_case_pattern (existing_names, p1, e)
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

      let rec preprocess_expr
          ((existing_names : StringSet.t), (orig_e : tag_expr)) :
          (StringSet.t * preprocessed_tag_expr, quotient_typing_error) Result.t
          =
        let open Result in
        let unop
            (recomb :
              StringSet.t ->
              preprocessed_tag_expr ->
              StringSet.t * preprocessed_tag_expr) (e1 : tag_expr) =
          preprocess_expr (existing_names, e1) >>| fun (existing_names, e1') ->
          recomb existing_names e1'
        in
        let binop
            (recomb :
              StringSet.t ->
              preprocessed_tag_expr ->
              preprocessed_tag_expr ->
              StringSet.t * preprocessed_tag_expr) (e1 : tag_expr)
            (e2 : tag_expr) =
          preprocess_expr (existing_names, e1) >>= fun (existing_names, e1') ->
          preprocess_expr (existing_names, e2) >>| fun (existing_names, e2') ->
          recomb existing_names e1' e2'
        in
        match orig_e with
        | UnitLit v -> Ok (existing_names, UnitLit v)
        | IntLit (v, x) -> Ok (existing_names, IntLit (v, x))
        | Add (v, e1, e2) ->
            binop
              (fun existing_names e1' e2' ->
                (existing_names, Add (v, e1', e2')))
              e1 e2
        | Neg (v, e1) ->
            unop (fun existing_names e1' -> (existing_names, Neg (v, e1'))) e1
        | Subtr (v, e1, e2) ->
            binop
              (fun existing_names e1' e2' ->
                (existing_names, Subtr (v, e1', e2')))
              e1 e2
        | Mult (v, e1, e2) ->
            binop
              (fun existing_names e1' e2' ->
                (existing_names, Mult (v, e1', e2')))
              e1 e2
        | BoolLit (v, b) -> Ok (existing_names, BoolLit (v, b))
        | BNot (v, e1) ->
            unop (fun existing_names e1' -> (existing_names, BNot (v, e1'))) e1
        | BOr (v, e1, e2) ->
            binop
              (fun existing_names e1' e2' ->
                (existing_names, BOr (v, e1', e2')))
              e1 e2
        | BAnd (v, e1, e2) ->
            binop
              (fun existing_names e1' e2' ->
                (existing_names, BAnd (v, e1', e2')))
              e1 e2
        | Pair (v, e1, e2) ->
            binop
              (fun existing_names e1' e2' ->
                (existing_names, Pair (v, e1', e2')))
              e1 e2
        | Eq (v, e1, e2) ->
            binop
              (fun existing_names e1' e2' -> (existing_names, Eq (v, e1', e2')))
              e1 e2
        | Gt (v, e1, e2) ->
            binop
              (fun existing_names e1' e2' -> (existing_names, Gt (v, e1', e2')))
              e1 e2
        | GtEq (v, e1, e2) ->
            binop
              (fun existing_names e1' e2' ->
                (existing_names, GtEq (v, e1', e2')))
              e1 e2
        | Lt (v, e1, e2) ->
            binop
              (fun existing_names e1' e2' -> (existing_names, Lt (v, e1', e2')))
              e1 e2
        | LtEq (v, e1, e2) ->
            binop
              (fun existing_names e1' e2' ->
                (existing_names, LtEq (v, e1', e2')))
              e1 e2
        | If (v, e1, e2, e3) ->
            preprocess_expr (existing_names, e1)
            >>= fun (existing_names, e1') ->
            preprocess_expr (existing_names, e2)
            >>= fun (existing_names, e2') ->
            preprocess_expr (existing_names, e3)
            >>| fun (existing_names, e3') ->
            (existing_names, If (v, e1', e2', e3'))
        | Var (v, name) -> Ok (existing_names, Var (v, name))
        | Let _ ->
            failwith "TODO - handle recursive and non-recursive let-bindings"
        | Fun (v, (param, typ), body) ->
            preprocess_expr (existing_names, body)
            >>| fun (existing_names, body') ->
            (existing_names, Fun (v, (param, typ), body'))
        | App (v, e1, e2) ->
            binop
              (fun existing_names e1' e2' ->
                (existing_names, App (v, e1', e2')))
              e1 e2
        | Fix _ -> Error MisplacedFixNode
        | Match (v, e1, cs) ->
            preprocess_expr (existing_names, e1)
            >>=
            fun ((existing_names : StringSet.t), (e1' : preprocessed_tag_expr))
            ->
            Nonempty_list.fold_result_consume_init ~init:() cs
              ~f:(fun
                  (acc :
                    ( unit,
                      StringSet.t
                      * (tag_pattern * preprocessed_tag_expr) Nonempty_list.t
                    )
                    Either.t)
                  ((p : tag_pattern), (e : tag_expr))
                ->
                preprocess_expr (existing_names, e)
                >>| fun (existing_names, e') ->
                match acc with
                | First () -> (existing_names, Nonempty_list.singleton (p, e'))
                | Second (existing_names, acc_cases) ->
                    (existing_names, Nonempty_list.cons (p, e') acc_cases))
            >>=
            fun ( (existing_names : StringSet.t),
                  (preprocessed_cases :
                    (tag_pattern * preprocessed_tag_expr) Nonempty_list.t) )
            ->
            Nonempty_list.fold_result_consume_init ~init:()
              ~f:(fun
                  (acc :
                    ( unit,
                      StringSet.t
                      * (flat_pattern * preprocessed_tag_expr) Nonempty_list.t
                    )
                    Either.t)
                  (p, e)
                ->
                let existing_names, acc_fn =
                  match acc with
                  | First () -> (existing_names, Nonempty_list.singleton)
                  | Second (existing_names, acc_cases) ->
                      (existing_names, Fn.flip Nonempty_list.cons acc_cases)
                in
                flatten_case_pattern (existing_names, p, e)
                >>| fun (existing_names, flattened_p, flattened_e) ->
                (existing_names, acc_fn (flattened_p, flattened_e)))
              preprocessed_cases
            >>| fun (existing_names, preprocessed_cases) ->
            (existing_names, Match (v, e1', preprocessed_cases))
        | Constructor (v, name, expr) ->
            unop
              (fun existing_names e' ->
                (existing_names, Constructor (v, name, e')))
              expr
    end

    (** Interactions with the SMT solver and state that can be provided to it *)
    module Smt = struct
      (** Details for a variable definition in the state provided to the SMT
          solver *)
      type state_elem_var_defn = {
        name : varname;
        t : vtype;
        recursive : bool;
        body : ExprPreprocessor.preprocessed_tag_expr;
      }

      (** A single item in the state provided to the SMT solver *)
      type state_elem =
        | VarDecl of varname * vtype
        | VarDefn of state_elem_var_defn
        | VariantTypeDefn of variant_type

      (** A state that can be provided to the SMT solver *)
      type state = state_elem list

      (** Functions for creating node representations of different parts of a
          program *)
      module SmtlibNodeRepresentation = struct
        open LispBuilder

        let get_vt_constructor_accessor_name (vt_name : string)
            (c_name : string) : string =
          sprintf "%s-%s-getval" vt_name c_name |> custom_special_prefix

        (** Representation of a vtype *)
        let node_of_vtype : vtype -> node = failwith "TODO"

        (** Representation of a variable declaration without definition *)
        let node_of_var_decl (xname : varname) (xt : vtype) : node =
          Op ("declare-fun", [ Atom xname; Unit; node_of_vtype xt ])

        (** Representation of a variant type definition *)
        let node_of_variant_type ((vt_name, vt_cs) : variant_type) : node =
          let for_constructor ((c_name, c_t) : variant_type_constructor) : node
              =
            Op
              ( c_name,
                [
                  Op
                    ( get_vt_constructor_accessor_name vt_name c_name,
                      [ node_of_vtype c_t ] );
                ] )
          in
          Op
            ( "declare-datatypes",
              [ Unit; List [ Op (vt_name, List.map ~f:for_constructor vt_cs) ] ]
            )

        (** Representation of a flattened pattern *)
        let node_of_flat_pattern : ExprPreprocessor.flat_pattern -> node =
          let open ExprPreprocessor in
          function
          | FlatPatName (_, xname, _) -> Atom xname
          | FlatPatPair _ -> failwith "TODO - once pair type definition done"
          | FlatPatConstructor (_, cname, (_, vname, _)) ->
              Op (cname, [ Atom vname ])

        (** Representation of an expression *)
        let rec node_of_expr : ExprPreprocessor.preprocessed_tag_expr -> node =
          function
          | UnitLit _ -> failwith "TODO - once unit type definition done"
          | IntLit (_, x) -> Atom (Int.to_string x)
          | Add (_, e1, e2) -> Op ("+", [ node_of_expr e1; node_of_expr e2 ])
          | Neg (_, e1) -> Op ("-", [ node_of_expr e1 ])
          | Subtr (_, e1, e2) -> Op ("-", [ node_of_expr e1; node_of_expr e2 ])
          | Mult (_, e1, e2) -> Op ("*", [ node_of_expr e1; node_of_expr e2 ])
          | BoolLit (_, b) -> Atom (if b then "true" else "false")
          | BNot (_, e1) -> Op ("not", [ node_of_expr e1 ])
          | BOr (_, e1, e2) -> Op ("or", [ node_of_expr e1; node_of_expr e2 ])
          | BAnd (_, e1, e2) -> Op ("and", [ node_of_expr e1; node_of_expr e2 ])
          | Pair _ -> failwith "TODO - once pair type definition done"
          | Eq (_, e1, e2) -> Op ("=", [ node_of_expr e1; node_of_expr e2 ])
          | Gt (_, e1, e2) -> Op (">", [ node_of_expr e1; node_of_expr e2 ])
          | GtEq (_, e1, e2) -> Op (">=", [ node_of_expr e1; node_of_expr e2 ])
          | Lt (_, e1, e2) -> Op ("<", [ node_of_expr e1; node_of_expr e2 ])
          | LtEq (_, e1, e2) -> Op ("<=", [ node_of_expr e1; node_of_expr e2 ])
          | If (_, e1, e2, e3) ->
              Op ("ite", [ node_of_expr e1; node_of_expr e2; node_of_expr e3 ])
          | Var (_, vname) -> Atom vname
          | LetNoRec (_, xname, e1, e2) ->
              Op ("let", [ Op (xname, [ node_of_expr e1 ]); node_of_expr e2 ])
          | LetRec _ ->
              failwith
                "TODO - need to keep track of let-rec bindings made to insert \
                 them at the start, and with fresh names"
          | Fun (_, (xname, xtype), e1) ->
              Op
                ( "lambda",
                  [
                    List [ Op (xname, [ node_of_vtype xtype ]) ];
                    node_of_expr e1;
                  ] )
          | App (_, e1, e2) ->
              Op ("select", [ node_of_expr e1; node_of_expr e2 ])
          | Match (_, e1, cs) ->
              let node_of_case
                  ( (p : ExprPreprocessor.flat_pattern),
                    (e : ExprPreprocessor.preprocessed_tag_expr) ) =
                Op ("case", [ node_of_flat_pattern p; node_of_expr e ])
              in
              Op
                ( "match",
                  [ node_of_expr e1 ]
                  @ List.map ~f:node_of_case (Nonempty_list.to_list cs) )
          | Constructor (_, cname, e1) -> Op (cname, [ node_of_expr e1 ])
      end

      (** The initial state *)
      let init : state =
        (* TODO - this should include the built-in type definitions (ie. unit, pairs, etc.) *)
        []

      (** Add a variable declaration without a definition *)
      let add_variable_declaration (xname : varname) (xt : vtype) :
          state -> state =
        List.cons (VarDecl (xname, xt))

      (** Add a variant type definition *)
      let add_variant_definition (vt : variant_type) : state -> state =
        List.cons (VariantTypeDefn vt)

      (** Create, or overwrite (if already existed), a variable's definition *)
      let write_variable_value
          ( (name : varname),
            (t : vtype),
            (recursive : [ `Recursive | `NonRecursive ]),
            (body : ExprPreprocessor.preprocessed_tag_expr) ) (state : state) :
          state =
        let recursive =
          match recursive with `Recursive -> true | `NonRecursive -> false
        in
        VarDefn { name; t; recursive; body } :: state

      (** Build a state into a representation of the state's type and variable
          definitions. To use this in the solver, the assertions need to be
          appended to this value *)
      let build (state : state) :
          (LispBuilder.node list, quotient_typing_error) Result.t =
        let open Result in
        let open LispBuilder in
        let define_fun_node
            ( (xname : string),
              (xt : vtype),
              (body : ExprPreprocessor.preprocessed_tag_expr) ) :
            (node, quotient_typing_error) Result.t =
          Op
            ( "define-fun",
              [
                Atom xname;
                SmtlibNodeRepresentation.node_of_vtype xt;
                SmtlibNodeRepresentation.node_of_expr body;
              ] )
          |> Ok
        in
        let define_fun_rec_node
            ( (xname : string),
              (xt : vtype),
              (body : ExprPreprocessor.preprocessed_tag_expr) ) :
            (node, quotient_typing_error) Result.t =
          Op
            ( "define-fun-rec",
              [
                Atom xname;
                SmtlibNodeRepresentation.node_of_vtype xt;
                SmtlibNodeRepresentation.node_of_expr body;
              ] )
          |> Ok
        in
        List.fold_result ~init:([], StringSet.empty)
          ~f:(fun (acc_exprs, acc_defined) -> function
            | VarDecl (xname, xt) ->
                Ok
                  ( SmtlibNodeRepresentation.node_of_var_decl xname xt
                    :: acc_exprs,
                    Set.add acc_defined xname )
            | VarDefn x ->
                if Set.mem acc_defined x.name then Ok (acc_exprs, acc_defined)
                else
                  (* Create the node that defines the value, then append to the accumulator *)
                  (if x.recursive then define_fun_rec_node (x.name, x.t, x.body)
                   else define_fun_node (x.name, x.t, x.body))
                  >>| fun defn_node ->
                  (defn_node :: acc_exprs, Set.add acc_defined x.name)
            | VariantTypeDefn vt ->
                Ok
                  ( SmtlibNodeRepresentation.node_of_variant_type vt :: acc_exprs,
                    acc_defined ))
          state
        >>| fst
    end

    let rec check_quotient_types_in_expr
        (orig_e : ExprPreprocessor.preprocessed_tag_expr)
        (((state : Smt.state), (type_ctx : TypeCtx.t)) as param) :
        (unit, quotient_typing_error) Result.t =
      let open Result in
      let open ExprPreprocessor in
      match orig_e with
      | UnitLit _ | IntLit _ | BoolLit _ | Var _ -> Ok ()
      | Neg (_, e1) | BNot (_, e1) | Constructor (_, _, e1) ->
          check_quotient_types_in_expr e1 param
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
      | LtEq (_, e1, e2)
      | App (_, e1, e2) ->
          check_quotient_types_in_expr e1 param >>= fun () ->
          check_quotient_types_in_expr e2 param
      | If (_, e1, e2, e3) ->
          check_quotient_types_in_expr e1 param >>= fun () ->
          check_quotient_types_in_expr e2 param >>= fun () ->
          check_quotient_types_in_expr e3 param
      | LetNoRec (_, xname, e1, e2) ->
          check_quotient_types_in_expr e1 param >>= fun () ->
          let e1_type = (ExprPreprocessor.preprocessed_expr_node_val e1).t in
          let state' =
            Smt.write_variable_value (xname, e1_type, `NonRecursive, e1) state
          in
          check_quotient_types_in_expr e2 (state', type_ctx)
      | LetRec (_, fname, params, e1, e2) ->
          let ftype =
            Nonempty_list.fold ~init:(preprocessed_expr_node_val e1).t
              ~f:(fun acc (_, xt) -> VTypeFun (xt, acc))
              params
          in
          let state_for_let_expr_checking : Smt.state =
            (* This is the state with the function name declared, as well as the function's parameters declared *)
            let state_with_fn_decl =
              Smt.add_variable_declaration fname ftype state
            in
            Nonempty_list.fold ~init:state_with_fn_decl
              ~f:(fun acc (pname, ptype) ->
                Smt.add_variable_declaration pname ptype acc)
              params
          in
          check_quotient_types_in_expr e1 (state_for_let_expr_checking, type_ctx)
          >>= fun () ->
          let state_for_in_expr_checking : Smt.state =
            (* This is the state with the function name defined *)
            Smt.write_variable_value (fname, ftype, `Recursive, e1) state
          in
          check_quotient_types_in_expr e2 (state_for_in_expr_checking, type_ctx)
      | Fun (_, (xname, xt), e1) ->
          let state' = Smt.add_variable_declaration xname xt state in
          check_quotient_types_in_expr e1 (state', type_ctx)
      | Match (_, e1, _) ->
          let e1_type : vtype =
            (ExprPreprocessor.preprocessed_expr_node_val e1).t
          in
          let e1_type_is_quotient_type : bool =
            match e1_type with
            | VTypeCustom ct_name -> (
                match TypeCtx.find_type_defn_by_name type_ctx ct_name with
                | Some (QuotientType _) -> true
                | Some _ -> false
                | None ->
                    failwith
                      (sprintf "Failed to find type definition for: %s" ct_name)
                )
            | _ -> false
          in
          if e1_type_is_quotient_type then
            failwith "TODO - perform the checking"
          else
            check_quotient_types_in_expr e1 param >>= fun () ->
            failwith "TODO - recurse into each of the cases"

    let check_quotient_types (prog : (ast_tag, pattern_tag) program) :
        (unit, quotient_typing_error) Result.t =
      let open Result in
      let state : Smt.state =
        List.fold ~init:Smt.init
          ~f:(fun acc -> function
            | VariantType vt -> Smt.add_variant_definition vt acc
            | QuotientType _ ->
                (* Note that quotient types don't need to be defined for the SMT solver, they only affect what constraints are generated *)
                acc)
          prog.custom_types
      in
      TypeCtx.create ~custom_types:prog.custom_types
      |> Result.map_error ~f:(fun err -> ProgramTypingError err)
      >>= fun type_ctx ->
      ExprPreprocessor.preprocess_expr (program_existing_names prog, prog.e)
      >>= fun (_, preprocessed_body) ->
      check_quotient_types_in_expr preprocessed_body (state, type_ctx)
  end
