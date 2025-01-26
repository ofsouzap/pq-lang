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
        | Let of
            ast_tag * varname * preprocessed_tag_expr * preprocessed_tag_expr
        | App of ast_tag * preprocessed_tag_expr * preprocessed_tag_expr
        | Match of
            ast_tag
            * preprocessed_tag_expr
            * (flat_pattern * preprocessed_tag_expr) Nonempty_list.t
        | Constructor of ast_tag * string * preprocessed_tag_expr

      (** A top-level function definition. Contains information about "captured"
          scoped variables that should be provided with calling the function *)
      type function_defn = {
        name : varname;
        scope_vars : (varname * vtype) list;
        args : (varname * vtype) list;
        body : preprocessed_tag_expr;
      }

      (** Complete representation of an AST expression *)
      type preprocessed_ast_repr = {
        funs : function_defn list;
        body : preprocessed_tag_expr;
      }

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
        | Let (v, _, _, _) -> v
        | Var (v, _) -> v
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
            let p1_t = (pattern_node_val p1).t in
            let p2_t = (pattern_node_val p2).t in
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
            let p1_t = (pattern_node_val p1).t in
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

      (** Accumulator for the preprocess_expr function. Values are carried
          across all function calls *)
      type preprocess_expr_acc = {
        fn_defns : function_defn list;
        existing_names : StringSet.t;
      }

      (** State for the preprocess_expr function. Values are "scoped", in that
          they aren't returned by the function, only passed to recursive calls
      *)
      type preprocess_expr_state = {
        scope_vars : (varname * vtype) list;
        scoped_varname_mapping : (varname * varname) list;
      }

      let preprocess_expr :
          StringSet.t * tag_expr ->
          (StringSet.t * preprocessed_ast_repr, quotient_typing_error) Result.t
          =
        let open Result in
        let rec aux (acc : preprocess_expr_acc) (state : preprocess_expr_state)
            (orig_e : tag_expr) :
            ( preprocess_expr_acc * preprocessed_tag_expr,
              quotient_typing_error )
            Result.t =
          let unop
              (recomb :
                preprocess_expr_acc ->
                preprocessed_tag_expr ->
                preprocess_expr_acc * preprocessed_tag_expr) (e1 : tag_expr) =
            aux acc state e1 >>| fun (acc, e1') -> recomb acc e1'
          in
          let binop
              (recomb :
                preprocess_expr_acc ->
                preprocessed_tag_expr ->
                preprocessed_tag_expr ->
                preprocess_expr_acc * preprocessed_tag_expr) (e1 : tag_expr)
              (e2 : tag_expr) =
            aux acc state e1 >>= fun (acc, e1') ->
            aux acc state e2 >>| fun (acc, e2') -> recomb acc e1' e2'
          in
          match orig_e with
          | UnitLit v -> Ok (acc, UnitLit v)
          | IntLit (v, x) -> Ok (acc, IntLit (v, x))
          | Add (v, e1, e2) ->
              binop (fun acc e1' e2' -> (acc, Add (v, e1', e2'))) e1 e2
          | Neg (v, e1) -> unop (fun acc e1' -> (acc, Neg (v, e1'))) e1
          | Subtr (v, e1, e2) ->
              binop (fun acc e1' e2' -> (acc, Subtr (v, e1', e2'))) e1 e2
          | Mult (v, e1, e2) ->
              binop (fun acc e1' e2' -> (acc, Mult (v, e1', e2'))) e1 e2
          | BoolLit (v, b) -> Ok (acc, BoolLit (v, b))
          | BNot (v, e1) -> unop (fun acc e1' -> (acc, BNot (v, e1'))) e1
          | BOr (v, e1, e2) ->
              binop (fun acc e1' e2' -> (acc, BOr (v, e1', e2'))) e1 e2
          | BAnd (v, e1, e2) ->
              binop (fun acc e1' e2' -> (acc, BAnd (v, e1', e2'))) e1 e2
          | Pair (v, e1, e2) ->
              binop (fun acc e1' e2' -> (acc, Pair (v, e1', e2'))) e1 e2
          | Eq (v, e1, e2) ->
              binop (fun acc e1' e2' -> (acc, Eq (v, e1', e2'))) e1 e2
          | Gt (v, e1, e2) ->
              binop (fun acc e1' e2' -> (acc, Gt (v, e1', e2'))) e1 e2
          | GtEq (v, e1, e2) ->
              binop (fun acc e1' e2' -> (acc, GtEq (v, e1', e2'))) e1 e2
          | Lt (v, e1, e2) ->
              binop (fun acc e1' e2' -> (acc, Lt (v, e1', e2'))) e1 e2
          | LtEq (v, e1, e2) ->
              binop (fun acc e1' e2' -> (acc, LtEq (v, e1', e2'))) e1 e2
          | If (v, e1, e2, e3) ->
              aux acc state e1 >>= fun (acc, e1') ->
              aux acc state e2 >>= fun (acc, e2') ->
              aux acc state e3 >>| fun (acc, e3') -> (acc, If (v, e1', e2', e3'))
          | Var (v, name) ->
              List.Assoc.find ~equal:equal_varname state.scoped_varname_mapping
                name
              |> Option.value ~default:name
              |> fun mapped_name -> Ok (acc, Var (v, mapped_name))
          | Let _ ->
              failwith
                "TODO - handle recursive and non-recursive let-bindings. \
                 Recursive ones need to be turned into function definitions. \
                 This function alters the `state` value"
          | Fun (v, (xname, xtype), e1) ->
              let state' =
                { state with scope_vars = (xname, xtype) :: state.scope_vars }
              in
              aux acc state' e1 >>| fun (acc, e1') ->
              let f_name, new_existing_names =
                generate_fresh_varname acc.existing_names
              in
              let scope_vars = state.scope_vars in
              ( {
                  existing_names = new_existing_names;
                  fn_defns =
                    {
                      name = f_name;
                      scope_vars;
                      args = [ (xname, xtype) ];
                      body = e1';
                    }
                    :: acc.fn_defns;
                },
                Var (v, f_name) )
          | App (v, e1, e2) ->
              binop (fun acc e1' e2' -> (acc, App (v, e1', e2'))) e1 e2
          | Fix _ -> Error MisplacedFixNode
          | Match (v, e1, cs) ->
              aux acc state e1 >>= fun (acc, e1') ->
              Nonempty_list.fold_result_consume_init ~init:() cs
                ~f:(fun acc_state ((p : tag_pattern), (e : tag_expr)) ->
                  aux acc state e >>| fun (acc, e') ->
                  match acc_state with
                  | First () -> (acc, Nonempty_list.singleton (p, e'))
                  | Second (acc, acc_cases) ->
                      (acc, Nonempty_list.cons (p, e') acc_cases))
              >>= fun (acc, preprocessed_cases) ->
              Nonempty_list.fold_result_consume_init ~init:()
                ~f:(fun acc_state (p, e) ->
                  let acc, acc_fn =
                    match acc_state with
                    | First () -> (acc, Nonempty_list.singleton)
                    | Second (acc, acc_cases) ->
                        (acc, Fn.flip Nonempty_list.cons acc_cases)
                  in
                  flatten_case_pattern (acc.existing_names, p, e)
                  >>| fun (existing_names, flattened_p, flattened_e) ->
                  ( { acc with existing_names },
                    acc_fn (flattened_p, flattened_e) ))
                preprocessed_cases
              >>| fun (acc, preprocessed_cases) ->
              (acc, Match (v, e1', preprocessed_cases))
          | Constructor (v, name, expr) ->
              unop (fun acc e' -> (acc, Constructor (v, name, e'))) expr
        in
        fun (existing_names, e) ->
          aux
            { fn_defns = []; existing_names }
            { scope_vars = []; scoped_varname_mapping = [] }
            e
          >>| fun (acc, e') ->
          (acc.existing_names, { funs = acc.fn_defns; body = e' })
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
        let rec node_of_vtype : vtype -> node = function
          | VTypeInt -> Atom "Int"
          | VTypeBool -> Atom "Bool"
          | VTypePair _ -> failwith "TODO - once pair type implemented"
          | VTypeFun (t1, t2) ->
              Op ("Array", [ node_of_vtype t1; node_of_vtype t2 ])
          | VTypeUnit -> failwith "TODO - once unit type implemented"
          | VTypeCustom ct_name -> Atom ct_name

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
          | Let (_, xname, e1, e2) ->
              Op
                ( "let",
                  [ List [ Op (xname, [ node_of_expr e1 ]) ]; node_of_expr e2 ]
                )
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
        (ast_repr : ExprPreprocessor.preprocessed_ast_repr)
        (((state : Smt.state), (type_ctx : TypeCtx.t)) as param) :
        (unit, quotient_typing_error) Result.t =
      let open Result in
      let open ExprPreprocessor in
      match ast_repr.body with
      | UnitLit _ | IntLit _ | BoolLit _ | Var _ -> Ok ()
      | Neg (_, e1) | BNot (_, e1) | Constructor (_, _, e1) ->
          check_quotient_types_in_expr { ast_repr with body = e1 } param
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
          check_quotient_types_in_expr { ast_repr with body = e1 } param
          >>= fun () ->
          check_quotient_types_in_expr { ast_repr with body = e2 } param
      | If (_, e1, e2, e3) ->
          check_quotient_types_in_expr { ast_repr with body = e1 } param
          >>= fun () ->
          check_quotient_types_in_expr { ast_repr with body = e2 } param
          >>= fun () ->
          check_quotient_types_in_expr { ast_repr with body = e3 } param
      | Let (_, _, e1, e2) ->
          check_quotient_types_in_expr { ast_repr with body = e1 } param
          >>= fun () ->
          check_quotient_types_in_expr { ast_repr with body = e2 } param
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
            check_quotient_types_in_expr { ast_repr with body = e1 } param
            >>= fun () -> failwith "TODO - recurse into each of the cases"

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
      >>= fun (_, preprocessed_ast_repr) ->
      check_quotient_types_in_expr preprocessed_ast_repr (state, type_ctx)
  end
