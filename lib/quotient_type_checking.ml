open Core
open Utils
open Vtype
open Variant_types
open Varname
open Pattern
open Ast
open Custom_types
open Program
open Typing

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

    type ast_tag = { t : vtype } [@@deriving sexp, equal]
    type pattern_tag = { t : vtype } [@@deriving sexp, equal]
    type tag_pattern = pattern_tag pattern
    type tag_expr = (ast_tag, pattern_tag) expr
    type tag_program = (ast_tag, pattern_tag) program

    type quotient_typing_error =
      | ProgramTypingError of typing_error
          (** A standard typing error occured *)
      | UnexpectedTrivialMatchCasePatternError
          (** The pattern for a case of a match construct was the trivial case
              (matching all variables, equivalent to just renaming a variable)
              unexpectedly and cannot be handled *)
      | PairTypeNotDefinedInState of vtype * vtype

    let custom_special_name x =
      "PQ-"
      ^ (function
          | `Var _ -> "Var-"
          | `VariantType _ -> "VT-"
          | `VariantTypeConstructor _ -> "VTC-"
          | `VariantTypeConstructorAccessor _ -> "VTCA-"
          | `Function _ -> "Fun-")
          x
      ^
      match x with
      | `Var name -> name
      | `VariantType (vt_name : string) -> vt_name
      | `VariantTypeConstructor ((vt_name : string), (c_name : string)) ->
          vt_name ^ "-" ^ c_name
      | `VariantTypeConstructorAccessor ((vt_name : string), (c_name : string))
        ->
          vt_name ^ "-" ^ c_name ^ "-val"
      | `Function (f_name : string) -> f_name

    (** Generate a fresh variable name, given a set of the currently-defined
        names *)
    let generate_fresh_varname (existing_names : StringSet.t) :
        varname * StringSet.t =
      (* TODO - have a "seed" name, so that the resulting name can look similar to the source code's variables' names *)
      let rec loop (i : int) : varname =
        let candidate = sprintf "x%d" i in
        if Set.mem existing_names candidate then loop (i + 1) else candidate
      in
      let new_name = loop 0 in
      (new_name, Set.add existing_names new_name)

    (** Provides functionality for flattened patterns in AST expressions *)
    module FlatPattern = struct
      type t =
        | FlatPatPair of
            pattern_tag
            * (pattern_tag * varname * vtype)
            * (pattern_tag * varname * vtype)
        | FlatPatConstructor of
            pattern_tag * string * (pattern_tag * varname * vtype)
      [@@deriving sexp, equal]

      type flat_pattern = t [@@deriving sexp, equal]

      type flat_expr =
        | UnitLit of ast_tag
        | IntLit of ast_tag * int
        | Add of ast_tag * flat_expr * flat_expr
        | Neg of ast_tag * flat_expr
        | Subtr of ast_tag * flat_expr * flat_expr
        | Mult of ast_tag * flat_expr * flat_expr
        | BoolLit of ast_tag * bool
        | BNot of ast_tag * flat_expr
        | BOr of ast_tag * flat_expr * flat_expr
        | BAnd of ast_tag * flat_expr * flat_expr
        | Pair of ast_tag * flat_expr * flat_expr
        | Eq of ast_tag * flat_expr * flat_expr
        | Gt of ast_tag * flat_expr * flat_expr
        | GtEq of ast_tag * flat_expr * flat_expr
        | Lt of ast_tag * flat_expr * flat_expr
        | LtEq of ast_tag * flat_expr * flat_expr
        | If of ast_tag * flat_expr * flat_expr * flat_expr
        | Var of ast_tag * string
        | Let of ast_tag * varname * flat_expr * flat_expr
        | App of ast_tag * flat_expr * flat_expr
        | Match of
            ast_tag * flat_expr * (flat_pattern * flat_expr) Nonempty_list.t
        | Constructor of ast_tag * string * flat_expr
      [@@deriving sexp, equal]

      type flat_top_level_defn = {
        recursive : bool;
        name : string;
        param : varname * vtype;
        return_t : vtype;
        body : flat_expr;
      }
      [@@deriving sexp, equal]

      type flat_program = {
        custom_types : custom_type list;
        top_level_defns : flat_top_level_defn list;
        e : flat_expr;
      }
      [@@deriving sexp, equal]

      let flat_pattern_node_val : flat_pattern -> pattern_tag = function
        | FlatPatPair (v, _, _) -> v
        | FlatPatConstructor (v, _, _) -> v

      let flat_node_val : flat_expr -> ast_tag = function
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
        | Match (v, _, _) -> v
        | Constructor (v, _, _) -> v

      (** Get the list of variables and their types that this pattern introduces
          to its case expression's variable context *)
      let defined_vars : t -> (varname * vtype) list = function
        | FlatPatPair (_, (_, x1name, x1type), (_, x2name, x2type)) ->
            [ (x1name, x1type); (x2name, x2type) ]
        | FlatPatConstructor (_, _, (_, xname, xtype)) -> [ (xname, xtype) ]

      (** Flatten a single case of a Match node so that the pattern is a flat
          pattern, and modify the case expression to perform any subsequent
          matching as needed *)
      let rec flatten_case_pattern ~(existing_names : StringSet.t)
          ((p : tag_pattern), (e : flat_expr)) :
          ( StringSet.t * flat_pattern * flat_expr,
            quotient_typing_error )
          Result.t =
        let open Result in
        let outer_expr_type : vtype = (flat_node_val e).t in
        match p with
        | PatName _ -> Error UnexpectedTrivialMatchCasePatternError
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

      (** Convert an AST expression to a flat expression *)
      let rec of_ast ~(existing_names : StringSet.t) :
          tag_expr -> (StringSet.t * flat_expr, quotient_typing_error) Result.t
          =
        let open Result in
        let unop (recomb : StringSet.t -> flat_expr -> StringSet.t * flat_expr)
            ~(existing_names : StringSet.t) (e1 : tag_expr) :
            (StringSet.t * flat_expr, quotient_typing_error) Result.t =
          of_ast ~existing_names e1 >>= fun (existing_names, e1') ->
          Ok (recomb existing_names e1')
        in
        let binop
            (recomb :
              StringSet.t -> flat_expr -> flat_expr -> StringSet.t * flat_expr)
            ~(existing_names : StringSet.t) (e1 : tag_expr) (e2 : tag_expr) :
            (StringSet.t * flat_expr, quotient_typing_error) Result.t =
          of_ast ~existing_names e1 >>= fun (existing_names, e1') ->
          of_ast ~existing_names e2 >>= fun (existing_names, e2') ->
          Ok (recomb existing_names e1' e2')
        in
        function
        | UnitLit v -> Ok (existing_names, UnitLit v)
        | IntLit (v, x) -> Ok (existing_names, IntLit (v, x))
        | Add (v, e1, e2) ->
            binop
              (fun existing_names e1' e2' ->
                (existing_names, Add (v, e1', e2')))
              ~existing_names e1 e2
        | Neg (v, e) ->
            unop
              (fun existing_names e' -> (existing_names, Neg (v, e')))
              ~existing_names e
        | Subtr (v, e1, e2) ->
            binop
              (fun existing_names e1' e2' ->
                (existing_names, Subtr (v, e1', e2')))
              ~existing_names e1 e2
        | Mult (v, e1, e2) ->
            binop
              (fun existing_names e1' e2' ->
                (existing_names, Mult (v, e1', e2')))
              ~existing_names e1 e2
        | BoolLit (v, b) -> Ok (existing_names, BoolLit (v, b))
        | BNot (v, e) ->
            unop
              (fun existing_names e' -> (existing_names, BNot (v, e')))
              ~existing_names e
        | BOr (v, e1, e2) ->
            binop
              (fun existing_names e1' e2' ->
                (existing_names, BOr (v, e1', e2')))
              ~existing_names e1 e2
        | BAnd (v, e1, e2) ->
            binop
              (fun existing_names e1' e2' ->
                (existing_names, BAnd (v, e1', e2')))
              ~existing_names e1 e2
        | Pair (v, e1, e2) ->
            binop
              (fun existing_names e1' e2' ->
                (existing_names, Pair (v, e1', e2')))
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
              (fun existing_names e1' e2' ->
                (existing_names, GtEq (v, e1', e2')))
              ~existing_names e1 e2
        | Lt (v, e1, e2) ->
            binop
              (fun existing_names e1' e2' -> (existing_names, Lt (v, e1', e2')))
              ~existing_names e1 e2
        | LtEq (v, e1, e2) ->
            binop
              (fun existing_names e1' e2' ->
                (existing_names, LtEq (v, e1', e2')))
              ~existing_names e1 e2
        | If (v, e1, e2, e3) ->
            of_ast ~existing_names e1 >>= fun (existing_names, e1') ->
            of_ast ~existing_names e2 >>= fun (existing_names, e2') ->
            of_ast ~existing_names e3 >>| fun (existing_names, e3') ->
            (existing_names, If (v, e1', e2', e3'))
        | Var (v, name) -> Ok (existing_names, Var (v, name))
        | Let (v, xname, e1, e2) ->
            binop
              (fun existing_names e1' e2' ->
                (existing_names, Let (v, xname, e1', e2')))
              ~existing_names e1 e2
        | App (v, e1, e2) ->
            binop
              (fun existing_names e1' e2' ->
                (existing_names, App (v, e1', e2')))
              ~existing_names e1 e2
        | Match (v, e, cs) ->
            of_ast ~existing_names e >>= fun (existing_names, e') ->
            Nonempty_list.fold_result_consume_init ~init:existing_names
              ~f:(fun acc (p, e) ->
                let existing_names =
                  match acc with
                  | First existing_names -> existing_names
                  | Second (existing_names, _) -> existing_names
                in
                of_ast ~existing_names e >>= fun (existing_names, e') ->
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
              ~existing_names e

      (** Convert a program to a flat program *)
      let of_program ~(existing_names : StringSet.t) (prog : tag_program) :
          (StringSet.t * flat_program, quotient_typing_error) Result.t =
        let open Result in
        List.fold_result ~init:(existing_names, [])
          ~f:(fun
              (existing_names, acc_defns_rev)
              (defn : (ast_tag, pattern_tag) top_level_defn)
            ->
            of_ast ~existing_names defn.body >>| fun (existing_names, body') ->
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
        of_ast ~existing_names prog.e >>| fun (existing_names, e') ->
        ( existing_names,
          {
            custom_types = prog.custom_types;
            top_level_defns = List.rev flat_defns_rev;
            e = e';
          } )
    end

    module Smt = struct
      module State = struct
        type var_defn = {
          name : varname;
          kind :
            [ `NonRec of (varname * vtype) option | `Rec of varname * vtype ];
          return_t : vtype;
          body : FlatPattern.flat_expr;
        }
        [@@deriving sexp, equal]

        type top_level_elem = VarDecl of varname * vtype | VarDefn of var_defn
        [@@deriving sexp, equal]

        module VtypePairMap = Map.Make (struct
          type t = vtype * vtype

          let compare ((t1_1, t1_2) : vtype * vtype)
              ((t2_1, t2_2) : vtype * vtype) : int =
            let first_comp = compare_vtype t1_1 t2_1 in
            if first_comp = 0 then compare_vtype t1_2 t2_2 else first_comp

          let t_of_sexp = pair_of_sexp vtype_of_sexp vtype_of_sexp
          let sexp_of_t = sexp_of_pair sexp_of_vtype sexp_of_vtype
        end)

        type variant_type_constructor_info = {
          name : string;
          accessor_name : string;
          t : vtype;
        }
        [@@deriving sexp, equal]

        type variant_type_info = {
          name : string;
          constructors : variant_type_constructor_info list;
        }
        [@@deriving sexp, equal]

        type pair_type_info = {
          name : string;
          t : vtype * vtype;
          constructor_name : string;
          fst_accessor_name : string;
          snd_accessor_name : string;
        }
        [@@deriving sexp, equal]

        type t = {
          pair_types_defined : pair_type_info VtypePairMap.t;
              (** A mapping from all pair types that have been defined to info
                  about them. This used to decide which possible instantiations
                  of the pair type to create in the initial datatype
                  declarations. Added automatically by module *)
          special_variant_types : LispBuilder.node list;
              (** These should be translated by: x -> "(declare-datatypes ()
                  x)". Created automatically in state initialization *)
          variant_types : variant_type_info list;
              (** The custom variant types defined. Added by module user *)
          declared_consts : (string * vtype * LispBuilder.node) list;
              (** Translated by: (xname, xtype, xnode) -> (declare-const xname
                  xtype xnode ). Created automatically in state initialization
              *)
          top_level_rev : top_level_elem list;
              (** The reversed list of top-level definitions of the program.
                  Added by module user *)
        }
        [@@deriving sexp, equal]

        let vt_unit_name : string = custom_special_name (`VariantType "unit")

        let vt_unit_constructor_name : string =
          custom_special_name (`VariantTypeConstructor ("unit", "unit"))

        let vt_unit : variant_type =
          (vt_unit_name, [ (vt_unit_constructor_name, VTypeUnit) ])

        let vt_unit_val : string = custom_special_name (`Var "unit")

        let state_initial : t =
          {
            pair_types_defined =
              (VtypePairMap.empty : pair_type_info VtypePairMap.t);
            special_variant_types =
              LispBuilder.
                [
                  (* Unit type *)
                  Op (vt_unit_name, [ Atom vt_unit_constructor_name ]);
                ];
            variant_types = [];
            declared_consts = [ (vt_unit_val, VTypeUnit, Atom vt_unit_name) ];
            top_level_rev = [];
          }

        (** Once defining signatures, keep this private! *)
        let state_add_pair_type ((t1 : vtype), (t2 : vtype)) (state : t) : t =
          let pair_type_vt_name : vtype * vtype -> string =
            (* Generates an implicitly-unique name for the instantiation of the
            pair type for the specified two parameter types *)
            let rec aux ((t1 : vtype), (t2 : vtype)) : string =
              custom_special_name (`VariantType "pair")
              ^ sprintf "-+%s++%s+" (vtype_name t1) (vtype_name t2)
            and vtype_name : vtype -> string = function
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
            custom_special_name
              (`VariantTypeConstructorAccessor (vt_name, constructor_name))
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

        let state_get_pair_type_info ((t1 : vtype), (t2 : vtype)) (state : t) :
            (pair_type_info, quotient_typing_error) Result.t =
          match Map.find state.pair_types_defined (t1, t2) with
          | Some info -> Ok info
          | None -> Error (PairTypeNotDefinedInState (t1, t2))

        let state_use_variant_type (vt : variant_type) (state : t) :
            t * variant_type_info =
          let vt_name, constructors = vt in
          let constructor_infos : variant_type_constructor_info list =
            List.map
              ~f:(fun (c_name, c_t) ->
                let name =
                  custom_special_name
                    (`VariantTypeConstructor (vt_name, c_name))
                in
                {
                  name;
                  accessor_name =
                    custom_special_name
                      (`VariantTypeConstructorAccessor (vt_name, c_name));
                  t = c_t;
                })
              constructors
          in
          let vt_info : variant_type_info =
            { name = vt_name; constructors = constructor_infos }
          in
          let state =
            if
              List.mem ~equal:equal_variant_type_info state.variant_types
                vt_info
            then state
            else { state with variant_types = vt_info :: state.variant_types }
          in
          (state, vt_info)

        let state_add_var_decl ((xname : varname), (xtype : vtype)) (state : t)
            : t =
          {
            state with
            top_level_rev = VarDecl (xname, xtype) :: state.top_level_rev;
          }

        let state_add_var_defn (defn : var_defn) (state : t) : t =
          let rec add_pair_types_used (state : t) : vtype -> t = function
            | VTypeUnit | VTypeInt | VTypeBool | VTypeCustom _ -> state
            | VTypePair (t1, t2) ->
                state_add_pair_type (t1, t2) state |> fun state ->
                add_pair_types_used (add_pair_types_used state t1) t2
            | VTypeFun (t1, t2) ->
                add_pair_types_used (add_pair_types_used state t1) t2
          in
          let rec search_add_pair_types_used (state : t) :
              FlatPattern.flat_expr -> t = function
            | UnitLit v -> add_pair_types_used state v.t
            | IntLit (v, _) -> add_pair_types_used state v.t
            | Add (v, e1, e2) ->
                add_pair_types_used state v.t |> fun state ->
                search_add_pair_types_used state e1 |> fun state ->
                search_add_pair_types_used state e2
            | Neg (v, e1) ->
                add_pair_types_used state v.t |> fun state ->
                search_add_pair_types_used state e1
            | Subtr (v, e1, e2) ->
                add_pair_types_used state v.t |> fun state ->
                search_add_pair_types_used state e1 |> fun state ->
                search_add_pair_types_used state e2
            | Mult (v, e1, e2) ->
                add_pair_types_used state v.t |> fun state ->
                search_add_pair_types_used state e1 |> fun state ->
                search_add_pair_types_used state e2
            | BoolLit (v, _) -> add_pair_types_used state v.t
            | BNot (v, e1) ->
                add_pair_types_used state v.t |> fun state ->
                search_add_pair_types_used state e1
            | BOr (v, e1, e2) ->
                add_pair_types_used state v.t |> fun state ->
                search_add_pair_types_used state e1 |> fun state ->
                search_add_pair_types_used state e2
            | BAnd (v, e1, e2) ->
                add_pair_types_used state v.t |> fun state ->
                search_add_pair_types_used state e1 |> fun state ->
                search_add_pair_types_used state e2
            | Pair (v, e1, e2) ->
                add_pair_types_used state v.t |> fun state ->
                search_add_pair_types_used state e1 |> fun state ->
                search_add_pair_types_used state e2
            | Eq (v, e1, e2) ->
                add_pair_types_used state v.t |> fun state ->
                search_add_pair_types_used state e1 |> fun state ->
                search_add_pair_types_used state e2
            | Gt (v, e1, e2) ->
                add_pair_types_used state v.t |> fun state ->
                search_add_pair_types_used state e1 |> fun state ->
                search_add_pair_types_used state e2
            | GtEq (v, e1, e2) ->
                add_pair_types_used state v.t |> fun state ->
                search_add_pair_types_used state e1 |> fun state ->
                search_add_pair_types_used state e2
            | Lt (v, e1, e2) ->
                add_pair_types_used state v.t |> fun state ->
                search_add_pair_types_used state e1 |> fun state ->
                search_add_pair_types_used state e2
            | LtEq (v, e1, e2) ->
                add_pair_types_used state v.t |> fun state ->
                search_add_pair_types_used state e1 |> fun state ->
                search_add_pair_types_used state e2
            | If (v, e1, e2, e3) ->
                add_pair_types_used state v.t |> fun state ->
                search_add_pair_types_used state e1 |> fun state ->
                search_add_pair_types_used state e2 |> fun state ->
                search_add_pair_types_used state e3
            | Var (v, _) -> add_pair_types_used state v.t
            | Let (v, _, e1, e2) ->
                add_pair_types_used state v.t |> fun state ->
                search_add_pair_types_used state e1 |> fun state ->
                search_add_pair_types_used state e2
            | App (v, e1, e2) ->
                add_pair_types_used state v.t |> fun state ->
                search_add_pair_types_used state e1 |> fun state ->
                search_add_pair_types_used state e2
            | Match (v, e1, cases) ->
                add_pair_types_used state v.t |> fun state ->
                search_add_pair_types_used state e1 |> fun state ->
                Nonempty_list.fold
                  ~f:(fun state (pat, e) ->
                    let pat_t = FlatPattern.flat_pattern_node_val pat in
                    add_pair_types_used state pat_t.t |> fun state ->
                    search_add_pair_types_used state e)
                  ~init:state cases
            | Constructor (v, _, e1) ->
                add_pair_types_used state v.t |> fun state ->
                search_add_pair_types_used state e1
          in
          let state =
            (* Add all used pair types to the state *)
            search_add_pair_types_used state defn.body
          in
          { state with top_level_rev = VarDefn defn :: state.top_level_rev }
      end

      module StateBuilder = struct
        let rec build_vtype (state : State.t) :
            vtype -> (LispBuilder.node, quotient_typing_error) Result.t =
          let open Result in
          let open LispBuilder in
          function
          | VTypeUnit -> Ok (Atom State.vt_unit_name)
          | VTypeInt -> Ok (Atom "Int")
          | VTypeBool -> Ok (Atom "Bool")
          | VTypePair (t1, t2) ->
              State.state_get_pair_type_info (t1, t2) state
              >>| fun pair_type_info -> Atom pair_type_info.name
          | VTypeFun (t1, t2) ->
              build_vtype state t1 >>= fun t1_node ->
              build_vtype state t2 >>= fun t2_node ->
              Ok (Op ("Array", [ t1_node; t2_node ]))
          | VTypeCustom ct_name -> Ok (Atom ct_name)

        let rec build_expr (state : State.t) :
            FlatPattern.flat_expr ->
            (LispBuilder.node, quotient_typing_error) Result.t =
          let open Result in
          let open LispBuilder in
          function
          | UnitLit _ -> Ok (Atom State.vt_unit_val)
          | IntLit (_, n) -> Ok (Atom (Int.to_string n))
          | BoolLit (_, b) -> Ok (Atom (Bool.to_string b))
          | Var (_, name) -> Ok (Atom name)
          | Add (_, e1, e2) ->
              build_expr state e1 >>= fun node1 ->
              build_expr state e2 >>= fun node2 ->
              Ok (Op ("+", [ node1; node2 ]))
          | Neg (_, e) ->
              build_expr state e >>= fun node -> Ok (Op ("-", [ node ]))
          | Subtr (_, e1, e2) ->
              build_expr state e1 >>= fun node1 ->
              build_expr state e2 >>= fun node2 ->
              Ok (Op ("-", [ node1; node2 ]))
          | Mult (_, e1, e2) ->
              build_expr state e1 >>= fun node1 ->
              build_expr state e2 >>= fun node2 ->
              Ok (Op ("*", [ node1; node2 ]))
          | BNot (_, e) ->
              build_expr state e >>= fun node -> Ok (Op ("not", [ node ]))
          | BOr (_, e1, e2) ->
              build_expr state e1 >>= fun node1 ->
              build_expr state e2 >>= fun node2 ->
              Ok (Op ("or", [ node1; node2 ]))
          | BAnd (_, e1, e2) ->
              build_expr state e1 >>= fun node1 ->
              build_expr state e2 >>= fun node2 ->
              Ok (Op ("and", [ node1; node2 ]))
          | Eq (_, e1, e2) ->
              build_expr state e1 >>= fun node1 ->
              build_expr state e2 >>= fun node2 ->
              Ok (Op ("=", [ node1; node2 ]))
          | Gt (_, e1, e2) ->
              build_expr state e1 >>= fun node1 ->
              build_expr state e2 >>= fun node2 ->
              Ok (Op (">", [ node1; node2 ]))
          | GtEq (_, e1, e2) ->
              build_expr state e1 >>= fun node1 ->
              build_expr state e2 >>= fun node2 ->
              Ok (Op (">=", [ node1; node2 ]))
          | Lt (_, e1, e2) ->
              build_expr state e1 >>= fun node1 ->
              build_expr state e2 >>= fun node2 ->
              Ok (Op ("<", [ node1; node2 ]))
          | LtEq (_, e1, e2) ->
              build_expr state e1 >>= fun node1 ->
              build_expr state e2 >>= fun node2 ->
              Ok (Op ("<=", [ node1; node2 ]))
          | If (_, e1, e2, e3) ->
              build_expr state e1 >>= fun node1 ->
              build_expr state e2 >>= fun node2 ->
              build_expr state e3 >>= fun node3 ->
              Ok (Op ("ite", [ node1; node2; node3 ]))
          | Let (_, x, e1, e2) ->
              build_expr state e1 >>= fun node1 ->
              build_expr state e2 >>= fun node2 ->
              Ok (Op ("let", [ List [ List [ Atom x; node1 ] ]; node2 ]))
          | Pair (_, e1, e2) ->
              let e1_t = (FlatPattern.flat_node_val e1).t in
              let e2_t = (FlatPattern.flat_node_val e2).t in
              State.state_get_pair_type_info (e1_t, e2_t) state
              >>= fun pair_type_info ->
              build_expr state e1 >>= fun e1' ->
              build_expr state e2 >>| fun e2' ->
              Op (pair_type_info.constructor_name, [ e1'; e2' ])
          | App _ -> Ok (failwith "TODO - check exact type of first thingy")
          | Constructor (_, name, e) ->
              build_expr state e >>= fun node -> Ok (Op (name, [ node ]))
          | Match (_, e, cases) ->
              build_expr state e >>= fun e_node ->
              let build_case
                  ( (pat : FlatPattern.flat_pattern),
                    (body : FlatPattern.flat_expr) ) :
                  (node, quotient_typing_error) Result.t =
                build_expr state body >>= fun body_node ->
                match pat with
                | FlatPattern.FlatPatPair (_, (_, x1name, x1t), (_, x2name, x2t))
                  ->
                    State.state_get_pair_type_info (x1t, x2t) state
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
              >>| fun case_nodes -> Op ("match", e_node :: case_nodes)

        let build_elem (state : State.t) :
            State.top_level_elem ->
            (LispBuilder.node, quotient_typing_error) Result.t =
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
                  build_expr state body >>= fun body_node ->
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
                  build_expr state body >>= fun body_node ->
                  Ok
                    (Op
                       ( "define-fun-rec",
                         [
                           Atom name;
                           List [ List [ Atom param_name; param_type_node ] ];
                           return_type_node;
                           body_node;
                         ] )))
      end
    end

    let rec check_expr (state : Smt.State.t) :
        FlatPattern.flat_expr -> (unit, quotient_typing_error) Result.t =
      let open Result in
      let open Smt.State in
      function
      | UnitLit _ | IntLit _ | BoolLit _ | Var _ -> Ok ()
      | Neg (_, e) -> check_expr state e
      | BNot (_, e) -> check_expr state e
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
          check_expr state e1 >>= fun () -> check_expr state e2
      | If (_, e1, e2, e3) ->
          check_expr state e1 >>= fun () ->
          check_expr state e2 >>= fun () -> check_expr state e3
      | Let (_, xname, e1, e2) ->
          check_expr state e1 >>= fun () ->
          let e1_type = (FlatPattern.flat_node_val e1).t in
          check_expr
            (state_add_var_defn
               {
                 name = xname;
                 kind = `NonRec None;
                 return_t = e1_type;
                 body = e1;
               }
               state)
            e2
      | App (_, e1, e2) -> check_expr state e1 >>= fun () -> check_expr state e2
      | Match _ -> failwith "TODO"
      | Constructor (_, _, e) -> check_expr state e

    let check_program (prog : tag_program) :
        (unit, quotient_typing_error) Result.t =
      let open Result in
      let existing_names = Program.existing_names prog in
      FlatPattern.of_program ~existing_names prog
      >>= fun (existing_names, flat_prog) ->
      TypeCtx.create ~custom_types:flat_prog.custom_types
      |> Result.map_error ~f:(fun e -> ProgramTypingError e)
      >>= fun type_ctx -> failwith "TODO"
  end
