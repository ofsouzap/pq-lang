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

    type ast_tag = { t : vtype } [@@deriving sexp, equal]
    type pattern_tag = { t : vtype } [@@deriving sexp, equal]
    type tag_pattern = pattern_tag pattern
    type tag_expr = (ast_tag, pattern_tag) expr
    type tag_program = (ast_tag, pattern_tag) program

    type quotient_typing_error =
      | ProgramTypingError of typing_error
          (** A standard typing error occured *)

    let custom_special_name x =
      "PQ-"
      ^ (function
          | `VariantType _ -> "VT-"
          | `VariantTypeConstructor (_, _) -> "VTC-"
          | `Function _ -> "Fun-")
          x
      ^
      match x with
      | `VariantType (vt_name : string) -> vt_name
      | `VariantTypeConstructor ((vt_name : string), (c_name : string)) ->
          vt_name ^ "-" ^ c_name
      | `Function (f_name : string) -> f_name

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

    (** Provides functionality for flattened patterns in AST expressions *)
    module FlatPattern = struct
      type t =
        | FlatPatName of pattern_tag * varname * vtype
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
        | FlatPatName (v, _, _) -> v
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
        | FlatPatName (_, x, t) -> [ (x, t) ]
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
        type state_vtype =
          | SVTypeUnit
          | SVTypeInt
          | SVTypeBool
          | SVTypePair of state_vtype * state_vtype
          | SVTypeCallableFun of state_vtype * state_vtype
          | SVTypeArrayFun of state_vtype * state_vtype
          | SVTypeCustom of string

        (** Map the type of an expression in a program to a state type. As this
            is the type of an expression, function types will map to the "array
            function" variants of functions, instead of the "callable function"
            variants *)
        let rec expr_vtype_to_state_vtype : vtype -> state_vtype = function
          | VTypeUnit -> SVTypeUnit
          | VTypeInt -> SVTypeInt
          | VTypeBool -> SVTypeBool
          | VTypePair (t1, t2) ->
              SVTypePair
                (expr_vtype_to_state_vtype t1, expr_vtype_to_state_vtype t2)
          | VTypeFun (t1, t2) ->
              SVTypeArrayFun
                (expr_vtype_to_state_vtype t1, expr_vtype_to_state_vtype t2)
          | VTypeCustom c_name -> SVTypeCustom c_name

        type var_defn = {
          name : varname;
          kind :
            [ `NonRec of (varname * state_vtype) option
            | `Rec of varname * state_vtype ];
          return_t : state_vtype;
          body : FlatPattern.flat_expr;
        }

        type top_level_elem =
          | VarDecl of varname * state_vtype
          | VarDefn of var_defn

        type t = {
          variant_types : variant_type list;
          top_level_rev : top_level_elem list;
        }

        let state_empty = []

        let state_add_var_decl ((xname : varname), (xtype : state_vtype))
            (state : t) : t =
          {
            state with
            top_level_rev = VarDecl (xname, xtype) :: state.top_level_rev;
          }

        let state_add_var_defn (defn : var_defn) (state : t) : t =
          { state with top_level_rev = VarDefn defn :: state.top_level_rev }
      end

      module StateBuilder = struct
        let build_state_vtype : State.state_vtype -> LispBuilder.node =
          failwith "TODO"

        let rec build_expr : FlatPattern.flat_expr -> LispBuilder.node =
          function
          | UnitLit _ -> failwith "TODO - once unit type made"
          | IntLit (_, n) -> LispBuilder.Atom (Int.to_string n)
          | BoolLit (_, b) -> LispBuilder.Atom (Bool.to_string b)
          | Var (_, name) -> LispBuilder.Atom name
          | Add (_, e1, e2) ->
              LispBuilder.Op ("+", [ build_expr e1; build_expr e2 ])
          | Neg (_, e) -> LispBuilder.Op ("-", [ build_expr e ])
          | Subtr (_, e1, e2) ->
              LispBuilder.Op ("-", [ build_expr e1; build_expr e2 ])
          | Mult (_, e1, e2) ->
              LispBuilder.Op ("*", [ build_expr e1; build_expr e2 ])
          | BNot (_, e) -> LispBuilder.Op ("not", [ build_expr e ])
          | BOr (_, e1, e2) ->
              LispBuilder.Op ("or", [ build_expr e1; build_expr e2 ])
          | BAnd (_, e1, e2) ->
              LispBuilder.Op ("and", [ build_expr e1; build_expr e2 ])
          | Eq (_, e1, e2) ->
              LispBuilder.Op ("=", [ build_expr e1; build_expr e2 ])
          | Gt (_, e1, e2) ->
              LispBuilder.Op (">", [ build_expr e1; build_expr e2 ])
          | GtEq (_, e1, e2) ->
              LispBuilder.Op (">=", [ build_expr e1; build_expr e2 ])
          | Lt (_, e1, e2) ->
              LispBuilder.Op ("<", [ build_expr e1; build_expr e2 ])
          | LtEq (_, e1, e2) ->
              LispBuilder.Op ("<=", [ build_expr e1; build_expr e2 ])
          | If (_, e1, e2, e3) ->
              LispBuilder.Op
                ("ite", [ build_expr e1; build_expr e2; build_expr e3 ])
          | Let (_, x, e1, e2) ->
              LispBuilder.Op
                ( "let",
                  [
                    LispBuilder.List
                      [ LispBuilder.List [ LispBuilder.Atom x; build_expr e1 ] ];
                    build_expr e2;
                  ] )
          | Pair (_, e1, e2) -> failwith "TODO - once pair type made"
          | App (_, e1, e2) ->
              failwith "TODO - check exact type of first thingy"
          | Constructor (_, name, e) -> LispBuilder.Op (name, [ build_expr e ])
          | Match (_, e, cases) ->
              let build_case (pat, body) =
                (* TODO - this was generated, check it *)
                match pat with
                | FlatPattern.FlatPatName _ ->
                    failwith
                      "TODO - remove this flat pattern node, it shouldn't exist"
                | FlatPattern.FlatPatPair (_, (_, x1, _), (_, x2, _)) ->
                    LispBuilder.List
                      [
                        LispBuilder.List
                          [ LispBuilder.Atom x1; LispBuilder.Atom x2 ];
                        build_expr body;
                      ]
                | FlatPattern.FlatPatConstructor (_, c_name, (_, x, _)) ->
                    LispBuilder.List
                      [
                        LispBuilder.List
                          [ LispBuilder.Atom c_name; LispBuilder.Atom x ];
                        build_expr body;
                      ]
              in
              LispBuilder.Op
                ( "match",
                  [
                    build_expr e;
                    LispBuilder.List
                      (List.map ~f:build_case (Nonempty_list.to_list cases));
                  ] )

        let build_elem : State.elem -> LispBuilder.node = function
          | VarDecl (xname, xtype) ->
              LispBuilder.Op
                ( "declare-const",
                  [ LispBuilder.Atom xname; build_state_vtype xtype ] )
          | VarDefn { name; kind; return_t; body } -> (
              match kind with
              | `NonRec None ->
                  LispBuilder.Op
                    ( "declare-const",
                      [ LispBuilder.Atom name; build_state_vtype return_t ] )
              | `NonRec (Some (param_name, param_t)) ->
                  LispBuilder.Op
                    ( "define-fun",
                      [
                        LispBuilder.Atom name;
                        LispBuilder.List
                          [
                            LispBuilder.List
                              [
                                LispBuilder.Atom param_name;
                                build_state_vtype param_t;
                              ];
                          ];
                        build_state_vtype return_t;
                        build_expr body;
                      ] )
              | `Rec (param_name, param_t) ->
                  LispBuilder.Op
                    ( "define-fun-rec",
                      [
                        LispBuilder.Atom name;
                        LispBuilder.List
                          [
                            LispBuilder.List
                              [
                                LispBuilder.Atom param_name;
                                build_state_vtype param_t;
                              ];
                          ];
                        build_state_vtype return_t;
                        build_expr body;
                      ] ))

        let build (state_rev : State.t) : LispBuilder.node list =
          let state = List.rev state_rev in
          List.map state ~f:build_elem
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
