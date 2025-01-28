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

        We use flattened patterns.

        Also, we apply the "unscoping" transformation to let-rec bindings, where
        functions don't capture variables, and instead will be provided all
        values as parameters. To help with this, the function application node
        contains a list of arguments to provide, instead of only allowing for a
        single argument *)
    module QuotientAnalysisRepr = struct
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

        type flat_pattern = t

        type flat_ast =
          | UnitLit of ast_tag
          | IntLit of ast_tag * int
          | Add of ast_tag * flat_ast * flat_ast
          | Neg of ast_tag * flat_ast
          | Subtr of ast_tag * flat_ast * flat_ast
          | Mult of ast_tag * flat_ast * flat_ast
          | BoolLit of ast_tag * bool
          | BNot of ast_tag * flat_ast
          | BOr of ast_tag * flat_ast * flat_ast
          | BAnd of ast_tag * flat_ast * flat_ast
          | Pair of ast_tag * flat_ast * flat_ast
          | Eq of ast_tag * flat_ast * flat_ast
          | Gt of ast_tag * flat_ast * flat_ast
          | GtEq of ast_tag * flat_ast * flat_ast
          | Lt of ast_tag * flat_ast * flat_ast
          | LtEq of ast_tag * flat_ast * flat_ast
          | If of ast_tag * flat_ast * flat_ast * flat_ast
          | Var of ast_tag * string
          | LetNoRec of ast_tag * varname * flat_ast * flat_ast
          | LetRec of
              ast_tag
              * varname
              * (varname * vtype)
              * vtype
              * flat_ast
              * flat_ast
          | Fun of ast_tag * (varname * vtype) * flat_ast
          | App of ast_tag * flat_ast * flat_ast
          | Match of
              ast_tag * flat_ast * (flat_pattern * flat_ast) Nonempty_list.t
          | Constructor of ast_tag * string * flat_ast

        let flat_pattern_node_val : flat_pattern -> pattern_tag = function
          | FlatPatName (v, _, _) -> v
          | FlatPatPair (v, _, _) -> v
          | FlatPatConstructor (v, _, _) -> v

        let flat_node_val : flat_ast -> ast_tag = function
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
          | LetRec (v, _, _, _, _, _) -> v
          | Fun (v, _, _) -> v
          | App (v, _, _) -> v
          | Match (v, _, _) -> v
          | Constructor (v, _, _) -> v

        (** Get the list of variables and their types that this pattern
            introduces to its case expression's variable context *)
        let defined_vars : t -> (varname * vtype) list = function
          | FlatPatName (_, x, t) -> [ (x, t) ]
          | FlatPatPair (_, (_, x1name, x1type), (_, x2name, x2type)) ->
              [ (x1name, x1type); (x2name, x2type) ]
          | FlatPatConstructor (_, _, (_, xname, xtype)) -> [ (xname, xtype) ]

        (** Flatten a single case of a Match node so that the pattern is a flat
            pattern, and modify the case expression to perform any subsequent
            matching as needed *)
        let rec flatten_case_pattern ~(existing_names : StringSet.t)
            ((p : tag_pattern), (e : flat_ast)) :
            ( StringSet.t * flat_pattern * flat_ast,
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
                FlatPatPair
                  (v_pair, (x1_v, x1_name, x1_t), (x2_v, x2_name, x2_t)),
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
          | PatConstructor (v_constructor, c_name, PatName (x_v, x_name, x_t))
            ->
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
            tag_expr -> (StringSet.t * flat_ast, quotient_typing_error) Result.t
            =
          let open Result in
          let unop (recomb : StringSet.t -> flat_ast -> StringSet.t * flat_ast)
              ~(existing_names : StringSet.t) (e1 : tag_expr) :
              (StringSet.t * flat_ast, quotient_typing_error) Result.t =
            of_ast ~existing_names e1 >>= fun (existing_names, e1') ->
            Ok (recomb existing_names e1')
          in
          let binop
              (recomb :
                StringSet.t -> flat_ast -> flat_ast -> StringSet.t * flat_ast)
              ~(existing_names : StringSet.t) (e1 : tag_expr) (e2 : tag_expr) :
              (StringSet.t * flat_ast, quotient_typing_error) Result.t =
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
                (fun existing_names e1' e2' ->
                  (existing_names, Eq (v, e1', e2')))
                ~existing_names e1 e2
          | Gt (v, e1, e2) ->
              binop
                (fun existing_names e1' e2' ->
                  (existing_names, Gt (v, e1', e2')))
                ~existing_names e1 e2
          | GtEq (v, e1, e2) ->
              binop
                (fun existing_names e1' e2' ->
                  (existing_names, GtEq (v, e1', e2')))
                ~existing_names e1 e2
          | Lt (v, e1, e2) ->
              binop
                (fun existing_names e1' e2' ->
                  (existing_names, Lt (v, e1', e2')))
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
                  (existing_names, LetNoRec (v, xname, e1', e2')))
                ~existing_names e1 e2
          | LetRec (v, fname, (xname, xtype), return_t, e1, e2) ->
              binop
                (fun existing_names e1' e2' ->
                  ( existing_names,
                    LetRec (v, fname, (xname, xtype), return_t, e1', e2') ))
                ~existing_names e1 e2
          | Fun (v, xname, e) ->
              unop
                (fun existing_names e' -> (existing_names, Fun (v, xname, e')))
                ~existing_names e
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
      end

      (** Provides functionality for unscoped AST expressions, where functions'
          captured variables are explicitly passed as parameters *)
      module Unscoping = struct
        type unscoped_ast =
          | UnitLit of ast_tag
          | IntLit of ast_tag * int
          | Add of ast_tag * unscoped_ast * unscoped_ast
          | Neg of ast_tag * unscoped_ast
          | Subtr of ast_tag * unscoped_ast * unscoped_ast
          | Mult of ast_tag * unscoped_ast * unscoped_ast
          | BoolLit of ast_tag * bool
          | BNot of ast_tag * unscoped_ast
          | BOr of ast_tag * unscoped_ast * unscoped_ast
          | BAnd of ast_tag * unscoped_ast * unscoped_ast
          | Pair of ast_tag * unscoped_ast * unscoped_ast
          | Eq of ast_tag * unscoped_ast * unscoped_ast
          | Gt of ast_tag * unscoped_ast * unscoped_ast
          | GtEq of ast_tag * unscoped_ast * unscoped_ast
          | Lt of ast_tag * unscoped_ast * unscoped_ast
          | LtEq of ast_tag * unscoped_ast * unscoped_ast
          | If of ast_tag * unscoped_ast * unscoped_ast * unscoped_ast
          | Var of ast_tag * string
          | LetNoRec of ast_tag * varname * unscoped_ast * unscoped_ast
          | LetRec of
              ast_tag
              * varname
              * (varname * vtype) Nonempty_list.t
              * unscoped_ast
              * unscoped_ast
          | Fun of ast_tag * (varname * vtype) * unscoped_ast
          | App of ast_tag * unscoped_ast * unscoped_ast list
          | Match of
              ast_tag
              * unscoped_ast
              * (FlatPattern.flat_pattern * unscoped_ast) Nonempty_list.t
          | Constructor of ast_tag * string * unscoped_ast

        let node_val : unscoped_ast -> ast_tag = function
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

        type unscope_state = {
          var_scope : vtype StringMap.t;
          name_rescopes : varname list StringMap.t;
        }

        let rec unscope (state : unscope_state) :
            FlatPattern.flat_ast ->
            (unscoped_ast, quotient_typing_error) Result.t =
          let open Result in
          let unop (recomb : unscoped_ast -> unscoped_ast)
              (state : unscope_state) (e : FlatPattern.flat_ast) :
              (unscoped_ast, quotient_typing_error) Result.t =
            unscope state e >>| fun e' -> recomb e'
          in
          let binop (recomb : unscoped_ast -> unscoped_ast -> unscoped_ast)
              (state : unscope_state) (e1 : FlatPattern.flat_ast)
              (e2 : FlatPattern.flat_ast) :
              (unscoped_ast, quotient_typing_error) Result.t =
            unscope state e1 >>= fun e1' ->
            unscope state e2 >>| fun e2' -> recomb e1' e2'
          in
          function
          | UnitLit v -> Ok (UnitLit v)
          | IntLit (v, x) -> Ok (IntLit (v, x))
          | Add (v, e1, e2) ->
              binop (fun e1' e2' -> Add (v, e1', e2')) state e1 e2
          | Neg (v, e1) -> unop (fun e1' -> Neg (v, e1')) state e1
          | Subtr (v, e1, e2) ->
              binop (fun e1' e2' -> Subtr (v, e1', e2')) state e1 e2
          | Mult (v, e1, e2) ->
              binop (fun e1' e2' -> Mult (v, e1', e2')) state e1 e2
          | BoolLit (v, b) -> Ok (BoolLit (v, b))
          | BNot (v, e1) -> unop (fun e1' -> BNot (v, e1')) state e1
          | BOr (v, e1, e2) ->
              binop (fun e1' e2' -> BOr (v, e1', e2')) state e1 e2
          | BAnd (v, e1, e2) ->
              binop (fun e1' e2' -> BAnd (v, e1', e2')) state e1 e2
          | Pair (v, e1, e2) ->
              binop (fun e1' e2' -> Pair (v, e1', e2')) state e1 e2
          | Eq (v, e1, e2) ->
              binop (fun e1' e2' -> Eq (v, e1', e2')) state e1 e2
          | Gt (v, e1, e2) ->
              binop (fun e1' e2' -> Gt (v, e1', e2')) state e1 e2
          | GtEq (v, e1, e2) ->
              binop (fun e1' e2' -> GtEq (v, e1', e2')) state e1 e2
          | Lt (v, e1, e2) ->
              binop (fun e1' e2' -> Lt (v, e1', e2')) state e1 e2
          | LtEq (v, e1, e2) ->
              binop (fun e1' e2' -> LtEq (v, e1', e2')) state e1 e2
          | If (v, e1, e2, e3) ->
              unscope state e1 >>= fun e1' ->
              unscope state e2 >>= fun e2' ->
              unscope state e3 >>| fun e3' -> If (v, e1', e2', e3')
          | Var (v, name) ->
              let scope_var_type (xname : varname) : vtype =
                Map.find_exn state.var_scope xname
              in
              (match Map.find state.name_rescopes name with
              | None -> Var (v, name)
              | Some rescope_names ->
                  let rescope_types =
                    List.map ~f:scope_var_type rescope_names
                  in
                  let unscoped_var_type =
                    List.fold_right ~init:v.t
                      ~f:(fun t acc -> VTypeFun (t, acc))
                      rescope_types
                  in
                  App
                    ( v,
                      Var ({ t = unscoped_var_type }, name),
                      List.map
                        ~f:(fun xname ->
                          Var ({ t = scope_var_type xname }, xname))
                        rescope_names ))
              |> Ok
          | LetNoRec (v, xname, e1, e2) ->
              let xtype = (FlatPattern.flat_node_val e1).t in
              binop
                (fun e1' e2' -> LetNoRec (v, xname, e1', e2'))
                {
                  state with
                  var_scope = Map.set state.var_scope ~key:xname ~data:xtype;
                }
                e1 e2
          | LetRec (v, fname, (xname, xtype), return_t, e1, e2) ->
              failwith "TODO"
          | Fun (v, (xname, xtype), e) ->
              unop
                (fun e' -> Fun (v, (xname, xtype), e'))
                {
                  state with
                  var_scope = Map.set state.var_scope ~key:xname ~data:xtype;
                }
                e
          | App (v, e1, e2) ->
              binop (fun e1' e2' -> App (v, e1', [ e2' ])) state e1 e2
          | Match (v, e1, cs) ->
              unscope state e1 >>= fun e1' ->
              Nonempty_list.fold_result_consume_init ~init:()
                ~f:(fun acc_state (p, e) ->
                  let state' =
                    {
                      state with
                      var_scope =
                        List.fold ~init:state.var_scope
                          ~f:(fun acc (xname, xtype) ->
                            Map.set acc ~key:xname ~data:xtype)
                          (FlatPattern.defined_vars p);
                    }
                  in
                  unscope state' e >>| fun e' ->
                  match acc_state with
                  | First () -> Nonempty_list.singleton (p, e')
                  | Second acc_cases -> Nonempty_list.cons (p, e') acc_cases)
                cs
              >>| fun cases' -> Match (v, e1', cases')
          | Constructor (v, name, e1) ->
              unop (fun e1' -> Constructor (v, name, e1')) state e1
      end

      type quotient_analysis_repr = Unscoping.unscoped_ast

      let process (_ : tag_expr) :
          (quotient_analysis_repr, quotient_typing_error) Result.t =
        failwith "TODO"
    end
  end
