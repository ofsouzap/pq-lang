open Core
open Utils
open Vtype
open Variant_types
open Varname
open Pattern
open Ast
open Program

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

module QuotientTypeChecker = struct
  type ast_tag = { t : vtype }
  type tag_expr = ast_tag expr

  module Smt = struct
    type processing_error = MisplacedFixNode

    type state_elem_var_defn = {
      name : varname;
      t : vtype;
      recursive : bool;
      body : tag_expr;
    }

    type state_elem =
      | VarDefn of state_elem_var_defn
      | VariantTypeDefn of variant_type

    type state = state_elem list

    (** A special symbol to be prepended to names defined in the SMTLIB2
        representation, so that they can't interfere with user-defined names.
        Therefore, this contains a character not usable by the user when
        defining variable names *)
    let custom_symbol : string -> string = String.append "PQ-"

    (** Preprocessing for arbitrary expressions, for usage in node
        representation generation *)
    module ExprPreprocessor = struct
      type flat_pattern =
        | FlatPatName of varname * vtype
        | FlatPatPair of (varname * vtype) * (varname * vtype)
        | FlatPatConstructor of string * (varname * vtype)

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
        | Var of ast_tag * string
        | LetNoRec of
            ast_tag * string * preprocessed_tag_expr * preprocessed_tag_expr
        | LetRec of
            ast_tag
            * varname
            * (varname * vtype) list
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

      let flatten_case_pattern :
          pattern * preprocessed_tag_expr ->
          flat_pattern * preprocessed_tag_expr =
        failwith "TODO"

      let rec preprocess_expr :
          tag_expr -> (preprocessed_tag_expr, processing_error) Result.t =
        let open Result in
        let unop (recomb : preprocessed_tag_expr -> preprocessed_tag_expr)
            (e1 : tag_expr) =
          preprocess_expr e1 >>| recomb
        in
        let binop
            (recomb :
              preprocessed_tag_expr ->
              preprocessed_tag_expr ->
              preprocessed_tag_expr) (e1 : tag_expr) (e2 : tag_expr) =
          preprocess_expr e1 >>= fun e1' ->
          preprocess_expr e2 >>| fun e2' -> recomb e1' e2'
        in
        function
        | UnitLit v -> Ok (UnitLit v)
        | IntLit (v, x) -> Ok (IntLit (v, x))
        | Add (v, e1, e2) -> binop (fun e1' e2' -> Add (v, e1', e2')) e1 e2
        | Neg (v, e1) -> unop (fun e1' -> Neg (v, e1')) e1
        | Subtr (v, e1, e2) -> binop (fun e1' e2' -> Subtr (v, e1', e2')) e1 e2
        | Mult (v, e1, e2) -> binop (fun e1' e2' -> Mult (v, e1', e2')) e1 e2
        | BoolLit (v, b) -> Ok (BoolLit (v, b))
        | BNot (v, e1) -> unop (fun e1' -> BNot (v, e1')) e1
        | BOr (v, e1, e2) -> binop (fun e1' e2' -> BOr (v, e1', e2')) e1 e2
        | BAnd (v, e1, e2) -> binop (fun e1' e2' -> BAnd (v, e1', e2')) e1 e2
        | Pair (v, e1, e2) -> binop (fun e1' e2' -> Pair (v, e1', e2')) e1 e2
        | Eq (v, e1, e2) -> binop (fun e1' e2' -> Eq (v, e1', e2')) e1 e2
        | Gt (v, e1, e2) -> binop (fun e1' e2' -> Gt (v, e1', e2')) e1 e2
        | GtEq (v, e1, e2) -> binop (fun e1' e2' -> GtEq (v, e1', e2')) e1 e2
        | Lt (v, e1, e2) -> binop (fun e1' e2' -> Lt (v, e1', e2')) e1 e2
        | LtEq (v, e1, e2) -> binop (fun e1' e2' -> LtEq (v, e1', e2')) e1 e2
        | If (v, e1, e2, e3) ->
            preprocess_expr e1 >>= fun e1' ->
            preprocess_expr e2 >>= fun e2' ->
            preprocess_expr e3 >>| fun e3' -> If (v, e1', e2', e3')
        | Var (v, name) -> Ok (Var (v, name))
        | Let _ ->
            failwith "TODO - handle recursive and non-recursive let-bindings"
        | Fun (v, (param, typ), body) ->
            preprocess_expr body >>| fun body' -> Fun (v, (param, typ), body')
        | App (v, e1, e2) -> binop (fun e1' e2' -> App (v, e1', e2')) e1 e2
        | Fix _ -> Error MisplacedFixNode
        | Match (v, e1, cs) ->
            preprocess_expr e1 >>= fun e1' ->
            Nonempty_list.fold_result_consume_init ~init:() cs
              ~f:(fun acc (p, e) ->
                preprocess_expr e >>| fun e' ->
                match acc with
                | First () -> Nonempty_list.singleton (p, e')
                | Second acc -> Nonempty_list.cons (p, e') acc)
            >>| Nonempty_list.map ~f:flatten_case_pattern
            >>= fun cs' -> Match (v, e1', cs') |> Ok
        | Constructor (v, name, expr) ->
            unop (fun e' -> Constructor (v, name, e')) expr

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
    end

    (** Functions for creating node representations of different parts of a
        program *)
    module SmtlibNodeRepresentation = struct
      open LispBuilder

      let get_vt_constructor_accessor_name (vt_name : string) (c_name : string)
          : string =
        sprintf "%s-%s-getval" vt_name c_name |> custom_symbol

      (** Representation of a vtype *)
      let node_of_vtype : vtype -> node = failwith "TODO"

      (** Representation of a variant type definition *)
      let node_of_variant_type ((vt_name, vt_cs) : variant_type) : node =
        let for_constructor ((c_name, c_t) : variant_type_constructor) : node =
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
        | FlatPatName (xname, _) -> Atom xname
        | FlatPatPair _ -> failwith "TODO - once pair type definition done"
        | FlatPatConstructor (cname, (vname, _)) -> Op (cname, [ Atom vname ])

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
                  List [ Op (xname, [ node_of_vtype xtype ]) ]; node_of_expr e1;
                ] )
        | App (_, e1, e2) -> Op ("select", [ node_of_expr e1; node_of_expr e2 ])
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

    (** Add a variant type definition *)
    let add_variant_definition (vt : variant_type) (state : state) : state =
      VariantTypeDefn vt :: state

    (** Create, or overwrite (if already existed), a variable's definition *)
    let write_variable_value
        ((name : varname), (t : vtype), (recursive : bool), (body : tag_expr))
        (state : state) : state =
      VarDefn { name; t; recursive; body } :: state

    (** Build a state into a representation of the state's type and variable
        definitions. To use this in the solver, the assertions need to be
        appended to this value *)
    let build (state : state) :
        (LispBuilder.node list, processing_error) Result.t =
      let open Result in
      let open LispBuilder in
      let define_fun_node ((xname : string), (xt : vtype), (body : tag_expr)) :
          (node, processing_error) Result.t =
        ExprPreprocessor.preprocess_expr body >>| fun body_preprocessed ->
        Op
          ( "define-fun",
            [
              Atom xname;
              SmtlibNodeRepresentation.node_of_vtype xt;
              SmtlibNodeRepresentation.node_of_expr body_preprocessed;
            ] )
      in
      let define_fun_rec_node ((xname : string), (xt : vtype), (body : tag_expr))
          : (node, processing_error) Result.t =
        ExprPreprocessor.preprocess_expr body >>| fun body_preprocessed ->
        Op
          ( "define-fun-rec",
            [
              Atom xname;
              SmtlibNodeRepresentation.node_of_vtype xt;
              SmtlibNodeRepresentation.node_of_expr body_preprocessed;
            ] )
      in
      List.fold_result ~init:([], StringSet.empty)
        ~f:(fun (acc_exprs, acc_defined) -> function
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

  let check_quotient_types_in_expr (orig_e : tag_expr) (state : Smt.state) :
      Smt.state =
    match orig_e with _ -> failwith "TODO"

  let check_quotient_types (prog : ast_tag program) =
    let _ : Smt.state =
      List.fold ~init:Smt.init
        ~f:(fun acc -> function
          | VariantType vt -> Smt.add_variant_definition vt acc
          | QuotientType _ ->
              (* Note that quotient types don't need to be defined for the SMT solver, they only affect what constraints are generated *)
              acc)
        prog.custom_types
    in
    failwith "TODO"
end
