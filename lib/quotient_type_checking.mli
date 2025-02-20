open Core
open Vtype
open Varname

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

  (** Build the source to a human-readable string *)
  val build_hum : node list -> string
end

module LispBuilder : LispBuilderSig

type expr_tag = { t : Vtype.vtype } [@@deriving sexp, equal]
type pattern_tag = { t : Vtype.vtype } [@@deriving sexp, equal]
type tag_pattern = pattern_tag Pattern.pattern [@@deriving sexp, equal]
type tag_expr = (expr_tag, pattern_tag) Expr.expr [@@deriving sexp, equal]

val pattern_tag_to_expr_tag : pattern_tag -> expr_tag

type tag_unifier = (expr_tag, pattern_tag) Unification.unifier
[@@deriving sexp, equal]

type tag_quotient_type_eqcons = (expr_tag, pattern_tag) QuotientType.eqcons
[@@deriving sexp, equal]

type tag_quotient_type = (expr_tag, pattern_tag) QuotientType.t
[@@deriving sexp, equal]

type tag_custom_type = (expr_tag, pattern_tag) Custom_types.custom_type
[@@deriving sexp, equal]

type tag_program = (expr_tag, pattern_tag) Program.program
[@@deriving sexp, equal]

type quotient_typing_error =
  | QuotientConstraintCheckFailed
  | SmtUnknownResult
  | UnexpectedTrivialMatchCasePatternError
  | PairTypeNotDefinedInState of Vtype.vtype * Vtype.vtype
  | UndefinedCustomTypeName of string
[@@deriving sexp, equal]

module FlatPattern : sig
  type t =
    | FlatPatPair of
        pattern_tag
        * (pattern_tag * string * Vtype.vtype)
        * (pattern_tag * string * Vtype.vtype)
    | FlatPatConstructor of
        pattern_tag * string * (pattern_tag * string * Vtype.vtype)
  [@@deriving sexp, equal]

  type flat_pattern = t [@@deriving sexp, equal]

  type flat_expr =
    | UnitLit of expr_tag
    | IntLit of expr_tag * int
    | Add of expr_tag * flat_expr * flat_expr
    | Neg of expr_tag * flat_expr
    | Subtr of expr_tag * flat_expr * flat_expr
    | Mult of expr_tag * flat_expr * flat_expr
    | BoolLit of expr_tag * bool
    | BNot of expr_tag * flat_expr
    | BOr of expr_tag * flat_expr * flat_expr
    | BAnd of expr_tag * flat_expr * flat_expr
    | Pair of expr_tag * flat_expr * flat_expr
    | Eq of expr_tag * flat_expr * flat_expr
    | Gt of expr_tag * flat_expr * flat_expr
    | GtEq of expr_tag * flat_expr * flat_expr
    | Lt of expr_tag * flat_expr * flat_expr
    | LtEq of expr_tag * flat_expr * flat_expr
    | If of expr_tag * flat_expr * flat_expr * flat_expr
    | Var of expr_tag * string
    | Let of expr_tag * string * flat_expr * flat_expr
    | App of expr_tag * flat_expr * flat_expr
    | Match of
        expr_tag
        * flat_expr
        * Vtype.vtype
        * (flat_pattern * flat_expr) Utils.Nonempty_list.t
    | Constructor of expr_tag * string * flat_expr
  [@@deriving sexp, equal]

  type flat_top_level_defn = {
    recursive : bool;
    name : string;
    param : string * Vtype.vtype;
    return_t : Vtype.vtype;
    body : flat_expr;
  }
  [@@deriving sexp, equal]

  type flat_program = {
    custom_types : tag_custom_type list;
    top_level_defns : flat_top_level_defn list;
    e : flat_expr;
  }
  [@@deriving sexp, equal]

  val flat_pattern_node_val : flat_pattern -> pattern_tag
  val flat_node_val : flat_expr -> expr_tag
  val defined_vars : t -> (string * Vtype.vtype) list

  val expr_rename_var :
    old_name:string -> new_name:string -> flat_expr -> flat_expr

  val to_non_flat_pattern : t -> tag_pattern

  val of_expr :
    existing_names:Utils.StringSet.t ->
    tag_expr ->
    (Utils.StringSet.t * flat_expr, quotient_typing_error) result

  val to_non_flat_expr : flat_expr -> tag_expr

  val of_program :
    existing_names:Utils.StringSet.t ->
    tag_program ->
    (Utils.StringSet.t * flat_program, quotient_typing_error) result
end

module Smt : sig
  module State : sig
    type var_defn = {
      name : varname;
      kind : [ `NonRec of (varname * vtype) option | `Rec of varname * vtype ];
      return_t : vtype;
      body : FlatPattern.flat_expr;
    }
    [@@deriving sexp, equal]

    type top_level_elem = VarDecl of varname * vtype | VarDefn of var_defn
    [@@deriving sexp, equal]

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
      t : Vtype.vtype * Vtype.vtype;
      constructor_name : string;
      fst_accessor_name : string;
      snd_accessor_name : string;
    }
    [@@deriving sexp, equal]

    type t [@@deriving sexp, equal]

    val state_init : tag_custom_type list -> t

    val find_root_base_type :
      t -> Vtype.vtype -> (Vtype.vtype, quotient_typing_error) result

    val state_get_pair_type_info :
      Vtype.vtype * Vtype.vtype ->
      t ->
      (pair_type_info, quotient_typing_error) result

    val state_get_vtype_special_eq_fun_name : t -> Vtype.vtype -> string option
    val state_add_variant_type : Variant_types.variant_type -> t -> t
    val state_add_var_decl : string * Vtype.vtype -> t -> t
    val state_add_var_defn : var_defn -> t -> (t, quotient_typing_error) result
  end

  module Assertion : sig
    type t =
      | Not of t
      | Eq of Vtype.vtype option * FlatPattern.flat_expr * FlatPattern.flat_expr

    type assertion = t

    module Builder : sig end
  end

  module Builder : sig
    val build_vtype :
      State.t -> Vtype.vtype -> (LispBuilder.node, quotient_typing_error) result

    val build_expr :
      directly_callable_fun_names:Utils.StringSet.t ->
      State.t ->
      FlatPattern.flat_expr ->
      (LispBuilder.node, quotient_typing_error) result

    val build_top_level_elem :
      State.t ->
      State.top_level_elem ->
      (LispBuilder.node, quotient_typing_error) result

    val build_state :
      existing_names:Utils.StringSet.t ->
      State.t ->
      (Utils.StringSet.t * LispBuilder.node list, quotient_typing_error) result

    val build_assertion :
      state:State.t ->
      Assertion.t ->
      (LispBuilder.node, quotient_typing_error) result
  end

  type formula

  val create_formula :
    existing_names:Utils.StringSet.t ->
    State.t ->
    Assertion.t list ->
    (Utils.StringSet.t * formula, quotient_typing_error) result

  val check_satisfiability : formula -> [ `Sat | `Unknown | `Unsat ]
end

val check_program : tag_program -> (unit, quotient_typing_error) result
