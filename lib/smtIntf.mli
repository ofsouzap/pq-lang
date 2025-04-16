open Core
open Utils
open FlatPattern

(** Provides an interface to an SMT solver for quotient type checking purposes
*)
module type S = sig
  module TypeChecker :
    TypeChecker.S
      with module Pattern = Pattern.StdPattern
       and module Expr = Expr.StdExpr
       and module Program = Program.StdProgram

  module TypeCtx = TypeChecker.TypeCtx
  module Pattern = Pattern.StdPattern
  module Expr = Expr.StdExpr
  module Unifier = Unifier.StdUnifier
  module QuotientType = QuotientType.StdQuotientType
  module CustomType = CustomType.StdCustomType
  module Program = Program.StdProgram

  type smt_intf_error [@@deriving sexp, equal]
  type expr_tag = { t : Vtype.t } [@@deriving sexp, equal]
  type pattern_tag = { t : Vtype.t } [@@deriving sexp, equal]
  type tag_pattern = pattern_tag Pattern.t [@@deriving sexp, equal]
  type tag_expr = (expr_tag, pattern_tag) Expr.t [@@deriving sexp, equal]
  type tag_flat_pattern = pattern_tag FlatPattern.M.t [@@deriving sexp, equal]

  type tag_flat_expr = (expr_tag, pattern_tag) FlatExpr.t
  [@@deriving sexp, equal]

  val pattern_tag_to_expr_tag : pattern_tag -> expr_tag

  type tag_unifier = (expr_tag, pattern_tag) Unifier.t [@@deriving sexp, equal]

  type tag_quotient_type_eqcons = (expr_tag, pattern_tag) QuotientType.eqcons
  [@@deriving sexp, equal]

  type tag_quotient_type = (expr_tag, pattern_tag) QuotientType.t
  [@@deriving sexp, equal]

  type tag_custom_type = (expr_tag, pattern_tag) CustomType.t
  [@@deriving sexp, equal]

  type tag_program = (expr_tag, pattern_tag) Program.t [@@deriving sexp, equal]

  module State : sig
    type var_defn = {
      name : Varname.t;
      kind :
        [ `NonRec of (Varname.t * Vtype.t) option | `Rec of Varname.t * Vtype.t ];
      return_t : Vtype.t;
      body : tag_flat_expr;
    }
    [@@deriving sexp, equal]

    type top_level_elem = VarDecl of Varname.t * Vtype.t | VarDefn of var_defn
    [@@deriving sexp, equal]

    type variant_type_constructor_info = {
      name : string;
      accessor_name : string;
      t : Vtype.t;
    }
    [@@deriving sexp, equal]

    type variant_type_info = {
      name : string;
      constructors : variant_type_constructor_info list;
    }
    [@@deriving sexp, equal]

    type pair_type_info = {
      name : string;
      t : Vtype.t * Vtype.t;
      constructor_name : string;
      fst_accessor_name : string;
      snd_accessor_name : string;
    }
    [@@deriving sexp, equal]

    type t

    val state_init : TypeCtx.t -> tag_custom_type list -> t
    val find_root_base_type : t -> Vtype.t -> (Vtype.t, smt_intf_error) result

    val state_get_pair_type_info :
      Vtype.t * Vtype.t -> t -> (pair_type_info, smt_intf_error) result

    val state_get_vtype_special_eq_fun_name : t -> Vtype.t -> string option

    val state_add_variant_type :
      VariantType.t -> t -> (t, smt_intf_error) result

    val state_add_var_decl : string * Vtype.t -> t -> t
    val state_add_var_defn : var_defn -> t -> (t, smt_intf_error) result
  end

  module Assertion : sig
    type t = Not of t | Eq of Vtype.t option * tag_flat_expr * tag_flat_expr
    type assertion = t

    module Builder : sig end
  end

  module Builder : sig
    val build_vtype : State.t -> Vtype.t -> (Sexp.t, smt_intf_error) result

    val build_expr :
      directly_callable_fun_names:Utils.StringSet.t ->
      State.t ->
      tag_flat_expr ->
      (Sexp.t, smt_intf_error) result

    val build_top_level_elem :
      State.t -> State.top_level_elem -> (Sexp.t, smt_intf_error) result

    val build_state :
      existing_names:Utils.StringSet.t ->
      State.t ->
      (Utils.StringSet.t * Sexp.t list, smt_intf_error) result

    val build_assertion :
      state:State.t -> Assertion.t -> (Sexp.t, smt_intf_error) result
  end

  type formula

  val create_formula :
    existing_names:Utils.StringSet.t ->
    State.t ->
    Assertion.t list ->
    (Utils.StringSet.t * formula, smt_intf_error) result

  type model = string StringMap.t

  val check_satisfiability :
    formula -> [ `Sat of model option | `Unknown | `Unsat ]
end

module Z3Intf
    (TypeChecker :
      TypeChecker.S
        with module Pattern = Pattern.StdPattern
         and module Expr = Expr.StdExpr
         and module Program = Program.StdProgram
         and module TypingError = TypeChecker.TypingError.StdTypingError
         and module TypeCtx.CustomType = CustomType.StdCustomType
         and module TypeCtx.TypingError = TypeChecker.TypingError.StdTypingError) :
  S with module TypeChecker = TypeChecker
