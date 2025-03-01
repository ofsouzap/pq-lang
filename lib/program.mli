open Utils

module type S = sig
  module Pattern : Pattern.S
  module Expr : Expr.S with module Pattern = Pattern
  module QuotientType : QuotientType.S
  module CustomType : CustomType.S with module QuotientType = QuotientType

  (** A flag denoting if the definition should be private *)
  type private_flag = Public | Private [@@deriving sexp, equal]

  (** A custom type declaration *)
  type ('tag_e, 'tag_p) custom_type_decl = {
    private_flag : private_flag;
    ct : ('tag_e, 'tag_p) CustomType.t;
  }
  [@@deriving sexp, equal]

  (** A top-level function definition *)
  type ('tag_e, 'tag_p) top_level_defn = {
    private_flag : private_flag;
    recursive : bool;
    name : string;
    param : Varname.t * Vtype.t;
    return_t : Vtype.t;
    body : ('tag_e, 'tag_p) Expr.t;
  }
  [@@deriving sexp, equal]

  type plain_top_level_defn = (unit, unit) top_level_defn

  (** Convert a t into source code *)
  val top_level_defn_to_source_code :
    use_newlines:bool -> ('tag_e, 'tag_p) top_level_defn -> string

  (** A t, consisting of any number of custom type definitions, top-level
      definitions and an expression to evaluate *)
  type ('tag_e, 'tag_p) t = {
    custom_types : ('tag_e, 'tag_p) custom_type_decl list;
    top_level_defns : ('tag_e, 'tag_p) top_level_defn list;
    e : ('tag_e, 'tag_p) Expr.t;
  }
  [@@deriving sexp, equal]

  (** A t with typing information *)
  type ('tag_e, 'tag_p) typed_t = (Vtype.t * 'tag_e, Vtype.t * 'tag_p) t
  [@@deriving sexp, equal]

  (** A t with no tagging *)
  type plain_t = (unit, unit) t [@@deriving sexp, equal]

  val to_plain_t : ('tag_e, 'tag_p) t -> plain_t

  (** Map a function onto the tags of expression nodes in a t *)
  val fmap_expr :
    f:('tag_e1 -> 'tag_e2) -> ('tag_e1, 'tag_p) t -> ('tag_e2, 'tag_p) t

  (** Map a function onto the tags of pattern nodes in a t *)
  val fmap_pattern :
    f:('tag_p1 -> 'tag_p2) -> ('tag_e, 'tag_p1) t -> ('tag_e, 'tag_p2) t

  (** Get all the names used (defined or referenced) in a t. Includes variable
      names, defined type names, variant type constructor names, etc. *)
  val existing_names : ('tag_e, 'tag_p) t -> StringSet.t

  (** Convert a t into source code *)
  val to_source_code : ?use_newlines:bool -> ('tag_e, 'tag_p) t -> string

  module QCheck_testing : functor
    (TagExpr : sig
       type t
     end)
    (TagPat : sig
       type t
     end)
    -> sig
    type gen_options = {
      mrd : int;
      max_variant_types : int;
      max_variant_type_constructors : int;
      max_top_level_defns : int;
      allow_fun_types : bool;
      body_type : Vtype.t option;
      expr_v_gen : TagExpr.t QCheck.Gen.t;
      pat_v_gen : TagPat.t QCheck.Gen.t;
    }

    val gen_top_level_defn :
      expr_v_gen:TagExpr.t QCheck.Gen.t ->
      pat_v_gen:TagPat.t QCheck.Gen.t ->
      top_level_defns:(Varname.t * (Vtype.t * Vtype.t)) list ->
      variant_types:VariantType.t list ->
      mrd:int ->
      (TagExpr.t, TagPat.t) top_level_defn QCheck.Gen.t

    type arb_options = {
      gen : gen_options;
      print : Expr.QCheck_testing(TagExpr)(TagPat).expr_print_method;
      shrink : Expr.QCheck_testing(TagExpr)(TagPat).shrink_options;
    }

    include
      QCheck_testing_sig
        with type t = (TagExpr.t, TagPat.t) t
         and type gen_options := gen_options
         and type print_options =
          Expr.QCheck_testing(TagExpr)(TagPat).expr_print_method
         and type shrink_options =
          Expr.QCheck_testing(TagExpr)(TagPat).shrink_options
         and type arb_options := arb_options
  end
end

module Make (Expr : Expr.S) (CustomType : CustomType.S) :
  S
    with module Pattern = Expr.Pattern
     and module Expr = Expr
     and module QuotientType = CustomType.QuotientType
     and module CustomType = CustomType

module StdProgram :
  S
    with module Pattern = Pattern.StdPattern
     and module Expr = Expr.StdExpr
     and module QuotientType = QuotientType.StdQuotientType
     and module CustomType = CustomType.StdCustomType
