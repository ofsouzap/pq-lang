open Utils

module type S = sig
  module Pattern : Pattern.S
  module Expr : Expr.S with module Pattern = Pattern

  (** A single equality constructor on a quotient type *)
  type ('tag_e, 'tag_p) eqcons = {
    bindings : (Varname.t * Vtype.t) list;
        (** The variable bindings for the equality constructor of the equality
            constructor *)
    body : 'tag_p Pattern.t * ('tag_e, 'tag_p) Expr.t;
        (** The equality definition of the equality constructor *)
        (* TODO - instead of having "body" which is the pattern and the expression,
      have something like body_pattern and body_expr, to make it nicer to use *)
  }
  [@@deriving sexp, equal]

  type plain_eqcons = (unit, unit) eqcons [@@deriving sexp, equal]

  type ('tag_e, 'tag_p) typed_eqcons =
    (Vtype.t * 'tag_e, Vtype.t * 'tag_p) eqcons
  [@@deriving sexp, equal]

  (** Get all names used in a quotient type equality constructor *)
  val eqcons_existing_names : ('tag_e, 'tag_p) eqcons -> StringSet.t

  val eqcons_fmap_expr :
    f:('tag_e1 -> 'tag_e2) ->
    ('tag_e1, 'tag_p) eqcons ->
    ('tag_e2, 'tag_p) eqcons

  val eqcons_fmap_pattern :
    f:('tag_p1 -> 'tag_p2) ->
    ('tag_e, 'tag_p1) eqcons ->
    ('tag_e, 'tag_p2) eqcons

  val eqcons_to_plain_eqcons : ('tag_e, 'tag_p) eqcons -> plain_eqcons

  (** Convert a quotient type equality constructor definition into source code.
  *)
  val eqcons_to_source_code :
    ?use_newlines:bool -> ('tag_e, 'tag_p) eqcons -> string

  (* TODO - QCheck_testing submodule for eqcons *)

  (** A quotient type: a variant type with a list of quotients *)
  type ('tag_e, 'tag_p) t = {
    name : string;  (** The name of the quotient type *)
    base_type_name : string;
        (** The name of the variant/quotient type that the quotient type is
            based on *)
    eqconss : ('tag_e, 'tag_p) eqcons list;
        (** The list of equality constructors for the quotient type *)
  }
  [@@deriving sexp, equal]

  type plain_t = (unit, unit) t [@@deriving sexp, equal]

  type ('tag_e, 'tag_p) typed_t = (Vtype.t * 'tag_e, Vtype.t * 'tag_p) t
  [@@deriving sexp, equal]

  (** Get all names used in a quotient type definition *)
  val existing_names : ('tag_e, 'tag_p) t -> StringSet.t

  val fmap_expr :
    f:('tag_e1 -> 'tag_e2) -> ('tag_e1, 'tag_p) t -> ('tag_e2, 'tag_p) t

  val fmap_pattern :
    f:('tag_p1 -> 'tag_p2) -> ('tag_e, 'tag_p1) t -> ('tag_e, 'tag_p2) t

  val to_plain_t : ('tag_e, 'tag_p) t -> plain_t

  (** Convert a quotient type definition into source code. *)
  val to_source_code : ?use_newlines:bool -> ('tag_e, 'tag_p) t -> string

  (* TODO - QCheck_testing submodule for t *)
end

(** Create a quotient type implementation for a given expression module *)
module Make (Expr : Expr.S) :
  S with module Pattern = Expr.Pattern and module Expr = Expr

(** The quotient type implementation using the standard pattern and standard
    expression implementations *)
module StdQuotientType :
  S with module Pattern = Pattern.StdPattern and module Expr = Expr.StdExpr
