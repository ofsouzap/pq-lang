(** Abstract syntax tree for the language. *)

open Core
open Utils

module type S = sig
  module Pattern : Pattern.S

  (** Expressions in the language. Tagged with arbitrary values on each node. *)
  type ('tag_e, 'tag_p) t =
    (* Unit type *)
    | UnitLit of 'tag_e  (** The literal of the unit type *)
    (* Integer arithmetic *)
    | IntLit of 'tag_e * int  (** An integer literal *)
    | Add of 'tag_e * ('tag_e, 'tag_p) t * ('tag_e, 'tag_p) t  (** Addition *)
    | Neg of 'tag_e * ('tag_e, 'tag_p) t  (** Negation *)
    | Subtr of 'tag_e * ('tag_e, 'tag_p) t * ('tag_e, 'tag_p) t
        (** Subtraction *)
    | Mult of 'tag_e * ('tag_e, 'tag_p) t * ('tag_e, 'tag_p) t
        (** Multiplication *)
    (* Boolean algebra *)
    | BoolLit of 'tag_e * bool  (** A boolean literal *)
    | BNot of 'tag_e * ('tag_e, 'tag_p) t  (** Boolean negation *)
    | BOr of 'tag_e * ('tag_e, 'tag_p) t * ('tag_e, 'tag_p) t  (** Boolean OR *)
    | BAnd of 'tag_e * ('tag_e, 'tag_p) t * ('tag_e, 'tag_p) t
        (** Boolean AND *)
    (* Pairs *)
    | Pair of 'tag_e * ('tag_e, 'tag_p) t * ('tag_e, 'tag_p) t  (** Pair *)
    (* Comparisons *)
    | Eq of 'tag_e * ('tag_e, 'tag_p) t * ('tag_e, 'tag_p) t  (** Equality *)
    | Gt of 'tag_e * ('tag_e, 'tag_p) t * ('tag_e, 'tag_p) t
        (** Greater than *)
    | GtEq of 'tag_e * ('tag_e, 'tag_p) t * ('tag_e, 'tag_p) t
        (** Greater than or equal to *)
    | Lt of 'tag_e * ('tag_e, 'tag_p) t * ('tag_e, 'tag_p) t  (** Less than *)
    | LtEq of 'tag_e * ('tag_e, 'tag_p) t * ('tag_e, 'tag_p) t
        (** Less than or equal to *)
    (* Control flow *)
    | If of
        'tag_e * ('tag_e, 'tag_p) t * ('tag_e, 'tag_p) t * ('tag_e, 'tag_p) t
        (** If-then-else *)
    (* Variables and functions *)
    | Var of 'tag_e * string  (** Variable references *)
    | Let of 'tag_e * string * ('tag_e, 'tag_p) t * ('tag_e, 'tag_p) t
        (** Let binding *)
    | App of 'tag_e * ('tag_e, 'tag_p) t * ('tag_e, 'tag_p) t
        (** Function application *)
    (* Pattern matching *)
    | Match of
        'tag_e
        * ('tag_e, 'tag_p) t
        * Vtype.t
        * ('tag_p Pattern.t * ('tag_e, 'tag_p) t) Nonempty_list.t
        (** Match expression *)
    (* Variant data types *)
    | Constructor of 'tag_e * string * ('tag_e, 'tag_p) t
        (** Constructor for a variant data type *)
  [@@deriving sexp, equal]

  (** Extract the value attached to a single node of a tagged Expr expression *)
  val node_val : ('tag_e, 'tag_p) t -> 'tag_e

  (** Map a function onto the value of a single node of a tagged Expr expression
  *)
  val node_map_val :
    f:('tag_e -> 'tag_e) -> ('tag_e, 'tag_p) t -> ('tag_e, 'tag_p) t

  (** Map a function onto all values in an entire tagged Expr expression *)
  val fmap :
    f:('tag_e1 -> 'tag_e2) -> ('tag_e1, 'tag_p) t -> ('tag_e2, 'tag_p) t

  (** Map a function onto all patterns in an entire tagged Expr expression with
      tagged patterns *)
  val fmap_pattern :
    f:('tag_p1 -> 'tag_p2) -> ('tag_e, 'tag_p1) t -> ('tag_e, 'tag_p2) t

  (** Get all the names used (defined or referenced) in an expression. Includes
      variable names, variant type constructor names, etc. *)
  val existing_names : ('tag_e, 'tag_p) t -> StringSet.t

  (** An expression in the language without any tagging data *)
  type plain_t = (unit, unit) t [@@deriving sexp, equal]

  (** An expression in the language with typing information *)
  type ('a, 'b) typed_t = (Vtype.t * 'a, Vtype.t * 'b) t
  [@@deriving sexp, equal]

  (** An expression in the language with typing information *)
  type plain_typed_t = (unit, unit) typed_t [@@deriving sexp, equal]

  (** Delete an Expr's tagging data to form a plain Expr *)
  val to_plain_expr : ('tag_e, 'tag_p) t -> plain_t

  (** Rename a variable in an expression *)
  val rename_var :
    old_name:Varname.t ->
    new_name:Varname.t ->
    ('tag_e, 'tag_p) t ->
    ('tag_e, 'tag_p) t

  exception ExprConverionFixError

  (** Convert an Expr expression into source code that corresponds to the Expr
      representation. If the input has a malformed usage of the Fix node, this
      will raise a `ExprConversionFixError` exception. *)
  val to_source_code : ?use_newlines:bool -> ('tag_e, 'tag_p) t -> string

  module QCheck_testing : functor
    (TagExpr : sig
       type t
     end)
    (TagPat : sig
       type t
     end)
    -> sig
    (** The printing method for an Expr representation of a program *)
    type expr_print_method =
      | NoPrint  (** Don't print the Expr *)
      | PrintSexp of (TagExpr.t -> Sexp.t) * (TagPat.t -> Sexp.t)
          (** Print the sexp of the Expr, using the provided sexp_of_ functions
              for the values in the Expr and the patterns *)
      | PrintExprSource
          (** Print the source code representation of the Expr, ignoring the
              tagging values *)

    (** Take an Expr printing method and return a function that implements the
        printing method. Returns None if no printing is specified. *)
    val get_expr_printer_opt :
      expr_print_method -> ((TagExpr.t, TagPat.t) t -> string) option

    (** Take an Expr printing method and return a function that implements the
        printing method. Returns a function always returning the empty string if
        no printing is specified. *)
    val get_expr_printer :
      expr_print_method -> (TagExpr.t, TagPat.t) t -> string

    (** A default Expr printing method *)
    val default_expr_print_method : expr_print_method

    type gen_vtype =
      | GenVTypeUnit
      | GenVTypeInt
      | GenVTypeBool
      | GenVTypePair of gen_vtype * gen_vtype
      | GenVTypeCustom of string

    val vtype_to_gen_vtype_unsafe : Vtype.t -> gen_vtype

    type gen_options = {
      t : gen_vtype option;
      variant_types : VariantType.t list;
      top_level_defns : (Varname.t * (Vtype.t * Vtype.t)) list;
      v_gen : TagExpr.t QCheck.Gen.t;
      pat_v_gen : TagPat.t QCheck.Gen.t;
      mrd : int;
    }

    type shrink_options = { preserve_type : bool }

    type arb_options = {
      gen : gen_options;
      print : expr_print_method;
      shrink : shrink_options;
    }

    include
      QCheck_testing_sig
        with type t = (TagExpr.t, TagPat.t) t
         and type gen_options := gen_options
         and type print_options = expr_print_method
         and type shrink_options := shrink_options
         and type arb_options := arb_options
  end
end

(** Make a standard expression implementation from some pattern implementation
*)
module Make (Pattern : Pattern.S) : S with module Pattern = Pattern

module StdExpr : S with module Pattern = Pattern.StdPattern

(** Create a possibly-open expression from a pattern *)
val std_expr_of_std_pattern :
  convert_tag:('tag_p -> 'tag_e) ->
  'tag_p Pattern.StdPattern.t ->
  ('tag_e, 'tag_p) StdExpr.t
