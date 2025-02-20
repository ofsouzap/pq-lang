(** Abstract syntax tree for the language. *)

open Core
open Utils
open Varname

(** Expressions in the language. Tagged with arbitrary values on each node. *)
type ('tag_e, 'tag_p) expr =
  (* Unit type *)
  | UnitLit of 'tag_e  (** The literal of the unit type *)
  (* Integer arithmetic *)
  | IntLit of 'tag_e * int  (** An integer literal *)
  | Add of 'tag_e * ('tag_e, 'tag_p) expr * ('tag_e, 'tag_p) expr
      (** Addition *)
  | Neg of 'tag_e * ('tag_e, 'tag_p) expr  (** Negation *)
  | Subtr of 'tag_e * ('tag_e, 'tag_p) expr * ('tag_e, 'tag_p) expr
      (** Subtraction *)
  | Mult of 'tag_e * ('tag_e, 'tag_p) expr * ('tag_e, 'tag_p) expr
      (** Multiplication *)
  (* Boolean algebra *)
  | BoolLit of 'tag_e * bool  (** A boolean literal *)
  | BNot of 'tag_e * ('tag_e, 'tag_p) expr  (** Boolean negation *)
  | BOr of 'tag_e * ('tag_e, 'tag_p) expr * ('tag_e, 'tag_p) expr
      (** Boolean OR *)
  | BAnd of 'tag_e * ('tag_e, 'tag_p) expr * ('tag_e, 'tag_p) expr
      (** Boolean AND *)
  (* Pairs *)
  | Pair of 'tag_e * ('tag_e, 'tag_p) expr * ('tag_e, 'tag_p) expr  (** Pair *)
  (* Comparisons *)
  | Eq of 'tag_e * ('tag_e, 'tag_p) expr * ('tag_e, 'tag_p) expr
      (** Equality *)
  | Gt of 'tag_e * ('tag_e, 'tag_p) expr * ('tag_e, 'tag_p) expr
      (** Greater than *)
  | GtEq of 'tag_e * ('tag_e, 'tag_p) expr * ('tag_e, 'tag_p) expr
      (** Greater than or equal to *)
  | Lt of 'tag_e * ('tag_e, 'tag_p) expr * ('tag_e, 'tag_p) expr
      (** Less than *)
  | LtEq of 'tag_e * ('tag_e, 'tag_p) expr * ('tag_e, 'tag_p) expr
      (** Less than or equal to *)
  (* Control flow *)
  | If of
      'tag_e
      * ('tag_e, 'tag_p) expr
      * ('tag_e, 'tag_p) expr
      * ('tag_e, 'tag_p) expr  (** If-then-else *)
  (* Variables and functions *)
  | Var of 'tag_e * string  (** Variable references *)
  | Let of 'tag_e * string * ('tag_e, 'tag_p) expr * ('tag_e, 'tag_p) expr
      (** Let binding *)
  | App of 'tag_e * ('tag_e, 'tag_p) expr * ('tag_e, 'tag_p) expr
      (** Function application *)
  (* Pattern matching *)
  | Match of
      'tag_e
      * ('tag_e, 'tag_p) expr
      * Vtype.t
      * ('tag_p Pattern.t * ('tag_e, 'tag_p) expr) Nonempty_list.t
      (** Match expression *)
  (* Variant data types *)
  | Constructor of 'tag_e * string * ('tag_e, 'tag_p) expr
      (** Constructor for a variant data type *)
[@@deriving sexp, equal]

(** Extract the value attached to a single node of a tagged Expr expression *)
val expr_node_val : ('tag_e, 'tag_p) expr -> 'tag_e

(** Map a function onto the value of a single node of a tagged Expr expression
*)
val expr_node_map_val :
  f:('tag_e -> 'tag_e) -> ('tag_e, 'tag_p) expr -> ('tag_e, 'tag_p) expr

(** Map a function onto all values in an entire tagged Expr expression *)
val fmap :
  f:('tag_e1 -> 'tag_e2) -> ('tag_e1, 'tag_p) expr -> ('tag_e2, 'tag_p) expr

(** Map a function onto all patterns in an entire tagged Expr expression with
    tagged patterns *)
val fmap_pattern :
  f:('tag_p1 -> 'tag_p2) -> ('tag_e, 'tag_p1) expr -> ('tag_e, 'tag_p2) expr

(** Get all the names used (defined or referenced) in an expression. Includes
    variable names, variant type constructor names, etc. *)
val existing_names : ('tag_e, 'tag_p) expr -> StringSet.t

(** An expression in the language without any tagging data *)
type plain_expr = (unit, unit) expr [@@deriving sexp, equal]

(** An expression in the language with typing information *)
type ('a, 'b) typed_expr = (Vtype.t * 'a, Vtype.t * 'b) expr
[@@deriving sexp, equal]

(** An expression in the language with typing information *)
type plain_typed_expr = (unit, unit) typed_expr [@@deriving sexp, equal]

(** Delete an Expr's tagging data to form a plain Expr *)
val expr_to_plain_expr : ('tag_e, 'tag_p) expr -> plain_expr

(** Rename a variable in an expression *)
val rename_var :
  old_name:varname ->
  new_name:varname ->
  ('tag_e, 'tag_p) expr ->
  ('tag_e, 'tag_p) expr

(** Create a possibly-open expression from a pattern *)
val of_pattern :
  convert_tag:('tag_p -> 'tag_e) -> 'tag_p Pattern.t -> ('tag_e, 'tag_p) expr

exception ExprConverionFixError

(** Convert an Expr expression into source code that corresponds to the Expr
    representation. If the input has a malformed usage of the Fix node, this
    will raise a `ExprConversionFixError` exception. *)
val to_source_code : ?use_newlines:bool -> ('tag_e, 'tag_p) expr -> string

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
    expr_print_method -> ((TagExpr.t, TagPat.t) expr -> string) option

  (** Take an Expr printing method and return a function that implements the
      printing method. Returns a function always returning the empty string if
      no printing is specified. *)
  val get_expr_printer :
    expr_print_method -> (TagExpr.t, TagPat.t) expr -> string

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
    top_level_defns : (varname * (Vtype.t * Vtype.t)) list;
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
      with type t = (TagExpr.t, TagPat.t) expr
       and type gen_options := gen_options
       and type print_options = expr_print_method
       and type shrink_options := shrink_options
       and type arb_options := arb_options
end
