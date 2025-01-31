(** Abstract syntax tree for the language. *)

open Core
open Utils
open Vtype
open Variant_types
open Pattern

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
  | Fun of 'tag_e * (string * vtype) * ('tag_e, 'tag_p) expr
      (** Function value *)
  | App of 'tag_e * ('tag_e, 'tag_p) expr * ('tag_e, 'tag_p) expr
      (** Function application *)
  | Fix of
      'tag_e
      * (string * vtype * vtype)
      * (string * vtype)
      * ('tag_e, 'tag_p) expr
      (** Application of fix operator: `((function_name_for_recursion,
          function_for_recursion_type1, function_for_recursion_type2),
          (param_name, param_type), expr)` *)
  (* Pattern matching *)
  | Match of
      'tag_e
      * ('tag_e, 'tag_p) expr
      * ('tag_p pattern * ('tag_e, 'tag_p) expr) Nonempty_list.t
      (** Match expression *)
  (* Variant data types *)
  | Constructor of 'tag_e * string * ('tag_e, 'tag_p) expr
      (** Constructor for a variant data type *)
[@@deriving sexp, equal]

(** Extract the value attached to a single node of a tagged AST expression *)
val expr_node_val : ('tag_e, 'tag_p) expr -> 'tag_e

(** Map a function onto the value of a single node of a tagged AST expression *)
val expr_node_map_val :
  f:('tag_e -> 'tag_e) -> ('tag_e, 'tag_p) expr -> ('tag_e, 'tag_p) expr

(** Map a function onto all values in an entire tagged AST expression *)
val fmap :
  f:('tag_e1 -> 'tag_e2) -> ('tag_e1, 'tag_p) expr -> ('tag_e2, 'tag_p) expr

(** Map a function onto values in an expression *)
val ( >|= ) :
  ('tag_e1, 'tag_p) expr -> ('tag_e1 -> 'tag_e2) -> ('tag_e2, 'tag_p) expr

(** Map a function onto all patterns in an entire tagged AST expression with
    tagged patterns *)
val fmap_pattern :
  f:('tag_p1 -> 'tag_p2) -> ('tag_e, 'tag_p1) expr -> ('tag_e, 'tag_p2) expr

(** An expression in the language without any tagging data *)
type plain_expr = (unit, unit) expr [@@deriving sexp, equal]

(** An expression in the language with typing information *)
type ('a, 'b) typed_expr = (vtype * 'a, vtype * 'b) expr
[@@deriving sexp, equal]

(** An expression in the language with typing information *)
type plain_typed_expr = (unit, unit) typed_expr [@@deriving sexp, equal]

(** Delete an AST's tagging data to form a plain AST *)
val expr_to_plain_expr : ('tag_e, 'tag_p) expr -> plain_expr

exception AstConverionFixError

(** Convert an AST expression into source code that corresponds to the AST
    representation. If the input has a malformed usage of the Fix node, this
    will raise a `AstConversionFixError` exception. *)
val ast_to_source_code : ?use_newlines:bool -> ('tag_e, 'tag_p) expr -> string

module QCheck_testing : functor
  (TagExpr : sig
     type t
   end)
  (TagPat : sig
     type t
   end)
  -> sig
  (** The printing method for an AST representation of a program *)
  type ast_print_method =
    | NoPrint  (** Don't print the AST *)
    | PrintSexp of (TagExpr.t -> Sexp.t) * (TagPat.t -> Sexp.t)
        (** Print the sexp of the AST, using the provided sexp_of_ functions for
            the values in the AST and the patterns *)
    | PrintExprSource
        (** Print the source code representation of the AST, ignoring the
            tagging values *)

  (** Take an AST printing method and return a function that implements the
      printing method. Returns None if no printing is specified. *)
  val get_ast_printer_opt :
    ast_print_method -> ((TagExpr.t, TagPat.t) expr -> string) option

  (** Take an AST printing method and return a function that implements the
      printing method. Returns a function always returning the empty string if
      no printing is specified. *)
  val get_ast_printer : ast_print_method -> (TagExpr.t, TagPat.t) expr -> string

  (** A default AST printing method *)
  val default_ast_print_method : ast_print_method

  type gen_options = {
    t : vtype option;
    variant_types : variant_type list;
    (* TODO - include quotient types *)
    v_gen : TagExpr.t QCheck.Gen.t;
    pat_v_gen : TagPat.t QCheck.Gen.t;
    mrd : int;
  }

  type shrink_options = { preserve_type : bool }

  type arb_options = {
    gen : gen_options;
    print : ast_print_method;
    shrink : shrink_options;
  }

  include
    QCheck_testing_sig
      with type t = (TagExpr.t, TagPat.t) expr
       and type gen_options := gen_options
       and type print_options = ast_print_method
       and type shrink_options := shrink_options
       and type arb_options := arb_options
end
