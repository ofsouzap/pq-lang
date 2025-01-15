(**
  Abstract syntax tree for the language.
*)

open Core
open Utils
open Vtype
open Variant_types
open Pattern

(**
  Expressions in the language.
  Tagged with arbitrary values on each node.
*)
type 'a expr =
  (* Unit type *)
  | UnitLit of 'a  (** The literal of the unit type *)
  (* Integer arithmetic *)
  | IntLit of 'a * int  (** An integer literal *)
  | Add of 'a * 'a expr * 'a expr  (** Addition *)
  | Neg of 'a * 'a expr  (** Negation *)
  | Subtr of 'a * 'a expr * 'a expr  (** Subtraction *)
  | Mult of 'a * 'a expr * 'a expr  (** Multiplication *)
  (* Boolean algebra *)
  | BoolLit of 'a * bool  (** A boolean literal *)
  | BNot of 'a * 'a expr  (** Boolean negation *)
  | BOr of 'a * 'a expr * 'a expr  (** Boolean OR *)
  | BAnd of 'a * 'a expr * 'a expr  (** Boolean AND *)
  (* Pairs *)
  | Pair of 'a * 'a expr * 'a expr  (** Pair *)
  (* Comparisons *)
  | Eq of 'a * 'a expr * 'a expr  (** Equality *)
  | Gt of 'a * 'a expr * 'a expr  (** Greater than *)
  | GtEq of 'a * 'a expr * 'a expr  (** Greater than or equal to *)
  | Lt of 'a * 'a expr * 'a expr  (** Less than *)
  | LtEq of 'a * 'a expr * 'a expr  (** Less than or equal to *)
  (* Control flow *)
  | If of 'a * 'a expr * 'a expr * 'a expr  (** If-then-else *)
  (* Variables and functions *)
  | Var of 'a * string  (** Variable references *)
  | Let of 'a * string * 'a expr * 'a expr  (** Let binding *)
  | Fun of 'a * (string * vtype) * 'a expr  (** Function value *)
  | App of 'a * 'a expr * 'a expr  (** Function application *)
  | Fix of 'a * (string * vtype * vtype) * (string * vtype) * 'a expr
      (** Application of fix operator: `((function_name_for_recursion, function_for_recursion_type1, function_for_recursion_type2), (param_name, param_type), expr)` *)
  (* Pattern matching *)
  | Match of 'a * 'a expr * (pattern * 'a expr) Nonempty_list.t
      (** Match expression *)
  (* Variant data types *)
  | Constructor of 'a * string * 'a expr
      (** Constructor for a variant data type *)
[@@deriving sexp, equal]

(** Extract the value attached to a single node of a tagged AST expression *)
val expr_node_val : 'a expr -> 'a

(** Map a function onto the value of a single node of a tagged AST expression *)
val expr_node_map_val : f:('a -> 'a) -> 'a expr -> 'a expr

(** Map a function onto all values in an entire tagged AST expression *)
val fmap : f:('a -> 'b) -> 'a expr -> 'b expr

(** Map a function onto values in an expression *)
val ( >|= ) : 'a expr -> ('a -> 'b) -> 'b expr

(** An expression in the language without any tagging data *)
type plain_expr = unit expr [@@deriving sexp, equal]

(** An expression in the language with typing information *)
type 'a typed_expr = (vtype * 'a) expr [@@deriving sexp, equal]

(** An expression in the language with typing information *)
type plain_typed_expr = unit typed_expr [@@deriving sexp, equal]

(** Delete an AST's tagging data to form a plain AST *)
val expr_to_plain_expr : 'a expr -> plain_expr

exception AstConverionFixError

(**
  Convert an AST expression into source code that corresponds to the AST representation.
  If the input has a malformed usage of the Fix node, this will raise a `AstConversionFixError` exception.
*)
val ast_to_source_code : ?use_newlines:bool -> 'a expr -> string

module QCheck_testing : functor
  (Tag : sig
     type t
   end)
  -> sig
  (** The printing method for an AST representation of a program *)
  type ast_print_method =
    | NoPrint  (** Don't print the AST *)
    | PrintSexp of (Tag.t -> Sexp.t)
        (** Print the sexp of the AST, using the provided sexp_of_ function for the values *)
    | PrintExprSource
        (** Print the source code representation of the AST, ignoring the tagging values *)

  (** Take an AST printing method and return a function that implements the printing method.
    Returns None if no printing is specified. *)
  val get_ast_printer_opt : ast_print_method -> (Tag.t expr -> string) option

  (** Take an AST printing method and return a function that implements the printing method.
    Returns a function always returning the empty string if no printing is specified. *)
  val get_ast_printer : ast_print_method -> Tag.t expr -> string

  (** A default AST printing method *)
  val default_ast_print_method : ast_print_method

  type gen_options = {
    t : vtype option;
    variant_types : variant_type list;
    (* TODO - include quotient types *)
    v_gen : Tag.t QCheck.Gen.t;
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
      with type t = Tag.t expr
       and type gen_options := gen_options
       and type print_options = ast_print_method
       and type shrink_options := shrink_options
       and type arb_options := arb_options
end
