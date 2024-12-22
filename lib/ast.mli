(**
  Abstract syntax tree for the language.
*)

open Utils
open Vtype
open Pattern

(* TODO - have programs composed of optional custom type defintions then a concluding expression to evaluate, instead of just allowing a single main expression to evaluate.
   Defining functions can just be done with "let f = ... in" *)

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
  (* Custom data types *)
  | Constructor of 'a * string * 'a expr
      (** Constructor for a custom data type *)
[@@deriving sexp, equal]

(* TODO - custom type definitions *)

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
val ast_to_source_code : 'a expr -> string
