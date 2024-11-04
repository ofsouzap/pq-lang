(**
  Abstract syntax tree for the language.
*)

(* TODO - have programs composed of optional custom type defintions then a concluding expression to evaluate, instead of just allowing a single main expression to evaluate.
   Defining functions can just be done with "let f = ... in" *)

(**
  Expressions in the language.
*)
type expr =
  (* Integer arithmetic *)
  | IntLit of int  (** An integer literal *)
  | Add of expr * expr  (** Addition *)
  | Neg of expr  (** Negation *)
  | Subtr of expr * expr  (** Subtraction *)
  | Mult of expr * expr  (** Multiplication *)
  (* Boolean algebra *)
  | BoolLit of bool  (** A boolean literal *)
  | BNot of expr  (** Boolean negation *)
  | BOr of expr * expr  (** Boolean OR *)
  | BAnd of expr * expr  (** Boolean AND *)
  (* Comparisons *)
  | Eq of expr * expr  (** Equality *)
  | Gt of expr * expr  (** Greater than *)
  | GtEq of expr * expr  (** Greater than or equal to *)
  | Lt of expr * expr  (** Less than *)
  | LtEq of expr * expr  (** Less than or equal to *)
  (* Control flow *)
  | If of expr * expr * expr  (** If-then-else *)
  (* Variables and functions *)
  | Var of string  (** Variable references *)
  | Let of string * expr * expr  (** Let binding *)
  | Fun of string * expr  (** Function definition *)
  | App of expr * expr  (** Function application *)
  | Fix of string * string * expr
      (** Application of fix operator: (function_name_for_recursion, param_name, expr) *)
[@@deriving equal]

(**
  Recursively converts an expression to its string representation.
  @param e The expression to convert.
  @return A string representing the expression.
*)
val show_ast : expr -> string

(* TODO - create function to convert AST to source code *)
