(**
  Abstract syntax tree for the language.
*)

(**
  Expressions in the language.
*)
type expr =
  | IntLit of int  (** An integer literal *)
  | Add of expr * expr  (** Addition *)
  | Subtr of expr * expr  (** Subtraction *)
  | Mult of expr * expr  (** Multiplication *)
  | BoolLit of bool  (** A boolean literal *)
  | BNot of expr  (** Boolean negation *)
  | BOr of expr * expr  (** Boolean OR *)
  | BAnd of expr * expr  (** Boolean AND *)
  | Eq of expr * expr  (** Equality *)
  | Gt of expr * expr  (** Greater than *)
  | GtEq of expr * expr  (** Greater than or equal to *)
  | Lt of expr * expr  (** Less than *)
  | LtEq of expr * expr  (** Less than or equal to *)
  | If of expr * expr * expr  (** If-then-else *)

(**
  Recursively converts an expression to its string representation.
  @param e The expression to convert.
  @return A string representing the expression.
*)
val show : expr -> string
