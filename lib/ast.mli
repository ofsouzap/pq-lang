(**
  Abstract syntax tree for the language.
*)

(* TODO - have programs composed of optional custom type defintions then a concluding expression to evaluate, instead of just allowing a single main expression to evaluate.
   Defining functions can just be done with "let f = ... in" *)

(**
  Expressions in the language.
  Tagged with arbitrary values on each node.
*)
type 'a expr =
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
  | Fun of 'a * string * 'a expr  (** Function definition *)
  | App of 'a * 'a expr * 'a expr  (** Function application *)
  | Fix of 'a * string * string * 'a expr
      (** Application of fix operator: (function_name_for_recursion, param_name, expr) *)
[@@deriving sexp, equal]

(** Extract the value attached to a single node of a tagged AST expression *)
val expr_node_val : 'a expr -> 'a

(** Map a function onto values in an expression *)
val fmap : f:('a -> 'b) -> 'a expr -> 'b expr

(** Map a function onto values in an expression *)
val ( >|= ) : 'a expr -> ('a -> 'b) -> 'b expr

(** An expression in the language without any tagging data *)
type plain_expr = unit expr [@@deriving sexp, equal]

(** Delete an AST's tagging data to form a plain AST *)
val expr_to_plain_expr : 'a expr -> plain_expr

exception AstConverionFixError

(**
  Convert an AST expression into source code that corresponds to the AST representation.
  If the input has a malformed usage of the Fix node, this will raise a `AstConversionFixError` exception.
*)
val ast_to_source_code : 'a expr -> string
