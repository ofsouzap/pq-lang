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
module Make (Pattern : Pattern.S) : S with module Pattern = Pattern = struct
  module Pattern = Pattern

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

  let node_map_val_with_result ~(f : 'tag_e -> 'tag_e) :
      ('tag_e, 'tag_p) t -> 'tag_e * ('tag_e, 'tag_p) t = function
    | UnitLit v -> (f v, UnitLit (f v))
    | IntLit (v, x) -> (f v, IntLit (f v, x))
    | Add (v, e1, e2) -> (f v, Add (f v, e1, e2))
    | Neg (v, e1) -> (f v, Neg (f v, e1))
    | Subtr (v, e1, e2) -> (f v, Subtr (f v, e1, e2))
    | Mult (v, e1, e2) -> (f v, Mult (f v, e1, e2))
    | BoolLit (v, b) -> (f v, BoolLit (f v, b))
    | BNot (v, e1) -> (f v, BNot (f v, e1))
    | BOr (v, e1, e2) -> (f v, BOr (f v, e1, e2))
    | BAnd (v, e1, e2) -> (f v, BAnd (f v, e1, e2))
    | Pair (v, e1, e2) -> (f v, Pair (f v, e1, e2))
    | Eq (v, e1, e2) -> (f v, Eq (f v, e1, e2))
    | Gt (v, e1, e2) -> (f v, Gt (f v, e1, e2))
    | GtEq (v, e1, e2) -> (f v, GtEq (f v, e1, e2))
    | Lt (v, e1, e2) -> (f v, Lt (f v, e1, e2))
    | LtEq (v, e1, e2) -> (f v, LtEq (f v, e1, e2))
    | If (v, e1, e2, e3) -> (f v, If (f v, e1, e2, e3))
    | Var (v, xname) -> (f v, Var (f v, xname))
    | Let (v, e1, e2, e3) -> (f v, Let (f v, e1, e2, e3))
    | App (v, e1, e2) -> (f v, App (f v, e1, e2))
    | Match (v, e1, t2, cs) -> (f v, Match (f v, e1, t2, cs))
    | Constructor (v, cname, e1) -> (f v, Constructor (f v, cname, e1))

  let node_val (e : ('tag_e, 'tag_p) t) : 'tag_e =
    node_map_val_with_result ~f:Fn.id e |> fst

  let node_map_val ~(f : 'tag_e -> 'tag_e) :
      ('tag_e, 'tag_p) t -> ('tag_e, 'tag_p) t =
    Fn.compose snd (node_map_val_with_result ~f)

  let rec fmap ~(f : 'tag_e1 -> 'tag_e2) (e : ('tag_e1, 'tag_p) t) :
      ('tag_e2, 'tag_p) t =
    match e with
    | UnitLit a -> UnitLit (f a)
    | IntLit (a, i) -> IntLit (f a, i)
    | Add (a, e1, e2) -> Add (f a, fmap ~f e1, fmap ~f e2)
    | Neg (a, e) -> Neg (f a, fmap ~f e)
    | Subtr (a, e1, e2) -> Subtr (f a, fmap ~f e1, fmap ~f e2)
    | Mult (a, e1, e2) -> Mult (f a, fmap ~f e1, fmap ~f e2)
    | BoolLit (a, b) -> BoolLit (f a, b)
    | BNot (a, e) -> BNot (f a, fmap ~f e)
    | BOr (a, e1, e2) -> BOr (f a, fmap ~f e1, fmap ~f e2)
    | BAnd (a, e1, e2) -> BAnd (f a, fmap ~f e1, fmap ~f e2)
    | Pair (a, e1, e2) -> Pair (f a, fmap ~f e1, fmap ~f e2)
    | Eq (a, e1, e2) -> Eq (f a, fmap ~f e1, fmap ~f e2)
    | Gt (a, e1, e2) -> Gt (f a, fmap ~f e1, fmap ~f e2)
    | GtEq (a, e1, e2) -> GtEq (f a, fmap ~f e1, fmap ~f e2)
    | Lt (a, e1, e2) -> Lt (f a, fmap ~f e1, fmap ~f e2)
    | LtEq (a, e1, e2) -> LtEq (f a, fmap ~f e1, fmap ~f e2)
    | If (a, e1, e2, e3) -> If (f a, fmap ~f e1, fmap ~f e2, fmap ~f e3)
    | Var (a, vname) -> Var (f a, vname)
    | Let (a, xname, e1, e2) -> Let (f a, xname, fmap ~f e1, fmap ~f e2)
    | App (a, e1, e2) -> App (f a, fmap ~f e1, fmap ~f e2)
    | Match (a, e, t2, cs) ->
        Match
          ( f a,
            fmap ~f e,
            t2,
            Nonempty_list.map ~f:(fun (p, c_e) -> (p, fmap ~f c_e)) cs )
    | Constructor (a, cname, e) -> Constructor (f a, cname, fmap ~f e)

  let rec fmap_pattern ~(f : 'tag_p1 -> 'tag_p2) (e : ('tag_e, 'tag_p1) t) :
      ('tag_e, 'tag_p2) t =
    match e with
    | UnitLit v -> UnitLit v
    | IntLit (v, i) -> IntLit (v, i)
    | Add (v, e1, e2) -> Add (v, fmap_pattern ~f e1, fmap_pattern ~f e2)
    | Neg (v, e) -> Neg (v, fmap_pattern ~f e)
    | Subtr (v, e1, e2) -> Subtr (v, fmap_pattern ~f e1, fmap_pattern ~f e2)
    | Mult (v, e1, e2) -> Mult (v, fmap_pattern ~f e1, fmap_pattern ~f e2)
    | BoolLit (v, b) -> BoolLit (v, b)
    | BNot (v, e) -> BNot (v, fmap_pattern ~f e)
    | BOr (v, e1, e2) -> BOr (v, fmap_pattern ~f e1, fmap_pattern ~f e2)
    | BAnd (v, e1, e2) -> BAnd (v, fmap_pattern ~f e1, fmap_pattern ~f e2)
    | Pair (v, e1, e2) -> Pair (v, fmap_pattern ~f e1, fmap_pattern ~f e2)
    | Eq (v, e1, e2) -> Eq (v, fmap_pattern ~f e1, fmap_pattern ~f e2)
    | Gt (v, e1, e2) -> Gt (v, fmap_pattern ~f e1, fmap_pattern ~f e2)
    | GtEq (v, e1, e2) -> GtEq (v, fmap_pattern ~f e1, fmap_pattern ~f e2)
    | Lt (v, e1, e2) -> Lt (v, fmap_pattern ~f e1, fmap_pattern ~f e2)
    | LtEq (v, e1, e2) -> LtEq (v, fmap_pattern ~f e1, fmap_pattern ~f e2)
    | If (v, e1, e2, e3) ->
        If (v, fmap_pattern ~f e1, fmap_pattern ~f e2, fmap_pattern ~f e3)
    | Var (v, name) -> Var (v, name)
    | Let (v, name, e1, e2) ->
        Let (v, name, fmap_pattern ~f e1, fmap_pattern ~f e2)
    | App (v, e1, e2) -> App (v, fmap_pattern ~f e1, fmap_pattern ~f e2)
    | Match (v, e, t2, cs) ->
        Match
          ( v,
            fmap_pattern ~f e,
            t2,
            Nonempty_list.map
              ~f:(fun (p, c_e) -> (Pattern.fmap ~f p, fmap_pattern ~f c_e))
              cs )
    | Constructor (v, name, e) -> Constructor (v, name, fmap_pattern ~f e)

  let rec existing_names : ('tag_e, 'tag_p) t -> StringSet.t = function
    | UnitLit _ | IntLit _ | BoolLit _ -> StringSet.empty
    | Add (_, e1, e2)
    | Subtr (_, e1, e2)
    | Mult (_, e1, e2)
    | BOr (_, e1, e2)
    | BAnd (_, e1, e2)
    | Pair (_, e1, e2)
    | Eq (_, e1, e2)
    | Gt (_, e1, e2)
    | GtEq (_, e1, e2)
    | Lt (_, e1, e2)
    | LtEq (_, e1, e2)
    | App (_, e1, e2) ->
        Set.union (existing_names e1) (existing_names e2)
    | Neg (_, e) | BNot (_, e) -> existing_names e
    | If (_, e1, e2, e3) ->
        Set.union (existing_names e1)
          (Set.union (existing_names e2) (existing_names e3))
    | Var (_, name) -> StringSet.singleton name
    | Let (_, name, e1, e2) ->
        Set.union (StringSet.singleton name)
          (Set.union (existing_names e1) (existing_names e2))
    | Match (_, e, _, cases) ->
        let case_names (p, e) =
          Set.union (Pattern.existing_names p) (existing_names e)
        in
        Set.union (existing_names e)
          (Nonempty_list.fold cases ~init:StringSet.empty ~f:(fun acc case ->
               Set.union acc (case_names case)))
    | Constructor (_, name, e) ->
        Set.union (StringSet.singleton name) (existing_names e)

  type plain_t = (unit, unit) t [@@deriving sexp, equal]

  type ('a, 'b) typed_t = (Vtype.t * 'a, Vtype.t * 'b) t
  [@@deriving sexp, equal]

  type plain_typed_t = (unit, unit) typed_t [@@deriving sexp, equal]

  let rec to_plain_expr (e : ('tag_e, 'tag_p) t) : plain_t =
    match e with
    | UnitLit _ -> UnitLit ()
    | IntLit (_, i) -> IntLit ((), i)
    | Add (_, e1, e2) -> Add ((), to_plain_expr e1, to_plain_expr e2)
    | Neg (_, e) -> Neg ((), to_plain_expr e)
    | Subtr (_, e1, e2) -> Subtr ((), to_plain_expr e1, to_plain_expr e2)
    | Mult (_, e1, e2) -> Mult ((), to_plain_expr e1, to_plain_expr e2)
    | BoolLit (_, b) -> BoolLit ((), b)
    | BNot (_, e) -> BNot ((), to_plain_expr e)
    | BOr (_, e1, e2) -> BOr ((), to_plain_expr e1, to_plain_expr e2)
    | BAnd (_, e1, e2) -> BAnd ((), to_plain_expr e1, to_plain_expr e2)
    | Pair (_, e1, e2) -> Pair ((), to_plain_expr e1, to_plain_expr e2)
    | Eq (_, e1, e2) -> Eq ((), to_plain_expr e1, to_plain_expr e2)
    | Gt (_, e1, e2) -> Gt ((), to_plain_expr e1, to_plain_expr e2)
    | GtEq (_, e1, e2) -> GtEq ((), to_plain_expr e1, to_plain_expr e2)
    | Lt (_, e1, e2) -> Lt ((), to_plain_expr e1, to_plain_expr e2)
    | LtEq (_, e1, e2) -> LtEq ((), to_plain_expr e1, to_plain_expr e2)
    | If (_, e1, e2, e3) ->
        If ((), to_plain_expr e1, to_plain_expr e2, to_plain_expr e3)
    | Var (_, vname) -> Var ((), vname)
    | Let (_, xname, e1, e2) ->
        Let ((), xname, to_plain_expr e1, to_plain_expr e2)
    | App (_, e1, e2) -> App ((), to_plain_expr e1, to_plain_expr e2)
    | Match (_, e, t2, cs) ->
        Match
          ( (),
            to_plain_expr e,
            t2,
            Nonempty_list.map
              ~f:(fun (p, c_e) ->
                (Pattern.fmap ~f:(fun _ -> ()) p, to_plain_expr c_e))
              cs )
    | Constructor (_, cname, e) -> Constructor ((), cname, to_plain_expr e)

  let rec rename_var ~(old_name : Varname.t) ~(new_name : Varname.t) = function
    | UnitLit _ as e -> e
    | IntLit _ as e -> e
    | BoolLit _ as e -> e
    | Add (v, e1, e2) ->
        Add
          ( v,
            rename_var ~old_name ~new_name e1,
            rename_var ~old_name ~new_name e2 )
    | Neg (v, e) -> Neg (v, rename_var ~old_name ~new_name e)
    | Subtr (v, e1, e2) ->
        Subtr
          ( v,
            rename_var ~old_name ~new_name e1,
            rename_var ~old_name ~new_name e2 )
    | Mult (v, e1, e2) ->
        Mult
          ( v,
            rename_var ~old_name ~new_name e1,
            rename_var ~old_name ~new_name e2 )
    | BNot (v, e) -> BNot (v, rename_var ~old_name ~new_name e)
    | BOr (v, e1, e2) ->
        BOr
          ( v,
            rename_var ~old_name ~new_name e1,
            rename_var ~old_name ~new_name e2 )
    | BAnd (v, e1, e2) ->
        BAnd
          ( v,
            rename_var ~old_name ~new_name e1,
            rename_var ~old_name ~new_name e2 )
    | Pair (v, e1, e2) ->
        Pair
          ( v,
            rename_var ~old_name ~new_name e1,
            rename_var ~old_name ~new_name e2 )
    | Eq (v, e1, e2) ->
        Eq
          ( v,
            rename_var ~old_name ~new_name e1,
            rename_var ~old_name ~new_name e2 )
    | Gt (v, e1, e2) ->
        Gt
          ( v,
            rename_var ~old_name ~new_name e1,
            rename_var ~old_name ~new_name e2 )
    | GtEq (v, e1, e2) ->
        GtEq
          ( v,
            rename_var ~old_name ~new_name e1,
            rename_var ~old_name ~new_name e2 )
    | Lt (v, e1, e2) ->
        Lt
          ( v,
            rename_var ~old_name ~new_name e1,
            rename_var ~old_name ~new_name e2 )
    | LtEq (v, e1, e2) ->
        LtEq
          ( v,
            rename_var ~old_name ~new_name e1,
            rename_var ~old_name ~new_name e2 )
    | If (v, e1, e2, e3) ->
        If
          ( v,
            rename_var ~old_name ~new_name e1,
            rename_var ~old_name ~new_name e2,
            rename_var ~old_name ~new_name e3 )
    | Var (v, xname) ->
        if equal_string old_name xname then Var (v, new_name) else Var (v, xname)
    | Let (v, xname, e1, e2) ->
        Let
          ( v,
            xname,
            rename_var ~old_name ~new_name e1,
            if equal_string xname old_name then e2
            else rename_var ~old_name ~new_name e2 )
    | App (v, e1, e2) ->
        App
          ( v,
            rename_var ~old_name ~new_name e1,
            rename_var ~old_name ~new_name e2 )
    | Match (v, e, t2, cases) ->
        Match
          ( v,
            rename_var ~old_name ~new_name e,
            t2,
            Nonempty_list.map cases ~f:(fun (case_p, case_e) ->
                ( case_p,
                  if
                    List.exists ~f:(equal_string old_name)
                      (Pattern.defined_vars case_p |> List.map ~f:fst)
                  then case_e
                  else rename_var ~old_name ~new_name case_e )) )
    | Constructor (v, name, e) ->
        Constructor (v, name, rename_var ~old_name ~new_name e)

  exception ExprConverionFixError

  let to_source_code ?(use_newlines : bool option) :
      ('tag_e, 'tag_p) t -> string =
    let rec convert ?(bracketed : bool option) (orig_e : ('tag_e, 'tag_p) t)
        (p : SourceCodeBuilder.state) : SourceCodeBuilder.state =
      let open SourceCodeBuilder in
      let bracketed = Option.value bracketed ~default:true in
      p
      |> (if bracketed then write "(" else Fn.id)
      |> (match orig_e with
         | UnitLit _ -> write "()"
         | IntLit (_, i) -> write (string_of_int i)
         | Add (_, e1, e2) -> convert e1 |.> write " + " |.> convert e2
         | Neg (_, e1) -> write "-" |.> convert e1
         | Subtr (_, e1, e2) -> convert e1 |.> write " - " |.> convert e2
         | Mult (_, e1, e2) -> convert e1 |.> write " * " |.> convert e2
         | BoolLit (_, b) -> write (string_of_bool b)
         | BNot (_, e1) -> write "~" |.> convert e1
         | BOr (_, e1, e2) -> convert e1 |.> write " || " |.> convert e2
         | BAnd (_, e1, e2) -> convert e1 |.> write " && " |.> convert e2
         | Pair (_, e1, e2) ->
             write "(" |.> convert e1 |.> write ", " |.> convert e2
             |.> write ")"
         | Eq (_, e1, e2) -> convert e1 |.> write " == " |.> convert e2
         | Gt (_, e1, e2) -> convert e1 |.> write " > " |.> convert e2
         | GtEq (_, e1, e2) -> convert e1 |.> write " >= " |.> convert e2
         | Lt (_, e1, e2) -> convert e1 |.> write " < " |.> convert e2
         | LtEq (_, e1, e2) -> convert e1 |.> write " <= " |.> convert e2
         | If (_, e1, e2, e3) ->
             write "if " |.> convert e1 |.> endline |.> write "then"
             |.> block (convert e2)
             |.> write "else"
             |.> block (convert e3)
             |.> write "end"
         | Var (_, vname) -> write vname
         | Let (_, xname, e1, e2) -> (
             let default_repr : state -> state =
               write "let " |.> write xname |.> write " = "
               |.> block (convert e1)
               |.> write "in"
               |.> block (convert e2)
               |.> write "end"
             in
             match e1 with _ -> default_repr)
         | App (_, e1, e2) -> convert e1 |.> write " " |.> convert e2
         | Match (_, e, t2, cs) ->
             let convert_case
                 ((p : 'tag_p Pattern.t), (c_e : ('tag_e, 'tag_p) t)) :
                 state -> state =
               write "| ("
               |.> write (Pattern.to_source_code p)
               |.> write ") ->"
               |.> block (convert c_e)
             in
             let blocked_converted_cases : (state -> state) Nonempty_list.t =
               Nonempty_list.map ~f:(convert_case |.> block) cs
             in
             let cases_converter : state -> state =
               Nonempty_list.fold ~init:Fn.id ~f:( |.> ) blocked_converted_cases
             in
             write "match"
             |.> block (convert e)
             |.> write "-> "
             |.> write (Vtype.to_source_code t2)
             |.> write " with" |.> block cases_converter |.> write "end"
         | Constructor (_, cname, e) -> write cname |.> write " " |.> convert e)
      |> if bracketed then write ")" else Fn.id
    in
    SourceCodeBuilder.from_converter ~converter:(convert ~bracketed:false)
      ~use_newlines:(Option.value ~default:true use_newlines)

  type ('tag_e, 'tag_p) expr = ('tag_e, 'tag_p) t

  module QCheck_testing (TagExpr : sig
    type t
  end) (TagPat : sig
    type t
  end) : sig
    type expr_print_method =
      | NoPrint
      | PrintSexp of (TagExpr.t -> Sexp.t) * (TagPat.t -> Sexp.t)
      | PrintExprSource

    val get_expr_printer_opt :
      expr_print_method -> ((TagExpr.t, TagPat.t) expr -> string) option

    val get_expr_printer :
      expr_print_method -> (TagExpr.t, TagPat.t) expr -> string

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
        with type t = (TagExpr.t, TagPat.t) expr
         and type gen_options := gen_options
         and type print_options = expr_print_method
         and type shrink_options := shrink_options
         and type arb_options := arb_options
  end = struct
    module TagPatternQCheckTesting = Pattern.QCheck_testing (TagPat)

    type expr_print_method =
      | NoPrint
      | PrintSexp of (TagExpr.t -> Sexp.t) * (TagPat.t -> Sexp.t)
      | PrintExprSource

    let get_expr_printer_opt :
        expr_print_method -> ((TagExpr.t, TagPat.t) expr -> string) option =
      function
      | NoPrint -> None
      | PrintSexp (f_e, f_p) ->
          Some (fun e -> sexp_of_t f_e f_p e |> Sexp.to_string_hum)
      | PrintExprSource -> Some (to_source_code ~use_newlines:true)

    let get_expr_printer (p : expr_print_method)
        (e : (TagExpr.t, TagPat.t) expr) : string =
      match get_expr_printer_opt p with None -> "" | Some f -> f e

    let default_expr_print_method : expr_print_method = PrintExprSource

    type t = (TagExpr.t, TagPat.t) expr

    type gen_vtype =
      | GenVTypeUnit
      | GenVTypeInt
      | GenVTypeBool
      | GenVTypePair of gen_vtype * gen_vtype
      | GenVTypeCustom of string

    let rec gen_vtype_to_vtype = function
      | GenVTypeUnit -> Vtype.VTypeUnit
      | GenVTypeInt -> VTypeInt
      | GenVTypeBool -> VTypeBool
      | GenVTypePair (t1, t2) ->
          VTypePair (gen_vtype_to_vtype t1, gen_vtype_to_vtype t2)
      | GenVTypeCustom ct_name -> VTypeCustom ct_name

    let rec vtype_to_gen_vtype_unsafe = function
      | Vtype.VTypeUnit -> GenVTypeUnit
      | VTypeInt -> GenVTypeInt
      | VTypeBool -> GenVTypeBool
      | VTypePair (t1, t2) ->
          GenVTypePair
            (vtype_to_gen_vtype_unsafe t1, vtype_to_gen_vtype_unsafe t2)
      | VTypeCustom ct_name -> GenVTypeCustom ct_name
      | VTypeFun _ -> failwith "Can't generate gen_vtype for function type"

    type gen_options = {
      t : gen_vtype option;
      variant_types : VariantType.t list;
      top_level_defns : (Varname.t * (Vtype.t * Vtype.t)) list;
      v_gen : TagExpr.t QCheck.Gen.t;
      pat_v_gen : TagPat.t QCheck.Gen.t;
      mrd : int;
    }

    type print_options = expr_print_method
    type shrink_options = { preserve_type : bool }

    type arb_options = {
      gen : gen_options;
      print : print_options;
      shrink : shrink_options;
    }

    let gen (initial_opts : gen_options) : t QCheck.Gen.t =
      let open QCheck in
      let open QCheck.Gen in
      (* Check that none of the variant type constructors take function types *)
      List.iter initial_opts.variant_types ~f:(fun (_, cs) ->
          let rec vtype_contains_fun_type = function
            | Vtype.VTypeUnit | VTypeInt | VTypeBool | VTypeCustom _ -> false
            | VTypePair (t1, t2) ->
                vtype_contains_fun_type t1 || vtype_contains_fun_type t2
            | VTypeFun _ -> true
          in
          List.iter cs ~f:(fun (_, t) ->
              if vtype_contains_fun_type t then
                failwith
                  "Can't generate variant type with function type constructors"
              else ()));
      let variant_types = initial_opts.variant_types in
      let variant_types_set =
        initial_opts.variant_types |> List.map ~f:fst |> StringSet.of_list
      in
      let v_gen = initial_opts.v_gen in
      let pat_v_gen = initial_opts.pat_v_gen in

      let varname_gen = Varname.QCheck_testing.gen () in
      let rec varname_filter_type
          ( (_ :
              int * (string * Vtype.t) list -> (TagExpr.t, TagPat.t) expr Gen.t),
            (_ : Vtype.t),
            ((_ : int), (ctx : (string * Vtype.t) list)),
            (_ : TagExpr.t) ) (tf : Vtype.t -> bool) :
          (Varname.t * Vtype.t) Gen.t option =
        (* Shorthand for generating a variable node for an existing variable with a given type in the context, filtered by a function of its type *)
        match
          List.filter_map
            ~f:(fun (xname, xt) -> if tf xt then Some (xname, xt) else None)
            ctx
        with
        | [] -> None
        | _ :: _ as varnames -> Some (varnames |> List.map ~f:return |> oneof)
      and gen_e_var_of_type
          (( (_ :
               int * (string * Vtype.t) list -> (TagExpr.t, TagPat.t) expr Gen.t),
             (_ : Vtype.t),
             ((_ : int), (_ : (string * Vtype.t) list)),
             (v : TagExpr.t) ) as param) (t : Vtype.t) :
          (TagExpr.t, TagPat.t) expr Gen.t option =
        (* Shorthand for generating a variable node for an existing variable with a given type in the context *)
        varname_filter_type param (Vtype.equal t)
        |> Option.map ~f:(Gen.map (fun (xname, _) -> Var (v, xname)))
      and gen_e_if
          ( (self :
              int * (string * Vtype.t) list -> (TagExpr.t, TagPat.t) expr Gen.t),
            (_ : Vtype.t),
            ((d : int), (ctx : (string * Vtype.t) list)),
            (v : TagExpr.t) ) : (TagExpr.t, TagPat.t) expr Gen.t =
        (* Shorthand for generating an if-then-else expression *)
        triple (gen_bool (d - 1, ctx)) (self (d - 1, ctx)) (self (d - 1, ctx))
        >|= fun (e1, e2, e3) -> If (v, e1, e2, e3)
      and gen_e_let_in
          ( (self :
              int * (string * Vtype.t) list -> (TagExpr.t, TagPat.t) expr Gen.t),
            (_ : Vtype.t),
            ((d : int), (ctx : (string * Vtype.t) list)),
            (v : TagExpr.t) ) : (TagExpr.t, TagPat.t) expr Gen.t =
        (* Shorthand for generating a let-in expression *)
        pair varname_gen (gen_any_of_type (d - 1, ctx))
        >>= fun (vname, (e1t, e1)) ->
        self (d - 1, List.Assoc.add ~equal:equal_string ctx vname e1t)
        >|= fun e2 -> Let (v, vname, e1, e2)
      and gen_e_app
          ( (_ :
              int * (string * Vtype.t) list -> (TagExpr.t, TagPat.t) expr Gen.t),
            (_ : Vtype.t),
            ((_ : int), (_ : (string * Vtype.t) list)),
            (_ : TagExpr.t) ) (_ : Vtype.t) :
          (TagExpr.t, TagPat.t) expr Gen.t option =
        (* Shorthand for generating a function application expression *)
        (* varname_filter_type
        (self, (d - 1, ctx), v)
        (function VTypeFun (_, x_t2) -> Vtype.equal t2 x_t2 | _ -> false)
      |> Option.map ~f:(fun x_gen ->
             x_gen >>= fun (xname, xt) ->
             v_gen >>= fun v2 ->
             match xt with
             | VTypeFun (x_t1, _) ->
                 gen (d - 1, ctx) x_t1 >|= fun e2 -> App (v, Var (v2, xname), e2)
             | _ -> failwith "Variable wasn't of function type, as was expected") *)
        (* TODO - the above code is partially complete, but I need to have a way to only return this
      if the call to `gen` will be able to find something of the return type, which may be a function type *)
        None
      and gen_e_match
          ( (self :
              int * (string * Vtype.t) list -> (TagExpr.t, TagPat.t) expr Gen.t),
            (self_t : Vtype.t),
            ((d : int), (ctx : (string * Vtype.t) list)),
            (v : TagExpr.t) ) : (TagExpr.t, TagPat.t) expr Gen.t =
        (* Shorthand for generating a match expression *)
        gen_any_of_type (d - 1, ctx) >>= fun (e1_t, e1) ->
        let pat_gen =
          TagPatternQCheckTesting.gen
            {
              t = e1_t;
              get_variant_type_constructors =
                (fun (vt_name : string) ->
                  List.find_map_exn
                    ~f:(fun (x_vt_name, cs) ->
                      if equal_string vt_name x_vt_name then Some cs else None)
                    variant_types);
              v_gen = pat_v_gen;
            }
        in
        let case_and_pat_gen =
          pat_gen >>= fun (p, p_ctx_list) ->
          let case_ctx : (string * Vtype.t) list =
            List.fold ~init:ctx
              ~f:(fun acc (x, t) -> List.Assoc.add ~equal:equal_string acc x t)
              p_ctx_list
          in
          self (d - 1, case_ctx) >|= fun e -> (p, e)
        in
        list_size (int_range 1 4) case_and_pat_gen
        >|= Nonempty_list.from_list_unsafe
        >|= fun cs -> Match (v, e1, self_t, cs)
      and standard_rec_gen_cases
          ( (self :
              int * (string * Vtype.t) list -> (TagExpr.t, TagPat.t) expr Gen.t),
            (self_t : Vtype.t),
            ((d : int), (ctx : (string * Vtype.t) list)),
            (v : TagExpr.t) ) (t : Vtype.t) :
          (TagExpr.t, TagPat.t) expr Gen.t list =
        (* The standard recursive generator cases for some provided type *)
        [
          gen_e_if (self, self_t, (d, ctx), v) (* If-then-else *);
          gen_e_let_in (self, self_t, (d, ctx), v) (* Let-in *);
          gen_e_match (self, self_t, (d, ctx), v) (* Match *);
        ]
        @ Option.to_list
            (gen_e_app (self, self_t, (d, ctx), v) t (* Function application *))
      and gen_unit (param : int * (string * Vtype.t) list) :
          (TagExpr.t, TagPat.t) expr Gen.t =
        (* Generate an expression that types as unit *)
        let t = Vtype.VTypeUnit in
        fix
          (fun self (d, ctx) ->
            v_gen >>= fun v ->
            let base_cases =
              [ return (UnitLit v) ]
              @ Option.to_list
                  (gen_e_var_of_type (self, t, (d, ctx), v) VTypeUnit)
            in
            let rec_cases =
              standard_rec_gen_cases (self, t, (d, ctx), v) VTypeUnit
            in
            if d > 0 then oneof (base_cases @ rec_cases) else oneof base_cases)
          param
      and gen_int (param : int * (string * Vtype.t) list) :
          (TagExpr.t, TagPat.t) expr Gen.t =
        (* Generate an expression that types as integer *)
        let t = Vtype.VTypeInt in
        fix
          (fun self (d, ctx) ->
            let self' = self (d - 1, ctx) in
            v_gen >>= fun v ->
            let base_cases =
              [ (nat >|= fun n -> IntLit (v, n)) ]
              @ Option.to_list
                  (gen_e_var_of_type (self, t, (d, ctx), v) VTypeInt)
            in
            let rec_cases =
              [
                ( pair self' self' >|= fun (e1, e2) -> Add (v, e1, e2)
                (* Addition *) );
                (self' >|= fun e -> Neg (v, e) (* Negation *));
                ( pair self' self' >|= fun (e1, e2) -> Subtr (v, e1, e2)
                (* Subtraction *) );
                ( pair self' self' >|= fun (e1, e2) -> Mult (v, e1, e2)
                (* Multiplication *) );
              ]
              @ standard_rec_gen_cases (self, t, (d, ctx), v) VTypeInt
            in
            if d > 0 then oneof (base_cases @ rec_cases) else oneof base_cases)
          param
      and gen_bool (param : int * (string * Vtype.t) list) :
          (TagExpr.t, TagPat.t) expr Gen.t =
        (* Generate an expression that types as boolean *)
        let t = Vtype.VTypeBool in
        fix
          (fun self (d, ctx) ->
            let self' = self (d - 1, ctx) in
            v_gen >>= fun v ->
            let base_cases =
              [ (bool >|= fun n -> BoolLit (v, n)) ]
              @ Option.to_list
                  (gen_e_var_of_type (self, t, (d, ctx), v) VTypeBool)
            in
            let rec_cases =
              [
                (self' >|= fun e -> BNot (v, e) (* Negation *));
                ( pair self' self' >|= fun (e1, e2) -> BOr (v, e1, e2)
                (* Disjunction *) );
                ( pair self' self' >|= fun (e1, e2) -> BAnd (v, e1, e2)
                (* Conjunction *) );
                ( pair (gen_int (d - 1, ctx)) (gen_int (d - 1, ctx))
                >|= fun (e1, e2) -> Eq (v, e1, e2)
                (* Integer equality *) );
                ( pair self' self' >|= fun (e1, e2) -> Eq (v, e1, e2)
                (* Boolean equality *) );
                ( pair (gen_int (d - 1, ctx)) (gen_int (d - 1, ctx))
                >|= fun (e1, e2) -> Gt (v, e1, e2)
                (* GT *) );
                ( pair (gen_int (d - 1, ctx)) (gen_int (d - 1, ctx))
                >|= fun (e1, e2) -> GtEq (v, e1, e2)
                (* GTEQ *) );
                ( pair (gen_int (d - 1, ctx)) (gen_int (d - 1, ctx))
                >|= fun (e1, e2) -> Lt (v, e1, e2)
                (* LT *) );
                ( pair (gen_int (d - 1, ctx)) (gen_int (d - 1, ctx))
                >|= fun (e1, e2) -> LtEq (v, e1, e2)
                (* LTEQ *) );
              ]
              @ standard_rec_gen_cases (self, t, (d, ctx), v) VTypeBool
            in
            if d > 0 then oneof (base_cases @ rec_cases) else oneof base_cases)
          param
      and gen_pair ((t1 : Vtype.t), (t2 : Vtype.t))
          (param : int * (string * Vtype.t) list) :
          (TagExpr.t, TagPat.t) expr Gen.t =
        (* Generate an expression that types as a pair of the provided types *)
        let t = Vtype.VTypePair (t1, t2) in
        fix
          (fun self (d, ctx) ->
            v_gen >>= fun v ->
            let base_cases =
              [
                ( pair (gen (d - 1, ctx) t1) (gen (d - 1, ctx) t2)
                >|= fun (e1, e2) -> Pair (v, e1, e2) );
              ]
              @ Option.to_list (gen_e_var_of_type (self, t, (d, ctx), v) t)
            in
            let rec_cases = standard_rec_gen_cases (self, t, (d, ctx), v) t in
            if d > 0 then oneof (base_cases @ rec_cases) else oneof base_cases)
          param
      and gen_variant ((vt_name, cs) : VariantType.t)
          (param : int * (string * Vtype.t) list) :
          (TagExpr.t, TagPat.t) expr Gen.t =
        (* Generate an expression that types as the provided variant type *)
        let t = Vtype.VTypeCustom vt_name in
        fix
          (fun self (d, ctx) ->
            v_gen >>= fun v ->
            let base_cases =
              [
                ( oneof (List.map ~f:return cs) >>= fun (c_name, c_t) ->
                  gen (d - 1, ctx) c_t >|= fun e' -> Constructor (v, c_name, e')
                );
              ]
              @ Option.to_list (gen_e_var_of_type (self, t, (d, ctx), v) t)
            in
            let rec_cases = standard_rec_gen_cases (self, t, (d, ctx), v) t in
            if d > 0 then oneof (base_cases @ rec_cases) else oneof base_cases)
          param
      and gen ((d : int), (ctx : (string * Vtype.t) list)) (t : Vtype.t) :
          (TagExpr.t, TagPat.t) expr Gen.t =
        match t with
        | VTypeUnit -> gen_unit (d, ctx)
        | VTypeInt -> gen_int (d, ctx)
        | VTypeBool -> gen_bool (d, ctx)
        | VTypeFun _ ->
            failwith "Can't generate arbitrary expression of function type"
        | VTypePair (t1, t2) -> gen_pair (t1, t2) (d, ctx)
        | VTypeCustom vt_name ->
            (Option.value_exn
               ~message:
                 (sprintf
                    "The variant type specified (%s) doesn't exist in the \
                     context"
                    vt_name)
               (List.find
                  ~f:(fun (x_vt_name, _) -> equal_string x_vt_name vt_name)
                  variant_types)
            |> gen_variant)
              (d, ctx)
      and gen_any_of_type ((d : int), (ctx : (string * Vtype.t) list)) :
          (Vtype.t * (TagExpr.t, TagPat.t) expr) Gen.t =
        (* TODO - once function types handled better, allow for them here *)
        Vtype.QCheck_testing.gen
          {
            variant_types = variant_types_set;
            allow_fun_types = false;
            mrd = d;
          }
        >>= fun t ->
        gen (d, ctx) t >|= fun e -> (t, e)
      in
      match initial_opts.t with
      | Some t -> gen (initial_opts.mrd, []) (gen_vtype_to_vtype t)
      | None -> gen_any_of_type (initial_opts.mrd, []) >|= snd

    let print : print_options -> t QCheck.Print.t = get_expr_printer

    let rec shrink (opts : shrink_options) : t QCheck.Shrink.t =
      let open QCheck.Iter in
      let preserve_type = opts.preserve_type in
      let unop_shrink (v, e1)
          (recomb : 'tag_e * ('tag_e, 'tag_p) expr -> ('tag_e, 'tag_p) expr) :
          ('tag_e, 'tag_p) expr QCheck.Iter.t =
        shrink opts e1 >|= fun e1' -> recomb (v, e1')
      in
      let binop_shrink ?(allow_return_subexpr : bool option) (v, e1, e2)
          (recomb :
            'tag_e * ('tag_e, 'tag_p) expr * ('tag_e, 'tag_p) expr ->
            ('tag_e, 'tag_p) expr) : ('tag_e, 'tag_p) expr QCheck.Iter.t =
        let allow_return_subexpr =
          Option.value ~default:true allow_return_subexpr
        in
        (if allow_return_subexpr then return e1 <+> return e2 else empty)
        <+> (shrink opts e1 >|= fun e1' -> recomb (v, e1', e2))
        <+> (shrink opts e2 >|= fun e2' -> recomb (v, e1, e2'))
      in
      function
      | UnitLit _ -> empty
      | IntLit _ -> empty
      | Add (v, e1, e2) ->
          binop_shrink (v, e1, e2) (fun (v', e1', e2') -> Add (v', e1', e2'))
      | Neg (v, e1) -> unop_shrink (v, e1) (fun (v', e1') -> Neg (v', e1'))
      | Subtr (v, e1, e2) ->
          binop_shrink (v, e1, e2) (fun (v', e1', e2') -> Subtr (v', e1', e2'))
      | Mult (v, e1, e2) ->
          binop_shrink (v, e1, e2) (fun (v', e1', e2') -> Mult (v', e1', e2'))
      | BoolLit _ -> empty
      | BNot (v, e1) -> unop_shrink (v, e1) (fun (v', e1') -> BNot (v', e1'))
      | BOr (v, e1, e2) ->
          binop_shrink (v, e1, e2) (fun (v', e1', e2') -> BOr (v', e1', e2'))
      | BAnd (v, e1, e2) ->
          binop_shrink (v, e1, e2) (fun (v', e1', e2') -> BAnd (v', e1', e2'))
      | Pair (v, e1, e2) ->
          binop_shrink (v, e1, e2) (fun (v', e1', e2') -> Pair (v', e1', e2'))
      | Eq (v, e1, e2) ->
          binop_shrink ~allow_return_subexpr:(not preserve_type) (v, e1, e2)
            (fun (v', e1', e2') -> Eq (v', e1', e2'))
      | Gt (v, e1, e2) ->
          binop_shrink ~allow_return_subexpr:(not preserve_type) (v, e1, e2)
            (fun (v', e1', e2') -> Gt (v', e1', e2'))
      | GtEq (v, e1, e2) ->
          binop_shrink ~allow_return_subexpr:(not preserve_type) (v, e1, e2)
            (fun (v', e1', e2') -> GtEq (v', e1', e2'))
      | Lt (v, e1, e2) ->
          binop_shrink ~allow_return_subexpr:(not preserve_type) (v, e1, e2)
            (fun (v', e1', e2') -> Lt (v', e1', e2'))
      | LtEq (v, e1, e2) ->
          binop_shrink ~allow_return_subexpr:(not preserve_type) (v, e1, e2)
            (fun (v', e1', e2') -> LtEq (v', e1', e2'))
      | If (v, e1, e2, e3) ->
          (if not preserve_type then return e1 <+> return e2 <+> return e3
           else empty)
          <+> (shrink opts e1 >|= fun e1' -> If (v, e1', e2, e3))
          <+> (shrink opts e2 >|= fun e2' -> If (v, e1, e2', e3))
          <+> (shrink opts e3 >|= fun e3' -> If (v, e1, e2, e3'))
      | Var _ -> empty
      | Let _ ->
          (* Because of let-rec definitions needing a specific form when using Expr to source code,
           this would need a filtered shrink function which I can't be bothered to write at the moment *)
          empty
      | App (v, e1, e2) ->
          binop_shrink ~allow_return_subexpr:(not preserve_type) (v, e1, e2)
            (fun (v', e1', e2') -> App (v', e1', e2'))
      | Match (v, e1, t2, ps) ->
          (if preserve_type then empty else return e1)
          <+> ( (* Try shrinking each case expression *)
                QCheck.Shrink.list_elems
                  (fun (c_p, c_e) ->
                    shrink opts c_e >|= fun c_e' -> (c_p, c_e'))
                  (Nonempty_list.to_list ps)
              >|= fun ps' ->
                Match (v, e1, t2, Nonempty_list.from_list_unsafe ps') )
      | Constructor (v, c, e1) ->
          unop_shrink (v, e1) (fun (v', e1') -> Constructor (v', c, e1'))

    let arbitrary (opts : arb_options) : t QCheck.arbitrary =
      QCheck.make ~print:(print opts.print) ~shrink:(shrink opts.shrink)
        (gen opts.gen)
  end
end

(** The standard expression implementation with the standard pattern
    implementation *)
module StdExpr : S with module Pattern = Pattern.StdPattern =
  Make (Pattern.StdPattern)

let rec std_expr_of_std_pattern ~(convert_tag : 'tag_p -> 'tag_e) :
    'tag_p Pattern.StdPattern.t -> ('tag_e, 'tag_p) StdExpr.t =
  let open StdExpr in
  function
  | PatName (v, xname, _) -> Var (convert_tag v, xname)
  | PatPair (v, p1, p2) ->
      Pair
        ( convert_tag v,
          std_expr_of_std_pattern ~convert_tag p1,
          std_expr_of_std_pattern ~convert_tag p2 )
  | PatConstructor (v, cname, p) ->
      Constructor (convert_tag v, cname, std_expr_of_std_pattern ~convert_tag p)
