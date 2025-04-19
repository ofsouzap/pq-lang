open Core
open Utils
module StdPattern = Pattern.StdPattern
module StdExpr = Expr.StdExpr
module StdProgram = Program.StdProgram
module StdTypeCtx = TypeChecker.TypeContext.StdSetTypeContext

type 'a flat_pattern =
  | FlatPatName of 'a * string * Vtype.t
  | FlatPatPair of 'a * ('a * string * Vtype.t) * ('a * string * Vtype.t)
  | FlatPatConstructor of 'a * string * ('a * string * Vtype.t)
[@@deriving sexp, equal]

module M : Pattern.S with type 'a t = 'a flat_pattern = struct
  type 'a t = 'a flat_pattern [@@deriving sexp, equal]
  type 'a typed_t = (Vtype.t * 'a) t [@@deriving sexp, equal]
  type plain_t = unit t [@@deriving sexp, equal]

  let to_plain_t : 'a t -> plain_t = function
    | FlatPatName (_, name, t) -> FlatPatName ((), name, t)
    | FlatPatPair (_, (_, p1name, p1t), (_, p2name, p2t)) ->
        FlatPatPair ((), ((), p1name, p1t), ((), p2name, p2t))
    | FlatPatConstructor (_, cname, (_, p1name, p1t)) ->
        FlatPatConstructor ((), cname, ((), p1name, p1t))

  let node_val = function
    | FlatPatName (v, _, _) -> v
    | FlatPatPair (v, _, _) -> v
    | FlatPatConstructor (v, _, _) -> v

  let fmap ~(f : 'a -> 'b) : 'a t -> 'b t = function
    | FlatPatName (v, name, t) -> FlatPatName (f v, name, t)
    | FlatPatPair (v, (v1, p1name, p1t), (v2, p2name, p2t)) ->
        FlatPatPair (f v, (f v1, p1name, p1t), (f v2, p2name, p2t))
    | FlatPatConstructor (v, cname, (v1, p1name, p1t)) ->
        FlatPatConstructor (f v, cname, (f v1, p1name, p1t))

  let rename_var ~(old_name : Varname.t) ~(new_name : Varname.t) = function
    | FlatPatName (v, name, t) ->
        FlatPatName
          (v, (if equal_string name old_name then new_name else name), t)
    | FlatPatPair (v, (v1, p1name, p1t), (v2, p2name, p2t)) ->
        FlatPatPair
          ( v,
            ( v1,
              (if equal_string p1name old_name then new_name else p1name),
              p1t ),
            ( v2,
              (if equal_string p2name old_name then new_name else p2name),
              p2t ) )
    | FlatPatConstructor (v, cname, (v1, p1name, p1t)) ->
        FlatPatConstructor
          ( v,
            cname,
            ( v1,
              (if equal_string p1name old_name then new_name else p1name),
              p1t ) )

  let existing_names : 'a t -> StringSet.t = function
    | FlatPatName (_, name, _) -> StringSet.singleton name
    | FlatPatPair (_, (_, p1name, _), (_, p2name, _)) ->
        StringSet.of_list [ p1name; p2name ]
    | FlatPatConstructor (_, c_name, (_, p1name, _)) ->
        StringSet.of_list [ c_name; p1name ]

  let defined_vars : 'a t -> (Varname.t * Vtype.t) list = function
    | FlatPatName (_, name, t) -> [ (name, t) ]
    | FlatPatPair (_, (_, p1name, p1t), (_, p2name, p2t)) ->
        [ (p1name, p1t); (p2name, p2t) ]
    | FlatPatConstructor (_, _, (_, p1name, p1t)) -> [ (p1name, p1t) ]

  let to_source_code : 'a t -> string = function
    | FlatPatName (_, name, t) ->
        sprintf "%s : (%s)" name (Vtype.to_source_code t)
    | FlatPatPair (_, (_, p1name, p1t), (_, p2name, p2t)) ->
        sprintf "(%s : (%s)), (%s : (%s))" p1name (Vtype.to_source_code p1t)
          p2name (Vtype.to_source_code p2t)
    | FlatPatConstructor (_, cname, (_, p1name, p1t)) ->
        sprintf "%s (%s : (%s))" cname p1name (Vtype.to_source_code p1t)

  module QCheck_testing =
  functor
    (Tag : sig
       type t
     end)
    ->
    struct
      type this_t = Tag.t t * (string * Vtype.t) list
      type t = this_t

      type gen_options = {
        get_variant_type_constructors : string -> VariantType.constructor list;
        v_gen : Tag.t QCheck.Gen.t;
        t : Vtype.t;
      }

      type print_options = unit
      type shrink_options = unit
      type arb_options = gen_options

      let gen = failwith "Not implemented"
      let gen_pat_name = failwith "Not implemented"
      let print = failwith "Not implemented"
      let shrink = failwith "Not implemented"
      let arbitrary = failwith "Not implemented"
    end
end

module FlatExpr : Expr.S with module Pattern = M = Expr.Make (M)

module FlatProgram :
  Program.S
    with module Pattern = M
     and module Expr = FlatExpr
     and module QuotientType = QuotientType.StdQuotientType
     and module CustomType = CustomType.StdCustomType =
  Program.Make (FlatExpr) (CustomType.StdCustomType)

let to_std_pattern : 'a M.t -> 'a StdPattern.t = function
  | FlatPatName (v, name, t) -> PatName (v, name, t)
  | FlatPatPair (v, (x1_v, x1_name, x1_t), (x2_v, x2_name, x2_t)) ->
      PatPair (v, PatName (x1_v, x1_name, x1_t), PatName (x2_v, x2_name, x2_t))
  | FlatPatConstructor (v, c_name, (x1_v, x1_name, x1_t)) ->
      PatConstructor (v, c_name, PatName (x1_v, x1_name, x1_t))

(** Convert a flat Expr expression to a non-flat Expr expression *)
let rec to_std_expr : ('tag_e, 'tag_p) FlatExpr.t -> ('tag_e, 'tag_p) StdExpr.t
    = function
  | UnitLit v -> UnitLit v
  | IntLit (v, n) -> IntLit (v, n)
  | Add (v, e1, e2) -> Add (v, to_std_expr e1, to_std_expr e2)
  | Neg (v, e) -> Neg (v, to_std_expr e)
  | Subtr (v, e1, e2) -> Subtr (v, to_std_expr e1, to_std_expr e2)
  | Mult (v, e1, e2) -> Mult (v, to_std_expr e1, to_std_expr e2)
  | BoolLit (v, b) -> BoolLit (v, b)
  | BNot (v, e) -> BNot (v, to_std_expr e)
  | BOr (v, e1, e2) -> BOr (v, to_std_expr e1, to_std_expr e2)
  | BAnd (v, e1, e2) -> BAnd (v, to_std_expr e1, to_std_expr e2)
  | Pair (v, e1, e2) -> Pair (v, to_std_expr e1, to_std_expr e2)
  | Eq (v, e1, e2) -> Eq (v, to_std_expr e1, to_std_expr e2)
  | Gt (v, e1, e2) -> Gt (v, to_std_expr e1, to_std_expr e2)
  | GtEq (v, e1, e2) -> GtEq (v, to_std_expr e1, to_std_expr e2)
  | Lt (v, e1, e2) -> Lt (v, to_std_expr e1, to_std_expr e2)
  | LtEq (v, e1, e2) -> LtEq (v, to_std_expr e1, to_std_expr e2)
  | If (v, e1, e2, e3) -> If (v, to_std_expr e1, to_std_expr e2, to_std_expr e3)
  | Var (v, name) -> Var (v, name)
  | Let (v, xname, e1, e2) -> Let (v, xname, to_std_expr e1, to_std_expr e2)
  | App (v, e1, e2) -> App (v, to_std_expr e1, to_std_expr e2)
  | Match (v, e, t2, cases) ->
      Match
        ( v,
          to_std_expr e,
          t2,
          Nonempty_list.map
            ~f:(fun (p, e) -> (to_std_pattern p, to_std_expr e))
            cases )
  | Constructor (v, name, e) -> Constructor (v, name, to_std_expr e)
