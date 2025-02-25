open Core
open Utils
open Expr
module Pattern = Pattern.StdPattern
module Expr = Expr.StdExpr
module Unifier = Unifier.StdUnifier
module QuotientType = QuotientType.StdQuotientType
module CustomType = CustomType.StdCustomType
module Program = Program.StdProgram
open FlatPattern

let sexp_op ((op : string), (args : Sexp.t list)) : Sexp.t =
  Sexp.List (Sexp.Atom op :: args)

(** Provides an interface to an SMT solver for quotient type checking purposes
*)
module type S = sig
  type smt_intf_error [@@deriving sexp, equal]
  type expr_tag = { t : Vtype.t } [@@deriving sexp, equal]
  type pattern_tag = { t : Vtype.t } [@@deriving sexp, equal]
  type tag_pattern = pattern_tag Pattern.t [@@deriving sexp, equal]
  type tag_expr = (expr_tag, pattern_tag) Expr.t [@@deriving sexp, equal]
  type tag_flat_pattern = pattern_tag FlatPattern.M.t [@@deriving sexp, equal]

  type tag_flat_expr = (expr_tag, pattern_tag) FlatExpr.t
  [@@deriving sexp, equal]

  val pattern_tag_to_expr_tag : pattern_tag -> expr_tag

  type tag_unifier = (expr_tag, pattern_tag) Unifier.t [@@deriving sexp, equal]

  type tag_quotient_type_eqcons = (expr_tag, pattern_tag) QuotientType.eqcons
  [@@deriving sexp, equal]

  type tag_quotient_type = (expr_tag, pattern_tag) QuotientType.t
  [@@deriving sexp, equal]

  type tag_custom_type = (expr_tag, pattern_tag) CustomType.t
  [@@deriving sexp, equal]

  type tag_program = (expr_tag, pattern_tag) Program.t [@@deriving sexp, equal]

  module State : sig
    type var_defn = {
      name : Varname.t;
      kind :
        [ `NonRec of (Varname.t * Vtype.t) option | `Rec of Varname.t * Vtype.t ];
      return_t : Vtype.t;
      body : tag_flat_expr;
    }
    [@@deriving sexp, equal]

    type top_level_elem = VarDecl of Varname.t * Vtype.t | VarDefn of var_defn
    [@@deriving sexp, equal]

    type variant_type_constructor_info = {
      name : string;
      accessor_name : string;
      t : Vtype.t;
    }
    [@@deriving sexp, equal]

    type variant_type_info = {
      name : string;
      constructors : variant_type_constructor_info list;
    }
    [@@deriving sexp, equal]

    type pair_type_info = {
      name : string;
      t : Vtype.t * Vtype.t;
      constructor_name : string;
      fst_accessor_name : string;
      snd_accessor_name : string;
    }
    [@@deriving sexp, equal]

    type t [@@deriving sexp, equal]

    val state_init : tag_custom_type list -> t
    val find_root_base_type : t -> Vtype.t -> (Vtype.t, smt_intf_error) result

    val state_get_pair_type_info :
      Vtype.t * Vtype.t -> t -> (pair_type_info, smt_intf_error) result

    val state_get_vtype_special_eq_fun_name : t -> Vtype.t -> string option
    val state_add_variant_type : VariantType.t -> t -> t
    val state_add_var_decl : string * Vtype.t -> t -> t
    val state_add_var_defn : var_defn -> t -> (t, smt_intf_error) result
  end

  module Assertion : sig
    type t = Not of t | Eq of Vtype.t option * tag_flat_expr * tag_flat_expr
    type assertion = t

    module Builder : sig end
  end

  module Builder : sig
    val build_vtype : State.t -> Vtype.t -> (Sexp.t, smt_intf_error) result

    val build_expr :
      directly_callable_fun_names:Utils.StringSet.t ->
      State.t ->
      tag_flat_expr ->
      (Sexp.t, smt_intf_error) result

    val build_top_level_elem :
      State.t -> State.top_level_elem -> (Sexp.t, smt_intf_error) result

    val build_state :
      existing_names:Utils.StringSet.t ->
      State.t ->
      (Utils.StringSet.t * Sexp.t list, smt_intf_error) result

    val build_assertion :
      state:State.t -> Assertion.t -> (Sexp.t, smt_intf_error) result
  end

  type formula

  val create_formula :
    existing_names:Utils.StringSet.t ->
    State.t ->
    Assertion.t list ->
    (Utils.StringSet.t * formula, smt_intf_error) result

  val check_satisfiability : formula -> [ `Sat | `Unknown | `Unsat ]
end

module Z3Intf : S = struct
  type smt_intf_error =
    | PairTypeNotDefinedInState of Vtype.t * Vtype.t
    | UndefinedCustomTypeName of string
    | PatternFlatteningError of FlatPattern.flattening_error
  [@@deriving sexp, equal]

  type expr_tag = { t : Vtype.t } [@@deriving sexp, equal]
  type pattern_tag = { t : Vtype.t } [@@deriving sexp, equal]
  type tag_pattern = pattern_tag Pattern.t [@@deriving sexp, equal]
  type tag_expr = (expr_tag, pattern_tag) Expr.t [@@deriving sexp, equal]
  type tag_flat_pattern = pattern_tag FlatPattern.M.t [@@deriving sexp, equal]

  type tag_flat_expr = (expr_tag, pattern_tag) FlatExpr.t
  [@@deriving sexp, equal]

  let pattern_tag_to_expr_tag ({ t } : pattern_tag) : expr_tag = { t }

  type tag_unifier = (expr_tag, pattern_tag) Unifier.t [@@deriving sexp, equal]

  type tag_quotient_type_eqcons = (expr_tag, pattern_tag) QuotientType.eqcons
  [@@deriving sexp, equal]

  type tag_quotient_type = (expr_tag, pattern_tag) QuotientType.t
  [@@deriving sexp, equal]

  type tag_custom_type = (expr_tag, pattern_tag) CustomType.t
  [@@deriving sexp, equal]

  type tag_program = (expr_tag, pattern_tag) Program.t [@@deriving sexp, equal]

  let custom_special_name x =
    "PQ-"
    ^ (function
        | `Var _ -> "Var-"
        | `QuotientEqBinding _ -> "QVar-"
        | `VariantType _ -> "VT-"
        | `VariantTypeConstructor _ -> "VTC-"
        | `VariantTypeConstructorAccessor _ -> "VTCA-"
        | `Function _ -> "Fun-"
        | `EqualityFunction _ -> "Eq-")
        x
    ^
    match x with
    | `Var name -> name
    | `QuotientEqBinding ((name : string), (side : [ `Pattern | `Expr ])) ->
        sprintf "%s-%s" name
          (match side with `Pattern -> "pat" | `Expr -> "expr")
    | `VariantType (vt_name : string) -> vt_name
    | `VariantTypeConstructor ((vt_name : string), (c_name : string)) ->
        vt_name ^ "-" ^ c_name
    | `VariantTypeConstructorAccessor ((vt_name : string), (c_name : string)) ->
        vt_name ^ "-" ^ c_name ^ "-val"
    | `Function (f_name : string) -> f_name
    | `EqualityFunction (qt_name : string) -> qt_name

  (** Provides a representation of a state that can be built into input for the
      solver *)
  module State = struct
    type var_defn = {
      name : Varname.t;
      kind :
        [ `NonRec of (Varname.t * Vtype.t) option | `Rec of Varname.t * Vtype.t ];
      return_t : Vtype.t;
      body : tag_flat_expr;
    }
    [@@deriving sexp, equal]

    type top_level_elem = VarDecl of Varname.t * Vtype.t | VarDefn of var_defn
    [@@deriving sexp, equal]

    module VtypePairMap = Map.Make (struct
      type t = Vtype.t * Vtype.t

      let compare ((t1_1, t1_2) : Vtype.t * Vtype.t)
          ((t2_1, t2_2) : Vtype.t * Vtype.t) : int =
        let first_comp = Vtype.compare t1_1 t2_1 in
        if first_comp = 0 then Vtype.compare t1_2 t2_2 else first_comp

      let t_of_sexp = pair_of_sexp Vtype.t_of_sexp Vtype.t_of_sexp
      let sexp_of_t = sexp_of_pair Vtype.sexp_of_t Vtype.sexp_of_t
    end)

    type variant_type_constructor_info = {
      name : string;
      accessor_name : string;
      t : Vtype.t;
    }
    [@@deriving sexp, equal]

    type variant_type_info = {
      name : string;
      constructors : variant_type_constructor_info list;
    }
    [@@deriving sexp, equal]

    type pair_type_info = {
      name : string;
      t : Vtype.t * Vtype.t;
      constructor_name : string;
      fst_accessor_name : string;
      snd_accessor_name : string;
    }
    [@@deriving sexp, equal]

    type t = {
      custom_types : tag_custom_type list;
          (** A list of the custom types defined. This isn't used for building
              the SMT formula, but is used for e.g. mapping quotient types to
              their root base type *)
      pair_types_defined : pair_type_info VtypePairMap.t;
          (** A mapping from all pair types that have been defined to info about
              them. This used to decide which possible instantiations of the
              pair type to create in the initial datatype declarations. Added
              automatically by module *)
      special_variant_types : Sexp.t list;
          (** These should be translated by: x -> "(declare-datatypes () x)".
              Created automatically in state initialization *)
      variant_types : variant_type_info list;
          (** The custom variant types defined. Added by module user *)
      declared_consts : (string * Vtype.t) list;
          (** Translated by: (xname, xtype) -> (declare-const xname xtype ).
              Created automatically in state initialization *)
      top_level_rev : top_level_elem list;
          (** The reversed list of top-level definitions of the program. Added
              by module user *)
    }
    [@@deriving sexp, equal]

    type state = t

    let vt_unit_name : string = custom_special_name (`VariantType "unit")

    let vt_unit_constructor_name : string =
      custom_special_name (`VariantTypeConstructor ("unit", "unit"))

    let vt_unit_val : string = custom_special_name (`Var "unit")

    let state_init (custom_types : tag_custom_type list) : t =
      {
        custom_types;
        pair_types_defined =
          (VtypePairMap.empty : pair_type_info VtypePairMap.t);
        special_variant_types =
          [
            sexp_op
              (* Unit type *)
              (vt_unit_name, [ Atom vt_unit_constructor_name ]);
          ];
        variant_types = [];
        declared_consts = [ (vt_unit_val, VTypeUnit) ];
        top_level_rev = [];
      }

    let rec find_root_base_type (state : t) (t : Vtype.t) :
        (Vtype.t, smt_intf_error) Result.t =
      let open Result in
      match t with
      | VTypeCustom ct_name -> (
          List.find state.custom_types ~f:(fun x_ct ->
              equal_string (CustomType.name x_ct) ct_name)
          |> Result.of_option ~error:(UndefinedCustomTypeName ct_name)
          >>= function
          | VariantType (vt_name, _) -> Ok (Vtype.VTypeCustom vt_name)
          | CustomType.QuotientType qt ->
              find_root_base_type state (VTypeCustom qt.base_type_name))
      | _ -> Ok t

    let state_add_pair_type ((t1 : Vtype.t), (t2 : Vtype.t)) (state : t) :
        (t, smt_intf_error) Result.t =
      let open Result in
      (* Make sure to only be using types for base variant types, not quotient types *)
      find_root_base_type state t1 >>= fun t1 ->
      find_root_base_type state t2 >>| fun t2 ->
      let pair_type_vt_name : Vtype.t * Vtype.t -> string =
        (* Generates an implicitly-unique name for the instantiation of the
            pair type for the specified two parameter types *)
        let rec aux ((t1 : Vtype.t), (t2 : Vtype.t)) : string =
          custom_special_name (`VariantType "pair")
          ^ sprintf "-+%s++%s+" (vtype_name t1) (vtype_name t2)
        and vtype_name : Vtype.t -> string = function
          | VTypeUnit -> "Unit"
          | VTypeInt -> "Int"
          | VTypeBool -> "Bool"
          | VTypePair (t1, t2) -> aux (t1, t2)
          | VTypeFun (t1, t2) ->
              sprintf "%s->%s" (vtype_name t1) (vtype_name t2)
          | VTypeCustom ct_name -> ct_name
        in
        aux
      in
      let vt_name : string = pair_type_vt_name (t1, t2) in
      let constructor_name : string =
        custom_special_name (`VariantTypeConstructor (vt_name, "pair"))
      in
      let accessor_name (side : [ `Fst | `Snd ]) : string =
        custom_special_name (`VariantTypeConstructorAccessor (vt_name, "pair"))
        ^ match side with `Fst -> "-fst" | `Snd -> "-snd"
      in
      let info =
        {
          name = vt_name;
          t = (t1, t2);
          constructor_name;
          fst_accessor_name = accessor_name `Fst;
          snd_accessor_name = accessor_name `Snd;
        }
      in
      if Map.mem state.pair_types_defined (t1, t2) then state
      else
        {
          state with
          pair_types_defined =
            Map.set state.pair_types_defined ~key:(t1, t2) ~data:info;
        }

    let state_get_pair_type_info ((t1 : Vtype.t), (t2 : Vtype.t)) (state : t) :
        (pair_type_info, smt_intf_error) Result.t =
      let open Result in
      find_root_base_type state t1 >>= fun t1 ->
      find_root_base_type state t2 >>= fun t2 ->
      match Map.find state.pair_types_defined (t1, t2) with
      | Some info -> Ok info
      | None -> Error (PairTypeNotDefinedInState (t1, t2))

    (** Get the name of the special function that should be defined to compare
        two values of the specified type. This is only for quotient types *)
    let state_get_vtype_special_eq_fun_name (state : t) :
        Vtype.t -> string option = function
      | VTypeCustom ct_name ->
          if
            List.exists state.custom_types ~f:(function
              | VariantType _ -> false
              | CustomType.QuotientType qt -> equal_string qt.name ct_name)
          then Some (custom_special_name (`EqualityFunction ct_name))
          else None
      | _ -> None

    (** Add a variant type definition to the state *)
    let state_add_variant_type ((vt_name, vt_cs) : VariantType.t) (state : t) :
        t =
      let constructor_info ((c_name, c_t) : VariantType.constructor) :
          variant_type_constructor_info =
        {
          name = c_name;
          accessor_name =
            custom_special_name
              (`VariantTypeConstructorAccessor (vt_name, c_name));
          t = c_t;
        }
      in
      {
        state with
        variant_types =
          { name = vt_name; constructors = List.map ~f:constructor_info vt_cs }
          :: state.variant_types;
      }

    (** Add a variable declaration to the state *)
    let state_add_var_decl ((xname : Varname.t), (xtype : Vtype.t)) (state : t)
        : t =
      {
        state with
        top_level_rev = VarDecl (xname, xtype) :: state.top_level_rev;
      }

    (** Add a variable definition to the state *)
    let state_add_var_defn (defn : var_defn) (state : t) :
        (t, smt_intf_error) Result.t =
      let open Result in
      let rec add_pair_types_used (state : state) :
          Vtype.t -> (state, smt_intf_error) Result.t = function
        | VTypeUnit | VTypeInt | VTypeBool | VTypeCustom _ -> Ok state
        | VTypePair (t1, t2) ->
            state_add_pair_type (t1, t2) state >>= fun state ->
            add_pair_types_used state t1 >>= fun state ->
            add_pair_types_used state t2
        | VTypeFun (t1, t2) ->
            add_pair_types_used state t1 >>= fun state ->
            add_pair_types_used state t2
      in
      let rec search_add_pair_types_used (state : state) :
          tag_flat_expr -> (state, smt_intf_error) Result.t = function
        | UnitLit v -> add_pair_types_used state v.t
        | IntLit (v, _) -> add_pair_types_used state v.t
        | Add (v, e1, e2) ->
            add_pair_types_used state v.t >>= fun state ->
            search_add_pair_types_used state e1 >>= fun state ->
            search_add_pair_types_used state e2
        | Neg (v, e1) ->
            add_pair_types_used state v.t >>= fun state ->
            search_add_pair_types_used state e1
        | Subtr (v, e1, e2) ->
            add_pair_types_used state v.t >>= fun state ->
            search_add_pair_types_used state e1 >>= fun state ->
            search_add_pair_types_used state e2
        | Mult (v, e1, e2) ->
            add_pair_types_used state v.t >>= fun state ->
            search_add_pair_types_used state e1 >>= fun state ->
            search_add_pair_types_used state e2
        | BoolLit (v, _) -> add_pair_types_used state v.t
        | BNot (v, e1) ->
            add_pair_types_used state v.t >>= fun state ->
            search_add_pair_types_used state e1
        | BOr (v, e1, e2) ->
            add_pair_types_used state v.t >>= fun state ->
            search_add_pair_types_used state e1 >>= fun state ->
            search_add_pair_types_used state e2
        | BAnd (v, e1, e2) ->
            add_pair_types_used state v.t >>= fun state ->
            search_add_pair_types_used state e1 >>= fun state ->
            search_add_pair_types_used state e2
        | Pair (v, e1, e2) ->
            add_pair_types_used state v.t >>= fun state ->
            search_add_pair_types_used state e1 >>= fun state ->
            search_add_pair_types_used state e2
        | Eq (v, e1, e2) ->
            add_pair_types_used state v.t >>= fun state ->
            search_add_pair_types_used state e1 >>= fun state ->
            search_add_pair_types_used state e2
        | Gt (v, e1, e2) ->
            add_pair_types_used state v.t >>= fun state ->
            search_add_pair_types_used state e1 >>= fun state ->
            search_add_pair_types_used state e2
        | GtEq (v, e1, e2) ->
            add_pair_types_used state v.t >>= fun state ->
            search_add_pair_types_used state e1 >>= fun state ->
            search_add_pair_types_used state e2
        | Lt (v, e1, e2) ->
            add_pair_types_used state v.t >>= fun state ->
            search_add_pair_types_used state e1 >>= fun state ->
            search_add_pair_types_used state e2
        | LtEq (v, e1, e2) ->
            add_pair_types_used state v.t >>= fun state ->
            search_add_pair_types_used state e1 >>= fun state ->
            search_add_pair_types_used state e2
        | If (v, e1, e2, e3) ->
            add_pair_types_used state v.t >>= fun state ->
            search_add_pair_types_used state e1 >>= fun state ->
            search_add_pair_types_used state e2 >>= fun state ->
            search_add_pair_types_used state e3
        | Var (v, _) -> add_pair_types_used state v.t
        | Let (v, _, e1, e2) ->
            add_pair_types_used state v.t >>= fun state ->
            search_add_pair_types_used state e1 >>= fun state ->
            search_add_pair_types_used state e2
        | App (v, e1, e2) ->
            add_pair_types_used state v.t >>= fun state ->
            search_add_pair_types_used state e1 >>= fun state ->
            search_add_pair_types_used state e2
        | Match (v, e1, t2, cases) ->
            add_pair_types_used state t2 >>= fun state ->
            add_pair_types_used state v.t >>= fun state ->
            search_add_pair_types_used state e1 >>= fun state ->
            Nonempty_list.fold_result
              ~f:(fun state (pat, e) ->
                let pat_t = FlatPattern.M.node_val pat in
                add_pair_types_used state pat_t.t >>= fun state ->
                search_add_pair_types_used state e)
              ~init:state cases
        | Constructor (v, _, e1) ->
            add_pair_types_used state v.t >>= fun state ->
            search_add_pair_types_used state e1
      in
      (* Add all used pair types to the state *)
      search_add_pair_types_used state defn.body >>| fun state ->
      { state with top_level_rev = VarDefn defn :: state.top_level_rev }
  end

  module Assertion = struct
    type t =
      | Not of t
      | Eq of Vtype.t option * tag_flat_expr * tag_flat_expr
          (** Equality node. Optionally the types of the two expressions can be
              specified. This allows for equality lifting for quotient types *)

    type assertion = t

    module Builder = struct end
  end

  (** Provides building of components of an SMT formula into input for the
      solver *)
  module Builder = struct
    open State
    open Assertion

    let rec build_vtype (state : State.t) (t : Vtype.t) :
        (Sexp.t, smt_intf_error) Result.t =
      let open Result in
      let open Sexp in
      (* Make sure to map the type to its root base type, in case it is a quotient type *)
      State.find_root_base_type state t >>= fun t ->
      match t with
      | VTypeUnit -> Ok (Atom vt_unit_name)
      | VTypeInt -> Ok (Atom "Int")
      | VTypeBool -> Ok (Atom "Bool")
      | VTypePair (t1, t2) ->
          state_get_pair_type_info (t1, t2) state >>| fun pair_type_info ->
          Atom pair_type_info.name
      | VTypeFun (t1, t2) ->
          build_vtype state t1 >>= fun t1_node ->
          build_vtype state t2 >>= fun t2_node ->
          Ok (sexp_op ("Array", [ t1_node; t2_node ]))
      | VTypeCustom ct_name -> Ok (Atom ct_name)

    let rec build_expr ~(directly_callable_fun_names : StringSet.t)
        (state : State.t) : tag_flat_expr -> (Sexp.t, smt_intf_error) Result.t =
      let open Result in
      let open Sexp in
      function
      | UnitLit _ -> Ok (Atom vt_unit_val)
      | IntLit (_, n) -> Ok (Atom (Int.to_string n))
      | BoolLit (_, b) -> Ok (Atom (Bool.to_string b))
      | Var (_, name) -> Ok (Atom name)
      | Add (_, e1, e2) ->
          build_expr ~directly_callable_fun_names state e1 >>= fun node1 ->
          build_expr ~directly_callable_fun_names state e2 >>= fun node2 ->
          Ok (sexp_op ("+", [ node1; node2 ]))
      | Neg (_, e) ->
          build_expr ~directly_callable_fun_names state e >>= fun node ->
          Ok (sexp_op ("-", [ node ]))
      | Subtr (_, e1, e2) ->
          build_expr ~directly_callable_fun_names state e1 >>= fun node1 ->
          build_expr ~directly_callable_fun_names state e2 >>= fun node2 ->
          Ok (sexp_op ("-", [ node1; node2 ]))
      | Mult (_, e1, e2) ->
          build_expr ~directly_callable_fun_names state e1 >>= fun node1 ->
          build_expr ~directly_callable_fun_names state e2 >>= fun node2 ->
          Ok (sexp_op ("*", [ node1; node2 ]))
      | BNot (_, e) ->
          build_expr ~directly_callable_fun_names state e >>= fun node ->
          Ok (sexp_op ("not", [ node ]))
      | BOr (_, e1, e2) ->
          build_expr ~directly_callable_fun_names state e1 >>= fun node1 ->
          build_expr ~directly_callable_fun_names state e2 >>= fun node2 ->
          Ok (sexp_op ("or", [ node1; node2 ]))
      | BAnd (_, e1, e2) ->
          build_expr ~directly_callable_fun_names state e1 >>= fun node1 ->
          build_expr ~directly_callable_fun_names state e2 >>= fun node2 ->
          Ok (sexp_op ("and", [ node1; node2 ]))
      | Eq (_, e1, e2) ->
          build_expr ~directly_callable_fun_names state e1 >>= fun node1 ->
          build_expr ~directly_callable_fun_names state e2 >>= fun node2 ->
          Ok (sexp_op ("=", [ node1; node2 ]))
      | Gt (_, e1, e2) ->
          build_expr ~directly_callable_fun_names state e1 >>= fun node1 ->
          build_expr ~directly_callable_fun_names state e2 >>= fun node2 ->
          Ok (sexp_op (">", [ node1; node2 ]))
      | GtEq (_, e1, e2) ->
          build_expr ~directly_callable_fun_names state e1 >>= fun node1 ->
          build_expr ~directly_callable_fun_names state e2 >>= fun node2 ->
          Ok (sexp_op (">=", [ node1; node2 ]))
      | Lt (_, e1, e2) ->
          build_expr ~directly_callable_fun_names state e1 >>= fun node1 ->
          build_expr ~directly_callable_fun_names state e2 >>= fun node2 ->
          Ok (sexp_op ("<", [ node1; node2 ]))
      | LtEq (_, e1, e2) ->
          build_expr ~directly_callable_fun_names state e1 >>= fun node1 ->
          build_expr ~directly_callable_fun_names state e2 >>= fun node2 ->
          Ok (sexp_op ("<=", [ node1; node2 ]))
      | If (_, e1, e2, e3) ->
          build_expr ~directly_callable_fun_names state e1 >>= fun node1 ->
          build_expr ~directly_callable_fun_names state e2 >>= fun node2 ->
          build_expr ~directly_callable_fun_names state e3 >>= fun node3 ->
          Ok (sexp_op ("ite", [ node1; node2; node3 ]))
      | Let (_, xname, e1, e2) ->
          build_expr ~directly_callable_fun_names state e1 >>= fun node1 ->
          build_expr ~directly_callable_fun_names state e2 >>= fun node2 ->
          Ok (sexp_op ("let", [ List [ List [ Atom xname; node1 ] ]; node2 ]))
      | Pair (_, e1, e2) ->
          let e1_t = (FlatPattern.FlatExpr.node_val e1).t in
          let e2_t = (FlatPattern.FlatExpr.node_val e2).t in
          state_get_pair_type_info (e1_t, e2_t) state >>= fun pair_type_info ->
          build_expr ~directly_callable_fun_names state e1 >>= fun e1' ->
          build_expr ~directly_callable_fun_names state e2 >>| fun e2' ->
          sexp_op (pair_type_info.constructor_name, [ e1'; e2' ])
      | App (_, e1, e2) -> (
          build_expr ~directly_callable_fun_names state e1 >>= fun e1_node ->
          build_expr ~directly_callable_fun_names state e2 >>| fun e2_node ->
          let get_default_repr () = sexp_op ("select", [ e1_node; e2_node ]) in
          match e1 with
          | Var (_, fname) ->
              if Core.Set.mem directly_callable_fun_names fname then
                sexp_op (fname, [ e2_node ])
              else get_default_repr ()
          | _ -> get_default_repr ())
      | Constructor (_, name, e) ->
          build_expr ~directly_callable_fun_names state e >>= fun node ->
          Ok (sexp_op (name, [ node ]))
      | Match (_, e, _, cases) ->
          build_expr ~directly_callable_fun_names state e >>= fun e_node ->
          let build_case ((pat : tag_flat_pattern), (body : tag_flat_expr)) :
              (Sexp.t, smt_intf_error) Result.t =
            build_expr ~directly_callable_fun_names state body
            >>= fun body_node ->
            match pat with
            | FlatPattern.FlatPatPair (_, (_, x1name, x1t), (_, x2name, x2t)) ->
                state_get_pair_type_info (x1t, x2t) state
                >>| fun pair_type_info ->
                List
                  [
                    sexp_op
                      ( pair_type_info.constructor_name,
                        [ Atom x1name; Atom x2name ] );
                    body_node;
                  ]
            | FlatPattern.FlatPatConstructor (_, c_name, (_, x, _)) ->
                Ok (List [ List [ Atom c_name; Atom x ]; body_node ])
          in
          Nonempty_list.fold_result ~init:[]
            ~f:(fun acc case ->
              build_case case >>| fun case_node -> case_node :: acc)
            cases
          >>| List.rev
          >>| fun case_nodes -> sexp_op ("match", e_node :: [ List case_nodes ])

    let build_top_level_elem (state : State.t) :
        top_level_elem -> (Sexp.t, smt_intf_error) Result.t =
      let open Result in
      let open Sexp in
      function
      | VarDecl (xname, xtype) ->
          build_vtype state xtype >>= fun type_node ->
          Ok (sexp_op ("declare-const", [ Atom xname; type_node ]))
      | VarDefn { name; kind; return_t; body } -> (
          match kind with
          | `NonRec None ->
              build_vtype state return_t >>= fun type_node ->
              Ok (sexp_op ("declare-const", [ Atom name; type_node ]))
          | `NonRec (Some (param_name, param_t)) ->
              build_vtype state param_t >>= fun param_type_node ->
              build_vtype state return_t >>= fun return_type_node ->
              build_expr ~directly_callable_fun_names:StringSet.empty state body
              >>= fun body_node ->
              Ok
                (sexp_op
                   ( "define-fun",
                     [
                       Atom name;
                       List [ List [ Atom param_name; param_type_node ] ];
                       return_type_node;
                       body_node;
                     ] ))
          | `Rec (param_name, param_t) ->
              build_vtype state param_t >>= fun param_type_node ->
              build_vtype state return_t >>= fun return_type_node ->
              build_expr ~directly_callable_fun_names:(StringSet.singleton name)
                state body
              >>= fun body_node ->
              Ok
                (sexp_op
                   ( "define-fun-rec",
                     [
                       Atom name;
                       List [ List [ Atom param_name; param_type_node ] ];
                       return_type_node;
                       body_node;
                     ] )))

    let build_state ~(existing_names : StringSet.t) (state : State.t) :
        (StringSet.t * Sexp.t list, smt_intf_error) Result.t =
      let open Result in
      let build_datatype_decl () : (Sexp.t, smt_intf_error) Result.t =
        (* Pair type declarations *)
        List.map (Map.to_alist state.pair_types_defined)
          ~f:(fun ((t1, t2), pair_type_info) ->
            build_vtype state t1 >>= fun t1_node ->
            build_vtype state t2 >>| fun t2_node ->
            sexp_op
              ( pair_type_info.name,
                [
                  sexp_op
                    ( pair_type_info.constructor_name,
                      [
                        sexp_op (pair_type_info.fst_accessor_name, [ t1_node ]);
                        sexp_op (pair_type_info.snd_accessor_name, [ t2_node ]);
                      ] );
                ] ))
        |> Result.all
        >>= fun (pair_type_decls : Sexp.t list) ->
        (* Special variant type declarations *)
        List.map state.special_variant_types ~f:(fun special_variant_type ->
            Ok special_variant_type)
        |> Result.all
        >>= fun (special_variant_type_decls : Sexp.t list) ->
        List.map state.variant_types ~f:(fun vt_info ->
            (* Create the nodes for the constructors *)
            List.map vt_info.constructors ~f:(fun c_info ->
                build_vtype state c_info.t >>| fun t_node ->
                sexp_op
                  (c_info.name, [ sexp_op (c_info.accessor_name, [ t_node ]) ]))
            |> Result.all
            >>| fun constructor_nodes ->
            (* Create the main variant type definition node *)
            sexp_op (vt_info.name, constructor_nodes))
        |> Result.all
        >>| fun (variant_type_decls : Sexp.t list) ->
        (* Combine all the datatype declarations into the single declare-datatypes node *)
        let datatype_decls =
          pair_type_decls @ special_variant_type_decls @ variant_type_decls
        in
        sexp_op ("declare-datatypes", [ List []; List datatype_decls ])
      in
      let build_consts_nodes () : (Sexp.t list, smt_intf_error) Result.t =
        List.fold_result ~init:[]
          ~f:(fun acc (xname, xtype) ->
            build_vtype state xtype >>| fun xtype_node ->
            sexp_op ("declare-const", [ Atom xname; xtype_node ]) :: acc)
          state.declared_consts
        >>| List.rev
      in
      let build_lifted_eq_fun_nodes ~(existing_names : StringSet.t) () :
          (StringSet.t * Sexp.t list, smt_intf_error) Result.t =
        List.fold_result state.custom_types ~init:(existing_names, [])
          ~f:(fun (existing_names, (acc_rev : Sexp.t list list)) -> function
          | VariantType _ -> Ok (existing_names, acc_rev)
          | CustomType.QuotientType qt ->
              let t = Vtype.VTypeCustom qt.name in
              let eq_fun_name =
                State.state_get_vtype_special_eq_fun_name state t
                |> Option.value_exn
                     ~message:"Quotient type has no special equality function"
              in
              build_vtype state t >>= fun t_node ->
              build_vtype state VTypeBool >>= fun bool_node ->
              (* Equality function definition node *)
              sexp_op
                ( "declare-fun",
                  [ Atom eq_fun_name; List [ t_node; t_node ]; bool_node ] )
              |> fun decl_node ->
              (* Assertion nodes *)
              let create_assert_node (bindings : (Varname.t * Vtype.t) list)
                  (l : tag_flat_expr) (r : tag_flat_expr) :
                  (Sexp.t, smt_intf_error) Result.t =
                List.fold_result bindings ~init:([], [])
                  ~f:(fun (qt_names, acc_rev) (xname, xtype) ->
                    (match xtype with
                    | VTypeCustom ct_name ->
                        List.find_map state.custom_types ~f:(function
                          | VariantType _ -> None
                          | CustomType.QuotientType qt ->
                              if equal_string ct_name qt.name then Some qt
                              else None)
                    | _ -> None)
                    |> fun x_qt ->
                    build_vtype state xtype >>| fun xtype_node ->
                    match x_qt with
                    | Some x_qt ->
                        let xname_l =
                          custom_special_name
                            (`QuotientEqBinding (xname, `Pattern))
                        in
                        let xname_r =
                          custom_special_name
                            (`QuotientEqBinding (xname, `Expr))
                        in
                        ( (xname, x_qt) :: qt_names,
                          sexp_op (xname_l, [ xtype_node ])
                          :: sexp_op (xname_r, [ xtype_node ])
                          :: acc_rev )
                    | None ->
                        (qt_names, sexp_op (xname, [ xtype_node ]) :: acc_rev))
                >>=
                fun ( (qt_binding_names : (string * tag_quotient_type) list),
                      bindings_nodes_rev )
                ->
                let bindings_nodes = List.rev bindings_nodes_rev in
                let l, r =
                  List.fold qt_binding_names ~init:(l, r)
                    ~f:(fun (l, r) (xname, _) ->
                      ( FlatPattern.FlatExpr.rename_var ~old_name:xname
                          ~new_name:
                            (custom_special_name
                               (`QuotientEqBinding (xname, `Pattern)))
                          l,
                        FlatPattern.FlatExpr.rename_var ~old_name:xname
                          ~new_name:
                            (custom_special_name
                               (`QuotientEqBinding (xname, `Expr)))
                          r ))
                in
                build_expr ~directly_callable_fun_names:StringSet.empty state l
                >>= fun l_node ->
                build_expr ~directly_callable_fun_names:StringSet.empty state r
                >>= fun r_node ->
                (let eq_node = sexp_op (eq_fun_name, [ l_node; r_node ]) in
                 List.fold qt_binding_names ~init:eq_node
                   ~f:(fun acc_node (xname, x_qt) ->
                     let xname_l =
                       custom_special_name
                         (`QuotientEqBinding (xname, `Pattern))
                     in
                     let xname_r =
                       custom_special_name (`QuotientEqBinding (xname, `Expr))
                     in
                     sexp_op
                       ( "=>",
                         [
                           sexp_op
                             ( State.state_get_vtype_special_eq_fun_name state
                                 (VTypeCustom x_qt.name)
                               |> Option.value_exn
                                    ~message:
                                      "Quotient type didn't have custom secial \
                                       equality function name",
                               [ Atom xname_l; Atom xname_r ] );
                           acc_node;
                         ] )))
                |> fun body_node ->
                Ok
                  (sexp_op
                     ( "assert",
                       [
                         sexp_op ("forall", [ List bindings_nodes; body_node ]);
                       ] ))
              in
              (let arg_name = custom_special_name (`Var "x") in
               sexp_op
                 ( "assert",
                   [
                     sexp_op
                       ( "forall",
                         [
                           List [ List [ Atom arg_name; t_node ] ];
                           sexp_op
                             (eq_fun_name, [ Atom arg_name; Atom arg_name ]);
                         ] );
                   ] ))
              |> fun assert_node_base ->
              List.fold_result qt.eqconss ~init:(existing_names, [])
                ~f:(fun (existing_names, acc_rev) eqcons ->
                  fst eqcons.body
                  |> std_expr_of_std_pattern
                       ~convert_tag:pattern_tag_to_expr_tag
                  |> FlatPattern.of_expr ~existing_names
                  |> Result.map_error ~f:(fun err -> PatternFlatteningError err)
                  >>= fun (existing_names, flat_l) ->
                  snd eqcons.body
                  |> FlatPattern.of_expr ~existing_names
                  |> Result.map_error ~f:(fun err -> PatternFlatteningError err)
                  >>= fun (existing_names, flat_r) ->
                  create_assert_node eqcons.bindings flat_l flat_r
                  >>| fun node -> (existing_names, node :: acc_rev))
              >>= fun (existing_names, assert_nodes_main_rev) ->
              let assert_nodes_main = List.rev assert_nodes_main_rev in
              (* Output *)
              [ decl_node; assert_node_base ] @ assert_nodes_main |> Ok
              >>= fun node -> Ok (existing_names, node :: acc_rev))
        >>| fun (existing_names, nodes_deep_rev) ->
        (existing_names, nodes_deep_rev |> List.rev |> List.concat)
      in
      let build_top_level_nodes () : (Sexp.t list, smt_intf_error) Result.t =
        (* Note that top_level_rev is reversed, but the folding here un-reverses it *)
        List.fold_result ~init:[]
          ~f:(fun acc elem ->
            build_top_level_elem state elem >>| fun elem_node ->
            elem_node :: acc)
          state.top_level_rev
      in
      build_datatype_decl () >>= fun (datatype_decl : Sexp.t) ->
      build_lifted_eq_fun_nodes ~existing_names ()
      >>= fun (existing_names, (lifted_eq_fun_nodes : Sexp.t list)) ->
      build_consts_nodes () >>= fun (declared_consts_nodes : Sexp.t list) ->
      build_top_level_nodes () >>| fun (top_level_nodes : Sexp.t list) ->
      ( existing_names,
        (datatype_decl :: declared_consts_nodes)
        @ lifted_eq_fun_nodes @ top_level_nodes )

    let build_assertion ~(state : State.t) :
        assertion -> (Sexp.t, smt_intf_error) Result.t =
      let open Result in
      let rec aux : assertion -> (Sexp.t, smt_intf_error) Result.t = function
        | Not x -> aux x >>| fun x_node -> sexp_op ("not", [ x_node ])
        | Eq (t_opt, e1, e2) ->
            build_expr ~directly_callable_fun_names:StringSet.empty state e1
            >>= fun e1_node ->
            build_expr ~directly_callable_fun_names:StringSet.empty state e2
            >>|
            let eq_fun_name =
              let default = "=" in
              match t_opt with
              | None -> default
              | Some t -> (
                  match State.state_get_vtype_special_eq_fun_name state t with
                  | None -> default
                  | Some eq_fun_name -> eq_fun_name)
            in
            fun e2_node -> sexp_op (eq_fun_name, [ e1_node; e2_node ])
      in
      fun assertion ->
        aux assertion >>| fun assertion_node ->
        sexp_op ("assert", [ assertion_node ])
  end

  type formula = Sexp.t list

  let create_formula ~(existing_names : StringSet.t) (state : State.t)
      (assertions : Assertion.t list) :
      (StringSet.t * formula, smt_intf_error) Result.t =
    let open Result in
    Builder.build_state ~existing_names state
    >>= fun (existing_names, state_nodes) ->
    List.map assertions ~f:(Builder.build_assertion ~state) |> Result.all
    >>| fun assertions_nodes -> (existing_names, state_nodes @ assertions_nodes)

  (** Check the satisifability of a given formula *)
  let check_satisfiability (formula : formula) : [ `Sat | `Unsat | `Unknown ] =
    let smtlib_string =
      List.map ~f:Sexp.to_string_hum
        (* I'm using human-readable string generation for debugging/readability *)
        formula
      |> String.concat ~sep:"\n"
    in
    let ctx = Z3.mk_context [ ("model", "false") ] in
    let ast_vec = Z3.SMT.parse_smtlib2_string ctx smtlib_string [] [] [] [] in
    let solver = Z3.Solver.mk_solver ctx None in
    (* Add the parsed formula into the solver *)
    Z3.Solver.add solver (Z3.AST.ASTVector.to_expr_list ast_vec);
    match Z3.Solver.check solver [] with
    | Z3.Solver.SATISFIABLE -> `Sat
    | Z3.Solver.UNSATISFIABLE -> `Unsat
    | Z3.Solver.UNKNOWN -> `Unknown
end
