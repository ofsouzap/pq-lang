open Core
open Utils
open Variant_types
open Vtype
open Pattern
open Ast
open Quotient_types
open Custom_types
open Program

type typing_error =
  | UndefinedVariable of string
  | EqconsVariableNotInBindings of string * vtype
  | TypeMismatch of vtype * vtype * string option
  | NoCommonRootType of vtype * vtype
  | PatternTypeMismatch of plain_pattern * vtype * vtype
  | EqConsBodyPatternTypeMismatch of plain_pattern * vtype * vtype
  | EqConsBodyExprTypeMismatch of plain_expr * vtype * vtype
  | EqualOperatorTypeMistmatch of vtype * vtype
  | ExpectedFunctionOf of vtype
  | UndefinedVariantTypeConstructor of string
  | PatternMultipleVariableDefinitions of string
  | MultipleTopLevelNameDefinitions of string
  | DuplicateTypeNameDefinition of string
  | UndefinedTypeName of string
  | MultipleVariantTypeConstructorDefinitions of string
[@@deriving sexp, equal]

let equal_typing_error_variant x y =
  match (x, y) with
  | UndefinedVariable _, UndefinedVariable _
  | EqconsVariableNotInBindings _, EqconsVariableNotInBindings _
  | TypeMismatch _, TypeMismatch _
  | NoCommonRootType _, NoCommonRootType _
  | PatternTypeMismatch _, PatternTypeMismatch _
  | EqConsBodyPatternTypeMismatch _, EqConsBodyPatternTypeMismatch _
  | EqConsBodyExprTypeMismatch _, EqConsBodyExprTypeMismatch _
  | EqualOperatorTypeMistmatch _, EqualOperatorTypeMistmatch _
  | ExpectedFunctionOf _, ExpectedFunctionOf _
  | UndefinedVariantTypeConstructor _, UndefinedVariantTypeConstructor _
  | PatternMultipleVariableDefinitions _, PatternMultipleVariableDefinitions _
  | MultipleTopLevelNameDefinitions _, MultipleTopLevelNameDefinitions _
  | DuplicateTypeNameDefinition _, DuplicateTypeNameDefinition _
  | UndefinedTypeName _, UndefinedTypeName _
  | ( MultipleVariantTypeConstructorDefinitions _,
      MultipleVariantTypeConstructorDefinitions _ ) ->
      true
  | UndefinedVariable _, _
  | EqconsVariableNotInBindings _, _
  | TypeMismatch _, _
  | NoCommonRootType _, _
  | PatternTypeMismatch _, _
  | EqConsBodyPatternTypeMismatch _, _
  | EqConsBodyExprTypeMismatch _, _
  | EqualOperatorTypeMistmatch _, _
  | ExpectedFunctionOf _, _
  | UndefinedVariantTypeConstructor _, _
  | PatternMultipleVariableDefinitions _, _
  | MultipleTopLevelNameDefinitions _, _
  | DuplicateTypeNameDefinition _, _
  | UndefinedTypeName _, _
  | MultipleVariantTypeConstructorDefinitions _, _ ->
      false

let print_typing_error = function
  | UndefinedVariable x -> "Undefined variable: " ^ x
  | EqconsVariableNotInBindings (xname, xtype) ->
      sprintf
        "Variable %s of type %s is not in the bindings of an eqcons but is used"
        xname
        (vtype_to_source_code xtype)
  | TypeMismatch (t1, t2, msg) ->
      sprintf "Type mismatch: expected %s but got %s%s"
        (vtype_to_source_code t1) (vtype_to_source_code t2)
        (match msg with None -> "" | Some msg -> sprintf " (%s)" msg)
  | NoCommonRootType (t1, t2) ->
      sprintf "Expected common root type for %s and %s but none found"
        (vtype_to_source_code t1) (vtype_to_source_code t2)
  | PatternTypeMismatch (p, t1, t2) ->
      sprintf "Type mismatch in pattern \"%s\": expected %s but got %s"
        (pattern_to_source_code p) (vtype_to_source_code t1)
        (vtype_to_source_code t2)
  | EqConsBodyPatternTypeMismatch (pattern, t1, t2) ->
      sprintf
        "Type mismatch in equivalence constructor body pattern for \"%s\": \
         expected %s but got %s"
        (pattern_to_source_code pattern)
        (vtype_to_source_code t1) (vtype_to_source_code t2)
  | EqConsBodyExprTypeMismatch (expr, t1, t2) ->
      sprintf
        "Type mismatch in equivalence constructor body pattern for \"%s\": \
         expected %s but got %s"
        (ast_to_source_code ~use_newlines:false expr)
        (vtype_to_source_code t1) (vtype_to_source_code t2)
  | EqualOperatorTypeMistmatch (t1, t2) ->
      sprintf "Trying to apply equality operator to %s and %s"
        (vtype_to_source_code t1) (vtype_to_source_code t2)
  | ExpectedFunctionOf t ->
      "Expected a function taking input of " ^ vtype_to_source_code t
  | UndefinedVariantTypeConstructor c_name ->
      sprintf "Undefined variant type constructor: %s" c_name
  | PatternMultipleVariableDefinitions xname ->
      sprintf "Variable named \"%s\" has been defined twice in a pattern" xname
  | MultipleTopLevelNameDefinitions xname ->
      sprintf "Multiple top-level definitions of name \"%s\"" xname
  | DuplicateTypeNameDefinition vt_name ->
      sprintf "Variant type named \"%s\" has been defined multiple times"
        vt_name
  | UndefinedTypeName vt_name -> sprintf "Undefined variant type: %s" vt_name
  | MultipleVariantTypeConstructorDefinitions c_name ->
      sprintf
        "Variant type constructor named \"%s\" has been defined multiple times"
        c_name

module type TypingTypeContext = sig
  type t

  val empty : t

  val create :
    custom_types:('tag_e, 'tag_p) custom_type list -> (t, typing_error) Result.t

  val find_type_defn_by_name : t -> string -> plain_custom_type option
  val type_defn_exists : t -> string -> bool

  val find_variant_type_with_constructor :
    t -> string -> (variant_type * variant_type_constructor) option

  val type_defns_to_ordered_list : t -> plain_custom_type list

  val is_quotient_descendant :
    t -> vtype -> vtype -> (bool, typing_error) Result.t

  val find_common_root_type :
    t -> vtype -> vtype -> (vtype option, typing_error) Result.t
end

module SetTypingTypeContext : TypingTypeContext = struct
  type t = {
    custom_types : plain_custom_type StringMap.t;
    custom_types_order : string list;
  }

  let empty : t = { custom_types = StringMap.empty; custom_types_order = [] }

  let create ~(custom_types : ('tag_e, 'tag_p) custom_type list) :
      (t, typing_error) Result.t =
    let type_defns_map_or_err =
      custom_types
      |> List.map ~f:to_plain_custom_type
      |> StringMap.of_list_with_key ~get_key:(function
           | VariantType (vt_name, _) -> vt_name
           | QuotientType qt -> qt.name)
    in
    match type_defns_map_or_err with
    | `Duplicate_key dup_name -> Error (DuplicateTypeNameDefinition dup_name)
    | `Ok type_defns_map ->
        Ok
          {
            custom_types = type_defns_map;
            custom_types_order =
              List.map ~f:(fun ct -> custom_type_name ct) custom_types;
          }

  let find_type_defn_by_name (ctx : t) :
      string -> ('tag_e, 'tag_p) custom_type option =
    Map.find ctx.custom_types

  let type_defn_exists (ctx : t) (vt_name : string) : bool =
    Option.is_some (find_type_defn_by_name ctx vt_name)

  let find_variant_type_with_constructor (ctx : t) (c_name : string) :
      (variant_type * variant_type_constructor) option =
    Map.fold_until ctx.custom_types ~init:()
      ~f:(fun ~key:_ ~(data : ('tag_e, 'tag_p) custom_type) () ->
        match data with
        | VariantType ((_, cs) as vt) -> (
            let search_res =
              List.find cs ~f:(fun (xc_name, _) -> equal_string c_name xc_name)
            in
            match search_res with
            | None -> Continue ()
            | Some c -> Stop (Some (vt, c)))
        | QuotientType _ -> Continue ())
      ~finish:(fun () -> None)

  let type_defns_to_ordered_list (ctx : t) : ('tag_e, 'tag_p) custom_type list =
    List.map
      ~f:(fun ct_name -> find_type_defn_by_name ctx ct_name |> Option.value_exn)
      ctx.custom_types_order

  let rec is_quotient_descendant (ctx : t) (t1 : vtype) (t2 : vtype) :
      (bool, typing_error) Result.t =
    let open Result in
    match (t1, t2) with
    | VTypeCustom ct1_name, VTypeCustom ct2_name -> (
        find_type_defn_by_name ctx ct1_name
        |> Result.of_option ~error:(UndefinedTypeName ct1_name)
        >>= fun ct1 ->
        find_type_defn_by_name ctx ct2_name
        |> Result.of_option ~error:(UndefinedTypeName ct2_name)
        >>= fun ct2 ->
        match (ct1, ct2) with
        | VariantType _, QuotientType _ -> Ok false
        | VariantType vt1, VariantType vt2 -> Ok (equal_variant_type vt1 vt2)
        | QuotientType qt1, VariantType vt2 ->
            is_quotient_descendant ctx (VTypeCustom qt1.base_type_name)
              (VTypeCustom (fst vt2))
        | QuotientType qt1, QuotientType qt2 ->
            if equal_quotient_type equal_unit equal_unit qt1 qt2 then Ok true
            else
              is_quotient_descendant ctx (VTypeCustom qt1.base_type_name)
                (VTypeCustom qt2.name))
    | VTypePair (t11, t12), VTypePair (t21, t22) ->
        is_quotient_descendant ctx t11 t21 >>= fun first ->
        is_quotient_descendant ctx t12 t22 >>| fun second -> first && second
    | VTypeFun (t11, t12), VTypeFun (t21, t22) ->
        is_quotient_descendant ctx t11 t21 >>= fun first ->
        (* Note that this part is the other way round to usual *)
        is_quotient_descendant ctx t22 t12 >>| fun second -> first && second
    | _ -> Ok (equal_vtype t1 t2)

  let rec find_common_root_type (ctx : t) (t1 : vtype) (t2 : vtype) :
      (vtype option, typing_error) Result.t =
    let open Result in
    match (t1, t2) with
    | VTypeCustom ct1_name, VTypeCustom ct2_name -> (
        find_type_defn_by_name ctx ct1_name
        |> Result.of_option ~error:(UndefinedTypeName ct1_name)
        >>= fun ct1 ->
        find_type_defn_by_name ctx ct2_name
        |> Result.of_option ~error:(UndefinedTypeName ct2_name)
        >>= fun ct2 ->
        match (ct1, ct2) with
        | VariantType vt1, VariantType vt2 ->
            if equal_variant_type vt1 vt2 then Ok (Some t1) else Ok None
        | QuotientType qt1, QuotientType qt2 ->
            if equal_quotient_type equal_unit equal_unit qt1 qt2 then
              Ok (Some t1)
            else
              find_common_root_type ctx (VTypeCustom qt1.base_type_name)
                (VTypeCustom qt2.name)
        | VariantType vt, QuotientType qt | QuotientType qt, VariantType vt ->
            find_common_root_type ctx (VTypeCustom qt.base_type_name)
              (VTypeCustom (fst vt)))
    | VTypePair (t11, t12), VTypePair (t21, t22) ->
        find_common_root_type ctx t11 t21 >>= fun t1 ->
        find_common_root_type ctx t12 t22 >>= fun t2 ->
        Ok
          Option.(
            t1 >>= fun t1 ->
            t2 >>| fun t2 -> VTypePair (t1, t2))
    | ( VTypeFun _,
        VTypeFun _
        (* TODO - for now, I won't try figure out how to implement this for function types. this is for another time *)
      )
    | _ ->
        if equal_vtype t1 t2 then Ok (Some t1) else Ok None
end

module type TypingVarContext = sig
  type t

  val empty : t
  val add : t -> string -> vtype -> t
  val find : t -> string -> vtype option
  val singleton : string -> vtype -> t
  val append : t -> t -> t
  val exists : t -> string -> bool
  val to_list : t -> (string * vtype) list
end

module ListTypingVarContext : TypingVarContext = struct
  type t = (string * vtype) list

  let empty = []
  let add ctx x t = List.Assoc.add ctx x t ~equal:String.equal
  let find ctx x = List.Assoc.find ctx x ~equal:String.equal
  let singleton = add empty

  let append ctx1 =
    List.fold ~init:ctx1 ~f:(fun ctx_acc (x, t) -> add ctx_acc x t)

  let exists ctx x = match find ctx x with None -> false | Some _ -> true
  let to_list = Fn.id
end

module type TypeCheckerSig = functor
  (TypeCtx : TypingTypeContext)
  (VarCtx : TypingVarContext)
  -> sig
  type checked_type_ctx

  val checked_empty_type_ctx : checked_type_ctx

  type ('tag_e, 'tag_p) typed_program

  val typed_program_get_program :
    ('tag_e, 'tag_p) typed_program ->
    (vtype * 'tag_e, vtype * 'tag_p) Program.program

  val check_vtype : checked_type_ctx -> vtype -> (unit, typing_error) Result.t

  val type_pattern :
    checked_type_ctx * VarCtx.t ->
    'tag_p pattern ->
    ((vtype * 'tag_p) pattern * VarCtx.t, typing_error) Result.t

  val type_expr :
    checked_type_ctx * VarCtx.t ->
    ('tag_e, 'tag_p) expr ->
    (('tag_e, 'tag_p) typed_expr, typing_error) Result.t

  val check_type_ctx : TypeCtx.t -> (checked_type_ctx, typing_error) Result.t

  val type_program :
    ('tag_e, 'tag_p) program ->
    (('tag_e, 'tag_p) typed_program, typing_error) Result.t
end

module TypeChecker : TypeCheckerSig =
functor
  (TypeCtx : TypingTypeContext)
  (VarCtx : TypingVarContext)
  ->
  struct
    type checked_type_ctx = TypeCtx.t

    let checked_empty_type_ctx = TypeCtx.empty

    type ('tag_e, 'tag_p) typed_program =
      (vtype * 'tag_e, vtype * 'tag_p) program

    let typed_program_get_program (tp : ('tag_e, 'tag_p) typed_program) :
        (vtype * 'tag_e, vtype * 'tag_p) program =
      tp

    let rec check_vtype (ctx : checked_type_ctx) :
        vtype -> (unit, typing_error) Result.t =
      let open Result in
      function
      | VTypeInt | VTypeBool | VTypeUnit -> Ok ()
      | VTypePair (t1, t2) | VTypeFun (t1, t2) ->
          check_vtype ctx t1 >>= fun () -> check_vtype ctx t2
      | VTypeCustom vt_name ->
          if TypeCtx.find_type_defn_by_name ctx vt_name |> Option.is_some then
            Ok ()
          else UndefinedTypeName vt_name |> Error

    let rec type_pattern
        (((type_ctx : checked_type_ctx), (var_ctx : VarCtx.t)) as ctx)
        (orig_p : 'tag_p pattern) :
        ((vtype * 'tag_p) pattern * VarCtx.t, typing_error) Result.t =
      let open Result in
      match orig_p with
      | PatName (v, x_name, x_t) ->
          check_vtype type_ctx x_t >>= fun () ->
          if VarCtx.exists var_ctx x_name then
            Error (PatternMultipleVariableDefinitions x_name)
          else
            Ok (PatName ((x_t, v), x_name, x_t), VarCtx.add var_ctx x_name x_t)
      | PatPair (v, p1, p2) ->
          type_pattern ctx p1 >>= fun (p1_typed, var_ctx_from_p1) ->
          type_pattern (type_ctx, var_ctx_from_p1) p2
          >>= fun (p2_typed, var_ctx_final) ->
          let p1_t = pattern_node_val p1_typed |> fst in
          let p2_t = pattern_node_val p2_typed |> fst in
          Ok
            ( PatPair ((VTypePair (p1_t, p2_t), v), p1_typed, p2_typed),
              var_ctx_final )
      | PatConstructor (v, c_name, p) -> (
          match TypeCtx.find_variant_type_with_constructor type_ctx c_name with
          | None -> Error (UndefinedVariantTypeConstructor c_name)
          | Some ((vt_name, _), (_, c_t)) ->
              type_pattern ctx p >>= fun (p_typed, var_ctx_from_p) ->
              let p_t = pattern_node_val p_typed |> fst in
              (* Check if the subpattern's determined type is valid for the expected constructor's argument type.
                For non-quotient type cases, this is just an equality check *)
              TypeCtx.is_quotient_descendant type_ctx p_t c_t
              >>= fun subpattern_type_matches ->
              if subpattern_type_matches then
                Ok
                  ( PatConstructor ((VTypeCustom vt_name, v), c_name, p_typed),
                    var_ctx_from_p )
              else
                Error
                  (PatternTypeMismatch (pattern_to_plain_pattern p, c_t, p_t)))

    let rec type_expr (((type_ctx : TypeCtx.t), (var_ctx : VarCtx.t)) as ctx)
        (orig_e : ('tag_e, 'tag_p) expr) :
        (('tag_e, 'tag_p) typed_expr, typing_error) Result.t =
      let open Result in
      let e_type (e : (vtype * 'tag_e, vtype * 'tag_p) expr) : vtype =
        e |> expr_node_val |> fst
      in
      let be_of_type ?(msg : string option) (exp : vtype)
          (e : (vtype * 'tag_e, vtype * 'tag_p) expr) :
          ((vtype * 'tag_e, vtype * 'tag_p) expr, typing_error) Result.t =
        if equal_vtype exp (e_type e) then Ok e
        else Error (TypeMismatch (exp, e_type e, msg))
      in
      let type_binop ?(msg : string option)
          (recomp :
            (vtype * 'tag_e, vtype * 'tag_p) expr ->
            (vtype * 'tag_e, vtype * 'tag_p) expr ->
            vtype ->
            (vtype * 'tag_e, vtype * 'tag_p) expr) (e1 : ('tag_e, 'tag_p) expr)
          (e2 : ('tag_e, 'tag_p) expr) (req_t : vtype) :
          ((vtype * 'tag_e, vtype * 'tag_p) expr, typing_error) Result.t =
        type_expr ctx e1 >>= fun e1' ->
        type_expr ctx e2 >>= fun e2' ->
        be_of_type
          ?msg:(Option.map msg ~f:(fun s -> sprintf "%s (first arg)" s))
          req_t e1'
        >>= fun _ ->
        be_of_type
          ?msg:(Option.map msg ~f:(fun s -> sprintf "%s (second arg)" s))
          req_t e2'
        >>= fun _ -> Ok (recomp e1' e2' (e_type e1'))
      in
      let type_unop ?(msg : string option)
          (recomp :
            (vtype * 'tag_e, vtype * 'tag_p) expr ->
            vtype ->
            (vtype * 'tag_e, vtype * 'tag_p) expr) (e1 : ('tag_e, 'tag_p) expr)
          (req_t : vtype) :
          ((vtype * 'tag_e, vtype * 'tag_p) expr, typing_error) Result.t =
        type_expr ctx e1 >>= fun e1' ->
        be_of_type ?msg req_t e1' >>= fun _ -> Ok (recomp e1' (e_type e1'))
      in
      let type_int_compare ?(msg : string option)
          (recomp :
            (vtype * 'tag_e, vtype * 'tag_p) expr ->
            (vtype * 'tag_e, vtype * 'tag_p) expr ->
            (vtype * 'tag_e, vtype * 'tag_p) expr) (e1 : ('tag_e, 'tag_p) expr)
          (e2 : ('tag_e, 'tag_p) expr) =
        type_expr ctx e1 >>= fun e1' ->
        type_expr ctx e2 >>= fun e2' ->
        let t1 = e_type e1' in
        let t2 = e_type e2' in
        match (t1, t2) with
        | VTypeInt, VTypeInt -> Ok (recomp e1' e2')
        | VTypeInt, _ -> Error (TypeMismatch (VTypeInt, t2, msg))
        | _, _ -> Error (TypeMismatch (VTypeInt, t1, msg))
      in
      match orig_e with
      | UnitLit v -> Ok (UnitLit (VTypeUnit, v))
      | IntLit (v, x) -> Ok (IntLit ((VTypeInt, v), x))
      | Add (v, e1, e2) ->
          type_binop ~msg:"Add node"
            (fun e1' e2' t -> Add ((t, v), e1', e2'))
            e1 e2 VTypeInt
      | Subtr (v, e1, e2) ->
          type_binop ~msg:"Subtr node"
            (fun e1' e2' t -> Subtr ((t, v), e1', e2'))
            e1 e2 VTypeInt
      | Mult (v, e1, e2) ->
          type_binop ~msg:"Mult node"
            (fun e1' e2' t -> Mult ((t, v), e1', e2'))
            e1 e2 VTypeInt
      | Neg (v, e1) ->
          type_unop ~msg:"Neg node" (fun e1' t -> Neg ((t, v), e1')) e1 VTypeInt
      | BoolLit (v, x) -> Ok (BoolLit ((VTypeBool, v), x))
      | BNot (v, e1) ->
          type_unop ~msg:"BNot node"
            (fun e1' t -> BNot ((t, v), e1'))
            e1 VTypeBool
      | BOr (v, e1, e2) ->
          type_binop ~msg:"BOr node"
            (fun e1' e2' t -> BOr ((t, v), e1', e2'))
            e1 e2 VTypeBool
      | BAnd (v, e1, e2) ->
          type_binop ~msg:"BAnd node"
            (fun e1' e2' t -> BAnd ((t, v), e1', e2'))
            e1 e2 VTypeBool
      | Pair (v, e1, e2) ->
          type_expr ctx e1 >>= fun e1' ->
          type_expr ctx e2 >>= fun e2' ->
          let t1 = e_type e1' in
          let t2 = e_type e2' in
          Ok (Pair ((VTypePair (t1, t2), v), e1', e2'))
      | Eq (v, e1, e2) -> (
          type_expr ctx e1 >>= fun e1' ->
          type_expr ctx e2 >>= fun e2' ->
          let t1 = e_type e1' in
          let t2 = e_type e2' in
          match (t1, t2) with
          | VTypeInt, VTypeInt | VTypeBool, VTypeBool ->
              Ok (Eq ((VTypeBool, v), e1', e2'))
          | _, _ -> Error (EqualOperatorTypeMistmatch (t1, t2)))
      | Gt (v, e1, e2) ->
          type_int_compare ~msg:"Gt node"
            (fun e1' e2' -> Gt ((VTypeBool, v), e1', e2'))
            e1 e2
      | GtEq (v, e1, e2) ->
          type_int_compare ~msg:"GtEq node"
            (fun e1' e2' -> GtEq ((VTypeBool, v), e1', e2'))
            e1 e2
      | Lt (v, e1, e2) ->
          type_int_compare ~msg:"Lt node"
            (fun e1' e2' -> Lt ((VTypeBool, v), e1', e2'))
            e1 e2
      | LtEq (v, e1, e2) ->
          type_int_compare ~msg:"LtEq node"
            (fun e1' e2' -> LtEq ((VTypeBool, v), e1', e2'))
            e1 e2
      | If (v, e1, e2, e3) -> (
          type_expr ctx e1 >>= fun e1' ->
          be_of_type ~msg:"If cond node" VTypeBool e1' >>= fun _ ->
          type_expr ctx e2 >>= fun e2' ->
          type_expr ctx e3 >>= fun e3' ->
          let t2 = e_type e2' in
          let t3 = e_type e3' in
          TypeCtx.find_common_root_type type_ctx t2 t3 >>= function
          | None -> Error (TypeMismatch (t2, t3, Some "If branches"))
          | Some branch_t -> Ok (If ((branch_t, v), e1', e2', e3')))
      | Var (v, xname) -> (
          match VarCtx.find var_ctx xname with
          | Some t -> Ok (Var ((t, v), xname))
          | None -> Error (UndefinedVariable xname))
      | Let (v, xname, e1, e2) ->
          type_expr ctx e1 >>= fun e1' ->
          let t1 = e_type e1' in
          type_expr (type_ctx, VarCtx.add var_ctx xname t1) e2 >>= fun e2' ->
          let t2 = e_type e2' in
          Ok (Let ((t2, v), xname, e1', e2'))
      | App (v, e1, e2) -> (
          type_expr ctx e1 >>= fun e1' ->
          type_expr ctx e2 >>= fun e2' ->
          let t1 = e_type e1' in
          let t2 = e_type e2' in
          match t1 with
          | VTypeFun (t11, t12) ->
              TypeCtx.is_quotient_descendant type_ctx t11 t2
              >>= fun arg_type_valid ->
              if arg_type_valid then Ok (App ((t12, v), e1', e2'))
              else
                Error
                  (TypeMismatch (t11, t2, Some "App node function argument"))
          | _ -> Error (ExpectedFunctionOf t1))
      | Match (v, e, t_out, cs) ->
          type_expr ctx e >>= fun e' ->
          let t_in = e_type e' in
          (* Type the cases and check them against each other, as well as determining the type of the output *)
          Nonempty_list.fold_result_consume_init ~init:()
            ~f:(fun
                (acc :
                  ( unit,
                    ((vtype * 'tag_p) pattern
                    * (vtype * 'tag_e, vtype * 'tag_p) expr)
                    Nonempty_list.t )
                  Either.t)
                ((p : 'tag_p pattern), (c_e : ('tag_e, 'tag_p) expr))
              ->
              (* First, try type the pattern *)
              (* TODO - don't allow the case patterns to be PatName. Check that they are compound patterns *)
              type_pattern (type_ctx, VarCtx.empty) p
              >>= fun (typed_p, p_ctx) ->
              let p_t : vtype = pattern_node_val typed_p |> fst in
              (* Check the pattern's type *)
              TypeCtx.find_common_root_type type_ctx p_t t_in
              >>= fun pattern_input_common_type ->
              if Option.is_some pattern_input_common_type then
                let case_ctx = VarCtx.append var_ctx p_ctx in
                (* Then, type the case's expression using the extended context *)
                type_expr (type_ctx, case_ctx) c_e >>= fun c_e' ->
                let t_c_e = e_type c_e' in
                TypeCtx.is_quotient_descendant type_ctx t_out t_c_e
                >>= fun case_t_compatible ->
                if case_t_compatible then
                  match acc with
                  | First () -> Ok (Nonempty_list.singleton (typed_p, c_e'))
                  | Second cs_prev_rev ->
                      Ok (Nonempty_list.cons (typed_p, c_e') cs_prev_rev)
                else
                  Error
                    (TypeMismatch
                       (t_out, t_c_e, Some "Match case expression type"))
              else
                Error
                  (PatternTypeMismatch (pattern_to_plain_pattern p, t_in, p_t)))
            cs
          >>|
          fun (cs_typed_rev :
                ((vtype * 'tag_p) pattern
                * (vtype * 'tag_e, vtype * 'tag_p) expr)
                Nonempty_list.t)
          -> Match ((t_out, v), e', t_out, Nonempty_list.rev cs_typed_rev)
      | Constructor (v, c_name, e1) -> (
          type_expr ctx e1 >>= fun e1' ->
          let t1 = e_type e1' in
          match TypeCtx.find_variant_type_with_constructor type_ctx c_name with
          | None -> Error (UndefinedVariantTypeConstructor c_name)
          | Some ((vt_name, _), (_, c_t)) ->
              (* Check if the subexpressions's determined type is valid for the expected constructor's argument type.
                For non-quotient type cases, this is just an equality check *)
              TypeCtx.is_quotient_descendant type_ctx t1 c_t
              >>= fun subexpr_type_matches ->
              if subexpr_type_matches then
                Ok (Constructor ((VTypeCustom vt_name, v), c_name, e1'))
              else Error (TypeMismatch (c_t, t1, Some "Constructor argument")))

    type ('tag_e, 'tag_p) checking_type_ctx_acc = {
      types : StringSet.t;
      constructors : StringSet.t;
      type_defns_list : ('tag_e, 'tag_p) custom_type list;
    }

    let check_type_ctx (ctx_in : TypeCtx.t) :
        (checked_type_ctx, typing_error) Result.t =
      let open Result in
      let acc_to_checked_type_ctx (acc : ('tag_e, 'tag_p) checking_type_ctx_acc)
          : (checked_type_ctx, typing_error) Result.t =
        TypeCtx.create ~custom_types:acc.type_defns_list
      in
      List.fold_result (TypeCtx.type_defns_to_ordered_list ctx_in)
        ~init:
          {
            types = StringSet.empty;
            constructors = StringSet.empty;
            type_defns_list = [];
          } ~f:(fun acc td ->
          let td_name = custom_type_name td in
          if Set.mem acc.types td_name then
            Error (DuplicateTypeNameDefinition td_name)
          else
            let acc : ('tag_e, 'tag_p) checking_type_ctx_acc =
              { acc with types = Set.add acc.types td_name }
            in
            (match td with
            | VariantType (_, cs) ->
                (* For variant types, we also need to check the constructors *)
                List.fold_result cs ~init:acc ~f:(fun acc (c_name, _) ->
                    if Set.mem acc.constructors c_name then
                      Error (MultipleVariantTypeConstructorDefinitions c_name)
                    else
                      Ok
                        {
                          acc with
                          constructors = Set.add acc.constructors c_name;
                        })
            | QuotientType qt ->
                (* First, check that the base type is an existing type *)
                if Set.mem acc.types qt.base_type_name then Ok acc
                else Error (UndefinedTypeName qt.base_type_name))
            >>| fun acc ->
            { acc with type_defns_list = td :: acc.type_defns_list })
      >>= acc_to_checked_type_ctx

    let type_eqcons ~(type_ctx : TypeCtx.t) ~(quotient_type_name : string)
        (eqcons : ('tag_e, 'tag_p) quotient_type_eqcons) :
        (('tag_e, 'tag_p) typed_quotient_type_eqcons, typing_error) Result.t =
      let open Result in
      let quotient_type = VTypeCustom quotient_type_name in
      let body_pattern, body_expr = eqcons.body in
      (* Create a variable context from only the bindings *)
      List.fold_result ~init:VarCtx.empty
        ~f:(fun acc_var_ctx (xname, xtype) ->
          check_vtype type_ctx xtype >>| fun () ->
          VarCtx.add acc_var_ctx xname xtype)
        eqcons.bindings
      >>= fun bindings_var_ctx ->
      (* Type the pattern *)
      type_pattern (type_ctx, VarCtx.empty) body_pattern
      >>= fun (typed_pattern, pattern_var_ctx) ->
      (* Check that all the defined variables in the pattern come from the bindings  *)
      List.fold_result ~init:()
        ~f:(fun () (xname, xtype) ->
          match VarCtx.find bindings_var_ctx xname with
          | Some ctx_xtype when equal_vtype ctx_xtype xtype -> Ok ()
          | _ -> Error (EqconsVariableNotInBindings (xname, xtype)))
        (pattern_var_ctx |> VarCtx.to_list)
      >>= fun () ->
      (* Type the expression, with the bindings' variable context *)
      type_expr (type_ctx, bindings_var_ctx) body_expr >>= fun typed_body ->
      let pattern_t = typed_pattern |> pattern_node_val |> fst in
      let body_expr_t = typed_body |> expr_node_val |> fst in
      TypeCtx.is_quotient_descendant type_ctx quotient_type pattern_t
      >>= fun pattern_t_valid ->
      if pattern_t_valid then
        TypeCtx.find_common_root_type type_ctx quotient_type body_expr_t
        >>= fun body_expr_t_common_root ->
        if Option.is_some body_expr_t_common_root then
          Ok { eqcons with body = (typed_pattern, typed_body) }
        else
          Error
            (EqConsBodyExprTypeMismatch
               (expr_to_plain_expr body_expr, quotient_type, body_expr_t))
      else
        Error
          (EqConsBodyPatternTypeMismatch
             (pattern_to_plain_pattern body_pattern, quotient_type, pattern_t))

    let type_custom_types ~(type_ctx : TypeCtx.t)
        (custom_types : ('tag_e, 'tag_p) custom_type list) =
      let open Result in
      List.map
        ~f:(function
          | VariantType vt -> VariantType vt |> Ok
          | QuotientType qt ->
              List.map
                ~f:(type_eqcons ~type_ctx ~quotient_type_name:qt.name)
                qt.eqconss
              |> Result.all
              >>| fun eqconss ->
              { qt with eqconss } |> fun qt' -> QuotientType qt')
        custom_types
      |> Result.all

    type ('tag_e, 'tag_p) type_program_tlds_acc = {
      defns_rev : (vtype * 'tag_e, vtype * 'tag_p) top_level_defn list;
      defns_var_ctx : VarCtx.t;
    }

    let type_program (prog : ('tag_e, 'tag_p) program) :
        (('tag_e, 'tag_p) typed_program, typing_error) Result.t =
      let open Result in
      TypeCtx.create ~custom_types:prog.custom_types >>= fun type_ctx ->
      type_custom_types ~type_ctx prog.custom_types
      >>= fun custom_types_typed ->
      check_type_ctx type_ctx >>= fun type_ctx ->
      List.fold_result (* Check the top-level definitions *)
        ~init:{ defns_rev = []; defns_var_ctx = VarCtx.empty }
        ~f:(fun acc defn ->
          (* Check the definition's name doesn't already exist *)
          if VarCtx.exists acc.defns_var_ctx defn.name then
            Error (MultipleTopLevelNameDefinitions defn.name)
          else
            (* Type the TLD's body *)
            let body_var_ctx =
              (* Add the parameter to the variable context *)
              VarCtx.add acc.defns_var_ctx (fst defn.param) (snd defn.param)
              |>
              (* If recursive, add the function's name to the variable context *)
              if defn.recursive then fun acc ->
                VarCtx.add acc defn.name
                  (VTypeFun (snd defn.param, defn.return_t))
              else Fn.id
            in
            type_expr (type_ctx, body_var_ctx) defn.body >>= fun typed_body ->
            let typed_body_t = typed_body |> expr_node_val |> fst in
            TypeCtx.is_quotient_descendant type_ctx defn.return_t typed_body_t
            >>= fun return_t_valid ->
            if return_t_valid then
              let defn_t = VTypeFun (snd defn.param, defn.return_t) in
              Ok
                {
                  defns_rev = { defn with body = typed_body } :: acc.defns_rev;
                  defns_var_ctx = VarCtx.add acc.defns_var_ctx defn.name defn_t;
                }
            else
              Error
                (TypeMismatch
                   ( defn.return_t,
                     typed_body_t,
                     Some "Top-level defintion return type" )))
        prog.top_level_defns
      >>= fun tld_fold_final_acc ->
      let var_ctx = tld_fold_final_acc.defns_var_ctx in
      let tlds = tld_fold_final_acc.defns_rev |> List.rev in
      type_expr (type_ctx, var_ctx) prog.e >>| fun typed_e ->
      { custom_types = custom_types_typed; top_level_defns = tlds; e = typed_e }
  end

module SimpleTypeChecker =
  TypeChecker (SetTypingTypeContext) (ListTypingVarContext)

let type_expr ~(type_ctx : SetTypingTypeContext.t)
    (e : ('tag_e, 'tag_p) Ast.expr) :
    (('tag_e, 'tag_p) typed_expr, typing_error) Result.t =
  let open Result in
  SimpleTypeChecker.check_type_ctx type_ctx >>= fun type_ctx ->
  SimpleTypeChecker.type_expr (type_ctx, ListTypingVarContext.empty) e

let type_program (prog : ('tag_e, 'tag_p) program) :
    (('tag_e, 'tag_p) SimpleTypeChecker.typed_program, typing_error) Result.t =
  SimpleTypeChecker.type_program prog
