open Core
open Utils
open Variant_types
open Vtype
open Pattern
open Ast
open Quotient_types
open Custom_types

type typing_error =
  | UndefinedVariable of string
  | TypeMismatch of vtype * vtype
  | PatternTypeMismatch of plain_pattern * vtype * vtype
  | EqConsBodyTypeMismatch of quotient_type_eqcons * vtype * vtype
  | EqualOperatorTypeMistmatch of vtype * vtype
  | ExpectedFunctionOf of vtype
  | UndefinedVariantTypeConstructor of string
  | PatternMultipleVariableDefinitions of string
  | DuplicateTypeNameDefinition of string
  | UndefinedTypeName of string
  | MultipleVariantTypeConstructorDefinitions of string
[@@deriving sexp, equal]

let equal_typing_error_variant x y =
  match (x, y) with
  | UndefinedVariable _, UndefinedVariable _
  | TypeMismatch _, TypeMismatch _
  | PatternTypeMismatch _, PatternTypeMismatch _
  | EqConsBodyTypeMismatch _, EqConsBodyTypeMismatch _
  | EqualOperatorTypeMistmatch _, EqualOperatorTypeMistmatch _
  | ExpectedFunctionOf _, ExpectedFunctionOf _
  | UndefinedVariantTypeConstructor _, UndefinedVariantTypeConstructor _
  | PatternMultipleVariableDefinitions _, PatternMultipleVariableDefinitions _
  | DuplicateTypeNameDefinition _, DuplicateTypeNameDefinition _
  | UndefinedTypeName _, UndefinedTypeName _
  | ( MultipleVariantTypeConstructorDefinitions _,
      MultipleVariantTypeConstructorDefinitions _ ) ->
      true
  | UndefinedVariable _, _
  | TypeMismatch _, _
  | PatternTypeMismatch _, _
  | EqConsBodyTypeMismatch _, _
  | EqualOperatorTypeMistmatch _, _
  | ExpectedFunctionOf _, _
  | UndefinedVariantTypeConstructor _, _
  | PatternMultipleVariableDefinitions _, _
  | DuplicateTypeNameDefinition _, _
  | UndefinedTypeName _, _
  | MultipleVariantTypeConstructorDefinitions _, _ ->
      false

let print_typing_error = function
  | UndefinedVariable x -> "Undefined variable: " ^ x
  | TypeMismatch (t1, t2) ->
      sprintf "Type mismatch: expected %s but got %s" (vtype_to_source_code t1)
        (vtype_to_source_code t2)
  | PatternTypeMismatch (p, t1, t2) ->
      sprintf "Type mismatch in pattern \"%s\": expected %s but got %s"
        (pattern_to_source_code p) (vtype_to_source_code t1)
        (vtype_to_source_code t2)
  | EqConsBodyTypeMismatch (eqcons, t1, t2) ->
      sprintf
        "Type mismatch in equivalence constructor body for \"%s\": expected %s \
         but got %s"
        (quotient_type_eqcons_to_source_code eqcons)
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
  val create : custom_types:custom_type list -> (t, typing_error) Result.t
  val find_type_defn_by_name : t -> string -> custom_type option
  val type_defn_exists : t -> string -> bool

  val find_variant_type_with_constructor :
    t -> string -> (variant_type * variant_type_constructor) option

  val type_defns_to_list : t -> custom_type list
end

module SetTypingTypeContext : TypingTypeContext = struct
  type t = { custom_types : custom_type StringMap.t }

  let empty : t = { custom_types = StringMap.empty }

  let create ~(custom_types : custom_type list) : (t, typing_error) Result.t =
    let type_defns_map_or_err =
      custom_types
      |> StringMap.of_list_with_key ~get_key:(function
           | VariantType (vt_name, _) -> vt_name
           | QuotientType qt -> qt.name)
    in
    match type_defns_map_or_err with
    | `Duplicate_key dup_name -> Error (DuplicateTypeNameDefinition dup_name)
    | `Ok type_defns_map -> Ok { custom_types = type_defns_map }

  let find_type_defn_by_name (ctx : t) : string -> custom_type option =
    Map.find ctx.custom_types

  let type_defn_exists (ctx : t) (vt_name : string) : bool =
    Option.is_some (find_type_defn_by_name ctx vt_name)

  let find_variant_type_with_constructor (ctx : t) (c_name : string) :
      (variant_type * variant_type_constructor) option =
    Map.fold_until ctx.custom_types ~init:()
      ~f:(fun ~key:_ ~(data : custom_type) () ->
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

  let type_defns_to_list (ctx : t) : custom_type list =
    Map.data ctx.custom_types
end

module type TypingVarContext = sig
  type t

  val empty : t
  val add : t -> string -> vtype -> t
  val find : t -> string -> vtype option
  val singleton : string -> vtype -> t
  val append : t -> t -> t
  val exists : t -> string -> bool
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
end

module type TypeCheckerSig = functor
  (TypeCtx : TypingTypeContext)
  (VarCtx : TypingVarContext)
  -> sig
  type checked_type_ctx

  val checked_empty_type_ctx : checked_type_ctx

  type ('tag_e, 'tag_p) typed_program_expression

  val typed_program_expression_get_type_ctx :
    ('tag_e, 'tag_p) typed_program_expression -> TypeCtx.t

  val typed_program_expression_get_expression :
    ('tag_e, 'tag_p) typed_program_expression ->
    (vtype * 'tag_e, vtype * 'tag_p) Ast.expr

  val check_vtype : checked_type_ctx -> vtype -> (unit, typing_error) Result.t

  val type_pattern :
    checked_type_ctx * VarCtx.t ->
    'tag_p pattern ->
    ((vtype * 'tag_p) pattern * VarCtx.t, typing_error) Result.t

  val type_expr :
    checked_type_ctx * VarCtx.t ->
    ('tag_e, 'tag_p) Ast.expr ->
    (('tag_e, 'tag_p) typed_program_expression, typing_error) result

  val check_type_ctx : TypeCtx.t -> (checked_type_ctx, typing_error) Result.t
end

module TypeChecker : TypeCheckerSig =
functor
  (TypeCtx : TypingTypeContext)
  (VarCtx : TypingVarContext)
  ->
  struct
    type checked_type_ctx = TypeCtx.t

    let checked_empty_type_ctx = TypeCtx.empty

    type ('tag_e, 'tag_p) typed_program_expression =
      TypeCtx.t * (vtype * 'tag_e, vtype * 'tag_p) Ast.expr

    let typed_program_expression_get_type_ctx (ctx, _) = ctx
    let typed_program_expression_get_expression (_, e) = e

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
              if equal_vtype c_t p_t then
                Ok
                  ( PatConstructor ((VTypeCustom vt_name, v), c_name, p_typed),
                    var_ctx_from_p )
              else
                Error
                  (PatternTypeMismatch (pattern_to_plain_pattern p, c_t, p_t)))

    let type_expr :
        checked_type_ctx * VarCtx.t ->
        ('tag_e, 'tag_p) expr ->
        (('tag_e, 'tag_p) typed_program_expression, typing_error) Result.t =
      let open Result in
      let rec type_expr (((type_ctx : TypeCtx.t), (var_ctx : VarCtx.t)) as ctx)
          (orig_e : ('tag_e, 'tag_p) expr) =
        let e_type (e : (vtype * 'tag_e, vtype * 'tag_p) expr) : vtype =
          e |> expr_node_val |> fst
        in
        let be_of_type (exp : vtype) (e : (vtype * 'tag_e, vtype * 'tag_p) expr)
            : ((vtype * 'tag_e, vtype * 'tag_p) expr, typing_error) Result.t =
          if equal_vtype exp (e_type e) then Ok e
          else Error (TypeMismatch (exp, e_type e))
        in
        let type_binop
            (recomp :
              (vtype * 'tag_e, vtype * 'tag_p) expr ->
              (vtype * 'tag_e, vtype * 'tag_p) expr ->
              vtype ->
              (vtype * 'tag_e, vtype * 'tag_p) expr)
            (e1 : ('tag_e, 'tag_p) expr) (e2 : ('tag_e, 'tag_p) expr)
            (req_t : vtype) :
            ((vtype * 'tag_e, vtype * 'tag_p) expr, typing_error) Result.t =
          type_expr ctx e1 >>= fun e1' ->
          type_expr ctx e2 >>= fun e2' ->
          be_of_type req_t e1' >>= fun _ ->
          be_of_type req_t e2' >>= fun _ -> Ok (recomp e1' e2' (e_type e1'))
        in
        let type_unop
            (recomp :
              (vtype * 'tag_e, vtype * 'tag_p) expr ->
              vtype ->
              (vtype * 'tag_e, vtype * 'tag_p) expr)
            (e1 : ('tag_e, 'tag_p) expr) (req_t : vtype) :
            ((vtype * 'tag_e, vtype * 'tag_p) expr, typing_error) Result.t =
          type_expr ctx e1 >>= fun e1' ->
          be_of_type req_t e1' >>= fun _ -> Ok (recomp e1' (e_type e1'))
        in
        let type_int_compare
            (recomp :
              (vtype * 'tag_e, vtype * 'tag_p) expr ->
              (vtype * 'tag_e, vtype * 'tag_p) expr ->
              (vtype * 'tag_e, vtype * 'tag_p) expr)
            (e1 : ('tag_e, 'tag_p) expr) (e2 : ('tag_e, 'tag_p) expr) =
          type_expr ctx e1 >>= fun e1' ->
          type_expr ctx e2 >>= fun e2' ->
          let t1 = e_type e1' in
          let t2 = e_type e2' in
          match (t1, t2) with
          | VTypeInt, VTypeInt -> Ok (recomp e1' e2')
          | VTypeInt, _ -> Error (TypeMismatch (VTypeInt, t2))
          | _, _ -> Error (TypeMismatch (VTypeInt, t1))
        in
        match orig_e with
        | UnitLit v -> Ok (UnitLit (VTypeUnit, v))
        | IntLit (v, x) -> Ok (IntLit ((VTypeInt, v), x))
        | Add (v, e1, e2) ->
            type_binop (fun e1' e2' t -> Add ((t, v), e1', e2')) e1 e2 VTypeInt
        | Subtr (v, e1, e2) ->
            type_binop
              (fun e1' e2' t -> Subtr ((t, v), e1', e2'))
              e1 e2 VTypeInt
        | Mult (v, e1, e2) ->
            type_binop (fun e1' e2' t -> Mult ((t, v), e1', e2')) e1 e2 VTypeInt
        | Neg (v, e1) -> type_unop (fun e1' t -> Neg ((t, v), e1')) e1 VTypeInt
        | BoolLit (v, x) -> Ok (BoolLit ((VTypeBool, v), x))
        | BNot (v, e1) ->
            type_unop (fun e1' t -> BNot ((t, v), e1')) e1 VTypeBool
        | BOr (v, e1, e2) ->
            type_binop (fun e1' e2' t -> BOr ((t, v), e1', e2')) e1 e2 VTypeBool
        | BAnd (v, e1, e2) ->
            type_binop
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
            type_int_compare
              (fun e1' e2' -> Gt ((VTypeBool, v), e1', e2'))
              e1 e2
        | GtEq (v, e1, e2) ->
            type_int_compare
              (fun e1' e2' -> GtEq ((VTypeBool, v), e1', e2'))
              e1 e2
        | Lt (v, e1, e2) ->
            type_int_compare
              (fun e1' e2' -> Lt ((VTypeBool, v), e1', e2'))
              e1 e2
        | LtEq (v, e1, e2) ->
            type_int_compare
              (fun e1' e2' -> LtEq ((VTypeBool, v), e1', e2'))
              e1 e2
        | If (v, e1, e2, e3) ->
            type_expr ctx e1 >>= fun e1' ->
            be_of_type VTypeBool e1' >>= fun _ ->
            type_expr ctx e2 >>= fun e2' ->
            type_expr ctx e3 >>= fun e3' ->
            let t2 = e_type e2' in
            be_of_type t2 e3' >>= fun _ -> Ok (If ((t2, v), e1', e2', e3'))
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
        | Fun (v, (xname, xtype), e') ->
            check_vtype type_ctx xtype >>= fun () ->
            type_expr (type_ctx, VarCtx.add var_ctx xname xtype) e'
            >>= fun e' ->
            let t = e_type e' in
            Ok (Fun ((VTypeFun (xtype, t), v), (xname, xtype), e'))
        | App (v, e1, e2) -> (
            type_expr ctx e1 >>= fun e1' ->
            type_expr ctx e2 >>= fun e2' ->
            let t1 = e_type e1' in
            let t2 = e_type e2' in
            match t1 with
            | VTypeFun (t11, t12) ->
                if equal_vtype t11 t2 then Ok (App ((t12, v), e1', e2'))
                else Error (TypeMismatch (t11, t2))
            | _ -> Error (ExpectedFunctionOf t1))
        | Fix
            (v, ((fname, ftype1, ftype2) as fvals), ((xname, xtype) as xvals), e)
          ->
            check_vtype type_ctx ftype1 >>= fun () ->
            check_vtype type_ctx ftype2 >>= fun () ->
            check_vtype type_ctx xtype >>= fun () ->
            let ftype = VTypeFun (ftype1, ftype2) in
            if equal_vtype ftype1 xtype then
              type_expr
                ( type_ctx,
                  VarCtx.add (VarCtx.add var_ctx fname ftype) xname xtype )
                e
              >>= fun e' ->
              be_of_type ftype2 e' >>= fun _ ->
              Ok (Fix ((ftype, v), fvals, xvals, e'))
            else Error (TypeMismatch (ftype1, xtype))
        | Match (v, e, cs) ->
            type_expr ctx e >>= fun e' ->
            let t_in = e_type e' in
            (* Type the cases and check them against each other, as well as determining the type of the output *)
            Nonempty_list.fold_result_consume_init ~init:()
              ~f:(fun
                  (acc :
                    ( unit,
                      vtype
                      * ((vtype * 'tag_p) pattern
                        * (vtype * 'tag_e, vtype * 'tag_p) expr)
                        Nonempty_list.t )
                    Either.t)
                  ((p : 'tag_p pattern), (c_e : ('tag_e, 'tag_p) expr))
                ->
                (* First, try type the pattern *)
                type_pattern (type_ctx, VarCtx.empty) p
                >>= fun (typed_p, p_ctx) ->
                let p_t : vtype = pattern_node_val typed_p |> fst in
                (* Check the pattern's type *)
                if equal_vtype t_in p_t then
                  let case_ctx = VarCtx.append var_ctx p_ctx in
                  (* Then, type the case's expression using the extended context *)
                  type_expr (type_ctx, case_ctx) c_e >>= fun c_e' ->
                  let t_c_e = e_type c_e' in
                  match acc with
                  | First _ ->
                      (* If this is the first case, use this as the output type *)
                      Ok (t_c_e, Nonempty_list.singleton (typed_p, c_e'))
                  | Second (t_out, cs_prev_rev) ->
                      (* If this isn't the first case, check the case's expression's type *)
                      if equal_vtype t_out t_c_e then
                        Ok
                          (t_out, Nonempty_list.cons (typed_p, c_e') cs_prev_rev)
                      else Error (TypeMismatch (t_out, t_c_e))
                else
                  Error
                    (PatternTypeMismatch (pattern_to_plain_pattern p, t_in, p_t)))
              cs
            >>|
            fun ( (t_out : vtype),
                  (cs_typed_rev :
                    ((vtype * 'tag_p) pattern
                    * (vtype * 'tag_e, vtype * 'tag_p) expr)
                    Nonempty_list.t) )
            -> Match ((t_out, v), e', Nonempty_list.rev cs_typed_rev)
        | Constructor (v, c_name, e1) -> (
            type_expr ctx e1 >>= fun e1' ->
            let t1 = e_type e1' in
            match
              TypeCtx.find_variant_type_with_constructor type_ctx c_name
            with
            | None -> Error (UndefinedVariantTypeConstructor c_name)
            | Some ((vt_name, _), (_, c_t)) ->
                if equal_vtype c_t t1 then
                  Ok (Constructor ((VTypeCustom vt_name, v), c_name, e1'))
                else Error (TypeMismatch (c_t, t1)))
      in
      fun ((type_ctx, _) as ctx) orig_e ->
        type_expr ctx orig_e >>= fun orig_e' -> Ok (type_ctx, orig_e')

    type checking_type_ctx_acc = {
      types : StringSet.t;
      constructors : StringSet.t;
      type_defns_list : custom_type list;
    }

    let check_eqcons (type_ctx : checked_type_ctx)
        (eqcons : quotient_type_eqcons) : (unit, typing_error) Result.t =
      let open Result in
      let body_pattern, body_expr = eqcons.body in
      List.fold_result ~init:VarCtx.empty
        ~f:(fun acc_var_ctx (xname, xtype) ->
          check_vtype type_ctx xtype >>| fun () ->
          VarCtx.add acc_var_ctx xname xtype)
        eqcons.bindings
      >>= fun var_ctx ->
      type_pattern (type_ctx, var_ctx) body_pattern
      >>= fun (typed_pattern, _) ->
      let pattern_t = pattern_node_val typed_pattern |> fst in
      type_expr (type_ctx, var_ctx) body_expr
      >>= fun (body_expr_tpe : (unit, unit) typed_program_expression) ->
      let body_expr_t : vtype =
        typed_program_expression_get_expression body_expr_tpe
        |> expr_node_val |> fst
      in
      if equal_vtype pattern_t body_expr_t then Ok ()
      else Error (EqConsBodyTypeMismatch (eqcons, pattern_t, body_expr_t))

    let check_type_ctx (ctx_in : TypeCtx.t) :
        (checked_type_ctx, typing_error) Result.t =
      let open Result in
      let acc_to_checked_type_ctx (acc : checking_type_ctx_acc) :
          (checked_type_ctx, typing_error) Result.t =
        TypeCtx.create ~custom_types:acc.type_defns_list
      in
      List.fold_result (TypeCtx.type_defns_to_list ctx_in)
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
            let acc : checking_type_ctx_acc =
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
                if Set.mem acc.types qt.base_type_name |> not then
                  Error (UndefinedTypeName qt.base_type_name)
                else
                  (* TODO - check that patterns on LHS of eqcons type as the quotient type's base variant type *)
                  List.fold_result ~init:()
                    ~f:(fun () eqcons ->
                      acc_to_checked_type_ctx acc >>= fun checked_type_ctx ->
                      check_eqcons checked_type_ctx eqcons)
                    qt.eqconss
                  >>| fun () -> acc)
            >>| fun acc ->
            { acc with type_defns_list = td :: acc.type_defns_list })
      >>= acc_to_checked_type_ctx
  end

module SimpleTypeChecker =
  TypeChecker (SetTypingTypeContext) (ListTypingVarContext)

let type_expr ~(type_ctx : SetTypingTypeContext.t)
    (e : ('tag_e, 'tag_p) Ast.expr) :
    ( ('tag_e, 'tag_p) SimpleTypeChecker.typed_program_expression,
      typing_error )
    result =
  let open Result in
  SimpleTypeChecker.check_type_ctx type_ctx >>= fun type_ctx ->
  SimpleTypeChecker.type_expr (type_ctx, ListTypingVarContext.empty) e
