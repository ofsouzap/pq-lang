open Core
open Utils

module TypingError : sig
  module type S = sig
    module Pattern : Pattern.S
    module Expr : Expr.S

    type err =
      | UndefinedVariable of string
          (** A variable was referenced that isn't defined in the scope *)
      | EqconsVariableNotInBindings of string * Vtype.t
          (** A variable-type pair was used that wasn't defined in an eqcons
              binding *)
      | TypeMismatch of Vtype.t * Vtype.t * string option
          (** An expression was expected to have the first type but had the
              second *)
      | NoCommonRootType of Vtype.t * Vtype.t
          (** The types given were expected to have a common root type but
              didn't *)
      | PatternTypeMismatch of Pattern.plain_t * Vtype.t * Vtype.t
          (** A pattern was expected to have the first type but had the second
          *)
      | EqConsBodyPatternTypeMismatch of Pattern.plain_t * Vtype.t * Vtype.t
          (** The pattern of an equivalence constructor body was expected to
              have the first type but had the second *)
      | EqConsBodyExprTypeMismatch of Expr.plain_t * Vtype.t * Vtype.t
          (** The expression of an equivalence constructor body was expected to
              have the first type but had the second *)
      | EqualOperatorTypeMistmatch of Vtype.t * Vtype.t
          (** An application of the equality operation had a type mismatch as
              the operands had the specified types instead of compatible ones *)
      | ExpectedFunctionOf of Vtype.t
          (** A value is used as a function but isn't a function. The expected
              input type of the function is the value *)
      | UndefinedVariantTypeConstructor of string
          (** The specified type constructor was used but hasn't been defined *)
      | PatternMultipleVariableDefinitions of string
          (** In a pattern, there are multiple definitions of some variable name
          *)
      | MultipleTopLevelNameDefinitions of string
          (** There are multiple definitions of the same top-level name *)
      | DuplicateTypeNameDefinition of string
          (** The specified type name has been defined multiple times *)
      | UndefinedTypeName of string
          (** The specified type name has been referenced but not defined *)
      | MultipleVariantTypeConstructorDefinitions of string
          (** The specified variant type constructor name has been defined
              multiple times *)
    [@@deriving sexp, equal]

    type t = Frontend.source_position option * err

    val equal_variant : t -> t -> bool
    val print : t -> string
  end

  module Make (Pattern : Pattern.S) (Expr : Expr.S) :
    S with module Pattern = Pattern and module Expr = Expr

  module StdTypingError :
    S with module Pattern = Pattern.StdPattern and module Expr = Expr.StdExpr
end = struct
  module type S = sig
    module Pattern : Pattern.S
    module Expr : Expr.S

    type err =
      | UndefinedVariable of string
          (** A variable was referenced that isn't defined in the scope *)
      | EqconsVariableNotInBindings of string * Vtype.t
          (** A variable-type pair was used that wasn't defined in an eqcons
              binding *)
      | TypeMismatch of Vtype.t * Vtype.t * string option
          (** An expression was expected to have the first type but had the
              second *)
      | NoCommonRootType of Vtype.t * Vtype.t
          (** The types given were expected to have a common root type but
              didn't *)
      | PatternTypeMismatch of Pattern.plain_t * Vtype.t * Vtype.t
          (** A pattern was expected to have the first type but had the second
          *)
      | EqConsBodyPatternTypeMismatch of Pattern.plain_t * Vtype.t * Vtype.t
          (** The pattern of an equivalence constructor body was expected to
              have the first type but had the second *)
      | EqConsBodyExprTypeMismatch of Expr.plain_t * Vtype.t * Vtype.t
          (** The expression of an equivalence constructor body was expected to
              have the first type but had the second *)
      | EqualOperatorTypeMistmatch of Vtype.t * Vtype.t
          (** An application of the equality operation had a type mismatch as
              the operands had the specified types instead of compatible ones *)
      | ExpectedFunctionOf of Vtype.t
          (** A value is used as a function but isn't a function. The expected
              input type of the function is the value *)
      | UndefinedVariantTypeConstructor of string
          (** The specified type constructor was used but hasn't been defined *)
      | PatternMultipleVariableDefinitions of string
          (** In a pattern, there are multiple definitions of some variable name
          *)
      | MultipleTopLevelNameDefinitions of string
          (** There are multiple definitions of the same top-level name *)
      | DuplicateTypeNameDefinition of string
          (** The specified type name has been defined multiple times *)
      | UndefinedTypeName of string
          (** The specified type name has been referenced but not defined *)
      | MultipleVariantTypeConstructorDefinitions of string
          (** The specified variant type constructor name has been defined
              multiple times *)
    [@@deriving sexp, equal]

    type t = Frontend.source_position option * err

    val equal_variant : t -> t -> bool
    val print : t -> string
  end

  module Make (Pattern : Pattern.S) (Expr : Expr.S) :
    S with module Pattern = Pattern and module Expr = Expr = struct
    module Pattern = Pattern
    module Expr = Expr

    type err =
      | UndefinedVariable of string
          (** A variable was referenced that isn't defined in the scope *)
      | EqconsVariableNotInBindings of string * Vtype.t
          (** A variable-type pair was used that wasn't defined in an eqcons
              binding *)
      | TypeMismatch of Vtype.t * Vtype.t * string option
          (** An expression was expected to have the first type but had the
              second *)
      | NoCommonRootType of Vtype.t * Vtype.t
          (** The types given were expected to have a common root type but
              didn't *)
      | PatternTypeMismatch of Pattern.plain_t * Vtype.t * Vtype.t
          (** A pattern was expected to have the first type but had the second
          *)
      | EqConsBodyPatternTypeMismatch of Pattern.plain_t * Vtype.t * Vtype.t
          (** The pattern of an equivalence constructor body was expected to
              have the first type but had the second *)
      | EqConsBodyExprTypeMismatch of Expr.plain_t * Vtype.t * Vtype.t
          (** The expression of an equivalence constructor body was expected to
              have the first type but had the second *)
      | EqualOperatorTypeMistmatch of Vtype.t * Vtype.t
          (** An application of the equality operation had a type mismatch as
              the operands had the specified types instead of compatible ones *)
      | ExpectedFunctionOf of Vtype.t
          (** A value is used as a function but isn't a function. The expected
              input type of the function is the value *)
      | UndefinedVariantTypeConstructor of string
          (** The specified type constructor was used but hasn't been defined *)
      | PatternMultipleVariableDefinitions of string
          (** In a pattern, there are multiple definitions of some variable name
          *)
      | MultipleTopLevelNameDefinitions of string
          (** There are multiple definitions of the same top-level name *)
      | DuplicateTypeNameDefinition of string
          (** The specified type name has been defined multiple times *)
      | UndefinedTypeName of string
          (** The specified type name has been referenced but not defined *)
      | MultipleVariantTypeConstructorDefinitions of string
          (** The specified variant type constructor name has been defined
              multiple times *)
    [@@deriving sexp, equal]

    type t = Frontend.source_position option * err

    let equal_variant (_, x) (_, y) =
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
      | ( PatternMultipleVariableDefinitions _,
          PatternMultipleVariableDefinitions _ )
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

    let print (_, err) =
      match err with
      | UndefinedVariable x -> "Undefined variable: " ^ x
      | EqconsVariableNotInBindings (xname, xtype) ->
          sprintf
            "Variable %s of type %s is not in the bindings of an eqcons but is \
             used"
            xname
            (Vtype.to_source_code xtype)
      | TypeMismatch (t1, t2, msg) ->
          sprintf "Type mismatch: expected %s but got %s%s"
            (Vtype.to_source_code t1) (Vtype.to_source_code t2)
            (match msg with None -> "" | Some msg -> sprintf " (%s)" msg)
      | NoCommonRootType (t1, t2) ->
          sprintf "Expected common root type for %s and %s but none found"
            (Vtype.to_source_code t1) (Vtype.to_source_code t2)
      | PatternTypeMismatch (p, t1, t2) ->
          sprintf "Type mismatch in pattern \"%s\": expected %s but got %s"
            (Pattern.to_source_code p) (Vtype.to_source_code t1)
            (Vtype.to_source_code t2)
      | EqConsBodyPatternTypeMismatch (pattern, t1, t2) ->
          sprintf
            "Type mismatch in equivalence constructor body pattern for \"%s\": \
             expected %s but got %s"
            (Pattern.to_source_code pattern)
            (Vtype.to_source_code t1) (Vtype.to_source_code t2)
      | EqConsBodyExprTypeMismatch (expr, t1, t2) ->
          sprintf
            "Type mismatch in equivalence constructor body pattern for \"%s\": \
             expected %s but got %s"
            (Expr.to_source_code ~use_newlines:false expr)
            (Vtype.to_source_code t1) (Vtype.to_source_code t2)
      | EqualOperatorTypeMistmatch (t1, t2) ->
          sprintf "Trying to apply equality operator to %s and %s"
            (Vtype.to_source_code t1) (Vtype.to_source_code t2)
      | ExpectedFunctionOf t ->
          "Expected a function taking input of " ^ Vtype.to_source_code t
      | UndefinedVariantTypeConstructor c_name ->
          sprintf "Undefined variant type constructor: %s" c_name
      | PatternMultipleVariableDefinitions xname ->
          sprintf "Variable named \"%s\" has been defined twice in a pattern"
            xname
      | MultipleTopLevelNameDefinitions xname ->
          sprintf "Multiple top-level definitions of name \"%s\"" xname
      | DuplicateTypeNameDefinition vt_name ->
          sprintf "Variant type named \"%s\" has been defined multiple times"
            vt_name
      | UndefinedTypeName vt_name ->
          sprintf "Undefined variant type: %s" vt_name
      | MultipleVariantTypeConstructorDefinitions c_name ->
          sprintf
            "Variant type constructor named \"%s\" has been defined multiple \
             times"
            c_name
  end

  module StdTypingError :
    S with module Pattern = Pattern.StdPattern and module Expr = Expr.StdExpr =
    Make (Pattern.StdPattern) (Expr.StdExpr)
end

(** Typing context for types (e.g. if a type of a certain name exists) *)
module TypeContext : sig
  module type S = sig
    module CustomType : CustomType.S

    module TypingError :
      TypingError.S
        with module Pattern = CustomType.QuotientType.Pattern
         and module Expr = CustomType.QuotientType.Expr

    type t

    (** Creates an empty typing context *)
    val empty : t

    (** Creates a typing context using the provided values *)
    val create :
      custom_types:('tag_e, 'tag_p) CustomType.t list ->
      (t, TypingError.t) Result.t

    (** Looks up a type definition, by name, in the context *)
    val find_type_defn_by_name : t -> string -> CustomType.plain_t option

    (** Check whether a type definition exists in the context, by name *)
    val type_defn_exists : t -> string -> bool

    (** Find a variant type in the type context with a variant of the specified
        name. If multiple exist, only one is returned *)
    val find_variant_type_with_constructor :
      t -> string -> (VariantType.t * VariantType.constructor) option

    (** Get a list of the variant types defined in the context *)
    val type_defns_to_ordered_list : t -> CustomType.plain_t list

    (** Check if one type is a subtype of another *)
    val subtype :
      ?source_position:Frontend.source_position ->
      t ->
      Vtype.t ->
      Vtype.t ->
      (bool, TypingError.t) Result.t
  end

  module MakeSet
      (CustomType : CustomType.S)
      (TypingError :
        TypingError.S
          with module Pattern = CustomType.QuotientType.Pattern
           and module Expr = CustomType.QuotientType.Expr) :
    S with module CustomType = CustomType and module TypingError = TypingError

  (** Typing context of types using a simple set-based approach *)
  module StdSetTypeContext :
    S
      with module CustomType = CustomType.StdCustomType
       and module TypingError = TypingError.StdTypingError
end = struct
  module type S = sig
    module CustomType : CustomType.S

    module TypingError :
      TypingError.S
        with module Pattern = CustomType.QuotientType.Pattern
         and module Expr = CustomType.QuotientType.Expr

    type t

    (** Creates an empty typing context *)
    val empty : t

    (** Creates a typing context using the provided values *)
    val create :
      custom_types:('tag_e, 'tag_p) CustomType.t list ->
      (t, TypingError.t) Result.t

    (** Looks up a type definition, by name, in the context *)
    val find_type_defn_by_name : t -> string -> CustomType.plain_t option

    (** Check whether a type definition exists in the context, by name *)
    val type_defn_exists : t -> string -> bool

    (** Find a variant type in the type context with a variant of the specified
        name. If multiple exist, only one is returned *)
    val find_variant_type_with_constructor :
      t -> string -> (VariantType.t * VariantType.constructor) option

    (** Get a list of the variant types defined in the context *)
    val type_defns_to_ordered_list : t -> CustomType.plain_t list

    (** Check if one type is a subtype of another *)
    val subtype :
      ?source_position:Frontend.source_position ->
      t ->
      Vtype.t ->
      Vtype.t ->
      (bool, TypingError.t) Result.t
  end

  module MakeSet
      (CustomType : CustomType.S)
      (TypingError :
        TypingError.S
          with module Pattern = CustomType.QuotientType.Pattern
           and module Expr = CustomType.QuotientType.Expr) :
    S with module CustomType = CustomType and module TypingError = TypingError =
  struct
    module CustomType = CustomType
    module TypingError = TypingError

    type t = {
      custom_types : CustomType.plain_t StringMap.t;
      custom_types_order : string list;
    }

    let empty : t = { custom_types = StringMap.empty; custom_types_order = [] }

    let create ~(custom_types : ('tag_e, 'tag_p) CustomType.t list) :
        (t, TypingError.t) Result.t =
      let type_defns_map_or_err =
        custom_types
        |> List.map ~f:CustomType.to_plain_t
        |> StringMap.of_list_with_key ~get_key:(function
             | VariantType (vt_name, _) -> vt_name
             | CustomType.QuotientType qt -> qt.name)
      in
      match type_defns_map_or_err with
      | `Duplicate_key dup_name ->
          Error (None, DuplicateTypeNameDefinition dup_name)
      | `Ok type_defns_map ->
          Ok
            {
              custom_types = type_defns_map;
              custom_types_order =
                List.map ~f:(fun ct -> CustomType.name ct) custom_types;
            }

    let find_type_defn_by_name (ctx : t) :
        string -> ('tag_e, 'tag_p) CustomType.t option =
      Map.find ctx.custom_types

    let type_defn_exists (ctx : t) (vt_name : string) : bool =
      Option.is_some (find_type_defn_by_name ctx vt_name)

    let find_variant_type_with_constructor (ctx : t) (c_name : string) :
        (VariantType.t * VariantType.constructor) option =
      Map.fold_until ctx.custom_types ~init:()
        ~f:(fun ~key:_ ~(data : ('tag_e, 'tag_p) CustomType.t) () ->
          match data with
          | VariantType ((_, cs) as vt) -> (
              let search_res =
                List.find cs ~f:(fun (xc_name, _) ->
                    equal_string c_name xc_name)
              in
              match search_res with
              | None -> Continue ()
              | Some c -> Stop (Some (vt, c)))
          | CustomType.QuotientType _ -> Continue ())
        ~finish:(fun () -> None)

    let type_defns_to_ordered_list (ctx : t) :
        ('tag_e, 'tag_p) CustomType.t list =
      List.map
        ~f:(fun ct_name ->
          find_type_defn_by_name ctx ct_name |> Option.value_exn)
        ctx.custom_types_order

    let rec subtype ?(source_position : Frontend.source_position option)
        (ctx : t) (t1 : Vtype.t) (t2 : Vtype.t) : (bool, TypingError.t) Result.t
        =
      let open Result in
      match (t1, t2) with
      | VTypeInt, VTypeInt | VTypeBool, VTypeBool | VTypeUnit, VTypeUnit ->
          Ok true
      | VTypeInt, _ | VTypeBool, _ | VTypeUnit, _ -> Ok false
      | VTypePair (t1a, t1b), VTypePair (t2a, t2b) ->
          subtype ctx t1a t2a >>= fun t1a_subtype ->
          subtype ctx t1b t2b >>= fun t1b_subtype ->
          Ok (t1a_subtype && t1b_subtype)
      | VTypePair _, _ -> Ok false
      | VTypeFun (t1a, t1b), VTypeFun (t2a, t2b) ->
          subtype ctx t2a t1a >>= fun t2a_subtype ->
          subtype ctx t1b t2b >>= fun t1b_subtype ->
          Ok (t2a_subtype && t1b_subtype)
      | VTypeFun _, _ -> Ok false
      | VTypeCustom c1_name, VTypeCustom c2_name -> (
          find_type_defn_by_name ctx c1_name
          |> Result.of_option
               ~error:(source_position, TypingError.UndefinedTypeName c1_name)
          >>= fun ct1 ->
          find_type_defn_by_name ctx c2_name
          |> Result.of_option
               ~error:(source_position, TypingError.UndefinedTypeName c2_name)
          >>= fun ct2 ->
          match (ct1, ct2) with
          | VariantType (vt1_name, _), VariantType (vt2_name, _) ->
              Ok (equal_string vt1_name vt2_name)
          | VariantType _, QuotientType qt2 ->
              subtype ctx t1 (VTypeCustom qt2.base_type_name)
          | CustomType.QuotientType qt1, QuotientType qt2 ->
              if equal_string qt1.name qt2.name then Ok true
              else subtype ctx t1 (VTypeCustom qt2.base_type_name)
          | CustomType.QuotientType _, VariantType _ -> Ok false)
      | VTypeCustom _, _ -> Ok false
  end

  (** Typing context of types using a simple set-based approach *)

  module StdSetTypeContext :
    S
      with module CustomType = CustomType.StdCustomType
       and module TypingError = TypingError.StdTypingError =
    MakeSet (CustomType.StdCustomType) (TypingError.StdTypingError)
end

(** Typing contexts of variables *)
module VarContext : sig
  module type S = sig
    type t

    (** Creates an empty typing context *)
    val empty : t

    (** Adds a new variable with its type to the context, overwriting any
        existing values *)
    val add : t -> string -> Vtype.t -> t

    (** Looks up a variable's type in the context *)
    val find : t -> string -> Vtype.t option

    (** Create a context with a single entry *)
    val singleton : string -> Vtype.t -> t

    (** Appends two variable contexts. When overwriting is necessary, the second
        argument overwrites the first *)
    val append : t -> t -> t

    (** Check whether a variable exists in the context, by name *)
    val exists : t -> string -> bool

    (** Convert the context to a list of variable-type pairs *)
    val to_list : t -> (string * Vtype.t) list
  end

  (** Typing context of variables using a simple list-based approach *)
  module ListTypingVarContext : S
end = struct
  module type S = sig
    type t

    (** Creates an empty typing context *)
    val empty : t

    (** Adds a new variable with its type to the context, overwriting any
        existing values *)
    val add : t -> string -> Vtype.t -> t

    (** Looks up a variable's type in the context *)
    val find : t -> string -> Vtype.t option

    (** Create a context with a single entry *)
    val singleton : string -> Vtype.t -> t

    (** Appends two variable contexts. When overwriting is necessary, the second
        argument overwrites the first *)
    val append : t -> t -> t

    (** Check whether a variable exists in the context, by name *)
    val exists : t -> string -> bool

    (** Convert the context to a list of variable-type pairs *)
    val to_list : t -> (string * Vtype.t) list
  end

  (** Typing context of variables using a simple list-based approach *)
  module ListTypingVarContext : S = struct
    type t = (string * Vtype.t) list

    let empty = []
    let add ctx x t = List.Assoc.add ctx x t ~equal:String.equal
    let find ctx x = List.Assoc.find ctx x ~equal:String.equal
    let singleton = add empty

    let append ctx1 =
      List.fold ~init:ctx1 ~f:(fun ctx_acc (x, t) -> add ctx_acc x t)

    let exists ctx x = match find ctx x with None -> false | Some _ -> true
    let to_list = Fn.id
  end
end

module type S = sig
  module Pattern : Pattern.S
  module Expr : Expr.S with module Pattern = Pattern

  module Program :
    Program.S with module Pattern = Pattern and module Expr = Expr

  module TypingError :
    TypingError.S with module Pattern = Pattern and module Expr = Expr

  module TypeCtx : TypeContext.S with module CustomType = Program.CustomType
  module VarCtx : VarContext.S

  (** The type of a type context that has been checked to be valid *)
  type checked_type_ctx

  (** A checked version of the empty type context *)
  val checked_empty_type_ctx : checked_type_ctx

  (** The type of a program that has passed type checking *)
  type ('tag_e, 'tag_p) typed_program

  (** Get the expression from a typed program expression *)
  val typed_program_get_program :
    ('tag_e, 'tag_p) typed_program ->
    (Vtype.t * 'tag_e, Vtype.t * 'tag_p) Program.t

  (** Get the checked type context from a typed program expression *)
  val typed_program_get_type_ctx_checked :
    ('tag_e, 'tag_p) typed_program -> checked_type_ctx

  (** Get the plain type context from a typed program expression *)
  val typed_program_get_type_ctx : ('tag_e, 'tag_p) typed_program -> TypeCtx.t

  (** Check that a Vtype.t is valid in the given context *)
  val check_vtype :
    source_position:Frontend.source_position option ->
    checked_type_ctx ->
    Vtype.t ->
    (unit, TypingError.t) Result.t

  (** Type checks a pattern in the given context, returning either the pattern's
      type and declared variables, or a pattern typing error *)
  val type_pattern :
    get_source_position:('tag_p -> Frontend.source_position option) ->
    checked_type_ctx * VarCtx.t ->
    'tag_p Pattern.t ->
    ((Vtype.t * 'tag_p) Pattern.t * VarCtx.t, TypingError.t) Result.t

  (** Type checks a single expression in the given context *)
  val type_expr :
    get_source_position:
      (('tag_e, 'tag_p) Either.t -> Frontend.source_position option) ->
    checked_type_ctx * VarCtx.t ->
    ('tag_e, 'tag_p) Expr.t ->
    (('tag_e, 'tag_p) Expr.typed_t, TypingError.t) Result.t

  (** Check a type context is valid *)
  val check_type_ctx : TypeCtx.t -> (checked_type_ctx, TypingError.t) Result.t

  (** Type checks a program in the given context, returning either a typed
      program or a typing error *)
  val type_program :
    get_source_position:
      (('tag_e, 'tag_p) Either.t -> Frontend.source_position option) ->
    ('tag_e, 'tag_p) Program.t ->
    (('tag_e, 'tag_p) typed_program, TypingError.t) Result.t
end

(** Functor for creating modules providing type-checking functionality *)
module MakeStd
    (TypeCtx :
      TypeContext.S
        with module CustomType = CustomType.StdCustomType
         and module TypingError = TypingError.StdTypingError)
    (VarCtx : VarContext.S) :
  S
    with module Pattern = Pattern.StdPattern
     and module Expr = Expr.StdExpr
     and module Program = Program.StdProgram
     and module TypingError = TypingError.StdTypingError
     and module TypeCtx = TypeCtx
     and module VarCtx = VarCtx = struct
  (* Required for referring to the std_pattern constructors *)
  open Pattern
  module Pattern = Pattern.StdPattern
  module Expr = Expr.StdExpr
  module QuotientType = QuotientType.StdQuotientType
  module CustomType = CustomType.StdCustomType
  module Program = Program.StdProgram
  module TypingError = TypingError.StdTypingError
  module TypeCtx = TypeCtx
  module VarCtx = VarCtx

  type checked_type_ctx = TypeCtx.t

  let checked_empty_type_ctx = TypeCtx.empty

  type ('tag_e, 'tag_p) typed_program =
    (Vtype.t * 'tag_e, Vtype.t * 'tag_p) Program.t * checked_type_ctx

  let typed_program_get_program :
      ('tag_e, 'tag_p) typed_program ->
      (Vtype.t * 'tag_e, Vtype.t * 'tag_p) Program.t =
    fst

  let typed_program_get_type_ctx_checked :
      ('tag_e, 'tag_p) typed_program -> TypeCtx.t =
    snd

  let typed_program_get_type_ctx : ('tag_e, 'tag_p) typed_program -> TypeCtx.t =
    snd

  let rec check_vtype ~(source_position : Frontend.source_position option)
      (ctx : checked_type_ctx) : Vtype.t -> (unit, TypingError.t) Result.t =
    let open Result in
    function
    | VTypeInt | VTypeBool | VTypeUnit -> Ok ()
    | VTypePair (t1, t2) | VTypeFun (t1, t2) ->
        check_vtype ~source_position ctx t1 >>= fun () ->
        check_vtype ~source_position ctx t2
    | VTypeCustom vt_name ->
        if TypeCtx.find_type_defn_by_name ctx vt_name |> Option.is_some then
          Ok ()
        else (source_position, TypingError.UndefinedTypeName vt_name) |> Error

  let rec type_pattern
      ~(get_source_position : 'tag_p -> Frontend.source_position option)
      (((type_ctx : checked_type_ctx), (var_ctx : VarCtx.t)) as ctx)
      (orig_p : 'tag_p Pattern.t) :
      ((Vtype.t * 'tag_p) Pattern.t * VarCtx.t, TypingError.t) Result.t =
    let open Result in
    let ( <: ) = TypeCtx.subtype type_ctx in
    match orig_p with
    | PatName (v, x_name, x_t) ->
        check_vtype ~source_position:(get_source_position v) type_ctx x_t
        >>= fun () ->
        if VarCtx.exists var_ctx x_name then
          Error
            (get_source_position v, PatternMultipleVariableDefinitions x_name)
        else Ok (PatName ((x_t, v), x_name, x_t), VarCtx.add var_ctx x_name x_t)
    | PatPair (v, p1, p2) ->
        type_pattern ~get_source_position ctx p1
        >>= fun (p1_typed, var_ctx_from_p1) ->
        type_pattern ~get_source_position (type_ctx, var_ctx_from_p1) p2
        >>= fun (p2_typed, var_ctx_final) ->
        let p1_t = Pattern.node_val p1_typed |> fst in
        let p2_t = Pattern.node_val p2_typed |> fst in
        Ok
          ( PatPair ((Vtype.VTypePair (p1_t, p2_t), v), p1_typed, p2_typed),
            var_ctx_final )
    | PatConstructor (v, c_name, p) -> (
        match TypeCtx.find_variant_type_with_constructor type_ctx c_name with
        | None ->
            Error (get_source_position v, UndefinedVariantTypeConstructor c_name)
        | Some ((vt_name, _), (_, c_t)) ->
            type_pattern ~get_source_position ctx p
            >>= fun (p_typed, var_ctx_from_p) ->
            let p_t = Pattern.node_val p_typed |> fst in
            c_t <: p_t >>= fun subpattern_type_matches ->
            if subpattern_type_matches then
              Ok
                ( PatConstructor
                    ((Vtype.VTypeCustom vt_name, v), c_name, p_typed),
                  var_ctx_from_p )
            else
              Error
                ( get_source_position v,
                  PatternTypeMismatch (Pattern.to_plain_t p, c_t, p_t) ))

  let rec type_expr
      ~(get_source_position :
         ('tag_e, 'tag_p) Either.t -> Frontend.source_position option)
      (((type_ctx : TypeCtx.t), (var_ctx : VarCtx.t)) as ctx)
      (orig_e : ('tag_e, 'tag_p) Expr.t) :
      (('tag_e, 'tag_p) Expr.typed_t, TypingError.t) Result.t =
    let open Result in
    let ( <: ) = TypeCtx.subtype type_ctx in
    let e_type (e : (Vtype.t * 'tag_e, Vtype.t * 'tag_p) Expr.t) : Vtype.t =
      e |> Expr.node_val |> fst
    in
    let be_of_type ?(msg : string option) (exp : Vtype.t)
        (e : (Vtype.t * 'tag_e, Vtype.t * 'tag_p) Expr.t) :
        ((Vtype.t * 'tag_e, Vtype.t * 'tag_p) Expr.t, TypingError.t) Result.t =
      e_type e <: exp >>= fun types_valid ->
      if types_valid then Ok e
      else
        Error
          ( get_source_position (e |> Expr.node_val |> snd |> First),
            TypeMismatch (exp, e_type e, msg) )
    in
    let type_binop ?(msg : string option)
        (recomp :
          (Vtype.t * 'tag_e, Vtype.t * 'tag_p) Expr.t ->
          (Vtype.t * 'tag_e, Vtype.t * 'tag_p) Expr.t ->
          Vtype.t ->
          (Vtype.t * 'tag_e, Vtype.t * 'tag_p) Expr.t)
        (e1 : ('tag_e, 'tag_p) Expr.t) (e2 : ('tag_e, 'tag_p) Expr.t)
        (req_t : Vtype.t) :
        ((Vtype.t * 'tag_e, Vtype.t * 'tag_p) Expr.t, TypingError.t) Result.t =
      type_expr ~get_source_position ctx e1 >>= fun e1' ->
      type_expr ~get_source_position ctx e2 >>= fun e2' ->
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
          (Vtype.t * 'tag_e, Vtype.t * 'tag_p) Expr.t ->
          Vtype.t ->
          (Vtype.t * 'tag_e, Vtype.t * 'tag_p) Expr.t)
        (e1 : ('tag_e, 'tag_p) Expr.t) (req_t : Vtype.t) :
        ((Vtype.t * 'tag_e, Vtype.t * 'tag_p) Expr.t, TypingError.t) Result.t =
      type_expr ~get_source_position ctx e1 >>= fun e1' ->
      be_of_type ?msg req_t e1' >>= fun _ -> Ok (recomp e1' (e_type e1'))
    in
    let type_int_compare ?(msg : string option)
        (recomp :
          (Vtype.t * 'tag_e, Vtype.t * 'tag_p) Expr.t ->
          (Vtype.t * 'tag_e, Vtype.t * 'tag_p) Expr.t ->
          (Vtype.t * 'tag_e, Vtype.t * 'tag_p) Expr.t)
        (e1 : ('tag_e, 'tag_p) Expr.t) (e2 : ('tag_e, 'tag_p) Expr.t) =
      type_expr ~get_source_position ctx e1 >>= fun e1' ->
      type_expr ~get_source_position ctx e2 >>= fun e2' ->
      let t1 = e_type e1' in
      let t2 = e_type e2' in
      match (t1, t2) with
      | VTypeInt, VTypeInt -> Ok (recomp e1' e2')
      | VTypeInt, _ ->
          Error
            ( get_source_position (e2 |> Expr.node_val |> First),
              TypeMismatch (VTypeInt, t2, msg) )
      | _, _ ->
          Error
            ( get_source_position (e1 |> Expr.node_val |> First),
              TypeMismatch (VTypeInt, t1, msg) )
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
        type_expr ~get_source_position ctx e1 >>= fun e1' ->
        type_expr ~get_source_position ctx e2 >>= fun e2' ->
        let t1 = e_type e1' in
        let t2 = e_type e2' in
        Ok (Expr.Pair ((Vtype.VTypePair (t1, t2), v), e1', e2'))
    | Eq (v, e1, e2) -> (
        type_expr ~get_source_position ctx e1 >>= fun e1' ->
        type_expr ~get_source_position ctx e2 >>= fun e2' ->
        let t1 = e_type e1' in
        let t2 = e_type e2' in
        match (t1, t2) with
        | VTypeInt, VTypeInt | VTypeBool, VTypeBool ->
            Ok (Expr.Eq ((Vtype.VTypeBool, v), e1', e2'))
        | _, _ ->
            Error
              ( get_source_position (First v),
                EqualOperatorTypeMistmatch (t1, t2) ))
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
    | If (v, e1, e2, e3) ->
        type_expr ~get_source_position ctx e1 >>= fun e1' ->
        be_of_type ~msg:"If cond node" VTypeBool e1' >>= fun _ ->
        type_expr ~get_source_position ctx e2 >>= fun e2' ->
        type_expr ~get_source_position ctx e3 >>= fun e3' ->
        let t2 = e_type e2' in
        let t3 = e_type e3' in
        t2 <: t3 >>= fun t2_sub_t3 ->
        t3 <: t2 >>= fun t3_sub_t2 ->
        (if t2_sub_t3 then Ok t3
         else if t3_sub_t2 then Ok t2
         else
           Error
             ( get_source_position (First v),
               TypingError.NoCommonRootType (t2, t3) ))
        >>= fun t_out -> Ok (Expr.If ((t_out, v), e1', e2', e3'))
    | Var (v, xname) -> (
        match VarCtx.find var_ctx xname with
        | Some t -> Ok (Var ((t, v), xname))
        | None -> Error (get_source_position (First v), UndefinedVariable xname)
        )
    | Let (v, xname, e1, e2) ->
        type_expr ~get_source_position ctx e1 >>= fun e1' ->
        let t1 = e_type e1' in
        type_expr ~get_source_position
          (type_ctx, VarCtx.add var_ctx xname t1)
          e2
        >>= fun e2' ->
        let t2 = e_type e2' in
        Ok (Expr.Let ((t2, v), xname, e1', e2'))
    | App (v, e1, e2) -> (
        type_expr ~get_source_position ctx e1 >>= fun e1' ->
        type_expr ~get_source_position ctx e2 >>= fun e2' ->
        let t1 = e_type e1' in
        let t2 = e_type e2' in
        match t1 with
        | VTypeFun (t11, t12) ->
            t2 <: t11 >>= fun arg_type_valid ->
            if arg_type_valid then Ok (Expr.App ((t12, v), e1', e2'))
            else
              Error
                ( get_source_position (e2 |> Expr.node_val |> First),
                  TypeMismatch (t11, t2, Some "App node function argument") )
        | _ ->
            Error
              ( get_source_position (e1 |> Expr.node_val |> First),
                ExpectedFunctionOf t2 ))
    | Match (v, e, t_out, cs) ->
        type_expr ~get_source_position ctx e >>= fun e' ->
        let t_in = e_type e' in
        (* Type the cases and check them against each other, as well as determining the type of the output *)
        Nonempty_list.fold_result_consume_init ~init:()
          ~f:(fun
              (acc :
                ( unit,
                  ((Vtype.t * 'tag_p) Pattern.t
                  * (Vtype.t * 'tag_e, Vtype.t * 'tag_p) Expr.t)
                  Nonempty_list.t )
                Either.t)
              ((p : 'tag_p Pattern.t), (c_e : ('tag_e, 'tag_p) Expr.t))
            ->
            (* First, try type the pattern *)
            (* TODO - don't allow the case patterns to be PatName. Check that they are compound patterns *)
            type_pattern
              ~get_source_position:(fun p -> get_source_position (Second p))
              (type_ctx, VarCtx.empty) p
            >>= fun (typed_p, p_ctx) ->
            let p_t : Vtype.t = Pattern.node_val typed_p |> fst in
            (* Check the pattern's type *)
            p_t <: t_in >>= fun pattern_type_valid ->
            if pattern_type_valid then
              let case_ctx = VarCtx.append var_ctx p_ctx in
              (* Then, type the case's expression using the extended context *)
              type_expr ~get_source_position (type_ctx, case_ctx) c_e
              >>= fun c_e' ->
              let t_c_e = e_type c_e' in
              t_c_e <: t_out >>= fun case_t_compatible ->
              if case_t_compatible then
                match acc with
                | First () -> Ok (Nonempty_list.singleton (typed_p, c_e'))
                | Second cs_prev_rev ->
                    Ok (Nonempty_list.cons (typed_p, c_e') cs_prev_rev)
              else
                Error
                  ( get_source_position (c_e |> Expr.node_val |> First),
                    TypeMismatch
                      (t_out, t_c_e, Some "Match case expression type") )
            else
              Error
                ( get_source_position (p |> Pattern.node_val |> Second),
                  PatternTypeMismatch (Pattern.to_plain_t p, t_in, p_t) ))
          cs
        >>|
        fun (cs_typed_rev :
              ((Vtype.t * 'tag_p) Pattern.t
              * (Vtype.t * 'tag_e, Vtype.t * 'tag_p) Expr.t)
              Nonempty_list.t)
        -> Expr.Match ((t_out, v), e', t_out, Nonempty_list.rev cs_typed_rev)
    | Constructor (v, c_name, e1) -> (
        type_expr ~get_source_position ctx e1 >>= fun e1' ->
        let t1 = e_type e1' in
        match TypeCtx.find_variant_type_with_constructor type_ctx c_name with
        | None ->
            Error
              ( get_source_position (First v),
                UndefinedVariantTypeConstructor c_name )
        | Some ((vt_name, _), (_, c_t)) ->
            c_t <: t1 >>= fun subexpr_type_valid ->
            if subexpr_type_valid then
              Ok
                (Expr.Constructor ((Vtype.VTypeCustom vt_name, v), c_name, e1'))
            else
              Error
                ( get_source_position (e1 |> Expr.node_val |> First),
                  TypeMismatch (c_t, t1, Some "Constructor argument") ))

  type ('tag_e, 'tag_p) checking_type_ctx_acc = {
    types : StringSet.t;
    constructors : StringSet.t;
    type_defns_list : ('tag_e, 'tag_p) CustomType.t list;
  }

  let check_type_ctx (ctx_in : TypeCtx.t) :
      (checked_type_ctx, TypingError.t) Result.t =
    let open Result in
    let acc_to_checked_type_ctx (acc : ('tag_e, 'tag_p) checking_type_ctx_acc) :
        (checked_type_ctx, TypingError.t) Result.t =
      TypeCtx.create ~custom_types:acc.type_defns_list
    in
    List.fold_result (TypeCtx.type_defns_to_ordered_list ctx_in)
      ~init:
        {
          types = StringSet.empty;
          constructors = StringSet.empty;
          type_defns_list = [];
        } ~f:(fun acc td ->
        let td_name = CustomType.name td in
        if Set.mem acc.types td_name then
          Error (None, TypingError.DuplicateTypeNameDefinition td_name)
        else
          let acc : ('tag_e, 'tag_p) checking_type_ctx_acc =
            { acc with types = Set.add acc.types td_name }
          in
          (match td with
          | VariantType (_, cs) ->
              (* For variant types, we also need to check the constructors *)
              List.fold_result cs ~init:acc ~f:(fun acc (c_name, _) ->
                  if Set.mem acc.constructors c_name then
                    Error
                      ( None,
                        TypingError.MultipleVariantTypeConstructorDefinitions
                          c_name )
                  else
                    Ok
                      {
                        acc with
                        constructors = Set.add acc.constructors c_name;
                      })
          | CustomType.QuotientType qt ->
              (* First, check that the base type is an existing type *)
              if Set.mem acc.types qt.base_type_name then Ok acc
              else Error (None, UndefinedTypeName qt.base_type_name))
          >>| fun acc ->
          { acc with type_defns_list = td :: acc.type_defns_list })
    >>= acc_to_checked_type_ctx

  let type_eqcons
      ~(get_source_position :
         ('tag_e, 'tag_p) Either.t -> Frontend.source_position option)
      ~(type_ctx : TypeCtx.t) ~(quotient_type_name : string)
      (eqcons : ('tag_e, 'tag_p) QuotientType.eqcons) :
      (('tag_e, 'tag_p) QuotientType.typed_eqcons, TypingError.t) Result.t =
    let open Result in
    let ( <: ) = TypeCtx.subtype type_ctx in
    let quotient_type = Vtype.VTypeCustom quotient_type_name in
    let body_pattern, body_expr = eqcons.body in
    (* Create a variable context from only the bindings *)
    List.fold_result ~init:VarCtx.empty
      ~f:(fun acc_var_ctx (xname, xtype) ->
        check_vtype ~source_position:None type_ctx xtype >>| fun () ->
        VarCtx.add acc_var_ctx xname xtype)
      eqcons.bindings
    >>= fun bindings_var_ctx ->
    (* Type the pattern *)
    type_pattern
      ~get_source_position:(fun p -> get_source_position (Second p))
      (type_ctx, VarCtx.empty) body_pattern
    >>= fun (typed_pattern, pattern_var_ctx) ->
    (* Check that all the defined variables in the pattern come from the bindings  *)
    List.fold_result ~init:()
      ~f:(fun () (xname, xtype) ->
        match VarCtx.find bindings_var_ctx xname with
        | Some ctx_xtype when Vtype.equal ctx_xtype xtype -> Ok ()
        | _ ->
            Error
              ( get_source_position
                  (typed_pattern |> Pattern.node_val |> snd |> Second),
                TypingError.EqconsVariableNotInBindings (xname, xtype) ))
      (pattern_var_ctx |> VarCtx.to_list)
    >>= fun () ->
    (* Type the expression, with the bindings' variable context *)
    type_expr ~get_source_position (type_ctx, bindings_var_ctx) body_expr
    >>= fun typed_body ->
    let pattern_t = typed_pattern |> Pattern.node_val |> fst in
    let expr_t = typed_body |> Expr.node_val |> fst in
    pattern_t <: quotient_type >>= fun pattern_t_valid ->
    if pattern_t_valid then
      expr_t <: quotient_type >>= fun expr_t_valid ->
      if expr_t_valid then Ok { eqcons with body = (typed_pattern, typed_body) }
      else
        Error
          ( get_source_position (eqcons.body |> snd |> Expr.node_val |> First),
            EqConsBodyExprTypeMismatch
              (Expr.to_plain_t body_expr, quotient_type, expr_t) )
    else
      Error
        ( get_source_position (eqcons.body |> fst |> Pattern.node_val |> Second),
          EqConsBodyPatternTypeMismatch
            (Pattern.to_plain_t body_pattern, quotient_type, pattern_t) )

  let type_custom_types
      ~(get_source_position :
         ('tag_e, 'tag_p) Either.t -> Frontend.source_position option)
      ~(type_ctx : TypeCtx.t)
      (ct_decls : ('tag_e, 'tag_p) Program.custom_type_decl list) =
    let open Result in
    List.map
      ~f:(fun ct_decl ->
        match ct_decl.ct with
        | VariantType vt ->
            { ct_decl with ct = CustomType.VariantType vt } |> Ok
        | CustomType.QuotientType qt ->
            List.map
              ~f:
                (type_eqcons ~get_source_position ~type_ctx
                   ~quotient_type_name:qt.name)
              qt.eqconss
            |> Result.all
            >>| fun eqconss ->
            { qt with eqconss } |> fun qt' ->
            { ct_decl with ct = CustomType.QuotientType qt' })
      ct_decls
    |> Result.all

  type ('tag_e, 'tag_p) type_program_tlds_acc = {
    defns_rev :
      (Vtype.t * 'tag_e, Vtype.t * 'tag_p) Program.top_level_defn list;
    defns_var_ctx : VarCtx.t;
  }

  let type_program
      ~(get_source_position :
         ('tag_e, 'tag_p) Either.t -> Frontend.source_position option)
      (prog : ('tag_e, 'tag_p) Program.t) :
      (('tag_e, 'tag_p) typed_program, TypingError.t) Result.t =
    let open Result in
    TypeCtx.create
      ~custom_types:
        (prog.custom_types
        |> List.map ~f:(fun Program.{ private_flag = _; ct } -> ct))
    >>= fun type_ctx ->
    type_custom_types ~get_source_position ~type_ctx prog.custom_types
    >>= fun custom_types_typed ->
    check_type_ctx type_ctx >>= fun type_ctx ->
    let ( <: ) = TypeCtx.subtype type_ctx in
    List.fold_result (* Check the top-level definitions *)
      ~init:{ defns_rev = []; defns_var_ctx = VarCtx.empty }
      ~f:(fun acc defn ->
        (* Check the definition's name doesn't already exist *)
        if VarCtx.exists acc.defns_var_ctx defn.name then
          Error
            ( get_source_position (defn.body |> Expr.node_val |> First),
              TypingError.MultipleTopLevelNameDefinitions defn.name )
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
          type_expr ~get_source_position (type_ctx, body_var_ctx) defn.body
          >>= fun typed_body ->
          let typed_body_t = typed_body |> Expr.node_val |> fst in
          typed_body_t <: defn.return_t >>= fun return_t_valid ->
          if return_t_valid then
            let defn_t = Vtype.VTypeFun (snd defn.param, defn.return_t) in
            Ok
              {
                defns_rev = { defn with body = typed_body } :: acc.defns_rev;
                defns_var_ctx = VarCtx.add acc.defns_var_ctx defn.name defn_t;
              }
          else
            Error
              ( get_source_position (defn.body |> Expr.node_val |> First),
                TypeMismatch
                  ( defn.return_t,
                    typed_body_t,
                    Some "Top-level defintion return type" ) ))
      prog.top_level_defns
    >>= fun tld_fold_final_acc ->
    let var_ctx = tld_fold_final_acc.defns_var_ctx in
    let tlds = tld_fold_final_acc.defns_rev |> List.rev in
    type_expr ~get_source_position (type_ctx, var_ctx) prog.e >>| fun typed_e ->
    ( Program.
        {
          custom_types = custom_types_typed;
          top_level_defns = tlds;
          e = typed_e;
        },
      type_ctx )
end

(** Type checker for standard programs, using standard type context and variable
    context implementations *)
module StdSimpleTypeChecker :
  S
    with module Pattern = Pattern.StdPattern
     and module Expr = Expr.StdExpr
     and module Program = Program.StdProgram
     and module TypingError = TypingError.StdTypingError
     and module TypeCtx = TypeContext.StdSetTypeContext
     and module VarCtx = VarContext.ListTypingVarContext =
  MakeStd (TypeContext.StdSetTypeContext) (VarContext.ListTypingVarContext)

let type_expr ~(type_ctx : StdSimpleTypeChecker.TypeCtx.t)
    ~(get_source_position :
       ('tag_e, 'tag_p) Either.t -> Frontend.source_position option)
    (e : ('tag_e, 'tag_p) StdSimpleTypeChecker.Expr.t) :
    ( ('tag_e, 'tag_p) StdSimpleTypeChecker.Expr.typed_t,
      StdSimpleTypeChecker.TypingError.t )
    Result.t =
  let open Result in
  StdSimpleTypeChecker.check_type_ctx type_ctx >>= fun type_ctx ->
  StdSimpleTypeChecker.type_expr ~get_source_position
    (type_ctx, StdSimpleTypeChecker.VarCtx.empty)
    e

let type_program
    ~(get_source_position :
       ('tag_e, 'tag_p) Either.t -> Frontend.source_position option)
    (prog : ('tag_e, 'tag_p) Program.StdProgram.t) :
    ( ('tag_e, 'tag_p) StdSimpleTypeChecker.typed_program,
      StdSimpleTypeChecker.TypingError.t )
    Result.t =
  StdSimpleTypeChecker.type_program ~get_source_position prog
