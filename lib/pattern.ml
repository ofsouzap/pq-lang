open Core
open Utils

module type S = sig
  (** A pattern in the language *)
  type 'a t [@@deriving sexp, equal]

  (** A pattern with type information *)
  type 'a typed_t = (Vtype.t * 'a) t [@@deriving sexp, equal]

  (** Pattern with no tagging information *)
  type plain_t = unit t [@@deriving sexp, equal]

  (** Convert a tagged pattern to an untagged pattern *)
  val to_plain_t : 'a t -> plain_t

  (** Get the value attached to a pattern node *)
  val node_val : 'a t -> 'a

  (** Map a function onto values in a pattern *)
  val fmap : f:('a -> 'b) -> 'a t -> 'b t

  (** Rename a variable in a pattern *)
  val rename_var : old_name:Varname.t -> new_name:Varname.t -> 'a t -> 'a t

  (** Get all names used in a pattern *)
  val existing_names : 'a t -> StringSet.t

  (** Get the list of variables and their types that this pattern introduces to
      its case expression's variable context *)
  val defined_vars : 'a t -> (Varname.t * Vtype.t) list

  (** Convert a pattern to a source code representation *)
  val to_source_code : 'a t -> string

  module QCheck_testing : functor
    (Tag : sig
       type t
     end)
    -> sig
    type gen_options = {
      get_variant_type_constructors : string -> VariantType.constructor list;
      v_gen : Tag.t QCheck.Gen.t;
      t : Vtype.t;
    }

    include
      QCheck_testing_sig
        with type t = Tag.t t * (string * Vtype.t) list
         and type gen_options := gen_options
         and type print_options = unit
         and type shrink_options = unit
         and type arb_options = gen_options
  end
end

type 'a std_pattern =
  | PatName of 'a * Varname.t * Vtype.t
      (** A named and typed variable in a pattern *)
  | PatPair of 'a * 'a std_pattern * 'a std_pattern  (** A pair pattern *)
  | PatConstructor of 'a * string * 'a std_pattern  (** A constructor pattern *)
[@@deriving sexp, equal]

module StdPattern : S with type 'a t = 'a std_pattern = struct
  type 'a t = 'a std_pattern [@@deriving sexp, equal]
  type 'a typed_t = (Vtype.t * 'a) t [@@deriving sexp, equal]
  type plain_t = unit t [@@deriving sexp, equal]

  let rec to_plain_t : 'a t -> plain_t = function
    | PatName (_, xname, t) -> PatName ((), xname, t)
    | PatPair (_, p1, p2) -> PatPair ((), to_plain_t p1, to_plain_t p2)
    | PatConstructor (_, cname, p) -> PatConstructor ((), cname, to_plain_t p)

  let node_val = function
    | PatName (v, _, _) -> v
    | PatPair (v, _, _) -> v
    | PatConstructor (v, _, _) -> v

  let rec fmap ~(f : 'a -> 'b) : 'a t -> 'b t = function
    | PatName (v, xname, t) -> PatName (f v, xname, t)
    | PatPair (v, p1, p2) -> PatPair (f v, fmap ~f p1, fmap ~f p2)
    | PatConstructor (v, cname, p) -> PatConstructor (f v, cname, fmap ~f p)

  let rec rename_var ~(old_name : Varname.t) ~(new_name : Varname.t) = function
    | PatName (v, xname, xtype) ->
        PatName
          (v, (if equal_string xname old_name then new_name else xname), xtype)
    | PatPair (v, p1, p2) ->
        PatPair
          ( v,
            rename_var ~old_name ~new_name p1,
            rename_var ~old_name ~new_name p2 )
    | PatConstructor (v, cname, p) ->
        PatConstructor (v, cname, rename_var ~old_name ~new_name p)

  let rec existing_names : 'a t -> StringSet.t = function
    | PatName (_, xname, _) -> StringSet.singleton xname
    | PatPair (_, p1, p2) -> Set.union (existing_names p1) (existing_names p2)
    | PatConstructor (_, c_name, p) ->
        Set.union (StringSet.singleton c_name) (existing_names p)

  let rec defined_vars : 'a t -> (Varname.t * Vtype.t) list = function
    | PatName (_, xname, xtype) -> [ (xname, xtype) ]
    | PatPair (_, p1, p2) -> defined_vars p1 @ defined_vars p2
    | PatConstructor (_, _, p) -> defined_vars p

  let rec to_source_code = function
    | PatName (_, xname, t) ->
        sprintf "%s : (%s)" xname (Vtype.to_source_code t)
    | PatPair (_, p1, p2) ->
        sprintf "(%s), (%s)" (to_source_code p1) (to_source_code p2)
    | PatConstructor (_, cname, p) -> sprintf "%s (%s)" cname (to_source_code p)

  module QCheck_testing : functor
    (Tag : sig
       type t
     end)
    -> sig
    type gen_options = {
      get_variant_type_constructors : string -> VariantType.constructor list;
      v_gen : Tag.t QCheck.Gen.t;
      t : Vtype.t;
    }

    include
      QCheck_testing_sig
        with type t = Tag.t t * (string * Vtype.t) list
         and type gen_options := gen_options
         and type print_options = unit
         and type shrink_options = unit
         and type arb_options = gen_options
  end =
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

      let gen (opts : gen_options) : this_t QCheck.Gen.t =
        let open QCheck.Gen in
        let v_gen = opts.v_gen in
        let rec gen_new_varname (vars : (string * Vtype.t) list) :
            string QCheck.Gen.t =
          (* Generate a new variable name that is not already in the context *)
          Varname.QCheck_testing.gen () >>= fun vname ->
          if List.exists ~f:(fun (xname, _) -> equal_string vname xname) vars
          then gen_new_varname vars
          else return vname
        and named_var (vars : (string * Vtype.t) list) (t : Vtype.t) :
            this_t QCheck.Gen.t =
          v_gen >>= fun v ->
          gen_new_varname vars >|= fun vname ->
          (PatName (v, vname, t), (vname, t) :: vars)
        and gen_unit (vars : (string * Vtype.t) list) : this_t QCheck.Gen.t =
          (* Generate a t that types as unit *)
          named_var vars VTypeUnit
        and gen_int (vars : (string * Vtype.t) list) : this_t QCheck.Gen.t =
          (* Generate a t that types as integer *)
          named_var vars VTypeInt
        and gen_bool (vars : (string * Vtype.t) list) : this_t QCheck.Gen.t =
          (* Generate a t that types as boolean *)
          named_var vars VTypeBool
        and gen_fun ((t1 : Vtype.t), (t2 : Vtype.t))
            (vars : (string * Vtype.t) list) : this_t QCheck.Gen.t =
          (* Generate a t that types as a function *)
          named_var vars (VTypeFun (t1, t2))
        and gen_pair ((t1 : Vtype.t), (t2 : Vtype.t))
            (vars : (string * Vtype.t) list) : this_t QCheck.Gen.t =
          (* Generate a t that types as a pair *)
          v_gen >>= fun v ->
          oneof
            [
              named_var vars (VTypePair (t1, t2));
              ( gen t1 vars >>= fun (p1, vars1) ->
                gen t2 vars1 >|= fun (p2, vars2) -> (PatPair (v, p1, p2), vars2)
              );
            ]
        and gen_variant (cs : VariantType.constructor list)
            (vars : (string * Vtype.t) list) :
            (* Generate a t that types as the specified variant type *)
            this_t QCheck.Gen.t =
          v_gen >>= fun v ->
          oneof (List.map ~f:return cs) >>= fun (c_name, c_t) ->
          gen c_t vars >>= fun (p, ctx') ->
          return (PatConstructor (v, c_name, p), ctx')
        and gen (t : Vtype.t) : (string * Vtype.t) list -> this_t QCheck.Gen.t =
          (* Generate a t of a specified type *)
          match t with
          | VTypeUnit -> gen_unit
          | VTypeInt -> gen_int
          | VTypeBool -> gen_bool
          | VTypeFun (t1, t2) -> gen_fun (t1, t2)
          | VTypePair (t1, t2) -> gen_pair (t1, t2)
          | VTypeCustom vt_name ->
              gen_variant (opts.get_variant_type_constructors vt_name)
        in
        gen opts.t []

      let print () : this_t QCheck.Print.t =
        QCheck.Print.(
          pair to_source_code (list (pair string Vtype.to_source_code)))

      let rec shrink () : this_t QCheck.Shrink.t =
        let open QCheck.Iter in
        fun (p, (defined_vars : (Varname.t * Vtype.t) list)) ->
          match p with
          | PatName (v, xname, t) ->
              QCheck.Shrink.(
                pair string (Vtype.QCheck_testing.shrink ()) (xname, t))
              >|= fun (xname', t') ->
              ( PatName (v, xname', t'),
                List.map
                  ~f:(fun (xi_name, xi_t) ->
                    if equal_string xname xi_name then (xname', t')
                    else (xi_name, xi_t))
                  defined_vars )
          | PatPair (v, p1, p2) ->
              shrink () (p1, defined_vars) >>= fun (p1', defined_vars') ->
              shrink () (p2, defined_vars') >|= fun (p2', defined_vars'') ->
              (PatPair (v, p1', p2'), defined_vars'')
          | PatConstructor (v, cname, p) ->
              QCheck.Shrink.(pair nil (shrink ())) (cname, (p, defined_vars))
              >|= fun (cname', (p', defined_vars')) ->
              (PatConstructor (v, cname', p'), defined_vars')

      let arbitrary (opts : arb_options) : this_t QCheck.arbitrary =
        QCheck.make ~print:(print ()) ~shrink:(shrink ()) (gen opts)
    end
end
