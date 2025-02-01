open Core
open Utils
open Varname
open Vtype

type 'a pattern =
  | PatName of 'a * varname * vtype
  | PatPair of 'a * 'a pattern * 'a pattern
  | PatConstructor of 'a * string * 'a pattern
[@@deriving sexp, equal]

type 'a typed_pattern = (vtype * 'a) pattern [@@deriving sexp, equal]
type plain_pattern = unit pattern [@@deriving sexp, equal]

let rec pattern_to_plain_pattern : 'a pattern -> plain_pattern = function
  | PatName (_, xname, t) -> PatName ((), xname, t)
  | PatPair (_, p1, p2) ->
      PatPair ((), pattern_to_plain_pattern p1, pattern_to_plain_pattern p2)
  | PatConstructor (_, cname, p) ->
      PatConstructor ((), cname, pattern_to_plain_pattern p)

let pattern_node_val = function
  | PatName (v, _, _) -> v
  | PatPair (v, _, _) -> v
  | PatConstructor (v, _, _) -> v

let rec fmap ~(f : 'a -> 'b) : 'a pattern -> 'b pattern = function
  | PatName (v, xname, t) -> PatName (f v, xname, t)
  | PatPair (v, p1, p2) -> PatPair (f v, fmap ~f p1, fmap ~f p2)
  | PatConstructor (v, cname, p) -> PatConstructor (f v, cname, fmap ~f p)

let rec existing_names : 'a pattern -> StringSet.t = function
  | PatName (_, xname, _) -> StringSet.singleton xname
  | PatPair (_, p1, p2) -> Set.union (existing_names p1) (existing_names p2)
  | PatConstructor (_, c_name, p) ->
      Set.union (StringSet.singleton c_name) (existing_names p)

let rec pattern_to_source_code = function
  | PatName (_, xname, t) -> sprintf "%s : (%s)" xname (vtype_to_source_code t)
  | PatPair (_, p1, p2) ->
      sprintf "(%s), (%s)"
        (pattern_to_source_code p1)
        (pattern_to_source_code p2)
  | PatConstructor (_, cname, p) ->
      sprintf "%s (%s)" cname (pattern_to_source_code p)

module QCheck_testing : functor
  (Tag : sig
     type t
   end)
  -> sig
  type gen_options = {
    get_variant_type_constructors :
      string -> Variant_types.variant_type_constructor list;
    v_gen : Tag.t QCheck.Gen.t;
    t : vtype;
  }

  include
    QCheck_testing_sig
      with type t = Tag.t pattern * (string * vtype) list
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
    type this_t = Tag.t pattern * (string * vtype) list
    type t = this_t

    type gen_options = {
      get_variant_type_constructors :
        string -> Variant_types.variant_type_constructor list;
      v_gen : Tag.t QCheck.Gen.t;
      t : vtype;
    }

    type print_options = unit
    type shrink_options = unit
    type arb_options = gen_options

    let gen (opts : gen_options) : this_t QCheck.Gen.t =
      let open QCheck.Gen in
      let open Variant_types in
      let v_gen = opts.v_gen in
      let rec gen_new_varname (vars : (string * vtype) list) :
          string QCheck.Gen.t =
        (* Generate a new variable name that is not already in the context *)
        Varname.QCheck_testing.gen () >>= fun vname ->
        if List.exists ~f:(fun (xname, _) -> equal_string vname xname) vars then
          gen_new_varname vars
        else return vname
      and named_var (vars : (string * vtype) list) (t : vtype) :
          this_t QCheck.Gen.t =
        v_gen >>= fun v ->
        gen_new_varname vars >|= fun vname ->
        (PatName (v, vname, t), (vname, t) :: vars)
      and gen_unit (vars : (string * vtype) list) : this_t QCheck.Gen.t =
        (* Generate a pattern that types as unit *)
        named_var vars VTypeUnit
      and gen_int (vars : (string * vtype) list) : this_t QCheck.Gen.t =
        (* Generate a pattern that types as integer *)
        named_var vars VTypeInt
      and gen_bool (vars : (string * vtype) list) : this_t QCheck.Gen.t =
        (* Generate a pattern that types as boolean *)
        named_var vars VTypeBool
      and gen_fun ((t1 : vtype), (t2 : vtype)) (vars : (string * vtype) list) :
          this_t QCheck.Gen.t =
        (* Generate a pattern that types as a function *)
        named_var vars (VTypeFun (t1, t2))
      and gen_pair ((t1 : vtype), (t2 : vtype)) (vars : (string * vtype) list) :
          this_t QCheck.Gen.t =
        (* Generate a pattern that types as a pair *)
        v_gen >>= fun v ->
        oneof
          [
            named_var vars (VTypePair (t1, t2));
            ( gen t1 vars >>= fun (p1, vars1) ->
              gen t2 vars1 >|= fun (p2, vars2) -> (PatPair (v, p1, p2), vars2)
            );
          ]
      and gen_variant (cs : variant_type_constructor list)
          (vars : (string * vtype) list) :
          (* Generate a pattern that types as the specified variant type *)
          this_t QCheck.Gen.t =
        v_gen >>= fun v ->
        oneof (List.map ~f:return cs) >>= fun (c_name, c_t) ->
        gen c_t vars >>= fun (p, ctx') ->
        return (PatConstructor (v, c_name, p), ctx')
      and gen (t : vtype) : (string * vtype) list -> this_t QCheck.Gen.t =
        (* Generate a pattern of a specified type *)
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
        pair pattern_to_source_code (list (pair string vtype_to_source_code)))

    let rec shrink () : this_t QCheck.Shrink.t =
      let open QCheck.Iter in
      fun (p, (defined_vars : (varname * vtype) list)) ->
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
