open Core
module StringSet = Set.Make (String)
module StringMap = Map.Make (String)

let lexer_keywords : string list =
  [
    "end";
    "if";
    "then";
    "else";
    "let";
    "rec";
    "in";
    "true";
    "false";
    "fun";
    "unit";
    "int";
    "bool";
    "match";
    "with";
    "type";
    "qtype";
    "of";
  ]

module type QCheck_utils_sig = sig
  exception Filter_ran_out_of_attempts

  val filter_gen :
    ?max_attempts:int -> 'a QCheck.Gen.t -> f:('a -> bool) -> 'a QCheck.Gen.t

  val gen_unique_pair :
    ?max_attempts:int ->
    equal:('a -> 'a -> bool) ->
    'a QCheck.Gen.t ->
    ('a * 'a) QCheck.Gen.t

  val result_arb :
    'a QCheck.arbitrary ->
    'b QCheck.arbitrary ->
    ('a, 'b) Result.t QCheck.arbitrary
end

module QCheck_utils : QCheck_utils_sig = struct
  exception Filter_ran_out_of_attempts

  let filter_gen ?(max_attempts : int option) (x_gen : 'a QCheck.Gen.t)
      ~(f : 'a -> bool) : 'a QCheck.Gen.t =
    let open QCheck.Gen in
    fix
      (fun self n_opt ->
        let _ =
          (* Termination check *)
          match n_opt with
          | None -> ()
          | Some n -> if n < 0 then raise Filter_ran_out_of_attempts
        in
        x_gen >>= fun x ->
        if f x then return x else self (Option.map n_opt ~f:(fun n -> n - 1)))
      max_attempts

  let gen_unique_pair ?(max_attempts : int option) ~(equal : 'a -> 'a -> bool)
      (g : 'a QCheck.Gen.t) : ('a * 'a) QCheck.Gen.t =
    let open QCheck.Gen in
    filter_gen
      ~max_attempts:(Option.value ~default:10000 max_attempts)
      ~f:(fun (x, y) -> not (equal x y))
      (pair g g)

  let result_arb (x_arb : 'a QCheck.arbitrary) (y_arb : 'b QCheck.arbitrary) :
      ('a, 'b) Result.t QCheck.arbitrary =
    QCheck.map
      (fun (b, x, y) -> if b then Ok x else Error y)
      (QCheck.triple QCheck.bool x_arb y_arb)
end

module type QCheck_testing_sig = sig
  open QCheck

  type t
  type gen_options
  type print_options
  type shrink_options
  type arb_options

  val gen : gen_options -> t Gen.t
  val print : print_options -> t Print.t
  val shrink : shrink_options -> t Shrink.t
  val arbitrary : arb_options -> t arbitrary
end

module type Nonempty_list_sig = sig
  type 'a t = 'a * 'a list [@@deriving sexp, equal]

  val make : 'a * 'a list -> 'a t
  val to_list : 'a t -> 'a list
  val from_list_unsafe : 'a list -> 'a t
  val length : 'a t -> int
  val head : 'a t -> 'a
  val tail : 'a t -> 'a list
  val singleton : 'a -> 'a t
  val cons : 'a -> 'a t -> 'a t
  val map : f:('a -> 'b) -> 'a t -> 'b t
  val mapi : f:(int -> 'a -> 'b) -> 'a t -> 'b t
  val fold : 'a t -> init:'b -> f:('b -> 'a -> 'b) -> 'b

  val fold_consume_init :
    'a t -> init:'init -> f:(('init, 'acc) Either.t -> 'a -> 'acc) -> 'acc

  val rev : 'a t -> 'a t

  (** Zip two non-empty lists together, returning None if they have unequal
      lengths *)
  val zip : 'a t -> 'b t -> ('a * 'b) t option

  val result_all : ('a, 'err) Result.t t -> ('a t, 'err) Result.t

  val fold_result :
    'a t -> init:'b -> f:('b -> 'a -> ('b, 'c) Result.t) -> ('b, 'c) Result.t

  (** A variant of fold_result that is known not to return the initial value, as
      the list is non-empty *)
  val fold_result_consume_init :
    'a t ->
    init:'init ->
    f:(('init, 'acc) Either.t -> 'a -> ('acc, 'err) Result.t) ->
    ('acc, 'err) Result.t

  module QCheck_testing : functor
    (V : sig
       type t

       val gen : t QCheck.Gen.t
       val print : t QCheck.Print.t
       val shrink : t QCheck.Shrink.t
     end)
    ->
    QCheck_testing_sig
      with type t = V.t t
       and type gen_options = unit
       and type print_options = unit
       and type shrink_options = unit
       and type arb_options = unit
end

module Nonempty_list : Nonempty_list_sig = struct
  type 'a t = 'a * 'a list [@@deriving sexp, equal]
  type 'a nonempty_list_t = 'a t

  let make = Fn.id
  let to_list ((h, ts) : 'a t) = h :: ts

  let from_list_unsafe (xs : 'a list) =
    match xs with [] -> failwith "Empty list" | h :: ts -> (h, ts)

  let length ((_, ts) : 'a t) = List.length ts + 1
  let head (h, _) = h
  let tail (_, ts) = ts
  let singleton (h : 'a) = (h, [])
  let cons (h : 'a) (ts : 'a t) = (h, to_list ts)
  let map ~(f : 'a -> 'b) ((h, ts) : 'a t) : 'b t = (f h, List.map ~f ts)

  let mapi ~(f : int -> 'a -> 'b) ((h, ts) : 'a t) : 'b t =
    (f 0 h, List.mapi ~f:(fun i x -> f (i + 1) x) ts)

  let fold (xs : 'a t) = List.fold (to_list xs)

  let fold_consume_init ((h, ts) : 'a t) ~init ~f =
    let init = f (First init) h in
    List.fold ts ~init ~f:(fun acc -> f (Second acc))

  let rev (xs : 'a t) =
    match tail xs with
    | [] -> head xs |> singleton
    | ts_h :: ts_ts ->
        fold
          ~init:(singleton (head xs))
          ~f:(fun acc x -> cons x acc)
          (make (ts_h, ts_ts))

  let zip (xs : 'a t) (ys : 'b t) : ('a * 'b) t option =
    match List.zip (tail xs) (tail ys) with
    | Ok zs_ts -> Some (make ((head xs, head ys), zs_ts))
    | Unequal_lengths -> None

  let result_all (xs : ('a, 'err) Result.t t) : ('a t, 'err) Result.t =
    let open Result in
    xs |> to_list |> Result.all >>| from_list_unsafe

  let fold_result (xs : 'a t) = List.fold_result (to_list xs)

  let fold_result_consume_init ((h, ts) : 'a t) ~(init : 'init)
      ~(f : ('init, 'acc) Either.t -> 'a -> ('acc, 'err) Result.t) :
      ('acc, 'err) Result.t =
    match f (First init) h with
    | Ok h_y -> List.fold_result ts ~init:h_y ~f:(fun acc -> f (Second acc))
    | Error err -> Error err

  module QCheck_testing (V : sig
    type t

    val gen : t QCheck.Gen.t
    val print : t QCheck.Print.t
    val shrink : t QCheck.Shrink.t
  end) :
    QCheck_testing_sig
      with type t = V.t t
       and type gen_options = unit
       and type print_options = unit
       and type shrink_options = unit
       and type arb_options = unit = struct
    type t = V.t nonempty_list_t
    type gen_options = unit
    type print_options = unit
    type shrink_options = unit
    type arb_options = unit

    let gen () : t QCheck.Gen.t =
      let open QCheck.Gen in
      pair V.gen (list V.gen) >|= make

    let print () : t QCheck.Print.t = QCheck.Print.(pair V.print (list V.print))

    let shrink () : t QCheck.Shrink.t =
      let open QCheck in
      let open QCheck.Iter in
      fun xs ->
        match xs with
        | h, [] -> V.shrink h >|= fun h' -> (h', [])
        | (h, (ts_h :: ts_ts as ts)) as xs ->
            (* Shrink the list elements, preserving the length *)
            to_list xs |> Shrink.list_elems V.shrink >|= from_list_unsafe
            <+>
            (* Shrink the tail elements, preserving the head *)
            (Shrink.list_spine ts >|= fun ts' -> (h, ts'))
            <+>
            (* Drop the head *)
            return (ts_h, ts_ts)

    let arbitrary () : t QCheck.arbitrary =
      QCheck.make ~print:(print ()) ~shrink:(shrink ()) (gen ())
  end
end

module type SourceCodeBuilderSig = sig
  type state

  val ( |.> ) : ('a -> 'b) -> ('b -> 'c) -> 'a -> 'c
  val init : use_newlines:bool -> state
  val nothing : state -> state
  val write : string -> state -> state
  val endline : state -> state
  val block : (state -> state) -> state -> state
  val build : state -> string

  val from_converter :
    converter:('a -> state -> state) -> use_newlines:bool -> 'a -> string
end

module SourceCodeBuilder : SourceCodeBuilderSig = struct
  type builder = (int * string) list
  type state = { builder : builder; use_newlines : bool; indent_level : int }

  let ( |.> ) (f1 : 'a -> 'b) (f2 : 'b -> 'c) : 'a -> 'c = Fn.compose f2 f1

  let init ~(use_newlines : bool) : state =
    { builder = []; use_newlines; indent_level = 0 }

  let nothing = Fn.id

  let write (s : string) (p : state) : state =
    {
      p with
      builder =
        (match p.builder with
        | [] -> [ (0, s) ]
        | (h_indent, h_str) :: t -> (h_indent, h_str ^ s) :: t);
    }

  let endline (p : state) : state =
    match p.builder with
    | [] -> (* No lines exist *) p
    | (_, "") :: ts ->
        (* Current line is empty *)
        { p with builder = (p.indent_level, "") :: ts }
    | _ :: _ ->
        (* Current line is non-empty *)
        { p with builder = (p.indent_level, "") :: p.builder }

  let block (f : state -> state) : state -> state =
    let indent_up (p : state) : state =
      { p with indent_level = p.indent_level + 1 }
    in
    let indent_down (p : state) : state =
      { p with indent_level = max 0 (p.indent_level - 1) }
    in
    indent_up |.> endline |.> f |.> indent_down |.> endline

  let build (p : state) : string =
    let build_fun : (int * string) list -> string =
      if p.use_newlines then
        List.map ~f:(fun (indent, str) -> String.make (indent * 2) ' ' ^ str)
        |.> String.concat ~sep:"\n"
      else List.map ~f:snd |.> String.concat ~sep:" "
    in
    p.builder |> List.rev |> build_fun

  let from_converter ~(converter : 'a -> state -> state) ~(use_newlines : bool)
      (x : 'a) : string =
    init ~use_newlines |> (x |> converter) |> build
end
