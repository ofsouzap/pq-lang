open Core
open Utils

module type S = sig
  module Program : Program.S

  type output = { ml_source : string; mli_source : string }

  (* TODO - have an opaque type created by quotient type checker
  so that only quotient type-checked programs can be converted to OCaml *)

  (** Convert a PQ program to OCaml source code *)
  val program_to_ocaml :
    ?get_source_position:('tag_e -> Frontend.source_position) ->
    ('tag_e, 'tag_p) Program.t ->
    output
end

module StdM : S with module Program = Program.StdProgram = struct
  module VariantType = VariantType
  module Pattern = Pattern.StdPattern
  module Expr = Expr.StdExpr
  module CustomType = CustomType.StdCustomType
  module QuotientType = QuotientType.StdQuotientType
  module Program = Program.StdProgram

  type ml_source = Ml of string
  type mli_source = Mli of string
  type output = { ml_source : string; mli_source : string }

  let get_source_pos_writer (source_position : Frontend.source_position) :
      SourceCodeBuilder.state -> SourceCodeBuilder.state =
    let open SourceCodeBuilder in
    Option.(
      source_position.Frontend.fname >>| fun filename ->
      sprintf "# %d \"%s\"" source_position.lnum filename)
    |> function
    | None ->
        printf "nothing\n";
        nothing
    | Some s ->
        printf "%s\n" s;
        write s |.> endline

  let get_source_pos_opt_writer
      (get_source_position : ('tag_e -> Frontend.source_position) option)
      (v : 'tag_e) : SourceCodeBuilder.state -> SourceCodeBuilder.state =
    let open SourceCodeBuilder in
    match get_source_position with
    | None -> nothing
    | Some get_source_position -> get_source_position v |> get_source_pos_writer

  let unwrap_ml_source (Ml x) = x
  let unwrap_mli_source (Mli x) = x
  let vtype_to_source : Vtype.t -> string = Vtype.to_source_code

  let pattern_to_ml (p : 'tag_p Pattern.t) : ml_source =
    p |> Pattern.to_source_code |> Ml

  let expr_to_ml : ('tag_e, 'tag_p) Expr.t -> ml_source =
    let rec convert ?(bracketed : bool option)
        (orig_e : ('tag_e, 'tag_p) Expr.t) (p : SourceCodeBuilder.state) :
        SourceCodeBuilder.state =
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
         | Var (_, vname) -> write vname
         | Let (_, xname, e1, e2) -> (
             let default_repr : state -> state =
               write "let " |.> write xname |.> write " = "
               |.> block (convert e1)
               |.> write "in"
               |.> block (convert e2)
             in
             match e1 with _ -> default_repr)
         | App (_, e1, e2) -> convert e1 |.> write " " |.> convert e2
         | Match (_, e, _, cs) ->
             let convert_case
                 ((p : 'tag_p Pattern.t), (c_e : ('tag_e, 'tag_p) Expr.t)) :
                 state -> state =
               write "| ("
               |.> write (pattern_to_ml p |> unwrap_ml_source)
               |.> write ") ->"
               |.> block (convert c_e)
             in
             let blocked_converted_cases : (state -> state) Nonempty_list.t =
               Nonempty_list.map ~f:(convert_case |.> block) cs
             in
             let cases_converter : state -> state =
               Nonempty_list.fold ~init:Fn.id ~f:( |.> ) blocked_converted_cases
             in
             write "match " |.> convert e |.> write " with"
             |.> block cases_converter
         | Constructor (_, cname, e) -> write cname |.> write " " |.> convert e)
      |> if bracketed then write ")" else Fn.id
    in
    fun e ->
      e
      |> SourceCodeBuilder.from_converter ~converter:(convert ~bracketed:false)
           ~use_newlines:true
      |> Ml

  let variant_type_to_source ~(private_flag : Program.private_flag) :
      VariantType.t -> string =
    VariantType.to_source_code
      ~expose_construction:
        (match private_flag with Public -> true | Private -> false)

  let variant_type_to_ml (vt : VariantType.t) : ml_source =
    vt |> variant_type_to_source ~private_flag:Public |> Ml

  let variant_type_to_mli ~(private_flag : Program.private_flag)
      (vt : VariantType.t) : mli_source =
    vt |> variant_type_to_source ~private_flag |> Mli

  let quotient_type_to_ml (qt : ('tag_e, 'tag_p) QuotientType.t) : ml_source =
    let open SourceCodeBuilder in
    let convert (qt : ('tag_e, 'tag_p) QuotientType.t) : state -> state =
      write "type " |.> write qt.name |.> write " = "
      |.> write qt.base_type_name
    in
    qt
    |> SourceCodeBuilder.from_converter ~converter:convert ~use_newlines:true
    |> Ml

  let quotient_type_to_mli (qt : ('tag_e, 'tag_p) QuotientType.t) : mli_source =
    let open SourceCodeBuilder in
    let convert (qt : ('tag_e, 'tag_p) QuotientType.t) : state -> state =
      (* Quotient types become opaque types, so that they can't be used incorrectly by the module's user *)
      write "type " |.> write qt.name
    in
    qt
    |> SourceCodeBuilder.from_converter ~converter:convert ~use_newlines:true
    |> Mli

  let tld_to_ml
      ~(get_source_position : ('tag_e -> Frontend.source_position) option) :
      ('tag_e, 'tag_p) Program.top_level_defn -> ml_source =
    let open SourceCodeBuilder in
    let convert (defn : ('tag_e, 'tag_p) Program.top_level_defn) :
        state -> state =
      get_source_pos_opt_writer get_source_position (defn.body |> Expr.node_val)
      |.> write "let "
      |.> (if defn.recursive then write "rec " else nothing)
      |.> write defn.name |.> write " ("
      |.> write (fst defn.param)
      |.> write " : "
      |.> write (vtype_to_source (snd defn.param))
      |.> write ") : "
      |.> write (vtype_to_source defn.return_t)
      |.> write " ="
      |.> block (write (expr_to_ml defn.body |> unwrap_ml_source))
    in
    fun tld ->
      tld
      |> SourceCodeBuilder.from_converter ~converter:convert ~use_newlines:true
      |> Ml

  let tld_to_mli
      ~(get_source_position : ('tag_e -> Frontend.source_position) option) :
      ('tag_e, 'tag_p) Program.top_level_defn -> mli_source =
    let open SourceCodeBuilder in
    let convert (defn : ('tag_e, 'tag_p) Program.top_level_defn) :
        state -> state =
      get_source_pos_opt_writer get_source_position (defn.body |> Expr.node_val)
      |.> write "val " |.> write defn.name |.> write " : "
      |.> write (vtype_to_source (snd defn.param))
      |.> write " -> "
      |.> write (vtype_to_source defn.return_t)
    in
    fun tld ->
      tld
      |> SourceCodeBuilder.from_converter ~converter:convert ~use_newlines:true
      |> Mli

  let program_to_ml
      ~(get_source_position : ('tag_e -> Frontend.source_position) option)
      (prog : ('tag_e, 'tag_p) Program.t) : ml_source =
    List.map prog.custom_types ~f:(fun ct_decl ->
        match ct_decl.ct with
        | QuotientType qt -> quotient_type_to_ml qt
        | VariantType vt -> variant_type_to_ml vt)
    |> fun vt_srcs ->
    List.map prog.top_level_defns ~f:(tld_to_ml ~get_source_position)
    |> fun tld_srcs ->
    vt_srcs @ tld_srcs
    |> List.map ~f:unwrap_ml_source
    |> String.concat ~sep:"\n\n" |> Ml

  let program_to_mli
      ~(get_source_position : ('tag_e -> Frontend.source_position) option)
      (prog : ('tag_e, 'tag_p) Program.t) : mli_source =
    List.map prog.custom_types ~f:(fun ct_decl ->
        match ct_decl.ct with
        | QuotientType qt -> quotient_type_to_mli qt
        | VariantType vt ->
            variant_type_to_mli ~private_flag:ct_decl.private_flag vt)
    |> fun vt_srcs ->
    List.filter_map prog.top_level_defns ~f:(fun tld ->
        match tld.private_flag with
        | Public -> tld_to_mli ~get_source_position tld |> Some
        | Private -> None)
    |> fun tld_srcs ->
    vt_srcs @ tld_srcs
    |> List.map ~f:unwrap_mli_source
    |> String.concat ~sep:"\n\n" |> Mli

  let program_to_ocaml
      ?(get_source_position : ('tag_e -> Frontend.source_position) option)
      (prog : ('tag_e, 'tag_p) Program.t) : output =
    {
      ml_source = program_to_ml ~get_source_position prog |> unwrap_ml_source;
      mli_source = program_to_mli ~get_source_position prog |> unwrap_mli_source;
    }
end
