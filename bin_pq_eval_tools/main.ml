open Core
open Cmdliner
open Pq_lang
module Program = Program.StdProgram
module QuotientTypeChecker = QuotientTypeChecker.MakeZ3
module ProgramExecutor = ProgramExecutor.SimpleExecutor
module TypeChecker = ProgramExecutor.TypeChecker

module ProgramGen =
  Program.QCheck_testing
    (struct
      type t = unit
    end)
    (struct
      type t = unit
    end)

(* Exit codes *)

let get_exits () : Cmd.Exit.info list = Cmd.Exit.defaults

(* Program generation *)

let gen_program (`Mrd (mrd : int)) (`MaxVariantTypes (max_variant_types : int))
    (`MaxVariantTypeConstructors (max_variant_type_constructors : int))
    (`MaxTopLevelDefns (max_top_level_defns : int)) : ProgramGen.t =
  ProgramGen.gen
    {
      mrd;
      max_variant_types;
      max_variant_type_constructors;
      max_top_level_defns;
      allow_fun_types = false;
      force_has_body = None;
      body_type = None;
      expr_v_gen = QCheck.Gen.unit;
      pat_v_gen = QCheck.Gen.unit;
    }
  |> QCheck.Gen.generate1

let print_program_source (program : ProgramGen.t) : unit =
  printf "%s\n" (Program.to_source_code ~use_newlines:true program)

(* Command line interface *)

let gen_program_term : int Term.t =
  let arg_mrd : int Term.t =
    let open Arg in
    value & opt int 10
    & info ~docv:"MRD" ~doc:"Maximum recursion depth of generation"
        [ "r"; "mrd" ]
  in
  let arg_max_variant_types : int Term.t =
    let open Arg in
    value & opt int 5
    & info ~docv:"MAX_VARIANT_TYPES"
        ~doc:"Maximum number of variant types to generate"
        [ "t"; "max-variant-types" ]
  in
  let arg_max_variant_type_constructors : int Term.t =
    let open Arg in
    value & opt int 5
    & info ~docv:"MAX_VARIANT_TYPE_CONSTRUCTORS"
        ~doc:"Maximum number of variant types constructors per variant type"
        [ "c"; "max-variant-type-constructors" ]
  in
  let arg_max_top_level_defns : int Term.t =
    let open Arg in
    value & opt int 5
    & info ~docv:"MAX_TOP_LEVEL_DEFNS"
        ~doc:"Maximum number of top level definitions to generate"
        [ "d"; "max-top-level-defns" ]
  in
  Term.(
    const (fun prog ->
        print_program_source prog;
        0)
    $ (const gen_program
      $ map (fun x -> `Mrd x) arg_mrd
      $ map (fun x -> `MaxVariantTypes x) arg_max_variant_types
      $ map
          (fun x -> `MaxVariantTypeConstructors x)
          arg_max_variant_type_constructors
      $ map (fun x -> `MaxTopLevelDefns x) arg_max_top_level_defns))

let gen_program_info : Cmd.info =
  let doc = "Generate a PQ program" in
  Cmd.info ~doc ~exits:(get_exits ()) "gen-program"

let gen_program_cmd : int Cmd.t = Cmd.(v gen_program_info gen_program_term)

(* Main command *)

let info : Cmd.info =
  let doc = "Tools for evaluating the PQ compiler" in
  Cmd.info ~doc ~exits:(get_exits ()) "pq_eval_tools"

let () =
  let subcommands : int Cmd.t list = [ gen_program_cmd ] in
  Cmd.group info subcommands |> Cmd.eval' |> exit
