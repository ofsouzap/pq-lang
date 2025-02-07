open Core
open Pq_lang

let () =
  let open Result in
  let res =
    Frontend.run_frontend_channel In_channel.stdin
    |> Result.map_error ~f:(fun err ->
           sprintf "Frontend error: %s\n"
             (Frontend.sexp_of_frontend_error err |> Sexp.to_string_hum))
    >>= fun inp_prog ->
    Typing.type_program inp_prog
    |> Result.map_error ~f:(fun err ->
           sprintf "Typing error: %s\n" (Typing.print_typing_error err))
    >>= fun inp_typed_program ->
    inp_typed_program |> Typing.SimpleTypeChecker.typed_program_get_program
    |> Program.fmap_expr ~f:(fun (t, ()) ->
           ({ t } : Quotient_type_checking.ast_tag))
    |> Program.fmap_pattern ~f:(fun (t, ()) ->
           ({ t } : Quotient_type_checking.pattern_tag))
    |> fun inp_for_quotient_type_checking ->
    match
      Quotient_type_checking.check_program inp_for_quotient_type_checking
    with
    | Ok () -> Ok ()
    | Error err ->
        Error
          (sprintf "Unexpected quotient type checking failure: %s"
             (err |> Quotient_type_checking.sexp_of_quotient_typing_error
            |> Sexp.to_string_hum))
  in
  match res with
  | Ok () -> printf "All fine!\n"
  | Error err_msg -> eprintf "%s" err_msg
