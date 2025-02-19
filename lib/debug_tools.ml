open Core

let flush_channel (channel : Out_channel.t) : unit = Out_channel.flush channel

module Logger = struct
  type config = { out_channel : Out_channel.t; flush : bool }

  let config : config ref =
    ref { out_channel = Out_channel.stdout; flush = true }

  let configure ?(out_channel : Out_channel.t = stdout) ?(flush = true) () :
      unit =
    config := { out_channel; flush }

  let debug (msg : string) : unit =
    Out_channel.output_string !config.out_channel (sprintf "%s\n" msg);
    if !config.flush then Out_channel.flush !config.out_channel else ()

  let debugf (fmt : ('a, unit, string, unit) format4) : 'a = ksprintf debug fmt
  let debug_sexp (sexp : Sexp.t) : unit = debug (Sexp.to_string_hum sexp ^ "\n")
end

let pause () =
  let open In_channel in
  printf "Press ENTER to continue...\n";
  flush_channel stdout;
  input_line stdin |> ignore
