open Quine_decls_trace

type mode =
  | Quines
  | Twines
  | Thrines
  | Quines_nodiseq

type config =
  { mutable mode : mode
  ; mutable n : int
  ; mutable quiet : bool
  }

let config = { mode = Quines; n = 1; quiet = false }

let () =
  Arg.parse
    [ "-quines", Arg.Unit (fun () -> config.mode <- Quines), " "
    ; "-twines", Arg.Unit (fun () -> config.mode <- Twines), " "
    ; "-thrines", Arg.Unit (fun () -> config.mode <- Thrines), " "
    ; "-quines-nodiseq", Arg.Unit (fun () -> config.mode <- Quines_nodiseq), " "
    ; "-n", Arg.Int (fun n -> config.n <- n), " "
    ; "-q", Arg.Unit (fun () -> config.quiet <- true), " "
    ]
    (fun _ -> assert false)
    "help"
;;

let () =
  clear_unifications ();
  let () =
    (match config.mode with
     | Quines -> fun n -> find_quines ~verbose:(not config.quiet) n
     | Quines_nodiseq ->
       fun n ->
         Quines_NoDiseq_trace.find_quines ~verbose:(not config.quiet) n;
         Quine_decls_trace.config.unifications <- Quines_NoDiseq_trace.config.unifications
     | Twines -> find_twines ~verbose:(not config.quiet)
     | Thrines -> find_thrines ~verbose:(not config.quiet))
      config.n
  in
  Printf.printf "unifications: %d\n" Quine_decls_trace.(config.unifications)
;;
