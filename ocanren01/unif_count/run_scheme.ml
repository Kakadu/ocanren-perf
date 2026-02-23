open Quine_decls_trace

type mode =
  | Quines
  | Twines
  | Thrines

type config =
  { mutable mode : mode
  ; mutable n : int
  }

let config = { mode = Quines; n = 1 }

let () =
  Arg.parse
    [ "-quines", Arg.Unit (fun () -> config.mode <- Quines), " "
    ; "-twines", Arg.Unit (fun () -> config.mode <- Twines), " "
    ; "-thrines", Arg.Unit (fun () -> config.mode <- Thrines), " "
    ; "-n", Arg.Int (fun n -> config.n <- n), " "
    ]
    (fun _ -> assert false)
    "help"
;;

let () =
  clear_unifications ();
  let () =
    (match config.mode with
     | Quines -> fun n -> find_quines ~verbose:(config.n = 1) n
     | Twines -> find_twines ~verbose:(config.n = 1)
     | Thrines -> find_thrines ~verbose:(config.n = 1))
      config.n
  in
  Printf.printf "unifications: %d\n" Quine_decls_trace.(config.unifications)
;;
