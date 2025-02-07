open Quine_decls_trace

(* let () =
   match Sys.getenv "SILENT_UNIFICATIONS" with
   | _ -> set_trace false
   | exception Not_found -> set_trace true
   ;; *)

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
     | Quines -> find_quines ~verbose:(config.n = 1)
     | Twines -> find_twines ~verbose:(config.n = 1)
     | Thrines -> find_thrines ~verbose:(config.n = 1))
      config.n
  in
  Printf.printf "unifications: %d\n" Quine_decls_trace.(config.unifications)
;;
