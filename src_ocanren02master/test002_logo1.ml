open MiniKanren
open MiniKanrenStd
open TimeHelper

(* let () = Gc.set { (Gc.get()) with Gc.verbose = 0x400d } *)

let do_measure rel ~verbose =
  let open Numero_decls in
  TimeHelper.wrap_run one rel
    ~reifier:num_reifier
    ~verbose
    (fun term -> Printf.printf "%s\n" (show_num_logic term))

let () =
  let open Numero_decls in
  TimeHelper.wrap @@ do_measure
    (fun q -> logo (build_num 243) (build_num 3) q (build_num 0) )

let () = Gc.print_stat stdout
