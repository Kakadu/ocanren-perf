open Numero_decls
open MiniKanren
open Tester

let () =
  run_exn show_num (-1)   q qh (REPR (fun q -> logo (build_num 243) (build_num 3) q (build_num 0) ))

let () = MiniKanren.report_counters ()
