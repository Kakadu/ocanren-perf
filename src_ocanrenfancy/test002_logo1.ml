open Numero_decls
open MiniKanren
open Tester

let () =
  run_exn show_num (-1)   q (REPR (fun q       -> logo (build_num 14) (build_num 2) (build_num 3) q))    qh
