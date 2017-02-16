open Numero_decls
open MiniKanren
open Tester

let () =
  run_exn show_num (-1)   q qh (REPR (fun q       -> logo (build_num 1025) (build_num 2) q (build_num 1) ))
