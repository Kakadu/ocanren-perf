open Numero_decls
open MiniKanren
open Tester

let () =
  run_exn show_num (-1)   q qh (REPR (fun q -> expo (build_num 3) (build_num 5) q ))
