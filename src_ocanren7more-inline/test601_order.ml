open Printf
open MiniKanren

(* let show_intl = GT.show logic string_of_int *)

let evalo (e: (int, int logic) injected) =
  (* let (===)  = unitrace show_reif_term in *)
  (* let (===================================) = unitrace show_reif_result in *)
  let (===) = unitrace (fun h t -> (GT.show logic string_of_int) @@ ManualReifiers.int_reifier h t) in
  conde
  [ fresh (_t)
      (e === !!0)
      (e === !!1)

  ; fresh (_es)
      (e === !!2)
  ; fresh (_zzzzs)
      (e === !!3)

  ]

open Tester
let () =
  run_exn string_of_int 2 q qh (REPR (evalo))

let () = MiniKanren.report_counters ()
