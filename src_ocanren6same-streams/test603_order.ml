open MiniKanren
open Tester

let asdf (e: (int, int logic) injected) =
  (* let (===)  = unitrace show_reif_term in *)
  (* let (===================================) = unitrace show_reif_result in *)
  let (===) ?loc = unitrace ?loc (fun h t -> (GT.show logic string_of_int) @@
    ManualReifiers.int_reifier h t) in
  conde
    [ (!!1 === !!2)
    ; (!!3 === !!4)
    ; (!!5 === !!6) &&& (!!7 === !!8) 
    ]

open Tester
let () =
  run_exn string_of_int 1 q qh (REPR (asdf))

let () = MiniKanren.report_counters ()
