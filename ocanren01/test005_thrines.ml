open OCanren
open TimeHelper

let () = Memtrace.trace_if_requested ~sampling_rate:1e-3 ~context:"thrines" ()
let () = TimeHelper.wrap (Quine_decls.find_thrines 2)

(* let () =
   Format.printf "OCanren unification = %d\n%!" (OCanren.Peep.unification_counter ())
   ;; *)
