open OCanren
open TimeHelper

let () = TimeHelper.wrap (Quine_decls.find_thrines 2)

(* let () =
  Format.printf "OCanren unification = %d\n%!" (OCanren.Peep.unification_counter ())
;; *)
