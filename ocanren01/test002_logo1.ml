open OCanren
open OCanren.Std
open TimeHelper

let do_measure rel ~verbose =
  let open Numero_decls in
  TimeHelper.wrap_run
    one
    rel
    ~reifier:(fun r -> r#reify Oleg.reify)
    ~verbose
    (fun term -> Printf.printf "%s\n" (Oleg.show_logic term))
;;

let () =
  let open Numero_decls in
  TimeHelper.wrap
  @@ do_measure (fun q -> logo (build_num 243) (build_num 3) q (build_num 0))
;;
