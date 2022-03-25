open OCanren
open OCanren.Std

let do_measure rel ~verbose =
  let open Numero_decls in
  TimeHelper.wrap_run
    one
    rel
    ~reifier:(fun r -> r#reify num_reifier) (* ~inj:(List.to_logic (fun x -> Value x) ) *)
    ~verbose
    (fun term -> Printf.printf "%s\n" (show_num_logic term))
;;

let () =
  TimeHelper.wrap @@ do_measure Numero_decls.(fun q -> expo (build_num 3) (build_num 5) q)
;;
