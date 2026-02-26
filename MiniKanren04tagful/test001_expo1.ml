let do_measure rel ~verbose =
  let open Numero_decls in
  TimeHelper.wrap_run
    OCanren.one
    rel
    ~reifier:(fun r -> r#reify num_reifier) (* ~inj:(List.to_logic (fun x -> Value x) ) *)
    ~verbose
    (fun term -> Printf.printf "%s\n" (Oleg.show_logic term))
;;

let () =
  TimeHelper.wrap @@ do_measure Numero_decls.(fun q -> expo (build_num 3) (build_num 5) q)
;;

(* let do_measure rel ~verbose =
  TimeHelper.wrap_run rel
    ~reifier:Numero_decls.num_reifier
    ~verbose
    (fun term -> Printf.printf "%s\n" (Numero_decls.show_num_logic term))

let () = TimeHelper.wrap @@
  do_measure Numero_decls.(fun q -> expo (build_num 3) (build_num 5) q ) *)
