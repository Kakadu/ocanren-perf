let do_measure rel ~verbose =
  TimeHelper.wrap_run rel
    ~reifier:Numero_decls.num_reifier
    ~verbose
    (fun term -> Printf.printf "%s\n" (Numero_decls.show_num_logic term))

let () =
  let open Numero_decls in
  TimeHelper.wrap @@ do_measure
    (fun q -> logo (build_num 243) (build_num 3) q (build_num 0) )
