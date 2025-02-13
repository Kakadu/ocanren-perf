let do_measure rel ~verbose =
  TimeHelper.wrap_run rel
    ~reifier:Numero_decls.num_reifier
    ~verbose
    (fun term -> Printf.printf "%s\n" (Numero_decls.show_num_logic term))

let () = TimeHelper.wrap @@
  do_measure Numero_decls.(fun q -> expo (build_num 3) (build_num 5) q )
