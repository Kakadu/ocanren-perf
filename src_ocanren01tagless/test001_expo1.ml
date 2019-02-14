open MiniKanren
open MiniKanrenStd

let do_measure rel ~verbose =
  let open Numero_decls in
  TimeHelper.wrap_run rel one
    ~reifier:num_reifier
    ~verbose
    (fun term -> Printf.printf "%s\n" (show_num_logic term))


let () = TimeHelper.wrap @@
  do_measure Numero_decls.(fun q -> expo (build_num 3) (build_num 5) q )
