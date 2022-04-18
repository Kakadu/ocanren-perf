open OCanren
open Quine_decls
open Gterm

let quineso0 q = evalo q nil (Gresult.val_ q)

(* Single quine should give 2085 unifications *)
let () =
  run q quineso0 (fun r -> r)
  |> OCanren.Stream.take ~n:1
  |> List.iter (fun q -> Printf.printf "%s\n\n" (wrap_term q))
;;

let () =
  Format.printf
    "Unification counter after  = %d\n%!"
    (OCanren.Peep.unification_counter ())
;;
