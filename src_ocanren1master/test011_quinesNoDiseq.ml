open Printf
open MiniKanren
open Tester
open Quines_NoDiseq
(* 
(* let _ = find_quines 3 *)

(*  (lambda ((vr _.0)) (list (vr _.0) (list 'quote (vr _.0))))
    '(lambda ((vr _.0)) (list (vr _.0) (list 'quote (vr _.0)))))
 *)
let () =
  let quine1 q0 =
    let open Gterm in
    app
      (lambda q0 (lst (vr q0) (lst quote (vr q0))))
      (app quote (lambda q0 (lst (vr q0) (lst quote (vr q0)))))
  in

  run q (fun q -> call_fresh (fun q0 -> ev nil (quine1 q0) q ))
    (fun qs -> Stream.take ~n:1 qs
      |> List.map wrap_result
      |> List.iter (printf "\n%s\n\n") );

  (* run q quineo @@ fun qs ->
    Stream.take ~n qs |> List.map wrap_term |> List.iter (printf "%s\n\n"); *)
  () *)
