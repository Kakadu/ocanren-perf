open Printf
open MiniKanren
open Tester
open Quines_NoDiseq

let _ = find_quines 100
(*
(*  (lambda ((vr _.0)) (list (vr _.0) (list 'quote (vr _.0))))
    '(lambda ((vr _.0)) (list (vr _.0) (list 'quote (vr _.0)))))
 *)

let fff () =
  let quine1 =
    let open Gterm in
    app
      (lambda (vr zero) (list2 (vr zero)
                        (list2 quotequote
                               (vr zero))))
      (quote !< (lambda (vr zero)
                        (list2 (vr zero)
                               (list2 quotequote
                                      (vr zero)))))
  in

  run q (fun q -> (ev nil quine1 (Gresult.code q) ))
    (fun qs -> Stream.take ~n:1 qs
      |> List.map wrap_term
      |> List.iter (printf "\n%s\n\n") );

  (* run q quineo @@ fun qs ->
    Stream.take ~n qs |> List.map wrap_term |> List.iter (printf "%s\n\n"); *)
  ()
*)
