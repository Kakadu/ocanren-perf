open Printf
open GT
open MiniKanren
open Quine_decls

let show_intl = GT.show logic string_of_int


let evalo (term: (int, int logic) injected) =
  (* let (===)  = unitrace show_reif_term in *)
  (* let (===================================) = unitrace show_reif_result in *)
  (* let (===) = unitrace (fun h t -> (GT.show logic string_of_int) @@ ManualReifiers.int_reifier h t) in *)
  conde
  [ fresh (t)
    (term === !!1)
    (* (t === !!2) *)

  ; fresh (es)
      (success)

  ]


(* let foo q =
  let (===) = unitrace (fun h x -> show_intl @@ ManualReifiers.int_reifier h x) in
  conde
    [ fresh (x)
        (q === !!1)
        (x === !!2)
        (* (y === !!3) *)
    ; fresh (x y)
        (q === !!4)
        (x === !!5)
        (y === !!6)
    ] *)

open Tester
let () =
  run_exn string_of_int 1 q qh (REPR (evalo))
