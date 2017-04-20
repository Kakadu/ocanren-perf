open Printf
open GT
open MiniKanren

let show_intl = GT.show logic string_of_int

let foo q =
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
    ]

open Tester
let () =
  run_exn string_of_int (2) q qh (REPR (foo))
