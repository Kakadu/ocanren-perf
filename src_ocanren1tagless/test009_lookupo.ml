open MiniKanren
open Tester
open Printf

let nels = 10
let input_ints = Array.init nels (fun i -> (2*i+1, 2*i+2)) |> Array.to_list
  (* |> List.map (fun ((a,b) as r) -> printf "(%d, %d)\n%!" a b; r) *)
  |> List.map (fun (x,y) -> inj_pair (inj@@lift x) (inj@@lift y) ) |> inj_list

let shower = GT.(show int)

let rec lookupo p xs q =
  conde
    [ (xs === nil ()) &&& failure
    ; Fresh.three (fun a b tl ->
        ((inj_pair a b) % tl === xs) &&&
        conde
          [ (p === a) &&& (b === q)
          ; lookupo p tl q
          ]
      )
    ]

let () =
  run_exn shower  (-1)  qr qrh (REPR (fun q r -> lookupo q input_ints r ))
