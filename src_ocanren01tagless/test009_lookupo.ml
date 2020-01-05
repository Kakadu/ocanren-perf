open OCanren
open Tester
open Printf

let nels = 10
let input_ints = Array.init nels (fun i -> (2*i+1, 2*i+2)) |> Array.to_list
  (* |> List.map (fun ((a,b) as r) -> printf "(%d, %d)\n%!" a b; r) *)
  |> List.map (fun (x,y) -> Std.Pair.pair (inj@@lift x) (inj@@lift y) ) |> Std.List.list

let shower = GT.(show int)

let rec lookupo p xs q =
  let open OCanren.Std in
  conde
    [ (xs === nil ()) &&& failure
    ; Fresh.three (fun a b tl ->
        ((Std.Pair.pair a b) % tl === xs) &&&
        conde
          [ (p === a) &&& (b === q)
          ; lookupo p tl q
          ]
      )
    ]

let () =
  run_exn shower  (-1)  qr qrh (REPR (fun q r -> lookupo q input_ints r ))
