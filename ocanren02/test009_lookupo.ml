open OCanren
open Tester
open Printf

let nels = 10

type injected = (int ilogic, int ilogic) Std.Pair.groundi Std.List.groundi

let input_ints : injected =
  Array.init nels (fun i -> (2 * i) + 1, (2 * i) + 2)
  |> Array.to_list
  (* |> List.map (fun ((a,b) as r) -> printf "(%d, %d)\n%!" a b; r) *)
  |> List.map (fun (x, y) -> Std.Pair.pair !!x !!y)
  |> Std.List.list
;;

let show_intl = GT.show OCanren.logic (GT.show GT.int)
let shower = GT.show Std.List.logic (GT.show Std.Pair.logic show_intl show_intl)
let reifier () = Std.List.reify (Std.Pair.reify OCanren.reify OCanren.reify)

let rec lookupo p (xs : injected) q =
  let open OCanren.Std in
  conde
    [ xs === nil () &&& failure
    ; Fresh.three (fun a b tl ->
        Std.Pair.pair a b % tl === xs &&& conde [ p === a &&& (b === q); lookupo p tl q ])
    ]
;;

let () =
  run_r OCanren.reify show_intl (-1) qr qrh (REPR (fun q r -> lookupo q input_ints r))
;;
