open GT
open MiniKanren
open Tester

let show_nat_list = GT.(show List.ground @@ show Nat.ground)
let show_nat      = GT.(show Nat.ground)

(* Relational minimum/maximum (for nats only) *)
let minmaxo a b min max =
  let open Nat in
  conde
    [ (min === a) &&& (max === b) &&& (a <= b)
    ; (max === a) &&& (min === b) &&& (a >  b)
    ]

(* [l] is a (non-empty) list, [s] is its smallest element,
   [l'] --- all other elements
*)
let rec smallesto l s l' =
  conde
    [ (l === !< s) &&& (l' === nil())
    ; fresh (h t s' t' max)
        (l' === max % t')
        (l === h % t)
        (minmaxo h s' s max)
        (smallesto t s' t')
    ]

(* Relational sort *)
let rec sorto x y =
  conde
    [ (* either both lists are empty *)
      (x === nil()) &&& (y === nil())
    ; fresh (s xs xs')
      (* or the sorted one is a concatenation of the
        smallest element (s) and sorted list of all other elements (xs')
      *)
        (y === s % xs')
        (sorto xs xs')       (* 1 *)
        (smallesto x s xs)   (* 2 *)
    ]

open Tester
let () =
  run_exn show_nat_list  (-1)  q qh (REPR (fun q -> sorto q (inj_nat_list [0;1;2;3;4;5]) ))
