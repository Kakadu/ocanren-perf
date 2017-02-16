open GT
open MiniKanren

(* Relational minimum/maximum (for nats only) *)
let minmaxo a b min max = Nat.(conde [
    (min === a) &&& (max === b) &&& (a <= b);
    (max === a) &&& (min === b) &&& (a >  b)
])

(* [l] is a (non-empty) list, [s] is its smallest element,
   [l'] --- all other elements
*)
let rec smallesto l s l' = conde [
  (l === !< s) &&& (l' === nil());
  fresh (h t s' t' max)
    (l' === max % t')
    (l === h % t) 
    (minmaxo h s' s max)
    (smallesto t s' t')
]

(* Relational sort *)
let rec sorto x y = conde [
  (* either both lists are empty *)
  (x === nil()) &&& (y === nil());
  fresh (s xs xs')
    (* or the sorted one is a concatenation of the
       smallest element (s) and sorted list of all other elements (xs')
    *)
    (y === s % xs')
    (sorto xs xs')       (* 1 *)
    (smallesto x s xs)   (* 2 *)
]

(* let show_int      = show(logic) (show int)
let show_int_list = show(List.logic) show_int
let show_nat_list   = show(List.logic) (show Nat.logic) *)
let show_int_list = GT.(show List.ground @@ show Nat.ground)

open Tester
let () =
  run_exn show_int_list   1  q qh (REPR (fun q -> sorto (inj_nat_list [4;3;2;1]) q ));
  ()
