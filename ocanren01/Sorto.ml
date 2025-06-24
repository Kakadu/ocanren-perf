(* SPDX-License-Identifier: LGPL-2.1-or-later *)

open Printf
open OCanren

(* Relational minimum/maximum (for nats only) *)
let minmaxo a b min max =
  let open Nat in
  conde
    [ (min === a &&& (max === b) &&& Std.Nat.(a <= b))
    ; (min === b &&& (max === a) &&& Std.Nat.(a > b))
    ]
;;

(* [l] is a (non-empty) list, [s] is its smallest element,
   [l'] --- all other elements
*)
let rec smallesto l s l' =
  conde
    [ l === Std.List.(!<s) &&& (l' === Std.nil ())
    ; fresh
        (h t s' t' max)
        (l' === Std.List.cons max t')
        (l === Std.List.cons h t)
        (minmaxo h s' s max)
        (smallesto t s' t')
    ]
;;

(* Relational sort *)
let rec sorto x y =
  conde
    [ (* either both lists are empty *)
      x === Std.nil () &&& (y === Std.nil ())
    ; (* or the sorted one is a concatenation of the
         smallest element (s) and sorted list of all other elements (xs')
      *)
      fresh (s xs xs') (y === Std.List.cons s xs') (sorto xs xs') (smallesto x s xs)
    ]
;;

(* Some shortcuts to make regular lists from relational ones *)
let int_list = Stdlib.List.map Std.Nat.to_int

(* Making regular sorting from relational one *)
let sort l =
  int_list
  @@ Stream.hd
  @@ run q (sorto @@ Std.nat_list l) (fun r -> r#reify (Std.List.prj_exn Std.Nat.prj_exn))
;;

(* A straightforward implementation of factorial *)
let rec fact = function
  | 0 -> 1
  | n -> n * fact (n - 1)
;;

(* Making permutations from relational sorting *)
let perm l =
  List.map int_list
  @@ Stream.take ~n:(fact @@ List.length l)
  @@ run
       q
       (fun q -> sorto q @@ Std.nat_list (List.sort Stdlib.compare l))
       (fun r -> r#reify (Std.List.prj_exn Std.Nat.prj_exn))
;;

(* More hardcore version: no standard sorting required *)
let perm' l =
  List.map int_list
  @@ Stream.take ~n:(fact @@ Stdlib.List.length l)
  @@ run
       q
       (fun q -> fresh r (sorto (Std.nat_list l) r) (sorto q r))
       (fun r -> r#reify (Std.List.prj_exn Std.Nat.prj_exn))
;;

(* Some auxilliary type shortcuts *)
type il = GT.int GT.list [@@deriving gt ~plugins:{ show }]
type ill = GT.int GT.list GT.list [@@deriving gt ~plugins:{ show }]

(* Entry point *)
let __ _ =
  (* Sorting: *)
  Printf.printf "%s\n\n%!" (GT.show il @@ sort []);
  Printf.printf "%s\n\n%!" (GT.show il @@ sort [ 1 ]);
  Printf.printf "%s\n\n%!" (GT.show il @@ sort [ 2; 1 ]);
  Printf.printf "%s\n\n%!" (GT.show il @@ sort [ 3; 2; 1 ]);
  Printf.printf "%s\n\n%!" (GT.show il @@ sort [ 4; 3; 2; 1 ]);
  (* Permutations: *)
  Printf.printf "%s\n\n%!" (GT.show ill @@ perm []);
  Printf.printf "%s\n\n%!" (GT.show ill @@ perm [ 1 ]);
  Printf.printf "%s\n\n%!" (GT.show ill @@ perm [ 1; 2 ]);
  Printf.printf "%s\n\n%!" (GT.show ill @@ perm [ 1; 2; 3 ]);
  Printf.printf "%s\n\n%!" (GT.show ill @@ perm [ 1; 2; 3; 4 ]);
  Printf.printf "%s\n\n%!" (GT.show ill @@ perm [ 1; 2; 3; 4; 5 ]);
  Printf.printf "%s\n\n%!" (GT.show ill @@ perm [ 1; 2; 3; 4; 5; 6 ]);
  Printf.printf "%s\n\n%!" (GT.show ill @@ perm' []);
  Printf.printf "%s\n\n%!" (GT.show ill @@ perm' [ 1 ]);
  Printf.printf "%s\n\n%!" (GT.show ill @@ perm' [ 1; 2 ]);
  Printf.printf "%s\n\n%!" (GT.show ill @@ perm' [ 1; 2; 3 ])
;;
