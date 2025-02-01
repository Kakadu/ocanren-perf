(* Relational arithmentics using binary numbers *)
open OCanren
open Tagged_stdlib.Std
open Tester

module Oleg = struct
  type injected = int OCanren.ilogic Wrapper.injected List.injected
  type ground = int GT.list

  let prj_exn : (injected, ground) Reifier.t =
    Reifier.fmap
      (List.list_of_ground (function Wrapper.W x -> x))
      (List.prj_exn (Wrapper.prj_exn OCanren.prj_exn))
  ;;

  let show_ground = GT.show GT.list (GT.show GT.int)

  type logic = int OCanren.logic Wrapper.logic List.logic

  let reify : (injected, logic) Reifier.t = List.reify (Wrapper.reify OCanren.reify)

  let show_logic : logic -> string =
    Format.asprintf "%a" [%fmt: GT.int OCanren.logic Wrapper.logic List.logic]
  ;;
end

let rec build_num : int -> Oleg.injected = function
  | 0 -> nil ()
  | n when n mod 2 == 0 -> Wrapper.w !!0 % build_num (n / 2)
  | n -> Wrapper.w !!1 % build_num (n / 2)
;;

let rec appendo l s out =
  let _ : _ List.injected = l in
  let _ : _ List.injected = s in
  let _ : _ List.injected = out in
  conde
    [ List.nullo l &&& (s === out)
    ; fresh (a d res) (a % d === l) (a % res === out) (appendo d s res)
    ]
;;

let poso : Oleg.injected -> _ = fun q -> fresh (h t) (q === h % t)
let gt1o (q : Oleg.injected) = fresh (h t tt) (q === h % (t % tt))
let ( ! ) x = Wrapper.w (inj x)

let full_addero b x y r c =
  conde
    [ !0 === b &&& (!0 === x) &&& (!0 === y) &&& (!0 === r) &&& (!0 === c)
    ; !1 === b &&& (!0 === x) &&& (!0 === y) &&& (!1 === r) &&& (!0 === c)
    ; !0 === b &&& (!1 === x) &&& (!0 === y) &&& (!1 === r) &&& (!0 === c)
    ; !1 === b &&& (!1 === x) &&& (!0 === y) &&& (!0 === r) &&& (!1 === c)
    ; !0 === b &&& (!0 === x) &&& (!1 === y) &&& (!1 === r) &&& (!0 === c)
    ; !1 === b &&& (!0 === x) &&& (!1 === y) &&& (!0 === r) &&& (!1 === c)
    ; !0 === b &&& (!1 === x) &&& (!1 === y) &&& (!0 === r) &&& (!1 === c)
    ; !1 === b &&& (!1 === x) &&& (!1 === y) &&& (!1 === r) &&& (!1 === c)
    ]
;;

let rec addero : _ -> Oleg.injected -> Oleg.injected -> Oleg.injected -> _ =
  fun d n m r ->
  conde
    [ !0 === d &&& (nil () === m) &&& (n === r)
    ; !0 === d &&& (nil () === n) &&& (m === r) &&& poso m
    ; !1 === d &&& (nil () === m) &&& defer (addero !0 n !<(!1) r)
    ; !1 === d &&& (nil () === n) &&& poso m &&& defer (addero !0 m !<(!1) r)
    ; ?&[ !<(!1) === n
        ; !<(!1) === m
        ; fresh (a c) (a %< c === r) (full_addero d !1 !1 a c)
        ]
    ; !<(!1) === n &&& gen_addero d n m r
    ; !<(!1) === m &&& gt1o n &&& gt1o r &&& defer (addero d !<(!1) n r)
    ; gt1o n &&& gen_addero d n m r
    ]

and gen_addero : _ -> Oleg.injected -> Oleg.injected -> Oleg.injected -> _ =
  fun d n m r ->
  fresh
    (a b c e x y z)
    (a % x === n)
    (b % y === m)
    (poso y)
    (c % z === r)
    (poso z)
    (full_addero d a b c e)
    (addero e x y z)
;;

let pluso n m k = addero !0 n m k
let minuso n m k = pluso m k n

let rec bound_multo
  : Oleg.injected -> Oleg.injected -> Oleg.injected -> Oleg.injected -> _
  =
  fun q p n m ->
  conde
    [ List.nullo q &&& poso p
    ; fresh
        (x y z)
        (List.tlo q x)
        (List.tlo p y)
        (conde
           [ List.nullo n &&& List.tlo m z &&& bound_multo x y z @@ nil ()
           ; List.tlo n z &&& bound_multo x y z m
           ])
    ]
;;

let rec multo : Oleg.injected -> Oleg.injected -> Oleg.injected -> _ =
  fun n m p ->
  conde
    [ nil () === n &&& (nil () === p)
    ; poso n &&& (nil () === m) &&& (nil () === p)
    ; !<(!1) === n &&& poso m &&& (m === p)
    ; gt1o n &&& (!<(!1) === m) &&& (n === p)
    ; fresh (x z) (!0 % x === n) (poso x) (!0 % z === p) (poso z) (gt1o m) (multo x m z)
    ; fresh (x y) (!1 % x === n) (poso x) (!0 % y === m) (poso y) (multo m n p)
    ; fresh (x y) (!1 % x === n) (poso x) (!1 % y === m) (poso y) (odd_multo x n m p)
    ]

and odd_multo x n m p =
  Fresh.one (fun q -> bound_multo q p n m &&& multo x m q &&& pluso (!0 % q) m p)
;;

let rec eqlo : Oleg.injected -> Oleg.injected -> _ =
  fun n m ->
  conde
    [ nil () === n &&& (nil () === m)
    ; !<(!1) === n &&& (!<(!1) === m)
    ; fresh (a x b y) (a % x === n) (poso x) (b % y === m) (poso y) (eqlo x y)
    ]
;;

let rec ltlo : Oleg.injected -> Oleg.injected -> _ =
  fun n m ->
  conde
    [ nil () === n &&& poso m
    ; !<(!1) === n &&& gt1o m
    ; fresh (a x b y) (a % x === n) (poso x) (b % y === m) (poso y) (ltlo x y)
    ]
;;

let lelo n m = conde [ eqlo n m; ltlo n m ]
let lto n m = conde [ ltlo n m; ?&[ eqlo n m; fresh x (poso x) (pluso n x m) ] ]
let leo n m = conde [ n === m; lto n m ]

let rec splito
  : Oleg.injected -> Oleg.injected -> Oleg.injected -> Oleg.injected -> OCanren.goal
  =
  fun n r l h ->
  conde
    [ nil () === n &&& (nil () === h) &&& (nil () === l)
    ; fresh (b n') (!0 % (b % n') === n) (nil () === r) (b % n' === h) (nil () === l)
    ; fresh n' (!1 % n' === n) (nil () === r) (n' === h) (!<(!1) === l)
    ; fresh
        (b n' a r')
        (!0 % (b % n') === n)
        (a % r' === r)
        (nil () === l)
        (splito (b % n') r' (nil ()) h)
    ; fresh
        (n' a r')
        (!1 % n' === n)
        (a % r' === r)
        (!<(!1) === l)
        (splito n' r' (nil ()) h)
    ; fresh
        (b n' a r' l')
        (b % n' === n)
        (a % r' === r)
        (b % l' === l)
        (poso l')
        (splito n' r' l' h)
    ]
;;

let rec divo
  : Oleg.injected -> Oleg.injected -> Oleg.injected -> Oleg.injected -> OCanren.goal
  =
  fun n m q r ->
  conde
    [ r === n &&& (nil () === q) &&& lto n m
    ; !<(!1) === q &&& eqlo n m &&& pluso r m n &&& lto r m
    ; ?&[ ltlo m n
        ; lto r m
        ; poso q
        ; fresh
            (nh nl qh ql qlm qlmr rr rh)
            (splito n r nl nh)
            (splito q r ql qh)
            (conde
               [ nil () === nh &&& (nil () === qh) &&& minuso nl r qlm &&& multo ql m qlm
               ; ?&[ poso nh
                   ; multo ql m qlm
                   ; pluso qlm r qlmr
                   ; minuso qlmr nl rr
                   ; splito rr r (nil ()) rh
                   ; divo nh m qh rh
                   ]
               ])
        ]
    ]
;;

let rec repeated_mul n q nq =
  conde
    [ poso n &&& (nil () === q) &&& (!<(!1) === nq)
    ; !<(!1) === q &&& (n === nq)
    ; ?&[ gt1o q
        ; fresh (q1 nq1) (pluso q1 !<(!1) q) (repeated_mul n q1 nq1) (multo nq1 n nq)
        ]
    ]
;;

let rec exp2 n b q =
  conde
    [ !<(!1) === n &&& (nil () === q)
    ; ?&[ gt1o n; !<(!1) === q; fresh s (splito n b s !<(!1)) ]
    ; fresh
        (q1 b2)
        (!0 % q1 === q)
        (poso q1)
        (ltlo b n)
        (appendo b (!1 % b) b2)
        (exp2 n b2 q1)
    ; fresh
        (q1 nh b2 s)
        (!1 % q1 === q)
        (poso q1)
        (poso nh)
        (splito n b s nh)
        (appendo b (!1 % b) b2)
        (exp2 nh b2 q1)
    ]
;;

let logo n b q r =
  conde
    [ !<(!1) === n &&& poso b &&& (nil () === q) &&& (nil () === r)
    ; nil () === q &&& lto n b &&& pluso r !<(!1) n
    ; !<(!1) === q &&& gt1o b &&& eqlo n b &&& pluso r b n
    ; !<(!1) === b &&& poso q &&& pluso r !<(!1) n
    ; nil () === b &&& poso q &&& (r === n)
    ; ?&[ !0 %< !1 === b
        ; fresh
            (a ad dd)
            (poso dd)
            (a % (ad % dd) === n)
            (exp2 n (nil ()) q)
            (fresh s (splito n dd r s))
        ]
    ; ?&[ fresh (a ad add ddd) (conde [ !1 %< !1 === b; a % (ad % (add % ddd)) === b ])
        ; ltlo b n
        ; fresh
            (bw1 bw nw nw1 ql1 ql s)
            (exp2 b (nil ()) bw1)
            (pluso bw1 !<(!1) bw)
            (ltlo q n)
            (fresh
               (q1 bwq1)
               (pluso q !<(!1) q1)
               (multo bw q1 bwq1)
               (lto nw1 bwq1)
               (exp2 n (nil ()) nw1)
               (pluso nw1 !<(!1) nw)
               (divo nw bw ql1 s)
               (pluso ql !<(!1) ql1)
               (lelo ql q)
               (fresh
                  (bql qh s qdh qd)
                  (repeated_mul b ql bql)
                  (divo nw bw1 qh s)
                  (pluso ql qdh qh)
                  (pluso ql qd q)
                  (leo qd qdh)
                  (fresh
                     (bqd bq1 bq)
                     (repeated_mul b qd bqd)
                     (multo bql bqd bq)
                     (multo b bq bq1)
                     (pluso bq r n)
                     (lto n bq1))))
        ]
    ]
;;

let expo : Oleg.injected -> Oleg.injected -> Oleg.injected -> _ =
  fun b q n -> logo n b q @@ nil ()
;;

(* let test17 n m = lelo n m &&& multo n (build_num 2) m *)
(* let test27 b q r = logo (build_num 68) b q r &&& gt1o q *)
(* let show_num num = Format.asprintf "%a" GT.(fmt List.ground (GT.fmt int)) num *)
(* let show_num_logic = Format.asprintf "%a" [%fmt: GT.int OCanren.logic List.logic] *)
(* GT.(show List.logic @@ show logic @@ show int) *)

(* let _ffoo _ =
   run_exn show_num (-1)  qr qrh (REPR (fun q r     -> multo q r (build_num 1)                          ));
   run_exn show_num (-1)   q  qh (REPR (fun q       -> multo (build_num 7) (build_num 63) q             ));
   run_exn show_num (-1)  qr qrh (REPR (fun q r     -> divo (build_num 3) (build_num 2) q r             ));
   run_exn show_num (-1)   q  qh (REPR (fun q       -> logo (build_num 14) (build_num 2) (build_num 3) q));
   run_exn show_num (-1)   q  qh (REPR (fun q       -> expo (build_num 3) (build_num 5) q               ));
   () *)

let num_reifier = Oleg.reify
let runL n = run_r num_reifier Oleg.show_logic n
