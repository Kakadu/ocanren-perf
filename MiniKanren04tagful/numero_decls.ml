(* Relational arithmentics using binary numbers *)
open OCanren
open OCanren.Std
open Tester
include Counters.Make ()

type ioleg = (int, int logic) Std.List.injected

module Oleg = struct
  type ibit = (int, int OCanren.logic) OCanren.injected
  type injected = (int, int OCanren.logic) Std.List.injected
  type ground = int GT.list

  let prj_exn : (int GT.list, int GT.list) Reifier.t = List.prj_exn OCanren.prj_exn
  let show_ground = GT.show GT.list (GT.show GT.int)

  type logic = int OCanren.logic List.logic

  let reify : (int GT.list, logic) Reifier.t = List.reify OCanren.reify

  let show_logic : logic -> string =
    Format.asprintf "%a" [%fmt: GT.int OCanren.logic List.logic]
  ;;
end

IFDEF TRACE THEN

include struct
  let are_unifications_silent =
    match Sys.getenv "SILENT_UNIFICATIONS" with
    | exception Not_found -> false
    | _ -> true

  (* let pp st x =
    (GT.show MiniKanren.logic @@ GT.show GT.int) @@
    reify_in_state st OCanren.reify x *)

  let ( ==== ) : Oleg.ibit -> Oleg.ibit -> goal =
   fun x y st ->
    incr_counter ();
    (* if not are_unifications_silent then
      Printf.printf "%s %s\n" (pp st x) (pp st y); *)
    OCanren.( === ) x y st
   [@@inline]

  let ( === ) : Oleg.injected -> Oleg.injected  -> goal =
   fun x y st ->
    incr_counter ();
    (* if not are_unifications_silent then
      Printf.printf "%s %s\n" (pp st x) (pp st y); *)
    OCanren.( === ) x y st
   [@@inline]
end
ELSE
  let ( ==== ) : (int, int logic) injected -> (int, int logic) injected  -> goal = OCanren.(===)
  let ( === ) : Oleg.injected  -> Oleg.injected  -> goal = OCanren.(===)
END
let inj x = OCanren.(inj (lift x))
let rec build_num = function
  | 0 -> nil ()
  | n when n mod 2 == 0 -> inj 0 % build_num (n / 2)
  | n -> inj 1 % build_num (n / 2)
;;

let rec appendo l s out =
  conde
    [ l === Std.nil() &&& (s === out)
    ; fresh (a d res) (a % d === l) (a % res === out) (appendo d s res)
    ]
;;

let ( ! ) = inj

type injected = Oleg.injected
let zero : injected = Std.nil()
let one : injected = !<(! 1)
let three : injected = !1 % !<(!1)

let zeroo n = zero === n
let poso  n =
  fresh (h t)
    ((===) n (h % t))

let gt1o n =
  fresh (a ad dd)
    ((===) n (a % (ad % dd)))

(** Satisfies [b] + [x] + [y] = [r] + 2 * [c]  *)
let full_addero b x y r c =
  conde
    [ !0 ==== b &&& (!0 ==== x) &&& (!0 ==== y) &&& (!0 ==== r) &&& (!0 ==== c)
    ; !1 ==== b &&& (!0 ==== x) &&& (!0 ==== y) &&& (!1 ==== r) &&& (!0 ==== c)
    ; !0 ==== b &&& (!1 ==== x) &&& (!0 ==== y) &&& (!1 ==== r) &&& (!0 ==== c)
    ; !1 ==== b &&& (!1 ==== x) &&& (!0 ==== y) &&& (!0 ==== r) &&& (!1 ==== c)
    ; !0 ==== b &&& (!0 ==== x) &&& (!1 ==== y) &&& (!1 ==== r) &&& (!0 ==== c)
    ; !1 ==== b &&& (!0 ==== x) &&& (!1 ==== y) &&& (!0 ==== r) &&& (!1 ==== c)
    ; !0 ==== b &&& (!1 ==== x) &&& (!1 ==== y) &&& (!0 ==== r) &&& (!1 ==== c)
    ; !1 ==== b &&& (!1 ==== x) &&& (!1 ==== y) &&& (!1 ==== r) &&& (!1 ==== c)
    ]
;;

(** Adds a carry-in bit [d] to arbitrarily large numbers [n] and [m] to produce a number [r]. *)
let rec addero d n m r st =
  conde
    [ !0 ==== d &&& (m === nil ()) &&& (n === r)
    ; !0 ==== d &&& (n === nil ()) &&& (m === r) &&& poso m
    ; !1 ==== d &&& (m === nil ()) &&& (addero !0 n one r)
    ; !1 ==== d &&& (n === nil ()) &&& poso m &&& (addero !0 one m r)
    ; (n === one) &&&
      (m === one) &&&
      (fresh (a c) (a %< c === r) (full_addero d !1 !1 a c))
    ; n === one &&& gen_addero d n m r
    ; m === one &&& gt1o n &&& gt1o r &&& (addero d one n r)
    ; gt1o n &&& gen_addero d n m r
    ] st

and gen_addero d n m r =
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

let rec bound_multo q p n m =
  conde
    [ q === zero &&& poso p
    ; fresh
        (a0 a1 a2 a3 x y z)
        (q === a0 % x)
        (p === a1 % y)
        (conde
           [ n === zero &&& (m === a2 % z) &&& bound_multo x y z zero
           ; n === a3 % z &&& bound_multo x y z m
           ])
    ]
;;

let rec multo n m p =
  conde
    [ ((===) n zero) &&& ((===) p zero)
    ; poso n &&& ((===) m zero) &&& ((===) p zero)
    ; ((===) n one) &&& poso m &&& ((===) m p)
    ; gt1o n &&& ((===) m one) &&& ((===) n p)
    ; fresh (x z)
         ((===) n (!0 % x))
         (poso x)
         ((===) p (!0 % z))
         (poso z)
         (gt1o m)
         (multo x m z)
    ; fresh (x y)
        ((===) n (!1 % x))
        (* (debug_var x (Fun.flip num_reifier) (function [q] ->
                let () = Format.printf "x = %s\n%!" (show_logic q) in
                success)) *)
        (poso x)
        ((===) m (!0 % y))
        (poso y)
        (multo m n p)
    ; fresh (x y)
        ((===) n (!1 % x))
        (poso x)
        ((===) m (!1 % y))
        (poso y)
        (odd_multo x n m p)
    ]

and odd_multo x n m p =
  fresh (q)
    (bound_multo q p n m)
    (multo x m q)
    (pluso (!0 % q) m p)
;;

(** have the same length *)
let rec eqlo n m =
  conde
    [ n === zero &&& (m === zero)
    ; n === one &&& (m === one)
    ; fresh (a x b y) (a % x === n) (poso x) (b % y === m) (poso y) (eqlo x y)
    ]
;;

(** [n] has smaller length than [m] *)
let rec ltlo n m =
  conde
    [ n === zero &&& poso m
    ; n === one &&& gt1o m
    ; fresh (a x b y) (a % x === n) (poso x) (b % y === m) (poso y) (ltlo x y)
    ]
;;

let lelo n m = conde [ eqlo n m; ltlo n m ]
let lto n m = conde [ ltlo n m; (eqlo n m) &&& (fresh x (poso x) (pluso n x m)) ]
let leo n m = conde [ n === m; lto n m ]

let rec splito n r l h =
  conde
    [ n === zero &&& (h === zero) &&& (l === zero)
    ; fresh (b n') (n === !0 % (b % n')) (r === zero) (h === b % n') (l === zero)
    ; fresh n' (n === !1 % n') (r === zero) (n' === h) (l === one)
    ; fresh
        (b n' a r')
        (n === !0 % (b % n'))
        (a % r' === r)
        (l === zero)
        (splito (b % n') r' zero h)
    ; fresh
        (n' a r')
        (n === !1 % n')
        (r === a % r')
        (l === one)
        (splito n' r' zero h)
    ; fresh
        (b n' a r' l')
        (n === b % n')
        (r === a % r')
        (l === b % l')
        (poso l')
        (splito n' r' l' h)
    ]
;;

(** Satisfies n = m * q + r, with 0 <= r < m. *)
let rec divo n m q r =
  conde
    [ r === n &&& (q === zero) &&& lto n m
    ; q === one &&& eqlo n m &&& pluso r m n &&& lto r m
    ; ?&[ ltlo m n
        ; lto r m
        ; poso q
        ; fresh
            (nh nl qh ql qlm qlmr rr rh)
            (splito n r nl nh)
            (splito q r ql qh)
            (conde
               [ nh === zero &&& (qh === zero) &&& minuso nl r qlm &&& multo ql m qlm
               ; ?&[ poso nh
                   ; multo ql m qlm
                   ; pluso qlm r qlmr
                   ; minuso qlmr nl rr
                   ; splito rr r zero rh
                   ; divo nh m qh rh
                   ]
               ])
        ]
    ]
;;

let rec repeated_mul n q nq =
  conde
    [ poso n &&& (q === zero) &&& (nq === one)
    ; (q === one) &&& (n === nq)
    ; ?&[ gt1o q
        ; fresh (q1 nq1) (pluso q1 one q) (repeated_mul n q1 nq1) (multo nq1 n nq)
        ]
    ]
;;

let rec exp2 n b q =
  conde
    [ n === one &&& (q === zero)
    ; ?&[ gt1o n; q === one; fresh s (splito n b s one) ]
    ; fresh
        (q1 b2)
        (q === !0 % q1)
        (poso q1)
        (ltlo b n)
        (appendo b (!1 % b) b2)
        (exp2 n b2 q1)
    ; fresh
        (q1 nh b2 s)
        (q === !1 % q1)
        (poso q1)
        (poso nh)
        (splito n b s nh)
        (appendo b (!1 % b) b2)
        (exp2 nh b2 q1)
    ]
;;

let logo n b q r =
  conde
    [ n === one &&& poso b &&& (q === zero) &&& (r === zero)
    ; q === zero &&& lto n b &&& pluso r one n
    ; ((===) q one) &&& gt1o b &&& eqlo n b &&& pluso r b n
    ; q === one &&& poso q &&& pluso r one n
    ; b === zero &&& poso q &&& (r === n)
    ; (b === (!0 %< !1)) &&&
      (fresh
          (a ad dd)
          (poso dd)
          (n === a % (ad % dd))
          (exp2 n (nil ()) q)
          (fresh s (splito n dd r s)))
    ; ?&[ fresh (a ad add ddd) (conde [ b === three; b === a % (ad % (add % ddd)) ])
        ; ltlo b n
        ; fresh
            (bw1 bw nw nw1 ql1 ql s)
            (exp2 b zero bw1)
            (pluso bw1 one bw)
            (ltlo q n)
            (fresh
               (q1 bwq1)
               (pluso q one q1)
               (multo bw q1 bwq1)
               (lto nw1 bwq1))
            (exp2 n zero nw1)
            (pluso nw1 one nw)
            (divo nw bw ql1 s)
            (pluso ql one ql1)
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
                  (lto n bq1)))
        ]
    ]
;;

let expo b q n = logo n b q zero
(* let test17 n m = lelo n m &&& multo n (build_num 2) m *)
(* let test27 b q r = logo (build_num 68) b q r &&& gt1o q *)
let show_num = GT.(show List.ground @@ show int)
let show_num_logic = GT.(show List.logic @@ show logic @@ show int)

(* let _ffoo _ =
   run_exn show_num (-1)  qr qrh (REPR (fun q r     -> multo q r (build_num 1)                          ));
   run_exn show_num (-1)   q  qh (REPR (fun q       -> multo (build_num 7) (build_num 63) q             ));
   run_exn show_num (-1)  qr qrh (REPR (fun q r     -> divo (build_num 3) (build_num 2) q r             ));
   run_exn show_num (-1)   q  qh (REPR (fun q       -> logo (build_num 14) (build_num 2) (build_num 3) q));
   run_exn show_num (-1)   q  qh (REPR (fun q       -> expo (build_num 3) (build_num 5) q               ));
   () *)

let num_reifier h = List.reify OCanren.reify h
let runL n = run_r num_reifier show_num_logic n
