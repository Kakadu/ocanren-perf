open OCanren
open Ulc_defs
open GLam

let rec substo in_ x newval rez =
  conde
    [ fresh y (in_ === v y) (y === x) (rez === newval)
    ; fresh
        (m n m2 n2)
        (in_ === app m n)
        (rez === app m2 n2)
        (substo m x newval m2)
        (substo n x newval n2)
    ; fresh
        (v b)
        (in_ === abs v b)
        (conde
           [ x === v &&& (rez === in_)
           ; fresh b' (rez === abs v b') (substo b x newval b')
           ])
    ]
;;

let rec evalo m n =
  conde
    [ fresh x (m === v x) (n === m)
    ; fresh (x l) (m === abs x l) (n === m)
    ; fresh
        (f a f' a')
        (m === app f a)
        (evalo f f')
        (evalo a a')
        (conde
           [ fresh (x l l') (f' === abs x l) (substo l x a' l') (evalo l' n)
           ; fresh (p q) (f' === app p q) (n === app f' a')
           ; fresh x (f' === v x) (n === app f' a')
           ])
    ]
;;

let wrap_lam rr = rr#reify glam_reifier |> show_llam
let quine q = fresh (l r) (q === app l r) (evalo q q)

let __ () =
  OCanren.run q quine Fun.id
  |> Stream.take ~n:10
  |> List.iter (fun lam -> Format.printf "%s\n%!" (wrap_lam lam))
;;

let thrine q r s = evalo (app q r) s &&& evalo (app r s) q &&& evalo (app s q) r

let wrap_3_lam ppf (a, b, c) =
  Format.fprintf
    ppf
    "(%s %s %s)"
    (show_llam (a#reify glam_reifier))
    (show_llam (b#reify glam_reifier))
    (show_llam (c#reify glam_reifier))
;;

let () =
  OCanren.run qrs thrine (fun a b c -> a, b, c)
  |> Stream.take ~n:2
  |> List.iter (fun lam -> Format.printf "%a\n%!" wrap_3_lam lam)
;;
