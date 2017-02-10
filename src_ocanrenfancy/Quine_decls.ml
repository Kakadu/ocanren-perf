(*
  Quines stuff by Dmitrii Rozplokhas. Adopted from
  https://raw.githubusercontent.com/rozplokhas/OCanren/master/regression/test015.ml
*)

open Printf
open GT
open MiniKanren

module Gterm = struct
  @type ('s,'list) t =
    | Symb  of 's
    | Seq   of 'list with show
    ;;
  let fmap f g = function
  | Symb s -> Symb (f s)
  | Seq xs -> Seq (g xs)
end

type fterm = ((string      , 'b List.flist) Gterm.t as 'b) fancy

let rec show_fterm (x: fterm fancy) =
  let rec helper (x: fterm) = GT.(show Gterm.t (show string) (List.show helper)) x in
  helper x

let (_: fterm fancy -> string) = show_fterm

type lterm = (string logic, lterm logic List.logic) Gterm.t lterm

let rec show_lterm (x: lterm logic) =
  show_logic GT.(show Gterm.t (show_logic (show string)) (show List.logic show_lterm)) x

let (_: lterm logic -> string) = show_lterm

module Wrappers = Fmap2(Gterm)
let symb s = Wrappers.fmap (Gterm.Symb s)
let seq xs = Wrappers.fmap (Gterm.Seq xs)

let (_: string fancy -> fterm fancy) = symb
(*
let rec show_fterm = (function
  | Symb s -> show_fancy (fun x -> x) s
  | Seq  s ->
      "(" ^ String.concat " " (List.map (show logic show_term) (List.to_list @@ List.prj (fun x -> x) s)) ^ ")")
*)
(* let show_term = show logic show_term *)

let list_combine3 xs ys zs =
  let rec helper acc = function
    | (x::xs, y::ys, z::zs) -> helper ((x,y,z)::acc) (xs,ys,zs)
    | ([],[],[]) -> List.rev acc
    | _ -> failwith "bad argument of list_combine3"
  in
  helper [] (xs,ys,zs)

let rec lookupo x env r =
  fresh (y t env')
    (env === (inj_pair y t) % env')
    (conde [
        (y === x) &&& (r === t);
        (y =/= x) &&& (lookupo x env' r)
      ])

let rec not_in_envo x env =
  conde [
    (env === nil());
    fresh (y t env')
      (env === (inj_pair y t) % env')
      (y =/= x)
      (not_in_envo x env')
  ]
;;

module Gresult = struct
  @type ('s, 't, 'xs) t =
  | Closure of 's * 't * 'xs
  | Val     of 't
  with show;;

  let fmap f g h = function
  | Closure (a,b,c) -> Closure (f a, g b, h c)
  | Val b -> Val (g b)
end

module Fresult = struct
  open Gresult
  type t = ((string, term, (string * 'b) List.flist) Gresult.t as 'b) fancy
end

module FMapResult = Fmap3(Gresult)
let closure s t xs : Fresult.t = FMapResult.fmap @@ Gresult.Closure (s,t,xs)
let val_ t = FMapResult.fmap @@ Gresult.Val t

type fresult = Fresult.t

let rec show_fresult (x: fresult) =
  let open GT in
  let show_pair: (string * _) -> string = show pair (show_fancy (show string)) show_fresult in
  and helper x = GT.(show Gresult.t (show string)) show_term (List.show show_pair)) x in
  show_fancy helper x

let (_: fresult -> string) = show_fresult

type lresult = (string logic, lterm logic, (string logic * lresult) logic List.logic) Gresult.t logic
let rec show_lresult (x: lresult) =
  let p : (string logic * lresult) logic -> _ = show_logic GT.(show pair (show_logic (fun s -> s)) show_lresult) in
  show_logic GT.(show Gresult.t (show_logic (show string)) show_lterm (show List.logic p)) x

let (_: lresult -> string) = show_lresult
(* type result =
  | Closure of string logic * term logic * (string logic * result logic) logic List.logic
  | Val     of term   logic *)

let (_: fterm fancy) = symb @@injlift ""

let (!!) = injlift
let rec map_evalo es env rs =
  conde [
    (es === nil ()) &&& (rs === nil ());
    fresh (e es' r rs')
      (es === e % es')
      (rs === r % rs')
      (evalo e env (val_ r))
      (map_evalo es' env rs')
  ]
and evalo term env r =
  conde [
    fresh (t)
      (term === seq (symb !!"quote") %< t)
      (r === (val_ t))
      (not_in_envo !!"quote" env);
    fresh (s)
      (term === (symb s))
      (lookupo s env r);
    fresh (x body)
      (term === !!(Seq (!!(Symb !!"lambda")      %
                        (!!(Seq (!< !!(Symb x))) %<
                         body))))
      (r === closure x body env)
      (not_in_envo !!"lambda" env);
    fresh (es rs)
      (term === (seq @@ symb !!"list") % es)
      (r === val_ (seq rs))
      (not_in_envo !!"list" env)
      (map_evalo es env rs)(*(List.mapo (fun e r -> evalo e env !!(Val r)) es rs)*);
    fresh (func arge arg x body env')
      (term === seq (func %< arge))
      (evalo arge env arg)
      (evalo func env closure x body env')
      (evalo body ((inj_pair x arg) % env') r)
  ]

let ( ~~ ) s  = symb s
let s      tl = seq (inj_list tl)

let quineo q =
  fresh (x y)
    (evalo q nil (val_ q))

let twineso q p =
  (q =/= p) &&& (evalo q nil (val_ p)) &&& (evalo p nil (val_ q))

let thrineso q p r =
  (q =/= p) &&& (p =/= r) &&& (r =/= q) &&&
  (evalo p nil (val_ q)) &&&
  (evalo q nil (val_ r)) &&&
  (evalo r nil (val_ p))


let run_term t = Printf.printf "> %s\n%s\n\n" (show_term t) @@
  run q (fun q -> evalo t nil (val_ q)) (fun qs -> if Stream.is_empty qs
                                           then "fail"
                                           else show_term @@ Stream.hd qs)

let gen_terms n r = Printf.printf "> %s\n" (show_term r);
  run q (fun q -> evalo q nil (val_ r))
    (fun qs -> List.iter (fun t -> printf "%s\n" @@ show_term t) @@
      Stream.take ~n:n qs);
  Printf.printf "\n"

let find_quines n = run q quineo
    (fun qs -> List.iter (fun t -> printf "%s\n\n" @@ show_term t) @@
      Stream.take ~n qs)

let find_twines n =
  run qr (fun q r -> twineso q r)
    (fun qs rs ->
      List.iter (fun (q,r) -> printf "%s,\n%s\n\n" (show_term q) (show_term r)) @@
      List.combine (Stream.take ~n qs) (Stream.take ~n rs))

let find_thrines n =
  run qrs thrineso
    (fun qs rs ss ->
      List.iter (fun (q,r,s) -> printf "%s,\n\t%s,\n\t%s\n\n" (show_term q) (show_term r) (show_term s))
      @@ list_combine3 (Stream.take ~n qs) (Stream.take ~n rs) (Stream.take ~n ss))

let quine_c =
  s[s[~~"lambda"; s[~~"x"];
      s[~~"list"; ~~"x"; s[~~"list"; s[~~"quote"; ~~"quote"]; ~~"x"]]];
    s[~~"quote";
      s[~~"lambda"; s[~~"x"];
        s[~~"list"; ~~"x"; s[~~"list"; s[~~"quote"; ~~"quote"]; ~~"x"]]]]]
(*
let _ =
  Printf.printf "Evaluate:\n\n%!";
  run_term @@ ~~"x";
  run_term @@ s[s[~~"quote"; ~~"x"]; s[~~"quote"; ~~"y"]];
  run_term @@ s[~~"quote"; ~~"x"; ~~"y"];
  run_term @@ s[~~"quote"; ~~"x"];
  run_term @@ s[~~"list"];
  run_term @@ s[~~"list"; s[~~"quote"; ~~"x"]; s[~~"quote"; ~~"y"]];
  run_term @@ s[s[~~"lambda"; s[~~"x"]; ~~"x"]; s[~~"list"]];
  run_term @@ s[s[s[~~"lambda"; s[~~"x"]; s[~~"lambda"; s[~~"y"]; s[~~"list"; ~~"x"; ~~"y"]]]; s[~~"quote"; ~~"1"]]; s[~~"quote"; ~~"2"]];
  run_term @@ s[s[~~"lambda"; s[~~"lambda"]; s[~~"lambda"; s[~~"list"]]]; s[~~"lambda"; s[~~"x"]; ~~"x"]];
  run_term @@ s[~~"quote"; ~~"list"];
  run_term @@ quine_c;

  Printf.printf "%!Generate:\n\n%!";
  gen_terms 5 @@ ~~"x";
  gen_terms 5 @@ s[];
  gen_terms 5 @@ s[~~"lambda"; s[~~"x"]; s[~~"x"; ~~"y"; ~~"z"]];

  Printf.printf "%!Quines:\n\n%!";
  find_quines 5;

  Printf.printf "%!Twines:\n\n%!";
  find_twines ()
  *)
