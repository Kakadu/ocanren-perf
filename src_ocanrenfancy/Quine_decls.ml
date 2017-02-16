(*
  Quines stuff by Dmitrii Rozplokhas. Adopted from
  https://raw.githubusercontent.com/rozplokhas/OCanren/master/regression/test015.ml
*)

open Printf
open GT
open MiniKanren

module Gterm = struct
  module X = struct
    @type ('s, 'xs) t =
      | Symb  of 's
      | Seq   of 'xs with show
      ;;
    let fmap f g = function
    | Symb s -> Symb (f s)
    | Seq xs -> Seq (g xs)
  end
  include X
  include Fmap2(X)

  type rterm = (string, rterm List.ground) X.t
  type lterm = (string logic, lterm List.logic) X.t logic
  type fterm = (rterm, lterm) fancy

  let rec show_rterm : rterm -> string = fun t -> GT.(show X.t (fun s -> s) (show List.ground show_rterm)) t
  let rec show_lterm : lterm -> string = fun x -> show_logic GT.(show X.t (show_logic (fun s -> s)) (show List.logic show_lterm) ) x

  let symb s : fterm = inj @@ distrib @@ Symb s
  let seq xs : fterm = inj @@ distrib @@ Seq xs
end

let rec gterm_reifier c : Gterm.fterm -> Gterm.lterm =
  Gterm.reifier ManualReifiers.string_reifier (List.reifier gterm_reifier) c

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

let (_: ('a,'b) fancy -> (('a * 'c) List.ground, ('b * 'd) logic List.logic) fancy -> ('c,'d) fancy -> goal) = lookupo

let rec not_in_envo x env =
  conde [
    (env === nil());
    fresh (y t env')
      (env === (inj_pair y t) % env')
      (y =/= x)
      (not_in_envo x env')
  ]

module Gresult = struct
  module X = struct
    @type ('s, 't, 'xs) t =
    | Closure of 's * 't * 'xs
    | Val     of 't
    with show;;

    let fmap f g h = function
    | Closure (a,b,c) -> Closure (f a, g b, h c)
    | Val b -> Val (g b)
  end

  (* include X *)
  include Fmap3(X)

  type rresult = (string, Gterm.rterm, (string * rresult) List.ground) X.t
  type lresult = (string logic, Gterm.lterm, (string logic * lresult) logic List.logic) X.t logic
  type fresult = (rresult, lresult) fancy

  let closure s t xs = inj @@ distrib @@ X.Closure (s,t,xs)
  let val_ t         = inj @@ distrib @@ X.Val t

  let show_string = GT.(show string)
  let show_stringl = show_logic show_string

  let rec show_rresult r = GT.(show X.t show_string Gterm.show_rterm
      @@ show List.ground (show pair show_string show_rresult)) r
  let rec show_lresult r = show_logic GT.(show X.t show_stringl Gterm.show_lterm
    @@ show List.logic (show_logic @@ show pair show_stringl show_lresult)) r

end


let rec gresult_reifier c : Gresult.fresult -> Gresult.lresult =
  let open ManualReifiers in
  Gresult.reifier string_reifier gterm_reifier
    (List.reifier (pair_reifier string_reifier gresult_reifier))
    c

let (_: var_checker -> Gresult.fresult -> Gresult.lresult) = gresult_reifier

let (!!) x = inj @@ lift x

open Gterm
open Gresult

type fenv = ( (string * rresult) List.ground, (string logic * lresult) logic List.logic) fancy

let rec map_evalo es env rs =
  conde [
    (es === nil ()) &&& (rs === nil ());
    fresh (e es' r rs')
      (es === e % es')
      (rs === r % rs')
      (evalo e env (val_ r))
      (map_evalo es' env rs')
  ]
and evalo (term: fterm) (env: fenv) (r: fresult) =
  conde [
    fresh (t)
      (term === seq ((symb !!"quote") %< t))
      (r === (val_ t))
      (not_in_envo !!"quote" env);
    fresh (s)
      (term === (symb s))
      (lookupo s env r);
    fresh (x body)
      (term === seq ( (symb !!"lambda") %
                      (seq (!< (symb x)) %< body)
                    ) )
      (r === (closure x body env))
      (not_in_envo !!"lambda" env);
    fresh (es rs)
      (term === seq ((symb !!"list") % es) )
      (r === val_ (seq rs))
      (not_in_envo !!"list" env)
      (map_evalo es env rs)(*(List.mapo (fun e r -> evalo e env !!(Val r)) es rs)*);
    fresh (func arge arg x body env')
      (term === seq (func %< arge))
      (evalo arge env arg)
      (evalo func env (closure x body env') )
      (evalo body ((inj_pair x arg) % env') r)
  ]

let ( ~~ ) s  = symb @@ inj @@ lift s
let s      tl = seq (inj_list tl)

let nil = nil ()
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

let run_term (text,t) = printf "> %s\n%!%s\n\n%!" text @@
  run q (fun q -> evalo t nil (val_ q)) (fun qs -> if Stream.is_empty qs
                                          then "fail"
                                          else match Stream.hd qs with
                                          | Final x -> show_rterm @@ Obj.magic x
                                          | HasFreeVars (f,x) ->
                                            let c = (object method isVar: 'a . 'a -> bool = fun x -> f @@ Obj.repr x end) in
                                            show_lresult @@ gresult_reifier c (Obj.magic x)
                                        )

let quine_c =
  s[s[~~"lambda"; s[~~"x"];
      s[~~"list"; ~~"x"; s[~~"list"; s[~~"quote"; ~~"quote"]; ~~"x"]]];
    s[~~"quote";
      s[~~"lambda"; s[~~"x"];
        s[~~"list"; ~~"x"; s[~~"list"; s[~~"quote"; ~~"quote"]; ~~"x"]]]]]

let () =
  printf "Evaluate:\n\n%!";
  run_term (REPR( ~~"x" ));
  run_term (REPR( (s[s[~~"quote"; ~~"x"]; s[~~"quote"; ~~"y"]]) ));
  run_term (REPR( s[~~"quote"; ~~"x"; ~~"y"] ));
  run_term (REPR( s[~~"quote"; ~~"x"] ));
  run_term (REPR( s[~~"list"] ));
  run_term (REPR( s[~~"list"; s[~~"quote"; ~~"x"]; s[~~"quote"; ~~"y"]] ));
  run_term (REPR( s[s[~~"lambda"; s[~~"x"]; ~~"x"]; s[~~"list"]]        ));
  run_term (REPR( s[ s[ s[~~"lambda"; s[~~"x"]; s[~~"lambda"; s[~~"y"]; s[~~"list"; ~~"x"; ~~"y"]]]; s[~~"quote"; ~~"1"]];
                     s[ ~~"quote"; ~~"2"]] ));
  run_term (REPR( s[s[~~"lambda"; s[~~"lambda"]; s[~~"lambda"; s[~~"list"]]]; s[~~"lambda"; s[~~"x"]; ~~"x"]] ));
  run_term (REPR( s[~~"quote"; ~~"list"] ));
  run_term (REPR( quine_c ));
  ()
;;
(*
let gen_terms n r = printf "> %s\n" (show_term r);
  run q (fun q -> evalo q nil (val_ r))
    (fun qs -> List.iter (fun t -> printf "%s\n" @@ show_term t) @@
      Stream.take ~n:n qs);
  Printf.printf "\n"
*)

(*
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