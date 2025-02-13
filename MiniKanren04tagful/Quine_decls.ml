(*
  Quines stuff by Dmitrii Rozplokhas. Adopted from
  https://raw.githubusercontent.com/rozplokhas/OCanren/master/regression/test015.ml
*)

open Printf
open GT
open OCanren
include Counters.Make()

let list_combine3 xs ys zs =
  let rec helper acc = function
    | (x::xs, y::ys, z::zs) -> helper ((x,y,z)::acc) (xs,ys,zs)
    | ([],[],[]) -> List.rev acc
    | _ -> failwith "bad argument of list_combine3"
  in
  helper [] (xs,ys,zs)

let list_iter3 f xs ys zs =
  let rec helper = function
    | (x::xs, y::ys, z::zs) -> f (x,y,z); helper (xs,ys,zs)
    | ([],[],[]) -> ()
    | _ -> failwith "bad argument of list_combine3"
  in
  helper (xs,ys,zs)

module Gterm = struct
  module X = struct
    type ('s, 'xs) t =
      | Symb  of 's
      | Seq   of 'xs [@@deriving gt ~plugins:{show}]

    let fmap f g = function
    | Symb s -> Symb (f s)
    | Seq xs -> Seq (g xs)

    let t = {t with
      gcata = ();
      plugins = object
        method gmap = fmap (* t.plugins#gmap *)
        method show fa fb bx =
           GT.transform(t)
              (fun fself -> object
                inherit ['a,'b, _] show_t_t (GT.lift fa) (GT.lift fb) fself
                method! c_Symb _ _s str =
                  sprintf "(symb '%s)" (fa str)
                method! c_Seq  _ _ xs =
                  sprintf "(seq %s)" (fb xs)
               end)
              ()
              bx
       end
    }

  end
  include X
  include Fmap2(X)

  type ground = (string, ground Std.List.ground) X.t
  type logic = (string OCanren.logic, logic Std.List.logic) X.t OCanren.logic
  type fterm = (ground, logic) injected

  let fmapt fa fb subj =
    let open Env.Monad in
    Env.Monad.return (GT.gmap t) <*> fa <*> fb <*> subj

  let reify: (ground, logic) Reifier.t =
    let open Env.Monad in
    Reifier.fix (fun self ->
      Reifier.reify <..>
        chain (Reifier.zed (Reifier.rework ~fv:(fmapt OCanren.reify (Std.List.reify self)))))

  let prj_exn : (ground, ground) Reifier.t =
    let open Env.Monad in
    Reifier.fix (fun self ->
      OCanren.prj_exn <..> chain (fmapt OCanren.prj_exn (Std.List.prj_exn self)))

  let rec show_rterm : ground -> string = fun t -> GT.(show X.t (fun s -> s) (show Std.List.ground show_rterm)) t
  let rec show_lterm : logic -> string =
    fun x -> GT.(show logic @@ show X.t (show logic (fun s -> s)) (show Std.List.logic show_lterm) ) x

  (* let rec to_logic : ground -> logic = fun term ->
    Value (GT.(gmap X.t) (fun s -> Value s) (Std.List.to_logic to_logic) term) *)

  let symb s : fterm = inj @@ distrib @@ Symb s
  let seq xs : fterm = inj @@ distrib @@ Seq xs
end

let gterm_reifier = Gterm.reify

module Gresult = struct
  module X = struct
    type ('s, 't, 'xs) t =
    | Closure of 's * 't * 'xs
    | Val     of 't
    [@@deriving gt ~plugins:{show; }]

    let fmap f g h = function
    | Closure (a,b,c) -> Closure (f a, g b, h c)
    | Val b -> Val (g b)

    let t = {t with
      gcata = ();
      plugins = object
        method gmap = fmap (* t.plugins#gmap *)
        method show = t.plugins#show
           (* GT.transform(t)
              (GT.lift fa) (GT.lift fb)
              (object inherit ['a,'b] show_t
                method c_Clos _ _ a b c  =
                  sprintf "(closure %s %s %s)" (a.GT.fx ()) (b.GT.fx ()) (c.GT.fx ())
                method c_Val  _ _ t =
                  sprintf "(val %s)" (t.GT.fx ())
               end)
              ()
              bx *)
       end
    }
  end

  include Fmap3(X)
  type ground = (string, Gterm.ground, (string * ground) Std.List.ground) X.t
  type logic = (string OCanren.logic, Gterm.logic, (string OCanren.logic * logic) OCanren.logic Std.List.logic) X.t OCanren.logic
  type fresult = (ground, logic) injected

  let closure s t xs = inj @@ distrib @@ X.Closure (s,t,xs)
  let val_ t         = inj @@ distrib @@ X.Val t
  let fmapt fa fb fc subj =
    let open Env.Monad in
    Env.Monad.return (GT.gmap X.t) <*> fa <*> fb <*> fc <*> subj

  let prj_exn : (ground, ground) Reifier.t =
    let open Env.Monad in
    Reifier.fix (fun self ->
      OCanren.prj_exn <..> chain (fmapt OCanren.prj_exn Gterm.prj_exn (Std.List.prj_exn (Std.Pair.prj_exn OCanren.prj_exn self))))

  let reify: (ground, logic) Reifier.t =
    let open Env.Monad in
    Reifier.fix (fun self ->
      Reifier.reify <..>
        chain (Reifier.zed (Reifier.rework ~fv:(fmapt OCanren.reify Gterm.reify (Std.List.reify (Std.Pair.reify OCanren.reify self))))))

  let show_string = GT.(show string)
  let show_stringl = GT.(show logic) show_string

  let rec show_rresult r = GT.(show X.t show_string Gterm.show_rterm
      @@ show Std.List.ground (show Std.Pair.t show_string show_rresult)) r
  let rec show_lresult r = GT.(show logic @@ show X.t show_stringl Gterm.show_lterm
    @@ show Std.List.logic (show logic @@ show Std.Pair.t show_stringl show_lresult)) r

    (* let rec to_logic : rresult -> lresult = fun res ->
      let arg3 xs = List.to_logic (fun (a,b) -> Value (Value a, to_logic b)) xs in
      Value (GT.(gmap X.t) (fun s -> Value s) (Gterm.to_logic) arg3 res) *)
end


let gresult_reifier = Gresult.reify

let (!!) x = inj @@ lift x

open Gterm
open Gresult

type fenv = ( (string * Gresult.ground) Std.List.ground,
              (string OCanren.logic * Gresult.logic) OCanren.logic Std.List.logic) injected

(* let show_reif_env h e =
  GT.(show Std.List.logic @@ show logic @@
        show Std.Pair.t  (show logic (fun s -> s)) show_lresult) @@
  (List.reify ManualReifiers.(pair string gresult_reifier))
  h e *)
(* let unienv ?loc = unitrace ?loc @@ show_reif_env *)

let show_reif_term h t = show_lterm @@ gterm_reifier h t
let show_reif_result h t = show_lresult @@ gresult_reifier h t
(* let uniresult ?loc = unitrace ?loc @@ show_reif_result *)
(* let uniterm ?loc = unitrace ?loc @@ show_reif_term *)
(* let uni_term_list ?loc =
  unitrace ?loc
    (fun h t -> GT.(fun t -> show List.logic Gterm.show_lterm t) @@
      (List.reify gterm_reifier h t)
    ) *)

(* let show_reif_string h t = GT.(show logic @@ show string) @@
  ManualReifiers.string h t
let unistring ?loc = unitrace ?loc @@ show_reif_string *)

let (=/=) = OCanren.(=/=)
let (=//=) = (=/=)

[@@@ocamlformat "disable"]

IFDEF TRACE THEN

 let ( === ) : Gterm.fterm -> Gterm.fterm -> goal =
   fun x y st ->
    incr_counter ();
    (* if not are_unifications_silent then
      Printf.printf "%s %s\n" (pp st x) (pp st y); *)
    OCanren.( === ) x y st
   [@@inline]
 let ( ===! ) : Gresult.fresult -> Gresult.fresult -> goal =
   fun x y st ->
    incr_counter ();
    (* if not are_unifications_silent then
      Printf.printf "%s %s\n" (pp st x) (pp st y); *)
    OCanren.( === ) x y st
   [@@inline]
 let ( ===!! ) : _ Std.List.injected -> _ Std.List.injected -> goal =
   fun x y st ->
    incr_counter ();
    (* if not are_unifications_silent then
      Printf.printf "%s %s\n" (pp st x) (pp st y); *)
    OCanren.( === ) x y st
   [@@inline]
 let ( ==== ) : (string, string OCanren.logic) injected -> (string, string OCanren.logic) injected -> goal =
   fun x y st ->
    incr_counter ();
    (* if not are_unifications_silent then
      Printf.printf "%s %s\n" (pp st x) (pp st y); *)
    OCanren.( === ) x y st
   [@@inline]
ELSE

let ( === ) = OCanren.( === )
let ( ===! ) = ( === )
let ( ===!! ) = ( === )
let ( ==== ) = ( === )
END

open OCanren.Std

let rec lookupo x env t =
  (* let (=/=) = diseqtrace show_reif_string in
  let (===) ?loc = unienv ?loc in
  let (===!) ?loc = unistring ?loc in
  let (===!!) ?loc = uniresult ?loc in *)

  fresh (rest y v)
    ((Std.pair y v) % rest ===!! env)
    (conde [
        (y ==== x) &&& (v ===! t);
        (y =/= x) &&& (lookupo x rest t)
      ])

let rec not_in_envo x env =
  (* let (=/=) = diseqtrace show_reif_string in
  let (===) ?loc = unienv ?loc in *)

  (* printfn "entering not_in_envo"; *)
  conde
    [ fresh (y v rest)
        (env ===!! (Std.pair y v) % rest)
        (y =/= x)
        (not_in_envo x rest)
    ; (nil () ===!! env)
    ]

let rec proper_listo es env rs =
  (* let (===) ?loc = uni_term_list ?loc in *)
  conde
    [ ((nil ()) ===!! es) &&& ((nil ()) ===!! rs)
    ; fresh (e d te td)
        (es ===!! e  % d)
        (rs ===!! te % td)
        (evalo e env (val_ te))
        (proper_listo d env td)
    ]

and evalo (term: fterm) (env: fenv) (r: fresult) =
  (* let (===)  ?loc = unitrace ?loc show_reif_term in
  let (===!) ?loc = unitrace ?loc show_reif_result in *)

  (* fun st -> *)
    (* let () = printfn "entering into evalo %s %s %s"
      (show_reif_term term) (show_reif_env env) (show_reif_result r) in *)
  conde
  [ fresh (t)
    (term === seq ((symb !!"quote") %< t))
    (r ===! (val_ t))
    (not_in_envo !!"quote" env)
  ; fresh (es rs)
      (term === seq ((symb !!"list") % es) )
      (r ===! val_ (seq rs))
      (not_in_envo !!"list" env)
      (proper_listo es env rs)
  ; fresh (s)
      (term === (symb s))
      (lookupo s env r)
  ; fresh (func arge arg x body env')
      (term === seq (func %< arge))
      (evalo arge env arg)
      (evalo func env (closure x body env') )
      (evalo body ((Std.pair x arg) % env') r)

  ; fresh (x body)
      (term === seq ( (symb !!"lambda") %
                      (seq (!< (symb x)) %< body)
                    ) )
      (not_in_envo !!"lambda" env)
      (r ===! (closure x body env))
  ]

(* let ( ~~ ) s  = symb @@ inj @@ lift s *)
let s      tl = seq (Std.list Fun.id tl)

let nil = Std.nil ()

(* let run_term (text,t) = printf "> %s\n%!%s\n\n%!" text @@
  run q (fun q -> evalo t nil (val_ q)) (fun qs ->
      if Stream.is_empty qs
      then "fail"
      else (Stream.hd qs)#refine gterm_reifier ~inj:Gterm.to_logic |> show_lterm
    ) *)
(*
let quine_c =
  s[s[~~"lambda"; s[~~"x"];
      s[~~"list"; ~~"x"; s[~~"list"; s[~~"quote"; ~~"quote"]; ~~"x"]]];
    s[~~"quote";
      s[~~"lambda"; s[~~"x"];
        s[~~"list"; ~~"x"; s[~~"list"; s[~~"quote"; ~~"quote"]; ~~"x"]]]]]

let _f () =
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

let gen_terms n r = printf "> %s\n" (show_term r);
  run q (fun q -> evalo q nil (val_ r))
    (fun qs -> List.iter (fun t -> printf "%s\n" @@ show_term t) @@
      Stream.take ~n:n qs);
  Printf.printf "\n"
*)

let quineso q = (evalo q nil (val_ q))

let twineso q p =
  (q =/= p) &&& (evalo q nil (val_ p)) &&& (evalo p nil (val_ q))

let thrineso x =
  (* let (=//=) = diseqtrace @@ show_reif_term in *)
  fresh (p q r)
    (p =//= q)
    (q =//= r)
    (r =//= p)
    (evalo p nil (val_ q))
    (evalo q nil (val_ r))
    (evalo r nil (val_ p))
    (OCanren.(===) (OCanren.Std.triple p q r) x)

(* let wrap_term rr = rr#refine gterm_reifier |> show_lterm
let wrap_result rr = rr#refine gresult_reifier |> show_lresult *)

let find_quines ~verbose n =
  run q quineso (fun r -> r#reify gterm_reifier)
  |> OCanren.Stream.take ~n
  |> Stdlib.List.iter (fun q -> if verbose then printf "%s\n\n" (show_lterm q) else ())
;;

let find_twines ~verbose n =
  run qr twineso (fun q r -> q#reify Gterm.reify, r#reify Gterm.reify)
  |> OCanren.Stream.take ~n
  |> Stdlib.List.iter (fun (q, r) ->
    if verbose then printf "%s,\n%s\n\n" (show_lterm q) (show_lterm r) else ())
;;

let wrap3terms = function
  | Var _ -> assert false
  | Value (a, b, c) ->
    printf
      "* %s\n  %s\n  %s\n\n"
      (Gterm.show_lterm a)
      (Gterm.show_lterm b)
      (Gterm.show_lterm c)
;;

let find_thrines ~verbose n =
  run q thrineso (fun r ->
    r#reify (Std.Triple.reify gterm_reifier gterm_reifier gterm_reifier))
  |> Stream.take ~n
  |> Stdlib.List.iter (fun a ->
    if verbose
    then (
      let () = wrap3terms a in
      print_newline ()))
;;


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
