(*
  Quines stuff by Dmitrii Rozplokhas. Adopted from
  https://raw.githubusercontent.com/rozplokhas/OCanren/master/regression/test015.ml
*)

open Printf
open GT
open OCanren

module Std = struct
  include Std

  module Triple = struct
    type ('a,'b,'c) ground = 'a * 'b * 'c [@@deriving gt ~options:{fmt;gmap}]
    module F = Fmap3(struct
        type ('a,'b,'c) t = ('a,'b,'c) ground
        let fmap eta = GT.gmap ground eta
      end)

    let reify fa fb fc = F.reify fa fb fc
    let make x y z = inj @@ F.distrib (x,y,z)
  end
end

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
      | Seq   of 'xs [@@deriving gt ~options:{show; fmt; gmap}]

    let fmap f g = function
    | Symb s -> Symb (f s)
    | Seq xs -> Seq (g xs)

    let t = {t with
      gcata = ();
      plugins = object(self)
        method gmap = fmap (* t.plugins#gmap *)
        method fmt fa fb fmt =
          GT.transform(t)
            (fun fself -> object
              inherit ['a,'b,_] fmt_t_t fa fb fself
              method! c_Symb fmt _ str =
                Format.fprintf fmt "(symb '%a)" fa str
              method! c_Seq  fmt _ xs =
                Format.fprintf fmt "(seq %a)" fb xs
            end)
            fmt
        method show fa fb () =
           GT.transform(t)
              (fun fself -> object
                inherit ['a,'b,_] show_t_t fa fb fself
                method! c_Symb _ _ str =
                  sprintf "(symb '%s)" (fa () str)
                method! c_Seq  _ _ xs =
                  sprintf "(seq %s)" (fb () xs)
               end)
              ()
       end
    }
  end
  include X
  include Fmap2(X)

  type rterm = (GT.string, rterm Std.List.ground) X.t [@@deriving gt ~options:{fmt}]
  type lterm = (GT.string logic, lterm Std.List.logic) X.t logic [@@deriving gt ~options:{fmt}]
  type fterm = (rterm, lterm) injected

  let show_rterm = Format.asprintf "%a" (GT.fmt rterm)
  let show_lterm = Format.asprintf "%a" (GT.fmt lterm)

  let symb s : fterm = inj @@ distrib @@ Symb s
  let seq xs : fterm = inj @@ distrib @@ Seq xs
end

let rec gterm_reifier c : Gterm.fterm -> Gterm.lterm =
  Gterm.reify OCanren.reify (Std.List.reify gterm_reifier) c

module Gresult = struct
  module X = struct
    type ('s, 't, 'xs) t =
    | Closure of 's * 't * 'xs
    | Val     of 't
    [@@deriving gt ~options:{show; fmt; gmap}]

    let fmap f g h = function
    | Closure (a,b,c) -> Closure (f a, g b, h c)
    | Val b -> Val (g b)

    (* let t = {t with
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
    } *)
  end

  include Fmap3(X)
  type rresult = (GT.string, Gterm.rterm, (GT.string * rresult) Std.List.ground) X.t [@@deriving gt ~options:{fmt}]
  type lresult = (GT.string logic, Gterm.lterm, (GT.string logic, lresult) Std.Pair.logic Std.List.logic) X.t logic [@@deriving gt ~options:{fmt}]
  type fresult = (rresult, lresult) injected

  let closure s t xs = inj @@ distrib @@ X.Closure (s,t,xs)
  let val_ t         = inj @@ distrib @@ X.Val t

  let show_string = GT.(show string)
  let show_stringl = GT.(show logic) show_string

  let rec show_rresult r = Format.asprintf "%a" (GT.fmt rresult) r
  let show_lresult (r: lresult) = Format.asprintf "%a" (GT.fmt lresult) r

end


let rec gresult_reifier c : Gresult.fresult -> Gresult.lresult =
  Gresult.reify OCanren.reify gterm_reifier
    (Std.List.reify (Std.Pair.reify OCanren.reify gresult_reifier))
    c

let (!!) x = inj @@ lift x

open Gterm
open Gresult

type lenv = (string logic, lresult) Std.Pair.logic Std.List.logic
type fenv = ( (string, rresult) Std.Pair.ground Std.List.ground,
              lenv ) injected

let reif_env e (x: fenv) : lenv =
  Std.List.reify (Std.Pair.reify OCanren.reify gresult_reifier) e x

(* let (_:int) = GT.(show Std.List.logic) *)
let show_lenv : lenv -> string =
  GT.show Std.List.logic
    GT.(show Std.Pair.logic (show logic @@ (fun s -> s)) show_lresult)

let show_reif_env h e =
  GT.(show Std.List.logic @@
        show Std.Pair.logic (show logic @@ (fun s -> s)) show_lresult) @@
  (* (fun _ _ -> assert false) *)
  (* (Std.List.reify @@ Std.Pair.reify OCanren.reify gresult_reifier) *)
  reif_env h e

(* let (_:int) = GT.(show Std.List.logic @@
      show Std.Pair.logic (show logic @@ (fun s -> s)) show_lresult)
let (_: _ -> fenv -> _) = show_reif_env *)

(* let unienv ?loc = unitrace ?loc @@ show_reif_env *)

let show_reif_term h t = show_lterm @@ gterm_reifier h t
let show_reif_result h t = show_lresult @@ gresult_reifier h t
(* let uniresult ?loc = unitrace ?loc @@ show_reif_result
let uniterm ?loc = unitrace ?loc @@ show_reif_term *)
(* let uni_term_list ?loc =
  unitrace ?loc
    (fun h t -> GT.(fun t -> show List.logic Gterm.show_lterm t) @@
      (List.reify gterm_reifier h t)
    ) *)

let show_reif_string h t = GT.(show logic @@ show string) @@
  OCanren.reify h t
(* let unistring ?loc = unitrace ?loc @@ show_reif_string *)

let (=/=) = OCanren.(=/=)
let (=//=) = (=/=)
let (===) = OCanren.(===)
let (===!) = (===)
let (===!!) = (===)

let rec lookupo x env t =
  let open OCanren.Std in
  (* let (=/=) = diseqtrace show_reif_string in
  let (===) ?loc = unienv ?loc in
  let (===!) ?loc = unistring ?loc in
  let (===!!) ?loc = uniresult ?loc in *)
  fresh (rest y v)
    ((Std.Pair.pair y v) % rest === env)
    (conde [
        (y ===! x) &&& (v ===!! t);
        (y =/= x) &&& (lookupo x rest t)
      ])

let rec not_in_envo x env =
  (* let (=/=) = diseqtrace show_reif_string in
  let (===) ?loc = unienv ?loc in *)
  let open OCanren.Std in
  (* printfn "entering not_in_envo"; *)
  conde
    [ fresh (y v rest)
        (env === (Std.pair y v) % rest)
        (y =/= x)
        (not_in_envo x rest)
    ; (nil () === env)
    ]

let rec proper_listo es env rs =
  (* let (===) ?loc = uni_term_list ?loc in *)
  let open OCanren.Std in
  conde
    [ (Std.nil () === es) &&& (Std.nil () === rs)
    ; fresh (e d te td)
        (es === e  % d)
        (rs === te % td)
        (evalo e env (val_ te))
        (proper_listo d env td)
    ]

and evalo (term: fterm) (env: fenv) (r: fresult) =
  (* let (===)  ?loc = unitrace ?loc show_reif_term in
  let (===!) ?loc = unitrace ?loc show_reif_result in *)

  (* fun st -> *)
    (* let () = printfn "entering into evalo %s %s %s"
      (show_reif_term term) (show_reif_env env) (show_reif_result r) in *)
  let open OCanren.Std in
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

let ( ~~ ) s  = symb @@ inj @@ lift s
let s      tl = seq (Std.List.list tl)

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
    ((Std.Triple.make p q r) === x)

let wrap_term rr = rr#reify gterm_reifier  |> show_lterm
let wrap_result rr = rr#reify gresult_reifier |> show_lresult

let find_quines ~verbose n = run q quineso (fun r -> r)
  |> OCanren.Stream.take ~n
  |> List.iter (fun q ->
    if verbose
    then printf "%s\n\n" (wrap_term q)
    else ()
  )

let find_twines ~verbose n = run qr twineso (fun q r -> (q,r))
  |> OCanren.Stream.take ~n
  |> List.iter (fun (q,r) ->
        if verbose
        then printf "%s,\n%s\n\n" (wrap_term q) (wrap_term r)
        else ()
    )

let wrap3terms t =
  t#reify
    (Std.Triple.reify gterm_reifier gterm_reifier gterm_reifier)
  |> (function
      | Var _ -> assert false
      | Value (a,b,c) ->
          printf "* %s\n  %s\n  %s\n\n"
          (Gterm.show_lterm a)
          (Gterm.show_lterm b)
          (Gterm.show_lterm c)
      )


let find_thrines ~verbose n = run q thrineso (fun a -> a)
  |> Stream.take ~n
  |> List.iter (fun a ->
      if verbose then
        let () = wrap3terms a in
        print_newline ()
  )

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
