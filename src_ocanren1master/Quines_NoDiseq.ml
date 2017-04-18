(*
  Quines stuff by Dmitrii Rozplokhas. Adopted from
  https://raw.githubusercontent.com/rozplokhas/OCanren/master/regression/test015.ml
*)

open Printf
open GT
open MiniKanren

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
    @type ('n, 't) t =
      | VR  of 'n   (* variable indexed by peano numbers *)
      | Quote of 't
      | Abs of 'n * 't
      | App of 't * 't
      | List of 't * 't
      with gmap,show
      ;;

    let fmap f g x = GT.gmap t f g x

    let t = {
      gcata = ();
      plugins = object
        method gmap = t.plugins#gmap
        method show fa fb bx =
          GT.transform(t)
            (GT.lift fa) (GT.lift fb)
            (object inherit ['a,'b] show_t_t
              method c_VR _ _ peano =
                sprintf "(vr %s)" (peano.GT.fx ())
              method c_Quote _ _ term =
                sprintf "(quote %s)" (term.GT.fx ())
              method c_Abs  _ _ var body =
                sprintf "(lambda (%s) %s)" (var.GT.fx ()) (body.GT.fx ())
              method c_App _ _ f arg =
                sprintf "(%s %s)" (f.GT.fx ()) (arg.GT.fx ())
              method c_List _ _ a b =
                sprintf "(list (%s) %s)" (a.GT.fx ()) (b.GT.fx ())
             end)
            ()
            bx
       end
    }

  end
  include X
  include Fmap2(X)

  type rterm = (Nat.ground, rterm) X.t
  type lterm = (Nat.logic,  lterm) X.t logic
  type fterm = (rterm, lterm) injected

  let rec show_rterm : rterm -> string = fun t -> GT.(show X.t (show Nat.ground) show_rterm) t
  let rec show_lterm : lterm -> string =
    fun x -> GT.(show logic @@ show X.t (show Nat.logic) show_lterm) x

  let rec to_logic : rterm -> lterm = fun term ->
    Value (GT.(gmap X.t) (Nat.inj_ground) to_logic term)

  let vr  n   : fterm = inj @@ distrib @@ VR n
  let quote t : fterm = inj @@ distrib @@ Quote t
  let abs v b : fterm = inj @@ distrib @@ Abs (v, b)
  let lambda v b : fterm = abs v b
  let app f g : fterm = inj @@ distrib @@ App (f,g)
  let lst a b : fterm = inj @@ distrib @@ List (a,b)
end

let rec gterm_reifier c : Gterm.fterm -> Gterm.lterm =
  Gterm.reify Nat.reify gterm_reifier c

module Gresult = struct
  module X = struct
    @type ('env, 'v, 't) t =
    | Closure of 'env * 'v * 't
    | Code    of 't
    with show;;

    let fmap f g h = function
    | Closure (a,b,c) -> Closure (f a, g b, h c)
    | Code b          -> Code (h b)
  end

  include Fmap3(X)
  type rvar = Nat.ground
  type lvar = Nat.logic
  let show_rvar = GT.show Nat.ground
  let show_lvar = GT.show Nat.logic

  type rresult = ((rvar * rresult) List.ground, rvar, Gterm.rterm) X.t
  type lresult = ((lvar * lresult) logic List.logic, lvar, Gterm.lterm) X.t logic
  type fresult = (rresult, lresult) injected

  let closure env v b = inj @@ distrib @@ X.Closure (env,v,b)
  let code    c       = inj @@ distrib @@ X.Code c

  let show_string = GT.(show string)
  let show_stringl = GT.(show logic) show_string

  let rec show_rresult r = GT.(show X.t
    (show List.ground (show pair show_rvar show_rresult))
    show_rvar
    Gterm.show_rterm) r
  let rec show_lresult r = GT.(show logic @@ show X.t
    (show List.logic (show logic @@ show pair show_lvar show_lresult))
    show_lvar
    Gterm.show_lterm) r

end

let var_reifier = Nat.reify

let rec gresult_reifier c : Gresult.fresult -> Gresult.lresult =
  let open ManualReifiers in
  Gresult.reify
    (List.reify (pair_reifier var_reifier gresult_reifier))
    var_reifier
    gterm_reifier
    c

let (!!) x = inj @@ lift x

open Gterm
open Gresult

(* TODO: put that into miniKanren mli *)
let zero : Nat.groundi = Obj.magic @@ !!O
let s n  : Nat.groundi = Obj.magic (S n)

let rec nat o =
  conde
    [ o === zero
    ; fresh (n)
        (o === (s n))
        (nat n)
    ]

let rec tm o =
  let (===) = unitrace (fun h t -> show_lterm @@ gterm_reifier h t) in
  conde
    [ fresh (n)
        (o === (vr n))
        (nat n)
    ; fresh (b)
        (o === quote b)
    ; fresh (n t)
        (o === (lambda n t))
        (nat n)
        (tm t)
    ; fresh (t1 t2)
        (o === (lst t1 t2))
        (tm t1)
        (tm t2)
    ]

let rec vl o =
  conde
    [ fresh (e n t)
        (o === (closure e n t))
        (venv e)
        (nat n)
        (tm t)
    ; fresh (t)
        (o === (code t))
        (tm t)
    ]
and venv o =
  conde
    [ (o === nil ())
    ; fresh (n v e)
        (o === ((inj_pair n v) % e))
        (nat n)
        (vl v)
        (venv e)
    ]

(* TODO: move to miniKanren.mli *)
let rec neq n1 n2 =
  conde
    [ (n1 === zero) &&&
      (fresh (prev)
        (n2 === s prev))
    ; (n2 === zero) &&&
      (fresh (prev)
        (n1 === s prev))
    ; fresh (p1 p2)
        (n1 === s p1)
        (n2 === s p2)
        (neq p1 p2)
    ]

let rec vlookup env x v =
  (* let (====)  = unitrace (fun h t -> show_lterm   @@ gterm_reifier   h t) in *)
  (* trace "vlookup" @@ *)
  conde
    [ fresh (er)
        (env === (inj_pair x v) % er)
    ; fresh (y vy er)
        (env === (inj_pair x vy) % er)
        (neq x y)
        (vlookup er x v)
    ]

(* let rec lookupo x env t =
  Fresh.three (fun rest y v ->
    (env === (inj_pair y v) % rest) &&&
    (conde [
        (y === x) &&& (v === t);
        (y =/= x) &&& (lookupo x rest t)
      ])
  )

let rec not_in_envo x env =
  conde
    [ (env === nil ())
    ; Fresh.three (fun y v rest ->
        (env === (inj_pair y v) % rest) &&&
        (y =/= x) &&&
        (not_in_envo x rest) )
    ] *)

(* type fenv = ( (string * rresult) List.ground, (string logic * lresult) logic List.logic) injected

let rec map_evalo es env rs =
  conde
    [ (es === nil ()) &&& (rs === nil ())
    ; fresh (e es' r rs')
        (es === e % es')
        (rs === r % rs')
        (evalo e env (val_ r))
        (map_evalo es' env rs')
    ]
and evalo (term: fterm) (env: fenv) (r: fresult) =
  conde
    [ call_fresh (fun t ->
        (term === seq ((symb !!"quote") %< t)) &&&
        (r === (val_ t))
           &&& (not_in_envo !!"quote" env)
        )
    ; fresh (es rs)
        (term === seq ((symb !!"list") % es) )
        (r === val_ (seq rs))
        (not_in_envo !!"list" env)
        (map_evalo es env rs)

    ; fresh (s)
        (term === (symb s))
        (lookupo s env r)

    ; fresh (func arge arg x body env')
        (term === seq (func %< arge))
        (evalo arge env arg)
        (evalo func env (closure x body env') )
        (evalo body ((inj_pair x arg) % env') r)
    ; fresh (x body)
        (term === seq ( (symb !!"lambda") %
                        (seq (!< (symb x)) %< body)
                      ) )
        (not_in_envo !!"lambda" env)
        (r === (closure x body env))
    ]

let ( ~~ ) s  = symb @@ inj @@ lift s
let s      tl = seq (inj_list tl) *)

let rec ev e t v =
  let (===)  = unitrace (fun h t -> show_lterm   @@ gterm_reifier   h t) in
  let (====) = unitrace (fun h t -> show_lresult @@ gresult_reifier h t) in
  conde
    [ Fresh.one (fun x -> delay @@ fun () ->
        (t === (vr x)) &&&
        (vlookup e x v)
      )
    (* ; Fresh.two (fun t0 x -> delay @@ fun () -> *)
    ; fresh (x t0)
        (t ===  (lambda x t0))
        (v ==== (closure e x t0))
    ; fresh (t0)
        (t ===  (quote t0))
        (v ==== (code t0))
    ; fresh (t1 t2 e0 x0 t0 v2)
        (t === (app t1 t2))
        (ev e t1 (closure e0 x0 t0))
        (ev e t2 v2)
        (ev ((inj_pair x0 v2) % e0) t0 v)
    ; fresh (t1 t2 c1 c2)
        (t ===  (lst t1 t2))
        (v ==== (code (lst c1 c2)))
        (ev e t1 (code c1))
        (ev e t2 (code c2))
    ]


let nil = nil ()
let quineo q =
  (ev nil q (code q))

(* let twineso q p =
  (q =/= p) &&& (evalo q nil (val_ p)) &&& (evalo p nil (val_ q))

let thrineso q p r =
  (q =/= p) &&& (p =/= r) &&& (r =/= q) &&&
  (evalo p nil (val_ q)) &&&
  (evalo q nil (val_ r)) &&&
  (evalo r nil (val_ p))

let run_term (text,t) = printf "> %s\n%!%s\n\n%!" text @@
  run q (fun q -> evalo t nil (val_ q)) (fun qs ->
      if Stream.is_empty qs
      then "fail"
      else (Stream.hd qs)#refine gterm_reifier ~inj:Gterm.to_logic |> show_lterm
    )

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
(*
let gen_terms n r = printf "> %s\n" (show_term r);
  run q (fun q -> evalo q nil (val_ r))
    (fun qs -> List.iter (fun t -> printf "%s\n" @@ show_term t) @@
      Stream.take ~n:n qs);
  Printf.printf "\n"
*)
*)
let wrap_term rr =
  rr#refine gterm_reifier ~inj:Gterm.to_logic |> show_lterm
 (* function
  | Final x -> show_rterm @@ Obj.magic x
  | HasFreeVars func -> show_lterm @@ func gterm_reifier *)

let find_quines n = run q quineo @@ fun qs ->
  Stream.take ~n qs |> List.map wrap_term |> List.iter (printf "%s\n\n")
(*
let find_twines n =
  run qr (fun q r -> twineso q r)
    (fun qs rs ->
      List.iter2 (fun q r -> printf "%s,\n%s\n\n%!" (wrap_term q) (wrap_term r))
        (Stream.take ~n qs) (Stream.take ~n rs)
    )

let find_thrines n =
  run qrs thrineso
    (fun qs rs ss ->
      list_iter3 (fun (q,r,s) -> printf "%s,\n\t%s,\n\t%s\n\n" (wrap_term q) (wrap_term r) (wrap_term s))
        (Stream.take ~n qs) (Stream.take ~n rs) (Stream.take ~n ss)
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
  *) *)
