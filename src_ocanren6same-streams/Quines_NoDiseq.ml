(*
  Quines stuff by Dmitrii Rozplokhas. Adopted from
  https://raw.githubusercontent.com/rozplokhas/OCanren/master/regression/test015.ml
*)
(*
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
    @type ('n, 'ts) t =
      | VR  of 'n   (* variable indexed by peano numbers *)
      | Quote
      | List
      | Lambda
      | Tuple of 'ts
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
              method c_Quote  _ _ = "quote"
              method c_List   _ _ = "list"
              method c_Lambda _ _ = "lambda"
              method c_Tuple _ _ xs = sprintf "(%s)" (xs.GT.fx ())
            end)
            ()
            bx
       end
    }

  end
  include X
  include Fmap2(X)

  type rterm = (Nat.ground, rterm List.ground) X.t
  type lterm = (Nat.logic,  lterm List.logic) X.t logic
  type fterm = (rterm, lterm) injected

  let rec show_rterm : rterm -> string = fun term ->
    GT.(show t (show Nat.ground) (show List.ground show_rterm)) term
  let rec show_lterm : lterm -> string = fun x ->
    GT.(show logic @@ show t (show Nat.logic) (show List.logic show_lterm)) x

  let rec to_logic : rterm -> lterm = fun term ->
    Value (GT.(gmap X.t) (Nat.inj_ground) (List.to_logic to_logic) term)

  let vr  n   : fterm = inj @@ distrib @@ VR n
  let quoteS  : fterm = inj @@ distrib @@ Quote
  let lambdaS : fterm = inj @@ distrib @@ Lambda
  let listS   : fterm = inj @@ distrib @@ List
  let tuple xs: fterm = inj @@ distrib @@ Tuple xs

  let lambda n body : fterm = tuple @@ List.inj_list [lambdaS; vr n; body]
end

let rec gterm_reifier c : Gterm.fterm -> Gterm.lterm =
  Gterm.reify Nat.reify (List.reify gterm_reifier) c

module Gresult = struct
  module X = struct
    @type ('env, 'v, 't) t =
    | Closure of 'env * 'v * 't
    | Code    of 't
    with show,gmap;;

    let fmap f g h = function
    | Closure (a,b,c) -> Closure (f a, g b, h c)
    | Code b          -> Code (h b)
  end

  include Fmap3(X)
  type rvar = Nat.ground
  type lvar = Nat.logic
  let show_rvar = GT.show Nat.ground
  let show_lvar = GT.show Nat.logic

  type rresult = (renv, rvar, Gterm.rterm) X.t
  and renv = (rvar * rresult) List.ground
  type lresult = (lenv, lvar, Gterm.lterm) X.t logic
  and lenv = (lvar * lresult) logic List.logic

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
    show_lenv
    show_lvar
    Gterm.show_lterm) r
  and show_lenv e =
    GT.(show List.logic (show logic @@ show pair show_lvar show_lresult)) e

  let pair_to_logic f g = fun (a,b) -> Value (f a, g b)
  let rec to_logic : rresult -> lresult = fun res ->
    Value (GT.gmap X.t env_to_logic Nat.inj_ground Gterm.to_logic res)
  and env_to_logic: renv -> lenv = fun e ->
    List.inj_ground (pair_to_logic Nat.inj_ground to_logic) e
end

let var_reifier = Nat.reify

let rec gresult_reifier c : Gresult.fresult -> Gresult.lresult =
  let open ManualReifiers in
  Gresult.reify env_reifier var_reifier gterm_reifier c
and env_reifier e =
  List.reify ManualReifiers.(pair_reifier var_reifier gresult_reifier) e


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

let rec tm o =
  let (===) = unitrace (fun h t -> show_lterm @@ gterm_reifier h t) in
  conde
    [ fresh (n)
        (o === (vr n))
        (nat n)
    ; (o === quoteS)
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

let rec vlookup env x v =
  (* let env_reifier e = List.reify (ManualReifiers.pair_reifier) *)
  let (<=>) = unitrace (fun h t -> show_lenv @@ env_reifier   h t) in
  (* let (====)  = unitrace (fun h t -> show_lterm   @@ gterm_reifier   h t) in *)
  (* trace "vlookup" @@ *)
  conde
    [ call_fresh_named "er" (fun er -> delay @@ fun () ->
        (env <=> (inj_pair x v) % er)
      )
    ; call_fresh_named "er" (fun er ->
      call_fresh_named "vy" (fun vy ->
      call_fresh_named "y"  (fun y  -> (*delay @@ fun () ->*)
        (env <=> (inj_pair y vy) % er) &&&
        (neq x y) &&&
        (vlookup er x v)
      )))
    ]

let rec ev e t v =
  let (===)  = unitrace (fun h t -> show_lterm   @@ gterm_reifier   h t) in
  let (====) = unitrace (fun h t -> show_lresult @@ gresult_reifier h t) in
  conde
    [ fresh (x)
        (t === (vr x))
        (delay @@ fun () -> vlookup e x v)
    ; fresh (x t0)
    (* ; Fresh.two (fun t0 x -> delay @@ fun () -> *)
    (* ; call_fresh_named "t0" (fun t0 ->
        call_fresh_named "x" (fun x ->
          delay @@ fun () -> *)
            (t ===  (lambda x t0))
            (v ==== (closure e x t0))

    ; fresh (t0)
        (t ===  (app quote t0))
        (v ==== (code t0))
    ; fresh (t1 t2 e0 x0 t0 v2)
        (t === (app t1 t2))
        (ev e t1 (closure e0 x0 t0))
        (ev e t2 v2)
        (ev (lst (app x0 v2) e0) t0 v)
    ; fresh (t1 t2 c1 c2)
        (t ===  (lst t1 t2))
        (v ==== (code (lst c1 c2)))
        (delay @@ fun () -> ev e t1 (code c1))
        (delay @@ fun () -> ev e t2 (code c2))
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

let wrap_result rr =
  rr#refine gresult_reifier ~inj:Gresult.to_logic |> show_lresult

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
*)
