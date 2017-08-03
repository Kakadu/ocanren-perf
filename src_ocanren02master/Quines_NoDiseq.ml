(*
  Quines stuff by Dmitrii Rozplokhas. Adopted from
  https://raw.githubusercontent.com/rozplokhas/OCanren/master/regression/test015.ml
*)

open Printf
open GT
open MiniKanren
open MiniKanrenStd

let (===<) = (===)
let (====) = (===)

module Gterm = struct
  module X = struct
    @type ('s, 'n, 'ts) t =
      | Symb  of 's
      | VR    of 'n   (* variable indexed by peano numbers *)
      | Tuple of 'ts
    with show

    let fmap f g h = function
    | Symb s   -> Symb (f s)
    | VR n     -> VR (g n)
    | Tuple xs -> Tuple (h xs)

    let t = {
      gcata = ();
      plugins = object
        (* method gmap = t.plugins#gmap *)
        method show fa fb fc bx =
          GT.transform(t)
            (GT.lift fa) (GT.lift fb) (GT.lift fc)
            (object inherit ['a,'b,'c] show_t_t
              method c_VR _ _ peano =
                sprintf "(vr %s)" (peano.GT.fx ())
              method c_Symb  _ _  s = "'" ^ (s.GT.fx ())
              method c_Tuple _ _ xs = sprintf "(%s)" (xs.GT.fx ())
            end)
            ()
            bx
       end
    }

  end
  include X
  include Fmap3(X)

  type rterm = (string, Nat.ground, rterm List.ground) X.t
  type lterm = (string logic, Nat.logic,  lterm List.logic) X.t logic
  type fterm = (rterm, lterm) injected

  let rec show_rterm : rterm -> string = fun term ->
    GT.(show t (show string)
               (show Nat.ground)
               (show List.ground show_rterm)) term

  let rec show_lterm : lterm -> string = fun x ->
    GT.(show logic @@ show t (show logic @@ (fun s -> s))
                              (show Nat.logic)
                              (show List.logic show_lterm)) x

  let rec to_logic : rterm -> lterm = fun term ->
    Value (fmap (fun s -> Value s)
                Nat.to_logic
                (List.to_logic to_logic) term)

  let vr  n   : fterm = inj @@ distrib @@ VR n
  let symb s  : fterm = inj @@ distrib @@ Symb s
  let tuple xs: fterm = inj @@ distrib @@ Tuple (inj_listi xs)
  let quote xs  = inj @@ distrib @@ Tuple ((symb !!"quote") % xs)
  let quotequote : fterm = quote (!< (symb !!"quote"))

  let lambda n body : fterm = tuple @@
    (* in the original code lambda takes a list of arguments *)
    [symb !!"lambda"; n; body]

  let app func arg = inj @@ distrib @@ Tuple (func %< arg )
  let list xs : fterm =
     inj @@ distrib @@ Tuple ((symb !!"list") % xs)

 let list2 a b : fterm =
    inj @@ distrib @@ Tuple ((symb !!"list") % (a %< b))

end

let rec gterm_reifier c : Gterm.fterm -> Gterm.lterm =
  Gterm.reify ManualReifiers.string
              Nat.reify
              (List.reify gterm_reifier) c

let (!!) x = inj @@ lift x

open Gterm

let rec nat o =
  let (===) ?loc = unitrace ?loc (fun h t -> GT.show Nat.logic @@   Nat.reify h t) in
  conde
    [ o === Nat.zero
    ; fresh (n)
        (o === (Nat.succ n))
        (nat n)
    ]

let rec tm o =
  let (===) ?loc = unitrace ?loc (fun h t -> show_lterm @@ gterm_reifier h t) in
  conde
    [ fresh (n)
        (o === (vr n))
        (nat n)
    ; (o === symb !!"quote")
    ; fresh (n t)
        (o === (lambda (vr n) t))
        (nat n)
        (tm t)
    ; fresh (t1 t2)
        (o === tuple [symb !!"list"; t1; t2])
        (tm t1)
        (tm t2)
    ]


module Gresult = struct
  module X = struct
    @type ('env, 'v, 't) t =
    | Closure of 'env * 'v * 't
    | Code    of 't
    with show

    let fmap f g h = function
    | Closure (a,b,c) -> Closure (f a, g b, h c)
    | Code b          -> Code (h b)
  end
  include X
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
  let clo   = closure
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
    Value (fmap env_to_logic Nat.to_logic Gterm.to_logic res)
  and env_to_logic: renv -> lenv = fun e ->
    List.to_logic (pair_to_logic Nat.to_logic to_logic) e
end

let var_reifier = Nat.reify

let rec gresult_reifier c : Gresult.fresult -> Gresult.lresult =
  let open ManualReifiers in
  Gresult.reify env_reifier var_reifier gterm_reifier c
and env_reifier e =
  List.reify ManualReifiers.(pair var_reifier gresult_reifier) e

open Gresult

(* TODO: move to miniKanren.mli *)
let rec neq n1 n2 =
  conde
    [ (n1 === Nat.zero) &&&
      (fresh (prev)
        (n2 === Nat.succ prev))
    ; (n2 === Nat.zero) &&&
      (fresh (prev)
        (n1 === Nat.succ prev))
    ; fresh (p1 p2)
        (n1 === Nat.succ p1)
        (n2 === Nat.succ p2)
        (neq p1 p2)
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
  (* let (===<) ?loc = unitrace ?loc (fun h t -> show_lenv @@ env_reifier   h t) in *)
  (* let (====)  = unitrace (fun h t -> show_lterm   @@ gterm_reifier   h t) in *)
  (* trace "vlookup" @@ *)
  conde
    [ fresh (er)
        (env ===< (inj_pair x v) % er)
    ; fresh (y vy er)
        (env ===< (inj_pair y vy) % er)
        (neq x y)
        (vlookup er x v)
    ]

let rec ev e t v =
  (* let (===) ?loc  = unitrace ?loc (fun h t -> show_lterm   @@ gterm_reifier   h t) in
  let (====) ?loc = unitrace ?loc (fun h t -> show_lresult @@ gresult_reifier h t) in *)
  conde
    [ fresh (x)
        (t === (vr x))
        (vlookup e x v)
    ; fresh (x t0)
        (t ===  (lambda (vr x) t0))
        (v ==== (closure e x t0))
    ; fresh (t0)
        (t ===  (app (symb !!"quote") t0))
        (v ==== (code t0))
    ; fresh (t1 t2 e0 x0 t0 v2)
        (t === (app t1 t2))
        (ev e t1 (clo e0 x0 t0))
        (ev e t2 v2)
        (ev ((inj_pair x0 v2)%e0) t0 v)
    ; fresh (t1 t2 c1 c2)
        (t ===  (list2 t1 t2))
        (v ==== (code (tuple [c1; c2])))
        (ev e t1 (code c1))
        (ev e t2 (code c2))
    ]

let nil = nil ()
let quineo q =
  (ev nil q (code q))

(*
The idea to implement twines and thrines is to implement
 * grounding, unifying indicies if fresh variables
 * filtering out the result stream
*)

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

let find_quines ~verbose n = run q quineo @@ fun qs ->
  Stream.take ~n qs |> List.iter (fun q ->
    if verbose
    then printf "%s\n\n" (wrap_term q)
    else ()
  )

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
