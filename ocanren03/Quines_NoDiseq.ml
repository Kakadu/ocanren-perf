(*
   Quines stuff by Dmitrii Rozplokhas. Adopted from
   https://raw.githubusercontent.com/rozplokhas/OCanren/master/regression/test015.ml
*)

open Printf
open OCanren
open Tagged_stdlib

let ( ===< ) = ( === )
let ( ==== ) = ( === )
let ( !! ) x : string ilogic Std.Wrapper.injected = Std.Wrapper.w (inj x)

module Gterm = struct
  type nonrec ('s, 'n, 'ts) t =
    | Symb of 's
    | VR of 'n (* variable indexed by peano numbers *)
    | Tuple of 'ts
  [@@deriving gt ~options:{ fmt; show; gmap }]

  let t =
    { t with
      gcata = ()
    ; plugins =
        object
          method show = t.GT.plugins#show
          method gmap = t.GT.plugins#gmap

          method fmt fs fn fts fmt =
            GT.transform
              t
              (fun fself ->
                object
                  inherit ['a, 'b, 'c, _] fmt_t_t fs fn fts fself
                  method! c_Symb fmt _ s = Format.fprintf fmt "'%a" fs s
                  method! c_VR fmt _ peano = Format.fprintf fmt "(vr %a)" fn peano
                  method! c_Tuple fmt _ xs = Format.fprintf fmt "(%a)" fts xs
                end)
              fmt
        end
    }
  ;;

  type ground =
    (GT.string Std.Wrapper.t, Std.Nat.ground, ground ListLo.ground) t Std.Wrapper.t
  [@@deriving gt ~options:{ fmt }]

  type injected =
    (GT.string ilogic Std.Wrapper.injected, Std.Nat.injected, injected ListLo.injected) t
      ilogic
      Std.Wrapper.injected

  let fmapt fa fb fc s =
    let open OCanren.Env.Monad in
    OCanren.Env.Monad.return (GT.gmap t) <*> fa <*> fb <*> fc <*> s
  ;;

  let prj_exn : (injected, ground) OCanren.Reifier.t =
    let open OCanren.Env.Monad in
    OCanren.Reifier.fix (fun self ->
      Std.Wrapper.prj_exn
        (OCanren.prj_exn
         <..> chain
                (fmapt
                   (Std.Wrapper.prj_exn OCanren.prj_exn)
                   Std.Nat.prj_exn
                   (ListLo.prj_exn self))))
  ;;

  type logic =
    (GT.string OCanren.logic Std.Wrapper.logic, Std.Nat.logic, logic ListLo.logic) t
      OCanren.logic
      Std.Wrapper.logic
  [@@deriving gt ~options:{ fmt }]

  let reify : (injected, logic) OCanren.Reifier.t =
    let open OCanren.Env.Monad in
    OCanren.Reifier.fix (fun self ->
      Std.Wrapper.reify
        (OCanren.reify
         <..> chain
                (OCanren.Reifier.zed
                   (OCanren.Reifier.rework
                      ~fv:
                        (fmapt
                           (Std.Wrapper.reify OCanren.reify)
                           Std.Nat.reify
                           (ListLo.reify self))))))
  ;;

  let show_rterm : ground -> string = Format.asprintf "%a" (GT.fmt ground)
  let show_lterm : logic -> string = Format.asprintf "%a" (GT.fmt logic)

  open OCanren.Std

  let symb n : injected = Std.Wrapper.w (inj @@ Symb n)
  let vr n : injected = Std.Wrapper.w (inj @@ VR n)
  let tuple xs : injected = Std.Wrapper.w (inj @@ Tuple (Std.list Fun.id xs))

  let quote xs : injected =
    Std.Wrapper.w (inj @@ Tuple (Std.List.cons (symb !!"quote") xs))
  ;;

  let quotequote : injected =
    let open Std in
    quote !<(symb !!"quote")
  ;;

  let lambda n body : injected =
    tuple
    @@ (* in the original code lambda takes a list of arguments *)
    [ symb !!"lambda"; n; body ]
  ;;

  let app : injected -> injected -> injected =
    fun func arg -> Std.Wrapper.w (inj @@ Tuple Std.List.(cons func (cons arg (nil ()))))
  ;;

  let list xs : injected = Std.Wrapper.w (inj @@ Tuple (Std.List.cons (symb !!"list") xs))

  let list2 a b : injected =
    let open Std in
    Wrapper.w (inj @@ Tuple (symb !!"list" % (a %< b)))
  ;;
end

open Gterm

let rec nat o =
  (* let (===) ?loc = unitrace ?loc (fun h t -> GT.show Nat.logic @@   Nat.reify h t) in *)
  conde [ o === Std.Nat.zero; fresh n (o === Std.Nat.succ n) (nat n) ]
;;

let rec tm o =
  (* let open OCanren.Std in *)
  (* let (===) ?loc = unitrace ?loc (fun h t -> show_lterm @@ gterm_reifier h t) in *)
  conde
    [ fresh n (o === vr n) (nat n)
    ; o === symb !!"quote"
    ; fresh (n t) (o === lambda (vr n) t) (nat n) (tm t)
    ; fresh (t1 t2) (o === tuple [ symb !!"list"; t1; t2 ]) (tm t1) (tm t2)
    ]
;;

module Var = struct
  type ground = Std.Nat.ground [@@deriving gt ~options:{ fmt; gmap }]
  type logic = Std.Nat.logic [@@deriving gt ~options:{ fmt; gmap }]
  type injected = Std.Nat.injected

  let prj_exn = Std.Nat.prj_exn
  let reify = Std.Nat.reify
end

module Gresult = struct
  type nonrec ('env, 'v, 't) t =
    | Closure of 'env * 'v * 't
    | Code of 't
  [@@deriving gt ~options:{ fmt; gmap }]

  type ground =
    ((Var.ground * ground) Std.Wrapper.ground Std.List.ground, Var.ground, Gterm.ground) t
      Std.Wrapper.t
  [@@deriving gt ~options:{ fmt }]

  let fmapt fa fb fc s =
    let open OCanren.Env.Monad in
    OCanren.Env.Monad.return (GT.gmap t) <*> fa <*> fb <*> fc <*> s
  ;;

  type env_injected = (Var.injected, injected) Std.Pair.injected Std.List.injected

  and injected =
    (env_injected, Var.injected, Gterm.injected) t OCanren.ilogic Std.Wrapper.injected

  let prj_exn : (injected, ground) OCanren.Reifier.t =
    let open OCanren.Env.Monad in
    OCanren.Reifier.fix (fun self ->
      Std.Wrapper.prj_exn
        (OCanren.prj_exn
         <..> chain
                (fmapt
                   (Std.List.prj_exn (Std.Pair.prj_exn Var.prj_exn self))
                   Var.prj_exn
                   Gterm.prj_exn)))
  ;;

  type logic =
    ((Var.logic, logic) Std.Pair.logic Std.List.logic, Var.logic, Gterm.logic) t
      OCanren.logic
      Std.Wrapper.logic
  [@@deriving gt ~options:{ fmt }]

  let reify : (injected, logic) OCanren.Reifier.t =
    let open OCanren.Env.Monad in
    OCanren.Reifier.fix (fun self ->
      Std.Wrapper.reify
        (OCanren.reify
         <..> chain
                (OCanren.Reifier.zed
                   (OCanren.Reifier.rework
                      ~fv:
                        (fmapt
                           (Std.List.reify (Std.Pair.reify Var.reify self))
                           Var.reify
                           Gterm.reify)))))
  ;;

  (* type rresult = (Var.ground * rresult) Std.List.ground ground
     and renv = (Var.ground * rresult) Std.List.ground [@@deriving gt ~options:{ fmt }]

     type lresult = lenv logic
     and lenv = (Var.logic * lresult) logic Std.List.logic [@@deriving gt ~options:{ fmt }] *)

  let closure env v b : injected = Std.Wrapper.w (inj @@ Closure (env, v, b))
  let code x : injected = Std.Wrapper.w (inj @@ Code x)
  let clo = closure

  let show_rresult r =
    Format.asprintf "%a" [%fmt: (Var.ground * ground) Std.Wrapper.ground Std.List.ground]
  ;;

  let show_lresult = Format.asprintf "%a" (GT.fmt logic)

  let show_lenv =
    Format.asprintf "%a" [%fmt: (Var.logic, logic) Std.Pair.logic Std.List.logic]
  ;;

  (* let pair_to_logic f g = fun (a,b) -> Value (f a, g b)
     let rec to_logic : rresult -> lresult = fun res ->
     Value (fmap env_to_logic Nat.to_logic Gterm.to_logic res)
     and env_to_logic: renv -> lenv = fun e ->
     List.to_logic (pair_to_logic Nat.to_logic to_logic) e *)
end

(* let gresult_reifier = Gresult.reify *)

open Gresult

(* TODO: move to miniKanren.mli *)
let rec neq n1 n2 =
  conde
    [ n1 === Std.Nat.zero &&& fresh prev (n2 === Std.Nat.succ prev)
    ; n2 === Std.Nat.zero &&& fresh prev (n1 === Std.Nat.succ prev)
    ; fresh (p1 p2) (n1 === Std.Nat.succ p1) (n2 === Std.Nat.succ p2) (neq p1 p2)
    ]
;;

let rec vl o =
  conde
    [ fresh (e n t) (o === closure e n t) (venv e) (nat n) (tm t)
    ; fresh t (o === code t) (tm t)
    ]

and venv o =
  conde
    [ o === Std.nil ()
    ; fresh (n v e) (o === Std.(Pair.pair n v % e)) (nat n) (vl v) (venv e)
    ]
;;

let rec vlookup env x v =
  (* let env_reifier e = List.reify (ManualReifiers.pair_reifier) *)
  (* let (===<) ?loc = unitrace ?loc (fun h t -> show_lenv @@ env_reifier   h t) in *)
  (* let (====)  = unitrace (fun h t -> show_lterm   @@ gterm_reifier   h t) in *)
  (* trace "vlookup" @@ *)
  conde
    [ fresh er Std.(env ===< Pair.pair x v % er)
    ; fresh (y vy er) Std.(env ===< Pair.pair y vy % er) (neq x y) (vlookup er x v)
    ]
;;

let rec ev : _ =
  fun e t v ->
  (* let (===) ?loc  = unitrace ?loc (fun h t -> show_lterm   @@ gterm_reifier   h t) in
     let (====) ?loc = unitrace ?loc (fun h t -> show_lresult @@ gresult_reifier h t) in *)
  conde
    [ fresh x (t === vr x) (vlookup e x v)
    ; fresh (x t0) (t === lambda (vr x) t0) (v ==== closure e x t0)
    ; fresh t0 (t === app (symb !!"quote") t0) (v ==== code t0)
    ; fresh
        (t1 t2 e0 x0 t0 v2)
        (t === app t1 t2)
        (ev e t1 (clo e0 x0 t0))
        (ev e t2 v2)
        (ev Std.(Pair.pair x0 v2 % e0) t0 v)
    ; fresh
        (t1 t2 c1 c2)
        (t === list2 t1 t2)
        (v ==== code (tuple [ c1; c2 ]))
        (ev e t1 (code c1))
        (ev e t2 (code c2))
    ]
;;

let nil = Std.nil ()
let quineo q = ev nil q (code q)

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
(* let wrap_term rr = rr#reify Gterm.reify |> show_lterm *)
let wrap_result rr = rr#reify Gresult.reify |> show_lresult

let find_quines ~verbose n =
  run q quineo (fun rr -> rr#reify Gterm.reify)
  |> Stream.take ~n
  |> List.iter (fun q -> if verbose then printf "%s\n\n" (show_lterm q))
;;

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
