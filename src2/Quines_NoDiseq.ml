(*
  Quines stuff by Dmitrii Rozplokhas. Adopted from
  https://raw.githubusercontent.com/rozplokhas/OCanren/master/regression/test015.ml
*)

open Printf
open GT
open OCanren

let ( ===< ) = ( === )
let ( ==== ) = ( === )

module Gterm = struct
  [%%distrib
  type nonrec ('s, 'n, 'ts) t =
    | Symb of 's
    | VR of 'n (* variable indexed by peano numbers *)
    | Tuple of 'ts
  [@@deriving gt ~options:{ fmt; show; gmap }]

  type ground = (GT.string, Std.Nat.ground, ground Std.List.ground) t]

  let _ = tuple

  let t =
    { t with
      gcata = ()
    ; plugins =
        object
          (* method gmap = t.plugins#gmap *)
          method show = t.GT.plugins#show

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

  type rterm = ground [@@deriving gt ~options:{ fmt }]

  (* type lterm = (string logic, Std.Nat.logic,  lterm Std.List.logic) X.t logic [@@deriving gt ~options:{fmt}] *)
  type fterm = injected

  (* let rec pp_rterm f t =
     GT.fmt X.t (GT.fmt GT.string) (GT.fmt Std.List.ground pp_rterm) f t *)
  let show_rterm : rterm -> string = Format.asprintf "%a" (GT.fmt rterm)
  let show_lterm : logic -> string = Format.asprintf "%a" (GT.fmt logic)

  open OCanren.Std

  let vr n : fterm = inj @@ VR n

  (* let symb s : fterm = inj @@ Symb s *)
  let tuple xs : fterm = inj @@ Tuple (Std.List.list xs)
  let quote xs = inj @@ Tuple (symb !!"quote" % xs)
  let quotequote : fterm = quote !<(symb !!"quote")

  let lambda n body : fterm =
    tuple
    @@ (* in the original code lambda takes a list of arguments *)
    [ symb !!"lambda"; n; body ]
  ;;

  let app func arg = inj @@ Tuple (func %< arg)
  let list xs : fterm = inj @@ Tuple (symb !!"list" % xs)
  let list2 a b : fterm = inj @@ Tuple (symb !!"list" % (a %< b))
end

let ( !! ) = inj

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
  type ground = Std.Nat.ground [@@deriving gt ~options:{ fmt; show; gmap }]
  type logic = Std.Nat.logic [@@deriving gt ~options:{ fmt; show; gmap }]
  type injected = Std.Nat.injected

  let prj_exn = Std.Nat.prj_exn
  let reify = Std.Nat.reify
end

module Gresult = struct
  [%%distrib
  type nonrec ('env, 'v, 't) t =
    | Closure of 'env * 'v * 't
    | Code of 't
  [@@deriving gt ~options:{ fmt; show; gmap }]

  type nonrec 'env ground = ('env, Var.ground, Gterm.ground) t]
  (* let fmap f g h = function
     | Closure (a, b, c) -> Closure (f a, g b, h c)
     | Code b -> Code (h b)] *)

  (* include X *)
  (* include Fmap3 (X) *)
  (* open OCanren.Std *)

  let show_rvar = GT.show Var.ground
  let show_lvar = GT.show Var.logic

  (* type renv = *)
  type rresult = renv ground
  and renv = (Var.ground * rresult) Std.List.ground [@@deriving gt ~options:{ fmt }]

  type lresult = lenv logic
  and lenv = (Var.logic * lresult) logic Std.List.logic [@@deriving gt ~options:{ fmt }]

  type fresult = (Var.injected, fresult) Std.Pair.injected Std.List.injected

  let reify_renv =
    Reifier.fix (fun self ->
        Std.List.reify (Std.Pair.reify Var.reify (Std.List.reify self)))
  ;;

  let reify_result = Std.List.reify reify_renv

  (* let closure env v b = inj @@ Closure (env, v, b) *)
  let clo = closure

  (* let code c = inj @@ Code c *)
  let show_string = GT.(show string)
  let show_stringl = GT.(show logic) show_string
  let rec show_rresult r = Format.asprintf "%a" (GT.fmt rresult)

  (* GT.(show X.t
     (show List.ground (show pair show_rvar show_rresult))
     show_rvar
     Gterm.show_rterm) r *)
  let show_lresult = Format.asprintf "%a" (GT.fmt lresult)

  (* GT.(show logic @@ show X.t
     show_lenv
     show_lvar
     Gterm.show_lterm) r *)
  let show_lenv = Format.asprintf "%a" (GT.fmt lresult)

  (* let pair_to_logic f g = fun (a,b) -> Value (f a, g b)
     let rec to_logic : rresult -> lresult = fun res ->
       Value (fmap env_to_logic Nat.to_logic Gterm.to_logic res)
     and env_to_logic: renv -> lenv = fun e ->
       List.to_logic (pair_to_logic Nat.to_logic to_logic) e *)
end

let var_reifier = Std.Nat.reify
let gresult_reifier = Gresult.reify_result
let env_reifier = Gresult.reify_renv

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

let rec ev e t v =
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
let wrap_result rr = rr#reify gresult_reifier |> show_lresult

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
