(*
   Quines stuff by Dmitrii Rozplokhas. Adopted from
   https://raw.githubusercontent.com/rozplokhas/OCanren/master/regression/test015.ml
*)
include Counters.Make ()
open Printf
open OCanren
open PrintHelpers

module Std = struct
  include Std

  module Triple = struct
    type nonrec ('a, 'b, 'c) t = 'a * 'b * 'c [@@deriving gt ~options:{ fmt; gmap }]

    let reify ra rb rc =
      let ( >>= ) = Env.Monad.bind in
      Reifier.fix (fun self ->
        Reifier.compose
          Reifier.reify
          (ra
           >>= fun fa ->
           rb
           >>= fun fb ->
           rc
           >>= fun fc ->
           let rec foo = function
             | Var (v, xs) -> Var (v, Stdlib.List.map foo xs)
             | Value x -> Value (GT.gmap t fa fb fc x)
           in
           Env.Monad.return foo))
    ;;

    let make x y z = inj @@ (x, y, z)
  end
end

let list_combine3 xs ys zs =
  let rec helper acc = function
    | x :: xs, y :: ys, z :: zs -> helper ((x, y, z) :: acc) (xs, ys, zs)
    | [], [], [] -> List.rev acc
    | _ -> failwith "bad argument of list_combine3"
  in
  helper [] (xs, ys, zs)
;;

let list_iter3 f xs ys zs =
  let rec helper = function
    | x :: xs, y :: ys, z :: zs ->
      f (x, y, z);
      helper (xs, ys, zs)
    | [], [], [] -> ()
    | _ -> failwith "bad argument of list_combine3"
  in
  helper (xs, ys, zs)
;;

module Gterm = struct
  [@@@ocaml.warnerror "-32-34"]

  [%%ocanren_inject
  type nonrec ('s, 'xs) t =
    | Symb of 's
    | Seq of 'xs
  [@@deriving gt ~options:{ fmt; gmap }]

  type ground = (StringLo.ground, ground ListLo.ground) t]

  let t =
    { t with
      gcata = ()
    ; plugins =
        object (self)
          method gmap = t.plugins#gmap

          method fmt fa fb fmt =
            GT.transform
              t
              (fun fself ->
                object
                  inherit ['a, 'b, _] fmt_t_t fa fb fself
                  method! c_Symb fmt _ str = Format.fprintf fmt "(symb '%a)" fa str
                  method! c_Seq fmt _ xs = Format.fprintf fmt "(seq %a)" fb xs
                end)
              fmt
        end
    }
  ;;

  (* This is a hack to apply custom printers for logic strings and lists *)
  type logic = (StringLo.logic, logic ListLo.logic) t OCanren.logic
  [@@deriving gt ~options:{ fmt; gmap }]

  type injected = (GT.string OCanren.ilogic, injected Std.List.groundi) t ilogic

  let show_rterm = Format.asprintf "%a" (GT.fmt ground)
  let show_lterm = Format.asprintf "%a" (GT.fmt logic)

  let rec project_exn : logic -> ground =
    function
    | Var _ -> raise OCanren.Not_a_value
    | Value (Seq xs ) -> Seq (ListLo.project project_exn xs )
    | Value (Symb (Var _)) -> raise Not_a_value
    | Value (Symb (Value s )) -> Symb s
end

let gterm_reifier = Gterm.reify

module Gresult = struct
  [%%distrib
  type nonrec ('s, 't, 'xs) t =
    | Closure of 's * 't * 'xs
    | Val_ of 't
  [@@deriving gt ~options:{ fmt; gmap }]

  type ground =
    ( StringLo.ground
      , Gterm.ground
      , (StringLo.ground, ground) Std.Pair.ground Std.List.ground )
      t]

  let show_string = GT.(show string)
  let show_stringl = GT.(show OCanren.logic) show_string
  let rec show_rresult r = Format.asprintf "%a" (GT.fmt ground) r
  let show_lresult (r : logic) = Format.asprintf "%a" (GT.fmt logic) r
end

let gresult_reifier = Gresult.reify
let ( !! ) x = inj x

open Gterm
open Gresult

type lenv = (GT.string OCanren.logic, Gresult.logic) Std.Pair.logic Std.List.logic
[@@deriving gt ~options:{ fmt }]

type fenv = (string OCanren.ilogic, Gresult.injected) Std.Pair.groundi Std.List.groundi

let reif_env : (_, lenv) Reifier.t =
  Std.List.reify (Std.Pair.reify OCanren.reify gresult_reifier)
;;

let show_reif_term h t = show_lterm @@ gterm_reifier h t
let show_reif_result h t = show_lresult @@ gresult_reifier h t

[@@@ocamlformat "disable"]

IFDEF TRACE THEN

 let ( === ) : Gterm.injected -> Gterm.injected -> goal =
   fun x y st ->
    incr_counter ();
    set_last_introduced_var (State.last_introduced_var st);
    (* if not are_unifications_silent then
      Printf.printf "%s %s\n" (pp st x) (pp st y); *)
    OCanren.( === ) x y st
   [@@inline]
 let ( ===! ) : Gresult.injected -> Gresult.injected -> goal =
   fun x y st ->
    incr_counter ();
    set_last_introduced_var (State.last_introduced_var st);
    (* if not are_unifications_silent then
      Printf.printf "%s %s\n" (pp st x) (pp st y); *)
    OCanren.( === ) x y st
   [@@inline]
 let ( ===!! ) : _ Std.List.injected -> _ Std.List.injected -> goal =
   fun x y st ->
    incr_counter ();
    set_last_introduced_var (State.last_introduced_var st);
    (* if not are_unifications_silent then
      Printf.printf "%s %s\n" (pp st x) (pp st y); *)
    OCanren.( === ) x y st
   [@@inline]
 let ( ==== ) : string ilogic -> string ilogic -> goal =
   fun x y st ->
    incr_counter ();
    set_last_introduced_var (State.last_introduced_var st);
    (* if not are_unifications_silent then
      Printf.printf "%s %s\n" (pp st x) (pp st y); *)
    OCanren.( === ) x y st
   [@@inline]
ELSE


let ( =/= ) = OCanren.( =/= )
let ( =//= ) = ( =/= )
let ( === ) = OCanren.( === )
let ( ===! ) = OCanren.( === )
let ( ===!! ) = OCanren.( === )
let ( ==== ) = OCanren.( === )
END
let rec lookupo : string ilogic -> (string ilogic, Gresult.injected) Std.Pair.injected Std.List.injected -> _
  = fun x env t ->
  let open OCanren.Std in
  fresh
    (rest y v)
    (Std.Pair.pair y v % rest ===!! env)
    (conde [ y ==== x &&& (v ===! t); y =/= x &&& lookupo x rest t ])
;;

let rec not_in_envo x env =
  let open OCanren.Std in
  conde
    [ fresh (y v rest) (env ===!! (Std.pair y v % rest)) (y =/= x) (not_in_envo x rest)
    ; nil () ===!! env
    ]
;;

let rec proper_listo es env rs =
  let open OCanren.Std in
  conde
    [ Std.nil () ===!! es &&& (Std.nil () ===!! rs)
    ; fresh
        (e d te td)
        (es ===!! e % d)
        (rs ===!! te % td)
        (evalo e env (val_ te))
        (proper_listo d env td)
    ]

and evalo (term : Gterm.injected) (env : fenv) (r : Gresult.injected) =
  let open OCanren.Std in
  conde
    [ fresh
        t
        (term === seq (symb !!"quote" %< t))
        (r ===! val_ t)
        (not_in_envo !!"quote" env)
    ; fresh
        (es rs)
        (term === seq (symb !!"list" % es))
        (r ===! val_ (seq rs))
        (not_in_envo !!"list" env)
        (proper_listo es env rs)
    ; fresh s (term === symb s) (lookupo s env r)
    ; fresh
        (func arge arg x body env')
        (term === seq (func %< arge))
        (evalo arge env arg)
        (evalo func env (closure x body env'))
        (evalo body (Std.pair x arg % env') r)
    ; fresh
        (x body)
        (term === seq (symb !!"lambda" % (seq !<(symb x) %< body)))
        (not_in_envo !!"lambda" env)
        (r ===! closure x body env)
    ]
;;

let s tl = seq (Std.list Fun.id tl)
let nil = Std.nil ()
let quineso q = evalo q nil (val_ q)
let twineso q p = q =/= p &&& evalo q nil (val_ p) &&& evalo p nil (val_ q)

let thrineso x =
  (* let (=//=) = diseqtrace @@ show_reif_term in *)
  fresh
    (p q r)
    (p =/= q)
    (q =/= r)
    (r =/= p)
    (evalo p nil (val_ q))
    (evalo q nil (val_ r))
    (evalo r nil (val_ p))
    (OCanren.(===) (Std.Triple.make p q r) x)
;;

let wrap_term rr = rr#reify gterm_reifier |> show_lterm
let wrap_result rr = rr#reify gresult_reifier |> show_lresult

let find_quines ~verbose ?(pp=show_lterm) n =
  run q quineso (fun r -> r#reify gterm_reifier)
  |> OCanren.Stream.take ~n
  |> List.iter (fun q -> if verbose then printf "%s\n\n" (pp q) else ())
;;

let find_twines ~verbose n =
  run qr twineso (fun q r -> q#reify Gterm.reify, r#reify Gterm.reify)
  |> OCanren.Stream.take ~n
  |> List.iter (fun (q, r) ->
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
  |> List.iter (fun a ->
    if verbose
    then (
      let () = wrap3terms a in
      print_newline ()))
;;
