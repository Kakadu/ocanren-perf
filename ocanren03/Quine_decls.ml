(*
   Quines stuff by Dmitrii Rozplokhas. Adopted from
   https://raw.githubusercontent.com/rozplokhas/OCanren/master/regression/test015.ml
*)

open Printf
open OCanren
open Tagged_stdlib

module StringLo = struct
  type ground = GT.string [@@deriving gt ~options:{ show; fmt; gmap }]
  type logic = string OCanren.logic

  let logic =
    { GT.gcata = ()
    ; fix = (fun _ _ -> assert false)
    ; plugins =
        object
          method fmt = GT.fmt OCanren.logic (fun ppf -> Format.fprintf ppf "%s")
          method gmap x = [%gmap: GT.string OCanren.logic] () x
        end
    }
  ;;

  type injected = GT.string OCanren.ilogic

  let prj_exn = OCanren.prj_exn
  let reify = OCanren.reify
end

module ListLo = struct
  type 'a ground = 'a Std.List.ground [@@deriving gt ~options:{ gmap; fmt }]
  type 'a logic = 'a Std.List.logic

  let logic =
    { Std.List.logic with
      plugins =
        object (self)
          (* method fmt_unwrapped fa ppf =
             function
             | Var _ -> assert false
             | Value Std.List.Nil -> ()
             | Value (Std.List.Cons (h, tl))

             Format.fprintf ppf "%a %a" fa h iter tl *)
          method fmt
            : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a logic -> unit =
            fun fa ppf xs ->
              match xs with
              | Value (Std.Wrapper.W (Var (idx, []))) | Var (idx, []) ->
                Format.fprintf ppf "_.%d" idx
              | Value (Std.Wrapper.W (Var (idx, _))) | Var (idx, _) -> assert false
              | Value (Std.Wrapper.W _) ->
                let rec iter ppf xs =
                  let _ : 'a logic = xs in
                  match xs with
                  | Value (Std.Wrapper.W (Value Std.List.Nil)) -> ()
                  | Value (Std.Wrapper.W (Value (Std.List.Cons (h, tl)))) ->
                    Format.fprintf ppf "%a %a" fa h iter tl
                  | Value (Std.Wrapper.W (Var (idx, []))) | Var (idx, []) ->
                    Format.fprintf ppf "_.%d" idx
                  | Value (Std.Wrapper.W (Var (idx, _))) | Var (idx, _) -> assert false
                in
                Format.fprintf ppf "(%a)" iter xs

          method gmap fa xs =
            let _ : _ logic = xs in
            [%gmap: 'a Std.List.logic] (GT.lift fa) () xs
        end
    }
  ;;

  type 'a injected = 'a Std.List.injected

  let prj_exn = Std.List.prj_exn
  let reify = Std.List.reify
end

let%expect_test _ =
  OCanren.(run q)
    (fun q -> q === Std.list ( !! ) [ 1; 2; 3 ])
    (fun s -> s#reify (Std.List.reify OCanren.reify))
  |> OCanren.Stream.iter (fun q ->
    Format.printf "%a" [%fmt: GT.int OCanren.logic ListLo.logic] q)
;;

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

  type nonrec ('s, 'xs) t =
    | Symb of 's
    | Seq of 'xs
  [@@deriving gt ~options:{ fmt; gmap }]

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

  type ground = (StringLo.ground Std.Wrapper.ground, ground ListLo.ground) t Std.Wrapper.t
  [@@deriving gt ~options:{ fmt }]

  type logic =
    (StringLo.logic Std.Wrapper.logic, logic ListLo.logic) t OCanren.logic
      Std.Wrapper.logic
  [@@deriving gt ~options:{ fmt }]

  type injected =
    (StringLo.injected Std.Wrapper.injected, injected ListLo.injected) t ilogic
      Std.Wrapper.injected

  let fmap_ fa fb eta = (GT.gmap t fa fb) eta

  let fmapt f__009_ f__010_ subj__011_ =
    let open OCanren.Env.Monad in
    OCanren.Env.Monad.return fmap_ <*> f__009_ <*> f__010_ <*> subj__011_
  ;;

  let (prj_exn : (injected, ground) OCanren.Reifier.t) =
    let open OCanren.Env.Monad in
    OCanren.Reifier.fix (fun self ->
      Std.Wrapper.prj_exn
        (OCanren.prj_exn
         <..> chain (fmapt (Std.Wrapper.prj_exn StringLo.prj_exn) (ListLo.prj_exn self))))
  ;;

  let (reify : (injected, logic) OCanren.Reifier.t) =
    let open OCanren.Env.Monad in
    OCanren.Reifier.fix (fun self ->
      Std.Wrapper.reify
        (OCanren.reify
         <..> chain
                (OCanren.Reifier.zed
                   (OCanren.Reifier.rework
                      ~fv:(fmapt (Std.Wrapper.reify StringLo.reify) (ListLo.reify self))))
        ))
  ;;

  let symb x : injected = Std.Wrapper.w (OCanren.inj (Symb (Std.Wrapper.w x)))
  let seq x : injected = Std.Wrapper.w (OCanren.inj (Seq x))

  (* This is a hack to apply custom printers for logic strings and lists *)
  (* include struct
     type logic =
     (StringLo.logic Std.Wrapper.logic, logic Std.List.logic) t OCanren.logic
     Std.Wrapper.logic
     [@@deriving gt ~options:{ fmt }]
     end *)

  let show_rterm = Format.asprintf "%a" (GT.fmt ground)
  let show_lterm = Format.asprintf "%a" (GT.fmt logic)
end

module Gresult = struct
  type nonrec ('s, 't, 'xs) t =
    | Closure of 's * 't * 'xs
    | Val_ of 't
  [@@deriving gt ~options:{ fmt; gmap }]

  type ground =
    ( StringLo.ground
      , Gterm.ground
      , (StringLo.ground, ground) Std.Pair.ground ListLo.ground )
      t
      Std.Wrapper.t
  [@@deriving gt ~options:{ fmt }]

  type injected =
    ( StringLo.injected
      , Gterm.injected
      , (StringLo.injected, injected) Std.Pair.injected ListLo.injected )
      t
      ilogic
      Std.Wrapper.injected

  let fmapt : ('a -> 'd) Env.m -> ('b -> 'e) Env.m -> ('c -> 'f) Env.m -> _ =
    fun fa fb fc subj ->
    let open OCanren.Env.Monad in
    OCanren.Env.Monad.return (GT.gmap t) <*> fa <*> fb <*> fc <*> subj
  ;;

  let (prj_exn : (injected, ground) OCanren.Reifier.t) =
    let open OCanren.Env.Monad in
    OCanren.Reifier.fix (fun self ->
      Std.Wrapper.prj_exn
        (OCanren.prj_exn
         <..> chain
                (fmapt
                   StringLo.prj_exn
                   Gterm.prj_exn
                   (ListLo.prj_exn (Std.Pair.prj_exn StringLo.prj_exn self)))))
  ;;

  type logic =
    (StringLo.logic, Gterm.logic, (StringLo.logic, logic) Std.Pair.logic ListLo.logic) t
      OCanren.logic
      Std.Wrapper.logic
  [@@deriving gt ~options:{ fmt }]

  let (reify : (injected, logic) OCanren.Reifier.t) =
    let open OCanren.Env.Monad in
    OCanren.Reifier.fix (fun self ->
      Std.Wrapper.reify
        (OCanren.reify
         <..> chain
                (OCanren.Reifier.zed
                   (OCanren.Reifier.rework
                      ~fv:
                        (fmapt
                           StringLo.reify
                           Gterm.reify
                           (ListLo.reify (Std.Pair.reify StringLo.reify self)))))))
  ;;

  let closure _x__060_ _x__061_ _x__062_ =
    Std.Wrapper.w (OCanren.inj (Closure (_x__060_, _x__061_, _x__062_)))
  ;;

  let val_ _x__063_ = Std.Wrapper.w (OCanren.inj (Val_ _x__063_))
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

type fenv = (string OCanren.ilogic, Gresult.injected) Std.Pair.injected Std.List.injected

let reif_env : (_, lenv) Reifier.t =
  Std.List.reify (Std.Pair.reify OCanren.reify gresult_reifier)
;;

let show_reif_term h t = show_lterm @@ Gterm.reify h t
let show_reif_result h t = show_lresult @@ Gresult.reify h t
let ( =/= ) = OCanren.( =/= )
let ( =//= ) = ( =/= )
let ( === ) = OCanren.( === )
let ( ===! ) = ( === )
let ( ===!! ) = ( === )

let rec lookupo : _ -> _ Std.Pair.injected ListLo.injected -> _ -> goal =
  fun x env t ->
  let open Tagged_stdlib.Std in
  fresh
    (rest y v)
    (Pair.pair y v % rest === env)
    (conde [ y ===! x &&& (v ===!! t); y =/= x &&& lookupo x rest t ])
;;

let rec not_in_envo x env =
  let _ : _ Std.Pair.injected Std.List.injected = env in
  let open Std in
  conde
    [ fresh (y v rest) (env === Std.pair y v % rest) (y =/= x) (not_in_envo x rest)
    ; nil () === env
    ]
;;

let rec proper_listo es env rs =
  let open Tagged_stdlib.Std in
  let _ : Gterm.injected ListLo.injected = es in
  let _ : _ Std.Pair.injected ListLo.injected = env in
  let _ : Gterm.injected ListLo.injected = rs in
  conde
    [ Std.nil () === es &&& (Std.nil () === rs)
    ; fresh
        (e d te td)
        (es === e % d)
        (rs === te % td)
        (evalo e env (val_ te))
        (proper_listo d env td)
    ]

and evalo (term : Gterm.injected) (env : fenv) (r : Gresult.injected) =
  let open Tagged_stdlib.Std in
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
    (p =//= q)
    (q =//= r)
    (r =//= p)
    (evalo p nil (val_ q))
    (evalo q nil (val_ r))
    (evalo r nil (val_ p))
    (Std.Triple.make p q r === x)
;;

let wrap_term rr = rr#reify Gterm.reify |> show_lterm
let wrap_result rr = rr#reify gresult_reifier |> show_lresult

let find_quines ~verbose n =
  run q quineso (fun r -> r#reify Gterm.reify)
  |> OCanren.Stream.take ~n
  |> List.iter (fun q -> if verbose then printf "%s\n\n" (show_lterm q) else ())
;;

let find_twines ~verbose n =
  run qr twineso (fun q r -> q#reify Gterm.reify, r#reify Gterm.reify)
  |> OCanren.Stream.take ~n
  |> List.iter (fun (q, r) ->
    if verbose then printf "%s,\n%s\n\n" (show_lterm q) (show_lterm r) else ())
;;

let wrap3terms = function
  | Value (Std.Wrapper.W (Var _)) | Var _ -> assert false
  | Value (Std.Wrapper.W (Value (a, b, c))) ->
    printf
      "* %s\n  %s\n  %s\n\n"
      (Gterm.show_lterm a)
      (Gterm.show_lterm b)
      (Gterm.show_lterm c)
;;

let find_thrines ~verbose n =
  run q thrineso (fun r -> r#reify (Std.Triple.reify Gterm.reify Gterm.reify Gterm.reify))
  |> Stream.take ~n
  |> List.iter (fun a ->
    if verbose
    then (
      let () = wrap3terms a in
      print_newline ()))
;;
