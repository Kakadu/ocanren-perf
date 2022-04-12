open OCanren
open Tester

let flip f a b = f b a
let reify_int_list = Std.List.reify (OCanren.reify : _ -> (int, _) injected -> _)

let trace msg =
  debug_var !!1 (flip OCanren.reify) (function
      | [ _ ] ->
        print_endline msg;
        success
      | _ -> assert false)
;;

let debug_intlist =
  Quine_decls.debug_wrapper
    (Std.Pair.reify reify_int_list reify_int_list)
    (GT.show OCanren.Std.List.logic @@ GT.show OCanren.logic (GT.show GT.int))
;;

let unilist ?(msg = "") u v =
  debug_intlist (Format.sprintf "%s\t===" msg) u v &&& (u === v)
;;

let rec appendo a b ab =
  let open OCanren.Std in
  let ( === ) a b msg = unilist a b ~msg in
  conde
    [ trace "appendo" &&& failure
    ; ( === ) (Std.nil ()) a "A" &&& ( === ) b ab "E"
    ; fresh
        (h t temp)
        (( === ) a (h % t) "F")
        (( === ) ab (h % temp) "C")
        (appendo t b temp)
    ]
;;

let nels = 3

let input_ints =
  Array.init nels (fun i -> 1 + (i mod 20))
  |> Array.to_list
  |> List.map (fun x -> inj @@ lift x)
;;

let () = assert (nels == List.length input_ints)
let show_int_list = GT.(show Std.List.ground @@ show int)
let show_intl_listl = GT.show Std.List.logic @@ GT.show OCanren.logic (GT.show GT.int)

(* let (_ : int) = runR reify_int_list show_int_list show_intl_listl *)
let run_list eta = runR reify_int_list show_int_list show_intl_listl eta

let run_list2 eta =
  runR
    (Std.Pair.reify reify_int_list reify_int_list)
    (GT.show Std.Pair.ground show_int_list show_int_list)
    (GT.show Std.Pair.logic show_intl_listl show_intl_listl)
    eta
;;

let __ () =
  run_list2
    3
    q
    qh
    (REPR
       (fun q ->
         fresh (a b) (appendo a b (Std.List.list input_ints)) (q === Std.pair a b)))
;;

(* Internally scheme uses run for single argument where this argument
  is a tuple of many arguments. That's why `run n` may give n extra
  unifications
   *)
let () =
  run_list2
    3
    q
    qh
    (REPR
       (fun q ->
         fresh (a b) (appendo (Std.List.list input_ints) a b) (q === Std.pair a b)))
;;

let () =
  Format.printf
    "Unification counter after  = %d\n%!"
    (OCanren.Peep.unification_counter ())
;;
