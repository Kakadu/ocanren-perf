open OCanren
open Tester

let flip f a b = f b a

let trace msg =
  debug_var !!1 (flip OCanren.reify) (function
      | [ _ ] ->
        print_endline msg;
        success
      | _ -> assert false)
;;

let debug_intlist =
  Quine_decls.debug_wrapper
    (Std.Pair.reify (Std.List.reify OCanren.reify) (Std.List.reify OCanren.reify))
    (GT.show OCanren.Std.List.logic @@ GT.show OCanren.logic (GT.show GT.int))
;;

let unilist u v = debug_intlist "\t===" u v &&& (u === v)

let rec appendo a b ab =
  let open OCanren.Std in
  let ( === ) = unilist in
  conde
    [ trace "appendo" &&& failure
    ; Std.nil () === a &&& trace "a" &&& (b === ab) &&& trace "e"
    ; fresh
        (h t temp)
        (trace "b")
        (a === h % t)
        (ab === h % temp)
        (trace "c")
        (appendo t b temp)
        (trace "d")
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

let () =
  run_exn
    show_int_list
    3
    qr
    qrh
    (REPR (fun q r -> appendo q r (Std.List.list input_ints)))
;;

let () =
  Format.printf
    "Unification counter after  = %d\n%!"
    (OCanren.Peep.unification_counter ())
;;
