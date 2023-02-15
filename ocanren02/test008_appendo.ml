open OCanren
open Tester

let nels = 20

let input_ints =
  Array.init nels (fun i -> 1 + (i mod 20)) |> Array.to_list |> List.map (fun x -> inj x)
;;

let () = assert (nels == List.length input_ints)

let rec appendo a b ab =
  let open Std in
  conde
    [ a === nil () &&& (b === ab)
    ; fresh (h t ab') (a === h % t) (h % ab' === ab) (appendo t b ab')
    ]
  [@@relation]
;;

(*  *)
let show_int_list = [%show: GT.int OCanren.logic Std.List.logic] ()
let run_list n = run_r (Std.List.reify OCanren.reify) show_int_list n
let rec delay_is_important q = failure &&& delay_is_important q

let __ () =
  TimeHelper.wrap (fun ~verbose ->
    run_list
      (-1)
      qr
      qrh
      (REPR (fun q r -> Std.List.appendo (Std.List.list [ !!1; !!2; !!3 ]) q r)))
;;

let () =
  TimeHelper.wrap (fun ~verbose ->
    run_list
      (-1)
      qr
      qrh
      (REPR (fun q r -> Std.List.appendo q r (Std.List.list input_ints))))
;;
