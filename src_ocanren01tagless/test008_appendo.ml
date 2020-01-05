open OCanren
open Tester
open Quine_decls

let nels = 20
let input_ints = Array.init nels (fun i -> 1 + (i mod 20)) |> Array.to_list |> List.map (fun x -> inj@@lift x)
let () = assert (nels == List.length input_ints)

let show_int_list = GT.(show Std.List.ground @@ show int)
let () =
  run_exn show_int_list  (-1)  qr qrh (REPR (fun q r -> Std.List.appendo q r (Std.List.list input_ints) ))
