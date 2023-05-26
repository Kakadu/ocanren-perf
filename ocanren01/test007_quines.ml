let count = 200
(* let () = TimeHelper.wrap (Quine_decls.find_quines count) *)

(* Unification counter after
      = 2085 for 1st quine
      = 6920 for 2nd quine
      = 8480
      = 18797 for 10th
      ...
      = 274012  for 200 quines
 *)

let () =
  (* warmup *)
  Quine_decls.find_quines ~verbose:true count
;;

open Benchmark

let repeat = 2
let style = Auto
let iterations = 15L
let confidence = 0.95

let () =
  let res =
    latency1
      ~repeat
      ~style
      iterations
      (fun c -> Quine_decls.find_quines ~verbose:false c)
      count
  in
  print_newline ();
  let numbers = snd (List.hd res) in
  assert (List.length numbers = repeat);
  List.iter (fun { Benchmark.wall } -> Printf.printf "%f s\n%!" wall) numbers;
  tabulate ~confidence res
;;
