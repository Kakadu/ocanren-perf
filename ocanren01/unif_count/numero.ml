open Numero_decls_trace

let wrap : ?n:int -> string * (Numero_decls_trace.Oleg.injected -> OCanren.goal) -> unit =
  fun ?(n = 1) (msg, goal) ->
  print_endline msg;
  OCanren.(run q) goal (fun rr -> rr#reify Numero_decls_trace.num_reifier)
  |> OCanren.Stream.take ~n
  |> List.iteri (fun i n ->
    Format.printf "%3d:\t%s\n%!" i (Numero_decls_trace.show_num_logic n))
;;

let expo1 () = wrap (REPR (fun q -> expo (build_num 3) (build_num 5) q))
let expo2 () = wrap (REPR (fun q -> expo (build_num 1) (build_num 2) q))
let expo3 () = wrap (REPR (fun q -> expo (build_num 2) (build_num 2) q))
let expo4 () = wrap (REPR (expo (build_num 2) (build_num 1)))
let expo5 () = wrap (REPR (expo (build_num 3) (build_num 2)))
let gen_mul a b () = wrap (Printf.sprintf "%dx%d=?" a b, multo (build_num a) (build_num b))
let mul1x1 () = wrap (REPR (multo (build_num 1) (build_num 1)))
let mul1x2 () = wrap (REPR (multo (build_num 1) (build_num 2)))
let mul1x2 () = wrap (REPR (multo (build_num 1) (build_num 2)))
let mul2x3 () = wrap (REPR (multo (build_num 2) (build_num 3)))
let mul3x3 () = wrap (REPR (multo (build_num 3) (build_num 3)))
let mul3x3all () = wrap ~n:(-1) (REPR (multo (build_num 3) (build_num 3)))
let mul3x5 () = wrap (REPR (multo (build_num 3) (build_num 5)))
let mul4x4 () = wrap (REPR (multo (build_num 4) (build_num 4)))
let mul5x3 () = wrap (REPR (multo (build_num 5) (build_num 3)))
let mul5x4 () = wrap (REPR (multo (build_num 5) (build_num 4)))
let mul5x5 () = wrap (REPR (multo (build_num 5) (build_num 5)))
let mul5x5_all () = wrap ~n:(-1) (REPR (multo (build_num 5) (build_num 5)))
let mul5x6 () = wrap (REPR (multo (build_num 5) (build_num 6)))
let mul7x7 () = wrap (REPR (multo (build_num 7) (build_num 7)))
let mul127x127 () = wrap (REPR (multo (build_num 127) (build_num 127)))
let mul255x255 () = wrap (REPR (multo (build_num 255) (build_num 255)))

(* let mul2 () = wrap (REPR (multo (build_num 255) (build_num 255))) *)
let repeatedMul1 () = wrap (REPR (repeated_mul (build_num 3) (build_num 2)))
let odd_multo1 () = wrap (REPR (odd_multo (build_num 1) (build_num 3) (build_num 3)))
let exp2in3 () = wrap (REPR (expo (build_num 2) (build_num 3)))
let exp3in5 () = wrap (REPR (expo (build_num 3) (build_num 5)))
let exp7in2 () = wrap (REPR (expo (build_num 7) (build_num 2)))

let logo8base2 () =
  wrap (REPR (fun q -> logo (build_num 8) (build_num 2) q (build_num 0)))
;;

let logo243base3 () =
  wrap (REPR (fun q -> logo (build_num 243) (build_num 3) q (build_num 0)))
;;

let () =
  let wrap name f =
    ( name
    , Arg.Unit
        (fun () ->
          Numero_decls_trace.clear_unifications ();
          f ();
          Printf.printf "unifications: %d\n" config.unifications)
    , "" )
  in
  Arg.parse
    [ wrap "--ex1" expo1
    ; wrap "--ex2" expo2
    ; wrap "--ex3" expo3
    ; wrap "--ex4" expo4
    ; wrap "--ex5" expo5
    ; wrap "--mul1x1" mul1x1
    ; wrap "--mul1x2" mul1x2
    ; wrap "--mul2x2" (gen_mul 2 2)
    ; wrap "--mul2x3" mul2x3
    ; wrap "--mul3x2" (gen_mul 3 2)
    ; wrap "--mul3x3" mul3x3
    ; wrap "--mul3x3-all" mul3x3all
    ; wrap "--mul3x5" mul3x5
    ; wrap "--mul4x4" mul4x4
    ; wrap "--mul5x3" mul5x3
    ; wrap "--mul5x4" mul5x4
    ; wrap "--mul5x5" mul5x5
    ; wrap "--mul5x5-all" mul5x5_all
    ; wrap "--mul5x6" mul5x6
    ; wrap "--mul7x7" mul7x7
    ; wrap "--mul127x127" mul127x127
    ; wrap "--mul255x255" mul255x255
    ; wrap "--exp2x3" exp2in3
    ; wrap "--exp3x5" exp3in5
    ; wrap "--exp7x2" exp7in2
    ; wrap "--repeatedMul1" repeatedMul1
    ; wrap "--odd_mul1" odd_multo1
    ; wrap "--logo8base2" logo8base2
    ; wrap "--logo243base3" logo243base3
    ]
    (fun _ -> assert false)
    ""
;;
