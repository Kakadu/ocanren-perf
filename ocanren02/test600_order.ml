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

let debug_int =
  Quine_decls.debug_wrapper
    (Std.Pair.reify OCanren.reify OCanren.reify)
    (GT.show OCanren.logic (GT.show GT.int))
;;

let uniint u v = debug_int "\t===" u v &&& (u === v)

let func e =
  let open OCanren.Std in
  let ( === ) = uniint in
  conde [ fresh t (e === !!0) (e === !!1); fresh es (e === !!2); fresh zzzz (e === !!3) ]
;;

let show_int_list = GT.(show Std.List.ground @@ show int)
let () = run_exn (GT.show GT.int) 2 q qh (REPR func)

let () =
  Format.printf
    "Unification counter after  = %d\n%!"
    (OCanren.Peep.unification_counter ())
;;
