open Numero_decls
open OCanren
open Tester

let%expect_test _ =
  run_r
    Oleg.prj_exn
    Oleg.show_ground
    (-1)
    q
    qh
    ("3^5=243", Numero_decls.(fun q -> expo (build_num 3) (build_num 5) q));
  [%expect "
    3^5=243, all answers {
    q=[1; 1; 0; 0; 1; 1; 1; 1];
    }"]
;;

let%expect_test _ =
  run_r
    Oleg.prj_exn
    Oleg.show_ground
    (-1)
    q
    qh
    ( "log_3 243=5"
    , Numero_decls.(fun q -> logo (build_num 243) (build_num 3) q (build_num 0)) );
  [%expect "
    log_3 243=5, all answers {
    q=[1; 0; 1];
    }"]
;;
