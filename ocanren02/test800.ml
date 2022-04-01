open OCanren
open Quine_decls
open Gterm

let seq xs = Gterm.seq (Std.List.list xs)
let symb_ s = Gterm.symb !!s

let quineso q =
  fresh
    (arg _3903)
    (arg
    === seq
          [ seq
              [ symb_ "lambda"
              ; seq [ symb _3903 ]
              ; seq
                  [ symb !!"list"
                  ; symb _3903
                  ; seq [ symb_ "list"; seq [ symb_ "quote"; symb_ "quote" ]; symb _3903 ]
                  ]
              ]
          ; seq
              [ symb_ "quote"
              ; seq
                  [ symb_ "lambda"
                  ; seq [ symb _3903 ]
                  ; seq
                      [ symb_ "list"
                      ; symb _3903
                      ; seq
                          [ symb_ "list"
                          ; seq [ symb_ "quote"; symb_ "quote" ]
                          ; symb _3903
                          ]
                      ]
                  ]
              ]
          ])
    (evalo arg nil (Gresult.val_ q))
;;

let () =
  run q quineso (fun r -> r)
  |> OCanren.Stream.take ~n:1
  |> List.iter (fun q -> Printf.printf "%s\n\n" (wrap_term q))
;;

let () =
  Format.printf
    "Unification counter after  = %d\n%!"
    (OCanren.Peep.unification_counter ())
;;
