open MiniKanren
open Tester
open Quine_decls



let find_quines n = run q quineso @@ fun qs ->
  Stream.take ~n qs |> List.map wrap_term |> List.iter (printfn "%s\n")

let () = find_quines 1

let eval_first_quine () =
  let make_quine q =
    let open Gterm in
    let seq xs = seq (List.of_list xs) in
    let q = symb q in
    let symb s = symb !!s in

    (seq  [ seq [ (symb "lambda")
                ; (seq [( q)])
                ; (seq [ (symb "list")
                      ; ( q)
                      ; (seq [(symb "list"); (seq [(symb "quote"); (symb "quote")]);  q])])]
          ; (seq [ (symb "quote")
                 ; (seq [(symb "lambda")
                        ; (seq [( q)])
                        ; (seq [(symb "list")
                               ; ( q)
                               ; (seq [ (symb "list")
                                      ; (seq [ (symb "quote")
                                             ; (symb "quote")])
                                      ; ( q)])])])])])
(*
    (seq  [ seq [ (symb 'lambda)
                ; (seq [(symb '_.3822 [=/= list; =/= quote])])
                ; (seq [ (symb "list")
                      ; (symb '_.3822 [=/= list; =/= quote])
                      ; (seq [(symb "list"); (seq [(symb 'quote); (symb 'quote)]); (symb '_.3822 [=/= list; =/= quote])])])]
          ; (seq [ (symb 'quote)
                 ; (seq [(symb 'lambda)
                        ; (seq [(symb '_.3822 [=/= list; =/= quote])])
                        ; (seq [(symb "list")
                               ; (symb '_.3822 [=/= list; =/= quote])
                               ; (seq [ (symb "list")
                                      ; (seq [(symb 'quote); (symb 'quote)])
                                      ; (symb '_.3822 [=/= list; =/= quote])])])])])]) *)
  in
  run q (fun r -> Fresh.one (fun temp ->
                    let quine = make_quine temp in
                    evalo quine nil r))
                    @@ fun qs ->
    Stream.take ~n:1 qs |> List.map wrap_result |> List.iter (printfn "%s\n")
let () = MiniKanren.report_counters ()
