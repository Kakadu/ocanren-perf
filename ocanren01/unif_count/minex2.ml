open Quine_decls_trace
open OCanren

let dlog_file = ref None
let goal_num = ref 1
let () =
  let args = Sys.argv in
  let i = ref 1 in
  while !i < Array.length args do
    if args.(!i) = "-dlog" then (dlog_file := Some args.(1 + !i); i := !i + 2)
    else (goal_num := int_of_string args.(!i); incr i)
  done
let () =
  match !dlog_file with None -> () | Some f -> OCanren.Disequality.set_log_port (open_out f)

let lnil () = OCanren.Std.nil ()
let lcons x y = OCanren.Std.(x % y)

(* g1: unify a var against a nested Gterm template (no recursion, no diseq) *)
let g1 = fun (q : Gterm.injected) ->
  fresh t (q === Gterm.seq (lcons (Gterm.symb (!! "quote")) (lcons t (lnil ()))))

(* g2: not_in_envo on a 3-element env (real recursive relation, uses built-in diseq) *)
let pair_of k v = OCanren.Std.Pair.pair (!! k) (Gresult.val_ (Gterm.symb (!! v)))
let three_env =
  lcons (pair_of "a" "v1") (lcons (pair_of "b" "v2") (lcons (pair_of "c" "v3") (lnil ())))
(* fresh env + unify, matching the Racket side exactly *)
let g2 = fun (q : Gresult.injected) ->
  fresh env (env ===!! three_env) (not_in_envo (!! "z") env) &&& (q ===! Gresult.val_ (Gterm.symb (!! "ok")))

(* g3: lookupo over a 2-element env, all (key,value) bindings; Gterm + diseq + recursion, multi-answer *)
let two_env = lcons (pair_of "a" "v1") (lcons (pair_of "b" "v2") (lnil ()))
let g3 = fun (q : Gresult.injected) -> fresh x (lookupo x two_env q)

let () =
  clear_unifications ();
  let n = !goal_num in
  if n = 1 then (
    let ans = run one g1 (fun _ -> ()) |> Stream.take ~n:10 in
    Printf.printf "g1 answers=%d\n" (Stdlib.List.length ans)
  ) else if n = 2 then (
    let ans = run one g2 (fun _ -> ()) |> Stream.take ~n:10 in
    Printf.printf "g2 answers=%d\n" (Stdlib.List.length ans)
  ) else (
    let ans = run one g3 (fun _ -> ()) |> Stream.take ~n:10 in
    Printf.printf "g3 answers=%d\n" (Stdlib.List.length ans)
  )
