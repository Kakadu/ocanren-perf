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

(* candidate goals over a plain logic var; selected by argv[1] *)
let g1 = fun q -> fresh t (q === t)
let g2 = fun q -> conde [ fresh t (q === t); fresh u (q === u) ]
let g3 = fun q -> conde [ fresh t (q === t); fresh u (q === u); fresh v (q === v) ]
let g4 = fun q -> conde [ fresh t (q === t); fresh (u v) (q === u) (v === u) ]
let g5 = fun q -> fresh t (conde [ q === t; q === t ])
let g6 = fun q -> conde [ fresh t (fresh u (q === t) (u === t)); fresh v (q === v) ]

(* structurally-faithful candidate: nested conde + fresh + bounded recursion *)
type 'a llist = ('a ilogic, 'a ilogic OCanren.Std.List.injected) OCanren.Std.List.t ilogic

let lnil () : 'a llist = OCanren.Std.nil ()
let lcons (x : 'a ilogic) (y : 'a llist) : 'a llist = OCanren.Std.(x % y)

(* q is any element of xs; branches at each level (take-head vs recurse-tail) *)
let rec member (xs : 'a llist) (q : 'a ilogic) : goal =
  conde [ fresh (h t) (xs === lcons h t) (q === h)
        ; fresh (h t) (xs === lcons h t) (member t q) ]
;;
let g7 = fun q -> member (lcons (inj 1) (lcons (inj 2) (lcons (inj 3) (lnil ())))) q

(* q is an element of xs not equal to 99; diseq + branching + recursion, multi-answer *)
let rec filter (xs : 'a llist) (q : 'a ilogic) : goal =
  conde [ fresh (h t) (xs === lcons h t) (h =/= inj 99) (q === h)
        ; fresh (h t) (xs === lcons h t) (filter t q) ]
;;
let g10 =
  fun q ->
  filter (lcons (inj 1) (lcons (inj 99) (lcons (inj 2) (lcons (inj 99) (lcons (inj 3) (lnil ())))))) q

(* mutual recursion (ma<->mb) + branching, elements of xs (multi-answer) *)
let rec ma (xs : 'a llist) (q : 'a ilogic) : goal =
  conde [ fresh (h t) (xs === lcons h t) (q === h); fresh (h t) (xs === lcons h t) (mb t q) ]
and mb (xs : 'a llist) (q : 'a ilogic) : goal =
  conde [ fresh (h t) (xs === lcons h t) (q === h); fresh (h t) (xs === lcons h t) (ma t q) ]
let g12 = fun q -> ma (lcons (inj 1) (lcons (inj 2) (lcons (inj 3) (lnil ())))) q

let () =
  let n = !goal_num in
  let g = match n with 1->g1|2->g2|3->g3|4->g4|5->g5|6->g6|7->g7|10->g10|12->g12|_->assert false in
  let ans = run one g (fun _ -> ()) |> Stream.take ~n:10 in
  Printf.printf "goal %d answers=%d\n" n (Stdlib.List.length ans)
