open MiniKanren
open Tester
open Quine_decls

let _ = find_quines 5

(* let () =
  Tester.runR gterm_reifier Gterm.show_rterm Gterm.show_lterm 13 q qh
	(REPR(fun q -> call_fresh (fun r -> evalo q nil r) )); *)
