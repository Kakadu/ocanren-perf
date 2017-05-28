open MiniKanren
open Tester
open Quine_decls

let () = find_twines 15

let () = MiniKanren.report_counters ()
