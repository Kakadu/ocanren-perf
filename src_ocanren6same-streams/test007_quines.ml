open MiniKanren
open Tester
open Quine_decls

let _ = find_quines 100

let () = MiniKanren.report_counters ()
