open MiniKanren
open Tester
open Quine_decls

let () = find_quines 100

let () = MiniKanren.report_counters ()
let () = Gc.print_stat stdout
