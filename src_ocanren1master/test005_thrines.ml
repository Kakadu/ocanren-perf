open MiniKanren
open Tester
open Quine_decls

let () = find_thrines 2

let () = MiniKanren.report_counters ()
let () = Gc.print_stat stdout
