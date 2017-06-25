open Printf
open MiniKanren
open Tester
open Quines_NoDiseq

let _ = find_quines 100

let () = MiniKanren.report_counters ()
let () = Gc.print_stat stdout
