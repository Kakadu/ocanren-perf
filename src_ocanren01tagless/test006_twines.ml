open MiniKanren
open Quine_decls

let () = TimeHelper.wrap (Quine_decls.find_twines 30)
