open MiniKanren
open TimeHelper

let () = TimeHelper.wrap (Quine_decls.find_thrines 2)
