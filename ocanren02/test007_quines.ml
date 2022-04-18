let count = 200
let () = TimeHelper.wrap (Quine_decls.find_quines count)

(* Unification counter after
      = 2085 for 1st quine
      = 6920 for 2nd quine
      = 8480
      = 18797 for 10th
      ...
      = 274012  for 200 quines
 *)
