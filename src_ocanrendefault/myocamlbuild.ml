open Ocamlbuild_plugin;;
open Command;;

dispatch begin function
  | After_rules -> begin
      flag ["ocaml";"compile";"native";"keep_asm"] (S [ A "-S"]);
    end
  | _ -> ()
end
