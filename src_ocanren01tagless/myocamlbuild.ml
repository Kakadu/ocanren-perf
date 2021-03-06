open Ocamlbuild_plugin;;
open Command;;

let () = dispatch begin function
  | Before_options ->
      Options.ocamldep   := S[A"ocamlfind";A"ocamldep";A"-verbose"]

  | After_rules -> begin
      flag ["ocaml";"compile";"native";"keep_asm"] (S [ A "-S"]);
    end
  | _ -> ()
end
