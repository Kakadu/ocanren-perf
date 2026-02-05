let count = ref 200
let () = Arg.parse [ "-c", Arg.Set_int count, " count" ] (fun _ -> assert false) "help"

let show : Quine_decls.Gterm.logic -> string =
  let open Format in
  let open Quine_decls.Gterm in
  let rec helper ppf repr =
    match project_exn repr with
    | term -> helper_ground ppf term
    | exception OCanren.Not_a_value ->
      (match repr with
       | OCanren.Var (n, _) -> fprintf ppf "_.%d" n
       | Value (Seq xs) -> GT.fmt Quine_decls.ListLo.logic helper ppf xs
       | Value (Symb (OCanren.Value s)) -> fprintf ppf "%s" s
       | Value (Symb (Var (idx, _))) -> fprintf ppf "_.%d" idx)
  and helper_ground ppf = function
    | Symb s -> fprintf ppf "%s" s
    | Seq [ Symb "quote"; Symb "quote" ] -> fprintf ppf "'quote"
    | Seq xs ->
      fprintf
        ppf
        "(%a)"
        (pp_print_list ~pp_sep:(fun ppf () -> fprintf ppf " ") helper_ground)
        xs
  in
  fun x -> Format.asprintf "%a" helper x
;;

let () = TimeHelper.wrap (Quine_decls.find_quines ~pp:show !count)

(* Unification counter after
   = 2085 for 1st quine
   = 6920 for 2nd quine
   = 8480
   = 18797 for 10th
   ...
   = 274012  for 200 quines
*)
