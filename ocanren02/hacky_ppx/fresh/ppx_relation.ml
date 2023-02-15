(*
 * OCanren. PPX suntax extensions.
 * Copyright (C) 2015-2022
 * Dmitri Boulytchev, Dmitry Kosarev, Alexey Syomin, Evgeny Moiseenko
 * St.Petersburg State University, JetBrains Research
 *
 * This software is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License version 2, as published by the Free Software Foundation.
 *
 * This software is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 *
 * See the GNU Library General Public License version 2 for more details
 * (enclosed in the file COPYING).
 *)

(*   Performs two minikanren-specific macro expansions:
 *  1) fresh (x1 ... xm) e1 ... en
 *            to
 *     Fresh.numeral (fun x1 ... xm -> e1 &&& ... &&& en)
 *
 *  2) TODO: write about defer
 *)

open Ppxlib
open Ppxlib.Ast_helper
open Stdppx

let rec has_attr name = function
  | x :: xs -> String.equal x.attr_name.Location.txt name || has_attr name xs
  | [] -> false
;;

let remove_attr name =
  List.filter ~f:(fun x -> not (String.equal x.attr_name.Location.txt name))
;;

let is_state_pattern pat =
  match pat.ppat_desc with
  | Ppat_var v when String.equal v.txt "st" || String.equal v.txt "state" -> Some v.txt
  | _ -> None
;;

let classify_name ~f e =
  match e.pexp_desc with
  | Pexp_ident i when f i.txt -> true
  | _ -> false
;;

let need_insert_fname ~name e = classify_name e ~f:(Stdlib.( = ) (Lident name))
(* match e.pexp_desc with
  | Pexp_ident i when i.txt = Lident name -> true
  | _ -> false *)

let is_defer = need_insert_fname ~name:"defer"

(* let is_conde = need_insert_fname ~name:"conde" *)
let is_fresh = need_insert_fname ~name:"fresh"
let is_call_fresh = need_insert_fname ~name:"call_fresh"

let is_unif =
  classify_name ~f:(function
    | Lident s -> String.length s >= 3 && String.equal (String.sub s ~pos:0 ~len:3) "==="
    | _ -> false)
;;

let is_conj = need_insert_fname ~name:"conj"
let is_disj e = need_insert_fname ~name:"disj" e || need_insert_fname ~name:"|||" e

(*
let rec walkthrough ~fname (expr: expression) =

  let add_fname () =
    [%expr [%e Ast_helper.Exp.constant (Pconst_string (fname,None))] <=>
           [%e expr]
    ]
  in
  match expr.pexp_desc with
  | Pexp_fun (_label, _opt, pat, e2) -> begin
      match is_state_pattern pat with
      | None ->
        { expr with pexp_desc =
                   Pexp_fun (_label, _opt, pat, walkthrough ~fname e2) }
      | Some argname ->
        (* printf "found good function with statearg '%s'\n%!" argname; *)
        let new_body =
          [%expr
             let () = Printf.printf "entering '%s'\n%!" [%e Ast_helper.Exp.constant (Pconst_string (fname,None))] in
             let ans = [%e e2] in
             let () = Printf.printf "leaving '%s'\n%!"  [%e Ast_helper.Exp.constant (Pconst_string (fname,None))] in
             ans
          ]
        in
        { expr with pexp_desc= Pexp_fun (_label, _opt, pat, new_body) }
    end
  | Pexp_apply (e,_) when is_call_fresh e -> add_fname ()
  | Pexp_apply (e,_) when is_disj e -> add_fname ()
  | Pexp_apply (e,_) when is_conj e -> add_fname ()

  | _ -> expr


let map_value_binding (vb : value_binding) =
  match vb.pvb_pat.ppat_desc with
  | Ppat_var name ->
    let fname = name.txt in
    { vb with pvb_expr = walkthrough ~fname vb.pvb_expr }
  |  _ -> vb

let smart_logger =
  { default_mapper with
    structure_item = fun mapper sitem ->
      match sitem.pstr_desc with
      | Pstr_value (_rec, vbs) ->
        { sitem with pstr_desc = Pstr_value (_rec, List.map vbs ~f:map_value_binding) }
      | x -> default_mapper.structure_item mapper sitem
  }
*)

let option_map ~f = function
  | Some x -> Some (f x)
  | None -> None
;;

let option_bind ~f = function
  | Some x -> f x
  | None -> None
;;

exception Not_an_ident

let reconstruct_args e =
  let open Longident in
  let are_all_idents (xs : (_ * expression) list) =
    try
      Some
        (List.map xs ~f:(fun (_, e) ->
           match e.pexp_desc with
           | Pexp_ident { txt = Longident.Lident i; _ } -> i
           | _ -> raise Not_an_ident))
    with
    | Not_an_ident -> None
  in
  match e.pexp_desc with
  | Pexp_apply ({ pexp_desc = Pexp_ident { txt = Longident.Lident arg1; _ } }, ys) ->
    (* fresh (var1 var2 var3) body *)
    option_map (are_all_idents ys) ~f:(fun xs -> arg1 :: xs)
  (* no fresh variables: just for geting rid of &&&  *)
  | Pexp_construct ({ txt = Lident "()" }, None) -> Some []
  (* [fresh arg0 body] -- single fresh variable  *)
  | Pexp_ident { txt = Lident arg1; _ } -> Some [ arg1 ]
  | _ -> None
;;

let list_fold ~f ~initer xs =
  match xs with
  | [] -> failwith "bad argument"
  | start :: xs -> List.fold_left ~init:(initer start) ~f xs
;;

let list_fold_right0 ~f ~initer xs =
  let rec helper = function
    | [] -> failwith "bad_argument"
    | x :: xs -> list_fold ~initer ~f:(fun acc x -> f x acc) (x :: xs)
  in
  helper (List.rev xs)
;;

let mapper =
  object (self)
    inherit Ast_traverse.map as super

    method! value_binding vb =
      let rec look_inside wrap e =
        match e.pexp_desc with
        | Pexp_fun (lab, v, pat, rhs) ->
          let rhs = look_inside wrap rhs in
          { e with pexp_desc = Pexp_fun (lab, v, pat, rhs) }
        | _ -> wrap e
      in
      if has_attr "relation" vb.pvb_attributes
      then (
        let wrap e =
          let loc = e.pexp_loc in
          [%expr delay (fun () -> [%e e])]
        in
        { vb with
          pvb_expr = look_inside wrap vb.pvb_expr
        ; pvb_attributes = remove_attr "relation" vb.pvb_attributes
        })
      else super#value_binding vb
  end
;;

let () = Ppxlib.Driver.register_transformation ~impl:mapper#structure "relation"
