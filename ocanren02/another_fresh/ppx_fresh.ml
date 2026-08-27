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
let is_conde = need_insert_fname ~name:"conde"
let is_fresh = need_insert_fname ~name:"fresh"
let is_call_fresh = need_insert_fname ~name:"call_fresh"

let is_unif =
  classify_name ~f:(function
    | Lident s -> String.length s >= 3 && String.equal (String.sub s ~pos:0 ~len:3) "==="
    | _ -> false)
;;

let is_conj = need_insert_fname ~name:"conj"
let is_infix_conj = need_insert_fname ~name:"&&&"
let is_bind_star = need_insert_fname ~name:"bind_star"
let is_mplus_star = need_insert_fname ~name:"mplus_star"
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
  | Pexp_apply ({ pexp_desc = Pexp_ident { txt = Longident.Lident arg1; _ }; _ }, ys) ->
    (* fresh (var1 var2 var3) body *)
    option_map (are_all_idents ys) ~f:(fun xs -> arg1 :: xs)
  (* no fresh variables: just for geting rid of &&&  *)
  | Pexp_construct ({ txt = Lident "()"; _ }, None) -> Some []
  (* [fresh arg0 body] -- single fresh variable  *)
  | Pexp_ident { txt = Lident arg1; _ } -> Some [ arg1 ]
  | _ -> None
;;

let list_fold ~f ~initer xs =
  match xs with
  | [] -> failwith "bad argument"
  | start :: xs -> List.fold_left ~init:(initer start) ~f xs
;;

let list_fold_left1 ~f xs = list_fold ~f ~initer:Fun.id xs

let list_fold_right0 ~f ~initer xs =
  let helper = function
    | [] -> failwith "bad_argument"
    | x :: xs -> list_fold ~initer ~f:(fun acc x -> f x acc) (x :: xs)
  in
  helper (List.rev xs)
;;

let my_list ~loc es =
  List.fold_right ~init:[%expr []] es ~f:(fun x acc -> [%expr [%e x] :: [%e acc]])
;;

let parse_to_list alist =
  let rec helper acc ele =
    match ele.pexp_desc with
    | Pexp_construct ({ txt = Lident "[]"; _ }, None) -> acc
    | Pexp_construct
        ({ txt = Lident "::"; _ }, Some { pexp_desc = Pexp_tuple [ y1; y2 ]; _ }) ->
      helper (y1 :: acc) y2
    | _ -> [ ele ]
  in
  List.rev @@ helper [] alist
;;

(** Transforms [a &&& b &&& c ... d] to a list [[a;b;c;d]] *)
let parse_many_conjunctions alist =
  let rec helper acc = function
    | [%expr [%e? a] &&& [%e? b]] -> helper (b :: acc) a
    | x -> x :: acc
  in
  helper [] alist
;;

let nolabelize_args = List.map ~f:(fun x -> Nolabel, x)

let mapper =
  object (self)
    inherit Ast_traverse.map as super

    method! expression e =
      let loc = e.pexp_loc in
      match e.pexp_desc with
      | Pexp_apply (_, []) ->
        e
        (* | Pexp_apply (e1, [ (Nolabel, argl); (Nolabel, argr) ]) when is_infix_conj e1 ->
        self#expression
          [%expr
            fun st -> bind_star ([%e self#expression argl] st) [%e self#expression argr]] *)
        (* conde *)
      | Pexp_apply (e1, (Nolabel, args) :: other_args) when is_conde e1 ->
        let goals = parse_to_list args in
        (* Format.eprintf "parsing conde gave %d goals\n%!" (List.length goals); *)
        let goals = List.map ~f:self#expression goals in
        let goals = List.map ~f:parse_many_conjunctions goals in

        let wrap = match other_args with
        | [] -> Fun.id
        | xs -> let open Ast_builder.Default in
                fun f -> pexp_apply ~loc f other_args
        in
        self#expression(wrap
          [%expr
            fun st ->
              pause (fun () ->
                let st = State.new_scope st in
                [%e
                  let open Ast_builder.Default in
                  pexp_apply ~loc [%expr mplus_star]
                  @@ nolabelize_args
                  @@ List.map goals ~f:(function
                       | [] -> assert false
                       | h :: tl ->
                         pexp_apply ~loc [%expr bind_star]
                         @@ ((Nolabel, [%expr [%e h] st]) :: nolabelize_args tl))])])
      | Pexp_apply (e1, args) when is_bind_star e1 ->
        self#expression
          (match args with
           | [] -> failwith "should not happen"
           | [ (_, e) ] -> e
           | [ (_, e); (_, g0) ] -> [%expr bind [%e e] [%e g0]]
           | (_, e) :: (_, g0) :: tl ->
             let open Ast_builder.Default in
             pexp_apply ~loc [%expr bind_star (bind [%e e] [%e g0])] tl)
      | Pexp_apply (e1, args) when is_mplus_star e1 ->
        self#expression
          (match args with
           | [] -> failwith "should not happen"
           | [ (_, e) ] -> e
           | (_, e0) :: tl ->
             let open Ast_builder.Default in
             [%expr
               mplus
                 [%e e0]
                 (pause (fun () -> [%e pexp_apply ~loc [%expr mplus_star] tl]))])
      (* pexp_apply ~loc [%expr bind_star (bind [%e e] [%e g0])] tl) *)
      (* fresh  *)
      | Pexp_apply (e1, [ _ ]) when is_fresh e1 ->
        (* bad syntax -- no body*)
        e
      | Pexp_apply (e1, (Nolabel, args) :: body) when is_fresh e1 ->
        assert (List.length body > 0);
        (* let body = List.map ~f:snd body in *)
        let new_body : expression =
          let open Ast_builder.Default in
          match body with
          | [] -> assert false
          | (_, h) :: tl ->
            pexp_apply ~loc [%expr bind_star] ((Nolabel, [%expr [%e h] st]) :: tl)
        in
        self#expression
          (match reconstruct_args args with
           | Some (xs : string list) ->
             let rec loop acc = function
               | [] -> acc
               | v :: tl ->
                 let px = Pat.var ~loc (Ast_builder.Default.Located.mk v ~loc) in
                 [%expr
                   let [%p px] = OCanren.State.fresh st in
                   [%e loop acc tl]]
               (* | x :: y :: z :: rest ->
                 let px = Pat.var ~loc (Ast_builder.Default.Located.mk x ~loc) in
                 let py = Pat.var ~loc (Ast_builder.Default.Located.mk y ~loc) in
                 let pz = Pat.var ~loc (Ast_builder.Default.Located.mk z ~loc) in
                 [%expr Fresh.three (fun [%p px] [%p py] [%p pz] -> [%e loop acc rest])]
               | x :: rest ->
                 let px = Pat.var ~loc (Ast_builder.Default.Located.mk x ~loc) in
                 [%expr Fresh.one (fun [%p px] -> [%e loop acc rest])] *)
             in
             [%expr
               fun st -> pause (fun () -> (* TODO: new scope  *)
                                          [%e loop new_body xs])]
           | None ->
             Format.eprintf "Can't reconstruct args of 'fresh'";
             { e with pexp_desc = Pexp_apply (e1, [ Nolabel, new_body ]) })
      | Pexp_apply (d, [ (_, body) ]) when is_defer d ->
        let ans = [%expr delay (fun () -> [%e self#expression body])] in
        ans
      | Pexp_apply (d, body) when is_unif d ->
        (* let loc_str =
          Caml.Format.asprintf "%a" Selected_ast.Ast.Location.print_compact e.pexp_loc;
        in
        let body = (Labelled "loc", Exp.constant (Pconst_string (loc_str,None))) :: body in *)
        Exp.apply ~loc:e.pexp_loc d body
      | Pexp_apply (e, xs) ->
        let ans =
          Pexp_apply
            (self#expression e, List.map ~f:(fun (lbl, e) -> lbl, self#expression e) xs)
        in
        let ans = { e with pexp_desc = ans } in
        ans
      | _ -> super#expression e
    (*    | _ ->
      Caml.Format.printf "%a\n%a\n%!" Location.print loc Pprintast.expression e;
      assert false*)
  end
;;

let () = Ppxlib.Driver.register_transformation ~impl:mapper#structure "pa_ocanren_hacky"
