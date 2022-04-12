open Printf
open GT
open OCanren
open OCanren.Std

module GLam = struct
  module T = struct
    type nonrec ('varname, 'self) t =
      | V of 'varname
      | App of 'self * 'self
      | Abs of 'varname * 'self
    [@@deriving gt ~options:{ fmt; gmap }]

    let fmap f g x = gmap t f g x
  end

  include T
  include Fmap2 (T)

  type rlam = (string, rlam) t [@@deriving gt ~options:{ fmt; gmap }]

  type llam = (string OCanren.logic, llam) t OCanren.logic
  [@@deriving gt ~options:{ fmt; gmap }]

  type flam = (rlam, llam) injected

  let v s = inj @@ distrib @@ V s
  let app x y = inj @@ distrib @@ App (x, y)
  let abs x y = inj @@ distrib @@ Abs (x, y)
  let show_rlam = Format.asprintf "%a" (GT.fmt rlam)
  let show_llam = Format.asprintf "%a" (GT.fmt llam)
  (* show logic (show t (show logic @@ show string) show_llam) term *)
end

open GLam

let varX = !!"x"
let varY = !!"y"
let varF = !!"f"

let rec glam_reifier : Env.t -> GLam.flam -> GLam.llam =
 fun c x -> GLam.reify OCanren.reify glam_reifier c x
;;
