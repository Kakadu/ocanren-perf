module Std = struct
  module Wrapper = struct
    [%%distrib
    type nonrec 'self w = W of 'self [@@deriving gt ~plugins:{gmap; fmt}]
    ]
  end
  module List : sig
    (* TODO: In intefaces we can't use 'plugins', only 'options' *)
    type nonrec ('a, 'self) t = Nil | Cons of 'a * 'self [@@deriving gt ~options:{gmap; fmt}]
    type 'a ground = ('a, 'a ground) t Wrapper.t [@@deriving gt ~options:{gmap; fmt}]
    type 'a logic = ('a, 'a logic) t OCanren.logic Wrapper.t OCanren.logic [@@deriving gt ~options:{gmap; fmt}]
    type 'a injected = ('a, 'a injected) t OCanren.ilogic Wrapper.injected

    (* val prj_exn: ('a, 'b) OCanren.Reifier.t -> ('a injected, 'b ground) OCanren.Reifier.t *)
    (* val reify: ('a, 'b) OCanren.Reifier.t -> ('a injected, 'b logic) OCanren.Reifier.t *)
  end = struct
    [%%distrib
    type nonrec ('a, 'self) ground = Nil | Cons of 'a * 'self [@@deriving gt ~plugins:{gmap; fmt}]
    ]


    type 'a ground = ('a, 'a ground) t Wrapper.t [@@deriving gt ~plugins:{gmap; fmt}]
    type 'a logic = ('a, 'a logic) t OCanren.logic Wrapper.t OCanren.logic [@@deriving gt ~plugins:{gmap; fmt}]
    type 'a injected = ('a, 'a injected) t OCanren.ilogic Wrapper.injected
    let nil () : 'a injected = Wrapper.w (nil())
    let cons h tl : 'a injected = Wrapper.w (cons h tl)

    let (prj_exn : ('a, 'b) OCanren.Reifier.t -> ('a injected, 'b ground) OCanren.Reifier.t)
      =
      fun ra ->
        let open OCanren.Env.Monad in
        OCanren.Reifier.fix
          (fun rself -> Wrapper.reify (OCanren.prj_exn <..> (chain (fmapt ra rself))))
    (* let (reify : ('a, 'b) OCanren.Reifier.t -> ('a injected, 'b logic) OCanren.Reifier.t)
              =
      fun rself ->
        let open OCanren.Env.Monad in
          OCanren.Reifier.fix
            (fun _ ->
                OCanren.reify <..>
                  (chain
                    (OCanren.Reifier.zed
                        (OCanren.Reifier.rework ~fv:(fmapt rself))))) *)
  end

  module Pair = struct

  end

  module Triple = struct
    type nonrec ('a, 'b, 'c) t = 'a * 'b * 'c [@@deriving gt ~options:{ fmt; gmap }]

    type ('a, 'b, 'c) injected = ('a * 'b * 'c) OCanren.ilogic Wrapper.injected

    let reify ra rb rc =
      let open OCanren in
      let ( >>= ) = Env.Monad.bind in
      Reifier.fix (fun self ->
        Reifier.compose
          Reifier.reify
          (ra
           >>= fun fa ->
           rb
           >>= fun fb ->
           rc
           >>= fun fc ->
           let rec foo = function
             | Var (v, xs) -> Var (v, Stdlib.List.map foo xs)
             | Value x -> Value (GT.gmap t fa fb fc x)
           in
           Env.Monad.return foo))
    ;;

    let make x y z = Wrapper.w (OCanren.inj (x, y, z))
  end
end