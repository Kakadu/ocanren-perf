module Std = struct
  module Wrapper = struct
    [%%distrib
      type nonrec 'self w = W of 'self
      [@@ocaml.boxed] [@@deriving gt ~plugins:{ gmap; fmt }]]

    let t =
      { t with
        GT.plugins =
          object
            method fmt fa ppf (W x) = fa ppf x
            method gmap = GT.gmap t
          end
      }
    ;;

    let ground = t

    let logic =
      { logic with
        GT.plugins =
          object (self)
            method fmt : (_ -> 'a -> unit) -> Format.formatter -> 'a logic -> unit =
              fun fa ppf -> function
                (* | OCanren.Value (W (OCanren.Var _ as v)) ->
                   GT.fmt OCanren.logic (self#fmt fa) ppf v *)
                | OCanren.Value (W x) -> fa ppf x
                | Var (idx, []) -> Format.fprintf ppf "_.%d" idx
                | Var _ -> assert false

            method gmap = GT.gmap logic
          end
      }
    ;;
  end

  module List : sig
    type nonrec ('a, 'self) t =
      | Nil
      | Cons of 'a * 'self
    [@@deriving gt ~options:{ gmap; fmt }]

    type 'a ground = ('a, 'a ground) t Wrapper.t [@@deriving gt ~options:{ gmap; fmt }]

    type 'a logic = ('a, 'a logic) t OCanren.logic Wrapper.logic
    [@@deriving gt ~options:{ gmap; fmt }]

    type 'a injected = ('a, 'a injected) t OCanren.ilogic Wrapper.injected

    val prj_exn : ('a, 'b) OCanren.Reifier.t -> ('a injected, 'b ground) OCanren.Reifier.t
    val reify : ('a, 'b) OCanren.Reifier.t -> ('a injected, 'b logic) OCanren.Reifier.t
    val nil : unit -> 'a injected
    val cons : 'a -> 'a injected -> 'a injected
    val list_of_ground : ('a -> 'b) -> 'a ground -> 'b GT.list

    open OCanren

    val nullo : _ injected -> goal
    val tlo : 'a ilogic injected -> 'a ilogic injected -> goal
  end = struct
    [%%ocanren_inject
      type nonrec ('a, 'self) ground =
        | Nil
        | Cons of 'a * 'self
      [@@deriving gt ~plugins:{ fmt; gmap }]]

    type 'a ground = ('a, 'a ground) t Wrapper.t [@@deriving gt ~plugins:{ gmap; fmt }]

    type 'a logic = ('a, 'a logic) t OCanren.logic Wrapper.logic
    [@@deriving gt ~plugins:{ gmap; fmt }]

    type 'a injected = ('a, 'a injected) t OCanren.ilogic Wrapper.injected

    let nil () : 'a injected = Wrapper.w (nil ())
    let cons h tl : 'a injected = Wrapper.w (cons h tl)

    let prj_exn : ('a, 'b) OCanren.Reifier.t -> ('a injected, 'b ground) OCanren.Reifier.t
      =
      fun ra ->
      let open OCanren.Env.Monad in
      OCanren.Reifier.fix (fun rself ->
        Wrapper.prj_exn (OCanren.prj_exn <..> chain (fmapt ra rself)))
    ;;

    let reify
      : 'a 'b. ('a, 'b) OCanren.Reifier.t -> ('a injected, 'b logic) OCanren.Reifier.t
      =
      fun ra ->
      let open OCanren.Env.Monad in
      OCanren.Reifier.fix (fun rself ->
        Wrapper.reify
          (OCanren.reify
           <..> chain (OCanren.Reifier.zed (OCanren.Reifier.rework ~fv:(fmapt ra rself)))
          ))
    ;;

    let nullo xs = OCanren.(xs === nil ())

    let tlo : 'a injected -> _ -> OCanren.goal =
      fun xs tl -> OCanren.call_fresh (fun h -> OCanren.unify xs (cons h tl))
    ;;

    let rec list_of_ground f : _ ground -> _ GT.list = function
      | Wrapper.W Nil -> []
      | Wrapper.W (Cons (h, tl)) -> f h :: list_of_ground f tl
    ;;

    let ground =
      { ground with
        plugins =
          object
            method gmap = ground.plugins#gmap
            method fmt fa ppf xs = GT.fmt GT.list fa ppf (list_of_ground Fun.id xs)
          end
      }
    ;;
  end

  let nil () = List.nil ()
  let ( % ) = List.cons
  let ( !< ) h = h % nil ()
  let ( %< ) h tl = h % (tl % nil ())

  let rec list f = function
    | [] -> nil ()
    | x :: xs -> List.cons (f x) (list f xs)
  ;;

  module Nat = struct
    type nonrec 'self t =
      | Z
      | S of 'self
    [@@deriving gt ~plugins:{ fmt; gmap }]

    (** Wrapped ground natural numbers *)
    type ground = ground t Wrapper.t [@@deriving gt ~plugins:{ fmt; gmap }]

    type logic = logic t OCanren.logic Wrapper.logic
    [@@deriving gt ~plugins:{ fmt; gmap }]

    type injected = injected t OCanren.ilogic Wrapper.injected

    let o () : injected = Wrapper.w (OCanren.inj Z)
    let zero : injected = Wrapper.w (OCanren.inj Z)
    let succ prev : injected = Wrapper.w (OCanren.inj (S prev))

    let fmapt fa s =
      let open OCanren.Env.Monad in
      OCanren.Env.Monad.return (GT.gmap t) <*> fa <*> s
    ;;

    let prj_exn : (injected, ground) OCanren.Reifier.t =
      let open OCanren.Env.Monad in
      OCanren.Reifier.fix (fun rself ->
        Wrapper.prj_exn (OCanren.prj_exn <..> chain (fmapt rself)))
    ;;

    let reify : (injected, logic) OCanren.Reifier.t =
      let open OCanren.Env.Monad in
      OCanren.Reifier.fix (fun rself ->
        Wrapper.reify
          (OCanren.reify
           <..> chain (OCanren.Reifier.zed (OCanren.Reifier.rework ~fv:(fmapt rself)))))
    ;;
  end

  module Pair = struct
    type nonrec ('a, 'b) t = 'a * 'b [@@deriving gt ~plugins:{ fmt; gmap }]
    type nonrec ('a, 'b) ground = ('a * 'b) Wrapper.t [@@deriving gt ~plugins:{ fmt }]

    type nonrec ('a, 'b) logic = ('a * 'b) OCanren.logic Wrapper.logic
    [@@deriving gt ~plugins:{ fmt }]

    type ('a, 'b) injected = ('a * 'b) OCanren.ilogic Wrapper.injected

    let fmapt fa fb s =
      let open OCanren.Env.Monad in
      OCanren.Env.Monad.return (GT.gmap t) <*> fa <*> fb <*> s
    ;;

    let prj_exn
      :  ('a, 'b) OCanren.Reifier.t -> ('c, 'd) OCanren.Reifier.t
      -> (('a, 'c) injected, ('b, 'd) ground) OCanren.Reifier.t
      =
      fun ra rb ->
      let open OCanren.Env.Monad in
      OCanren.Reifier.fix (fun rself ->
        Wrapper.prj_exn (OCanren.prj_exn <..> chain (fmapt ra rb)))
    ;;

    let reify
      : 'a 'b 'c 'd.
      ('a, 'b) OCanren.Reifier.t
      -> ('c, 'd) OCanren.Reifier.t
      -> (('a, 'c) injected, ('b, 'd) logic) OCanren.Reifier.t
      =
      fun ra rb ->
      let open OCanren.Env.Monad in
      OCanren.Reifier.fix (fun _ ->
        Wrapper.reify
          (OCanren.reify
           <..> chain (OCanren.Reifier.zed (OCanren.Reifier.rework ~fv:(fmapt ra rb)))))
    ;;

    let make x y = Wrapper.w (OCanren.inj (x, y))
    let pair = make
  end

  let pair = Pair.make

  module Triple = struct
    type nonrec ('a, 'b, 'c) t = 'a * 'b * 'c [@@deriving gt ~plugins:{ fmt; gmap }]
    type ('a, 'b, 'c) ground = ('a * 'b * 'c) Wrapper.t
    type ('a, 'b, 'c) injected = ('a * 'b * 'c) OCanren.ilogic Wrapper.injected

    let fmapt fa fb fc s =
      let open OCanren.Env.Monad in
      OCanren.Env.Monad.return (GT.gmap t) <*> fa <*> fb <*> fc <*> s
    ;;

    let prj_exn
      : 'a 'b 'c 'd 'e 'f.
      ('a, 'b) OCanren.Reifier.t
      -> ('c, 'd) OCanren.Reifier.t
      -> ('e, 'f) OCanren.Reifier.t
      -> (('a, 'c, 'e) injected, ('b, 'd, 'f) ground) OCanren.Reifier.t
      =
      fun ra rb rc ->
      let open OCanren.Env.Monad in
      OCanren.Reifier.fix (fun _ ->
        Wrapper.prj_exn (OCanren.prj_exn <..> chain (fmapt ra rb rc)))
    ;;

    type nonrec ('a, 'b, 'c) logic = ('a * 'b * 'c) OCanren.logic Wrapper.logic
    [@@deriving gt ~plugins:{ fmt }]

    let reify
      : 'a 'b 'c 'd 'e 'f.
      ('a, 'b) OCanren.Reifier.t
      -> ('c, 'd) OCanren.Reifier.t
      -> ('e, 'f) OCanren.Reifier.t
      -> (('a, 'c, 'e) injected, ('b, 'd, 'f) logic) OCanren.Reifier.t
      =
      fun ra rb rc ->
      let open OCanren.Env.Monad in
      OCanren.Reifier.fix (fun _ ->
        Wrapper.reify
          (OCanren.reify
           <..> chain (OCanren.Reifier.zed (OCanren.Reifier.rework ~fv:(fmapt ra rb rc)))
          ))
    ;;

    let make x y z = Wrapper.w (OCanren.inj (x, y, z))
  end
end

module ListLo = struct
  type 'a ground = 'a Std.List.ground [@@deriving gt ~options:{ gmap; fmt }]
  type 'a logic = 'a Std.List.logic

  let logic =
    { Std.List.logic with
      plugins =
        object (self)
          method fmt
            : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a logic -> unit =
            fun fa ppf xs ->
              match xs with
              | Value (Std.Wrapper.W (Var (idx, []))) | Var (idx, []) ->
                Format.fprintf ppf "_.%d" idx
              | Value (Std.Wrapper.W (Var (idx, _))) | Var (idx, _) -> assert false
              | Value (Std.Wrapper.W _) ->
                let rec iter ppf xs =
                  let _ : 'a logic = xs in
                  match xs with
                  | Value (Std.Wrapper.W (Value Std.List.Nil)) -> ()
                  | Value (Std.Wrapper.W (Value (Std.List.Cons (h, tl)))) ->
                    Format.fprintf ppf "%a %a" fa h iter tl
                  | Value (Std.Wrapper.W (Var (idx, []))) | Var (idx, []) ->
                    Format.fprintf ppf "_.%d" idx
                  | Value (Std.Wrapper.W (Var (idx, _))) | Var (idx, _) -> assert false
                in
                Format.fprintf ppf "(%a)" iter xs

          method gmap fa xs =
            let _ : _ logic = xs in
            [%gmap: 'a Std.List.logic] (GT.lift fa) () xs
        end
    }
  ;;

  type 'a injected = 'a Std.List.injected

  let prj_exn = Std.List.prj_exn
  let reify = Std.List.reify
end
