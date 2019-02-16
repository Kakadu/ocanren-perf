open Printf
open MiniKanren

module Node = struct
  module X = struct
    @type 'a t =
       | Leaf
       | Node of 'a * 'a * 'a * 'a
     with show

    let fmap f = function
    | Leaf -> Leaf
    | Node (a,b,c,d) -> Node (f a, f b, f c, f d)

    let t = { GT.gcata = ();
      plugins = object
        method gmap = fmap (* t.plugins#gmap *)
        method show = GT.show(t)
      end
    }

  end
  include X
  include Fmap(X)

  type rterm = rterm X.t
  type lterm = lterm X.t logic
  type fterm = (rterm, lterm) injected

  let rec show_rterm : rterm -> string = fun t -> GT.(show X.t show_rterm) t
  let rec show_lterm : lterm -> string =
    fun x -> GT.(show logic @@ show X.t show_lterm) x

  let rec to_logic : rterm -> lterm = fun term ->
    MiniKanren.to_logic (GT.(gmap X.t) to_logic term)

  let leaf () : fterm = inj @@ distrib @@ Leaf
  let node a b c d : fterm = inj @@ distrib @@ Node (a,b,c,d)
end


let rec make ~depth =
  let rec helper = function
    | 0 -> Node.leaf ()
    | n ->
      let a = helper (n-1) in
      let b = helper (n-1) in
      let c = helper (n-1) in
      let d = helper (n-1) in
      Node.node a b c d
  in
  helper depth

let do_measure rel ~verbose =
  TimeHelper.wrap_run rel one
    ~reifier:MiniKanrenCore.reify
    ~verbose
    (fun term -> Printf.printf "%s\n" (GT.show logic (GT.show GT.int) term))

let u,v =
  let depth = 13 in
  (make ~depth, make ~depth)

let () = assert (not (u == v))

let rel q = (u === v)

let () = TimeHelper.wrap @@ do_measure rel
