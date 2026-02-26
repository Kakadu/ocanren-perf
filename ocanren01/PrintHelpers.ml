module StringLo = struct
  type ground = GT.string [@@deriving gt ~options:{ show; fmt; gmap }]
  type logic = string OCanren.logic

  let logic =
    { GT.gcata = ()
    ; fix = (fun _ _ -> assert false)
    ; plugins =
        object
          method fmt = GT.fmt OCanren.logic (fun ppf -> Format.fprintf ppf "%s")
          method gmap x = [%gmap: GT.string OCanren.logic] () x
        end
    }
  ;;

  type injected = GT.string OCanren.ilogic

  let prj_exn = OCanren.prj_exn
  let reify = OCanren.reify
end

module ListLo = struct
  open OCanren

  type 'a ground = 'a Std.List.ground [@@deriving gt ~options:{ gmap; fmt }]
  type 'a logic = 'a Std.List.logic

  let logic =
    { Std.List.logic with
      plugins =
        object
          method fmt fa ppf xs =
            let default ppf xs = (GT.fmt Std.List.logic) fa ppf xs in
            match xs with
            | Var _ -> default ppf xs
            | Value _ ->
              let rec iter ppf xs =
                match xs with
                | Value Std.List.Nil -> ()
                | Value (Std.List.Cons (h, tl)) -> Format.fprintf ppf "%a %a" fa h iter tl
                | Var _ -> Format.fprintf ppf " . %a" default xs
              in
              Format.fprintf ppf "(%a)" iter xs

          method gmap fa xs = [%gmap: 'a Std.List.logic] (GT.lift fa) () xs
        end
    }
  ;;

  type 'a injected = 'a Std.List.groundi

  let prj_exn = Std.List.prj_exn
  let reify = Std.List.reify
  let rec project f xs = Std.List.logic_to_ground_exn f xs
end
