module Make () = struct
  type config =
    { mutable unifications : int
    ; mutable last_introduced_var : int
    }

  let config = { unifications = 0; last_introduced_var = 0 }
  let clear_unifications () = config.unifications <- 0
  let incr_counter () = config.unifications <- config.unifications + 1

  let set_last_introduced_var n =
    config.last_introduced_var <- Int.max config.last_introduced_var n
  ;;

  let pp_config () =
    Printf.printf "unifications: %d\n" config.unifications;
    Printf.printf "last known var: %d\n" config.last_introduced_var
  ;;
end
