open OCanren
let () =
  let g = fun a -> Fresh.two (fun b c -> (a =/= b) &&& (a =/= c)) in
  let ans = run one g (fun _ -> ()) |> Stream.take ~n:1 in
  Printf.printf "ok: %d answers\n" (List.length ans)
