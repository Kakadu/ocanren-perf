open Printf

let the_time_file = "/tmp/ocanren_time"

let wrap_run num rel ?(n= -1) ~reifier ~verbose onVerbose =
  MiniKanren.run num rel
    (fun timings s ->
      MiniKanren.Stream.take ~n s |>
      List.iter (fun r ->
        let term = r#reify reifier in
        if verbose then onVerbose term else ()
      );
      timings
    )

let time f =
  let t = Mtime_clock.counter () in
  let _res = f () in
  (Mtime_clock.count t |> Mtime.Span.to_s)
;;

let wrap (do_measure : verbose:bool -> MiniKanren.Timings.t) =
  try ignore (Sys.getenv "DONT_RUN_CHEZ");
      (* warmup *)
      let _ : MiniKanren.Timings.t = do_measure ~verbose:false in

      (* do benchmarking *)
      let n = 20 in
      let acc = ref 0. in
      for i=1 to n do
        let () = Gc.compact () in
        let () = Gc.full_major () in
        acc := !acc +. (time @@ fun () -> do_measure ~verbose:false);
      done;
      let ans =  (!acc /. (float_of_int n)) in
      let unif_time = 0.0 in
      let (_:int) = Sys.command @@ sprintf "echo %f > %s" ans the_time_file in
      Printf.printf "%f\n" ans

  with Not_found ->
    (* do normal run *)
    let _: MiniKanren.Timings.t = do_measure ~verbose:true in
    ()
