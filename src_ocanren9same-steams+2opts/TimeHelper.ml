let wrap_run num rel ?(n= -1) ~reifier ~inj ~verbose onVerbose =
  MiniKanren.run num rel
    (fun s ->
      MiniKanren.Stream.take ~n s |>
      List.iter (fun r ->
        let term = r#refine reifier ~inj in
        if verbose then onVerbose term else ()
        )
      )

let time f =
  let t = Unix.gettimeofday () in
  let _res = f () in
  (Unix.gettimeofday () -. t);
;;

let wrap (do_measure : verbose:bool -> unit) =
  try ignore (Sys.getenv "DONT_RUN_CHEZ");
      (* do benchmarking *)
      let acc = ref 0. in
      let n = 5 in
      for i=1 to n do
        let () = Gc.full_major () in
        let () = Gc.compact () in
        acc := !acc +. (time @@ fun () -> do_measure ~verbose:false);
      done;
      Printf.printf "%f\n" (!acc /. (float_of_int n))

  with Not_found ->
    (* do normal run *)
    let () = do_measure ~verbose:true in
    let () = MiniKanren.report_counters () in
    ()
