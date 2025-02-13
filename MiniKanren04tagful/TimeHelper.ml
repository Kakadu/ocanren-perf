open Printf

let the_time_file = "/tmp/ocanren_time"

let _: int OCanren.logic OCanren.Stream.t = OCanren.(run one (fun q -> q===q)) (fun rr -> rr#reify OCanren.reify)
let wrap_run rel ?(n= -1) ~reifier ~verbose onVerbose =
  OCanren.(run q) rel (fun rr -> rr#reify reifier)
  |>OCanren.Stream.take ~n  |>
    List.iter (fun term ->
      if verbose then onVerbose term else ()
      )
    

let time f =
  let start = Mtime_clock.elapsed () in
  let _res = f () in
  let fin = Mtime_clock.elapsed() in
  let span = Mtime.Span.abs_diff start fin in
  let ns = Mtime.Span.to_float_ns span in
  let s = ns /. 1e9 in
  s
;;

let wrap (do_measure : verbose:bool -> unit) =
  try ignore (Sys.getenv "BENCH_MODE");
      (* warmup *)
      let () = do_measure ~verbose:false in

      (* do benchmarking *)
      let n = 10 in
      let acc = ref 0. in
      for _=1 to n do
        let () = Gc.compact () in
        let () = Gc.full_major () in
        acc := !acc +. (time @@ fun () -> do_measure ~verbose:false);
      done;
      let ans =  (!acc /. (float_of_int n)) in
      let (_:int) = Sys.command @@ sprintf "echo %f > %s" ans the_time_file in
      Printf.printf "%f\n" ans


      (* let samples = Benchmark.latency1 (Int64.of_int n) (fun () -> do_measure ~verbose:false)  () in
      match samples with
      | [(_name,xs)] ->
          assert (List.length xs = 1);
          let h = List.hd xs in
          let ans = h.Benchmark.utime /. (float_of_int n) in
          let (_:int) = Sys.command @@ sprintf "echo %f > %s" ans the_time_file in
          printf "%f\n" ans
      | _ -> failwith "should not happen" *)

  with Not_found ->
    (* do normal run *)
    let () = do_measure ~verbose:true in
    (* let () = OCanren.report_counters () in *)
    ()
