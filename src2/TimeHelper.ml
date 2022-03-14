open Printf

let the_time_file = "/tmp/ocanren_time"

let wrap_run num rel ?(n= -1) ~reifier ~verbose onVerbose =
  OCanren.run num rel reifier
  |> OCanren.Stream.take ~n
  |> List.iter (fun s ->
        if verbose then onVerbose s else ()
    )

let __foo num reifier : _ OCanren.Stream.t  = OCanren.run num (fun _ -> OCanren.success) (fun rr -> rr#reify reifier)

(* let __ eta eta2 = __foo OCanren.one eta eta2 *)

(* let _ = wrap_run OCanren.one *)

let time f =
  let t = Mtime_clock.counter () in
  let _res = f () in
  (Mtime_clock.count t |> Mtime.Span.to_s)
;;

let wrap (do_measure : verbose:bool -> unit) =
  try ignore (Sys.getenv "DONT_RUN_CHEZ");
      (* warmup *)
      let () = do_measure ~verbose:false in

      (* do benchmarking *)
      let n = 1 in
      let acc = ref 0. in
      for _i=1 to n do
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
