[%%define STATS]
[%%undef STATS]

let the_time_file = "/tmp/ocanren_time"

let wrap_run num rel ?(n = -1) ~reifier ~verbose onVerbose =
  OCanren.run num rel reifier
  |> OCanren.Stream.take ~n
  |> List.iter (fun s -> if verbose then onVerbose s else ())
;;

let time f =
  let t = Mtime_clock.counter () in
  let _res = f () in
  Float.div (Mtime.Span.to_float_ns (Mtime_clock.count t)) 1e9
;;

[%%ifdef STATS]

let wrap_single_measure f =
  Format.printf
    "Unification counter before = %d\n%!"
    (OCanren.Peep.unification_counter ());
  let rez = f () in
  Format.printf
    "Unification counter after  = %d\n%!"
    (OCanren.Peep.unification_counter ());
  rez
;;

[%%else]

let wrap_single_measure f = f ()

[%%endif]

let output_timings ~dright ~dleft ~avg =
  let triple = Printf.sprintf "%f: -%2.1f%% : %2.1f%%" avg dleft dright in
  let cmd = Printf.sprintf "echo '%s' > %s" triple the_time_file in
  assert (0 = Sys.command cmd);
  ()
;;

let n =
  match Sys.getenv "REPEAT" with
  | exception Not_found -> 10
  | s ->
    (match int_of_string_opt s with
     | Some n ->
       assert (n >= 1);
       n
     | None -> 10)
;;

let wrap (do_measure : verbose:bool -> unit) =
  try
    (* ignore (Sys.getenv "BENCH_MODE"); *)
    (* warmup *)
    let () = do_measure ~verbose:false in
    (* do benchmarking *)
    let timings = Array.init n (fun _ -> 0.0) in
    let acc = ref 0. in
    for i = 0 to n - 1 do
      let () = Gc.compact () in
      let () = Gc.full_major () in
      let delta = time (fun () -> do_measure ~verbose:false) in
      timings.(i) <- delta;
      acc := !acc +. delta
    done;
    let min = Array.fold_left Float.min Float.max_float timings in
    let max = Array.fold_left Float.max Float.min_float timings in
    let avg = !acc /. float_of_int n in
    let dleft = (avg -. min) /. avg *. 100. in
    let dright = (max -. avg) /. avg *. 100. in
    output_timings ~avg ~dleft ~dright
  with
  | Not_found ->
    (* do normal run *)
    wrap_single_measure (fun () -> do_measure ~verbose:true)
;;
