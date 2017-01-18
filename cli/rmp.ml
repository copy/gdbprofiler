open Printf
module String = ExtLib.String

let printfn fmt = ksprintf print_endline fmt
let eprintfn fmt = ksprintf prerr_endline fmt

let log_verbose = try ignore @@ Sys.getenv "RMP_LOG_VERBOSE"; true with Not_found -> false
let section = Lwt_log.Section.make "gdb"
let () = Lwt_log.Section.set_level section (if log_verbose then Lwt_log.Debug else Lwt_log.Warning)
let logger = Lwt_log.channel ~close_mode:`Keep ~channel:Lwt_io.stdout ()
let log fmt = Lwt_log.ign_debug_f ~logger ~section (fmt ^^ "\n")


let show_frame_cached =
  let eq (f1 : Gdb.Proto.frame) (f2 : Gdb.Proto.frame) = f1.func = f2.func && f1.from = f2.from in
  let hash (f : Gdb.Proto.frame) = Hashtbl.hash (f.func, f.from) in
  let cache = CCCache.unbounded ~eq ~hash 1024 in
  CCCache.with_cache cache Gdb.show_frame_function

(** @return most frequent first *)
let analyze h =
  let total = Hashtbl.fold (fun _ count acc -> count + acc) h 0 in
  Hashtbl.fold (fun k v acc -> (k, v) :: acc) h []
  |> List.sort (fun (_,a) (_,b) -> compare b a)
  |> List.map (fun (frames,n) -> sprintf "%5d (%5.1f%%) %s" n (float n /. float total *. 100.) (String.concat " " @@ List.map show_frame_cached frames))

let print_frames frames =
  (String.concat " " @@ List.map Gdb.show_frame_function frames)

let print_frame frame =
  let print_opt = function
    | None -> "None"
    | Some x -> Printf.sprintf "Some(%s)" x
  in
  Printf.sprintf "{ level=%d addr=%s func=%s from=%s file=%s fullname=%s line=%d }"
    frame.Gdbmi_proto.level frame.addr frame.func (print_opt frame.from) (print_opt frame.file) (print_opt frame.fullname)
    (match frame.line with None -> 0 | Some l -> l)


let rec sample gdb =
  log "Sending sigint";
  (Gdb.inferior gdb)#kill Sys.sigint;
  match%lwt Lwt_unix.with_timeout 0.1 begin fun () ->
      Gdb.execute gdb "" (* read notifications TODO check stopped *)
    end with
  | exception Lwt_unix.Timeout ->
    log "timeout, retrying";
    sample gdb
  | lines ->
    log "got lines";
    List.iter (fun r -> log "%s" @@ Gdb.Types.show_output_record r) lines;
    Gdb.Cmd.stack_list_frames gdb


let save_profile records end_time cpuprofile_file callgrind_file =
  let start_create_cpuprofile = Unix.gettimeofday () in
  let profile, node = Cpuprofile.of_frames records end_time in
  let took = Unix.gettimeofday () -. start_create_cpuprofile in
  log "creating profile took %f" @@ took;
  let%lwt () = match cpuprofile_file with
    | None -> Lwt.return_unit
    | Some cpuprofile_file ->
      Lwt_io.with_file ~mode:Lwt_io.Output cpuprofile_file begin fun channel ->
        Printf.printf "%s written\n" @@ cpuprofile_file;
        Lwt_io.write channel @@ Yojson.Safe.to_string (Cpuprofile.to_yojson profile)
      end
  in
  let%lwt () = match callgrind_file with
    | None -> Lwt.return_unit
    | Some callgrind_file ->
      Lwt_io.with_file ~mode:Lwt_io.Output callgrind_file begin fun channel ->
        Printf.printf "%s written\n" @@ callgrind_file;
        Lwt_io.write channel @@ Callgrind.of_node node
      end
  in
  Lwt.return_unit


let check_result = function
  | Gdbmi_types.Done vars -> Lwt.return_unit
  | Connected -> Lwt.return_unit
  | OpError (err, _) -> Lwt.fail_with err
  | Exit -> Lwt.return_unit


let pmp debugger_type debugger_path pid cpuprofile_file callgrind_file =
  let%lwt () = Lwt_io.printl "Press enter to stop" in
  log "starting";
  let debugger = match debugger_type, debugger_path with
    | `Gdb, None ->
      [| "gdb"; "--interpreter=mi"; "-n" |]
    | `Gdb, Some path ->
      [| path; "--interpreter=mi"; "-n" |]
    | `Lldb, None ->
      [| "lldb-mi" |]
    | `Lldb, Some path ->
      [| path |]
  in
  let%lwt gdb = Gdb.launch ~debugger () in
  log "launched";
  begin
    let%lwt result = Gdb.mi gdb "target-attach" [string_of_int pid] in
    let%lwt () = check_result result in
    log "attached";
    let h = Hashtbl.create 10 in
    let records = ref [] in
    let should_exit = ref false in
    let rec loop_sampling next_tick =
      match !should_exit with
      | true -> Lwt.return ()
      | false ->
      log "continuing ...";
      let%lwt result = Gdb.mi gdb "exec-continue" [] in
      let%lwt () = check_result result in
      log "continued";
(*       let%lwt () = Lwt_unix.sleep (CCFloat.max 0.001 (next_tick -. Unix.gettimeofday ())) in *)
      let%lwt () = Lwt_unix.sleep 0.001 in
(*       let%lwt () = Lwt_unix.yield () in *)
      log "sampling gdb";
      let time = Unix.gettimeofday () in
      let%lwt frames = sample gdb in
      let frames = Gdb.collapse_recursive_frames frames in
      records := (frames, time) :: !records;
      log "sampled gdb";
      Hashtbl.replace h frames @@ ExtLib.Hashtbl.find_default h frames 0 + 1;
      log "%d entries" (Hashtbl.length h);
      if log_verbose then log "frames: %s" @@ print_frames frames;
      if log_verbose then List.iter (fun frame -> log "frame: %s" @@ print_frame frame) frames;
      log "next iteration";
      loop_sampling (next_tick +. 0.010)
    in
    let wait_for_stop () =
      let%lwt _ = Lwt_io.read_char Lwt_io.stdin in
      let%lwt () = Lwt_io.printl "Exiting ..." in
      Lwt.return_unit
    in
    let%lwt () = Lwt.pick [loop_sampling (Unix.gettimeofday () +. 0.010); wait_for_stop ()] in
    let end_time = Unix.gettimeofday () in
    save_profile (List.rev !records) end_time cpuprofile_file callgrind_file
  end [%finally Gdb.quit gdb]


let dump_file file =
  let parse_line s =
    try
      match String.strip s with
      | "" -> ()
      | s ->
        match Gdb.parse_io s with
        | Prompt -> printfn "---"
        | Input _ -> printfn "IN: %s" s
        | Output r -> printfn "OUT: %s" @@ Gdb.Types.show_output_record r
    with
      exn -> eprintfn "%s" (Printexc.to_string exn)
  in
  Lwt_io.lines_of_file file |> Lwt_stream.iter parse_line


let () =
  let () = Printexc.record_backtrace true in
  let pid = ref (-1) in
  let callgrind_file = ref "" in
  let cpuprofile_file = ref "" in
  let debugger = ref "" in
  let use_lldb = ref false in
  let spec = [
    "-p", Arg.Set_int pid,
      ": process id (pid)";
    "--cpuprofile", Arg.Set_string cpuprofile_file,
      ": Write out cpuprofile file to the given path (can be opened with Chromium)";
    "--callgrind", Arg.Set_string callgrind_file,
      ": Write out callgrind file to the given path (can be opened with kcachegrind)";
    "--use-lldb", Arg.Set use_lldb,
      ": pass this to use lldb instead of gdb";
    "--debugger", Arg.Set_string debugger,
      ": the debugger to invoke. " ^
      "Defaults to 'lldb-mi' if --use-lldb is passed and 'gdb' otherwise";
  ]
  in
  let usage = "Usage: gdbprofiler -p <pid> [--use-lldb] [--debugger path] " ^
              "[--cpuprofile path] [--callgrind path]" in
  Arg.parse spec (fun _ -> ()) usage;
  let debugger_type = if !use_lldb then `Lldb else `Gdb in
  let debugger = if !debugger = "" then None else Some !debugger in
  let callgrind_file = if !callgrind_file = "" then None else Some !callgrind_file in
  let cpuprofile_file = if !cpuprofile_file = "" then None else Some !cpuprofile_file in
  if callgrind_file = None && cpuprofile_file = None then begin
    prerr_endline "Warning: No output file specified"
  end;
  if !pid = -1 then begin
    Arg.usage spec usage
  end
  else
    Lwt_main.run @@ pmp debugger_type debugger !pid cpuprofile_file callgrind_file
