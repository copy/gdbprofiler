open Printf

module Gdb = Gdb_lib.Gdb
module Gdbmi_proto = Gdb_lib.Gdbmi_proto
module Gdbmi_types = Gdb_lib.Gdbmi_types
module Cpuprofile = Gdb_lib.Cpuprofile
module Callgrind = Gdb_lib.Callgrind

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

let print_frames frames =
  (String.concat " " @@ List.map Gdb.show_frame_function frames)

let print_frame frame =
  let print_opt = function
    | None -> "None"
    | Some x -> Printf.sprintf "Some(%s)" x
  in
  Printf.sprintf "{ level=%d addr=%s func=%s from=%s file=%s fullname=%s line=%d }"
    frame.Gdbmi_proto.level frame.addr frame.func
    (print_opt frame.from) (print_opt frame.file) (print_opt frame.fullname)
    (match frame.line with None -> 0 | Some l -> l)


let rec sample gdb =
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
  | Gdbmi_types.Done _vars -> Lwt.return_unit
  | Connected -> Lwt.return_unit
  | OpError (err, _) ->
    begin match err with
      | "ptrace: Operation not permitted." ->
        Lwt.fail Gdb.Not_permitted
      | _ ->
        Lwt.fail_with (Printf.sprintf "OpError(%s)" err)
    end
  | Exit -> Lwt.return_unit

let run_pmp gdb pid cpuprofile_file callgrind_file =
  begin
    let%lwt result = Gdb.mi gdb "target-attach" [string_of_int pid] in
    let%lwt () = check_result result in
    log "attached";
    let records = ref [] in
    let should_exit = ref false in
    let time = ref @@ Unix.gettimeofday () in
    let%lwt () = Lwt_io.printl "Press enter to stop" in
    let rec loop_sampling () =
      match !should_exit with
      | true -> Lwt.return ()
      | false ->
      log "continuing ...";
      let run_start = Unix.gettimeofday () in
      let%lwt result = Gdb.mi gdb "exec-continue" [] in
      let%lwt () = check_result result in
      log "continued";
      let%lwt () = Lwt_unix.sleep 0.001 in
      log "Sending sigint (waking up)";
      (Gdb.inferior gdb)#kill Sys.sigint;
      let run_end = Unix.gettimeofday () in
      log "sampling gdb";
      let%lwt frames = sample gdb in
      let frames = Gdb.collapse_recursive_frames frames in
      time := !time +. run_end -. run_start;
      records := (frames, !time) :: !records;
      log "sampled gdb";
      if log_verbose then log "frames: %s" @@ print_frames frames;
      if log_verbose then List.iter (fun frame -> log "frame: %s" @@ print_frame frame) frames;
      log "next iteration";
      loop_sampling ()
    in
    let wait_for_stop () =
      let%lwt _ = Lwt_io.read_char Lwt_io.stdin in
      let%lwt () = Lwt_io.printl "Exiting ..." in
      should_exit := true;
      Lwt.return_unit
    in
    let%lwt () = wait_for_stop () and () = loop_sampling () in
    let end_time = Unix.gettimeofday () in
    save_profile (List.rev !records) end_time cpuprofile_file callgrind_file;
  end [%finally (Gdb.quit gdb)]


let pmp debugger_type debugger_path pid cpuprofile_file callgrind_file =
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
  match%lwt Gdb.launch ~debugger () with
  | Ok gdb ->
    log "launched";
    run_pmp gdb pid cpuprofile_file callgrind_file
  | Error state ->
    let state = match state with
      | WEXITED n -> sprintf "Exited with %d" n
      | WSIGNALED n -> sprintf "Signaled %d" n
      | WSTOPPED n -> sprintf "Stopped with %d" n
    in
    Lwt_io.printf "Failed to start debugger '%s': %s\n" debugger.(0) state


let dump_file file =
  let parse_line s =
    try
      match CCString.trim s with
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
  let usage = "Usage: gdbprofiler -p <pid> [--use-lldb] [--debugger path] " ^
              "[--cpuprofile path] [--callgrind path]" in
  let rec spec = [
    "-p", Arg.Set_int pid,
    ": process id (pid)";

    "--cpuprofile", Arg.Set_string cpuprofile_file,
    ": Write out cpuprofile file to the given path (can be opened with Chromium)." ^
    " Defaults to out.cpuprofile";

    "--callgrind", Arg.Set_string callgrind_file,
    ": Write out callgrind file to the given path (can be opened with kcachegrind)." ^
    " Defaults to /dev/null";

    "--use-lldb", Arg.Set use_lldb,
    ": pass this to use lldb instead of gdb";

    "--debugger", Arg.Set_string debugger,
    ": the debugger to invoke. " ^
    "Defaults to 'lldb-mi' if --use-lldb is passed and 'gdb' otherwise";

    "--help", Arg.Unit help_and_exit,
    ": Display this list of options";

    "-help", Arg.Unit help_and_exit, "";
  ]
  and help_and_exit () = Arg.usage spec usage; exit 2
  in
  Arg.parse spec (fun _ -> ()) usage;
  let debugger_type = if !use_lldb then `Lldb else `Gdb in
  let debugger = if !debugger = "" then None else Some !debugger in
  let callgrind_file = if !callgrind_file = "" then None else Some !callgrind_file in
  let fix_extension name =
    if CCString.suffix ~suf:".cpuprofile" name
    then name
    else name ^ ".cpuprofile"
  in
  let cpuprofile_file =
    Some (if !cpuprofile_file = "" then "out.cpuprofile" else fix_extension !cpuprofile_file)
  in
  if !pid = -1 then help_and_exit ();
  try
    Lwt_main.run @@ pmp debugger_type debugger !pid cpuprofile_file callgrind_file
  with
  | Gdb.Not_permitted ->
    print_endline (
      "Fatal:\n" ^
      "Got 'ptrace: Operation not permitted'.\n" ^
      "If you're on Linux, run:\n" ^
      "\n" ^
      "  su -c 'sysctl kernel.yama.ptrace_scope=0'\n" ^
      "\n" ^
      "See https://rajeeshknambiar.wordpress.com/2015/07/16/attaching-debugger-and-ptrace_scope/ for more infos"
    )
