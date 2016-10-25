open Printf
module String = ExtLib.String

let printfn fmt = ksprintf print_endline fmt
let eprintfn fmt = ksprintf prerr_endline fmt

let log_verbose = false

let section = Lwt_log.Section.make "gdb"
let () = if log_verbose then Lwt_log.Section.set_level section Lwt_log.Debug
let logger = Lwt_main.run @@ Lwt_log.file ~mode:`Append ~file_name:"rmp.log" ()
let log fmt = Lwt_log.ign_debug_f ~logger ~section (fmt ^^ "\n")
(* let log fmt = Printf.eprintf (fmt ^^ "\n") *)

(** @return most frequent first *)
let analyze h =
  let total = Hashtbl.fold (fun _ count acc -> count + acc) h 0 in
  Hashtbl.fold (fun k v acc -> (k, v) :: acc) h []
  |> List.sort (fun (_,a) (_,b) -> compare b a)
  |> List.map (fun (frames,n) -> sprintf "%5d (%5.1f%%) %s" n (float n /. float total *. 100.) (String.concat " " @@ List.map Gdb.show_frame_function frames))

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

let display term h =
  let open LTerm_geom in
  let {rows;cols} = LTerm.size term in
  let line s =
    let s = if String.length s > cols then String.slice ~last:(cols - 2) s ^ " >" else s in
    LTerm.fprintl term s
  in
  let%lwt () = LTerm.goto term { row = 0; col = 0; } in
  Lwt_list.iter_s line @@ CCList.take (rows - 1) @@ analyze h

let is_exit_key key =
  let open LTerm_key in
  let module C = CamomileLibraryDyn.Camomile.UChar in
  match code key with
  | Escape -> true
  | Char ch when C.char_of ch = 'q' -> true
  | Char ch when control key && C.char_of ch = 'c' -> true
  | _ -> false

let init_term () =
  let%lwt term = Lazy.force LTerm.stdout in
  let%lwt () = LTerm.clear_screen term in
  Lwt.return term

let rec wait_for_user_quit should_exit term =
  match%lwt LTerm.read_event term with
  | LTerm_event.Key key when is_exit_key key -> should_exit := true; Lwt.return ()
  | _ -> wait_for_user_quit should_exit term

let save_profile records end_time cpuprofile_file callgrind_file =
  let start_create_cpuprofile = Unix.gettimeofday () in
  let profile, node = Cpuprofile.of_frames records end_time in
  let took = Unix.gettimeofday () -. start_create_cpuprofile in
  log "creating profile took %f" @@ took;
  let%lwt () = if cpuprofile_file = "" then Lwt.return_unit else
      Lwt_io.with_file ~mode:Lwt_io.Output cpuprofile_file begin fun channel ->
      Printf.printf "%s written\n" @@ cpuprofile_file;
      Lwt_io.write channel @@ Yojson.Safe.to_string (Cpuprofile.to_yojson profile)
    end
  in
  let%lwt () = if callgrind_file = "" then Lwt.return_unit else
    Lwt_io.with_file ~mode:Lwt_io.Output callgrind_file begin fun channel ->
      Printf.printf "%s written\n" @@ callgrind_file;
      Lwt_io.write channel @@ Callgrind.of_node node
    end
  in
  Lwt.return_unit

let pmp pid cpuprofile_file callgrind_file =
  log "starting";
  let%lwt gdb = Gdb.launch () in
  log "launched";
  begin
    let%lwt term = init_term () in
    let%lwt mode = LTerm.enter_raw_mode term in
    log "term raw mode";
  begin
    let%lwt () = Gdb.run gdb "attach %d" pid in
    log "attached";
    let h = Hashtbl.create 10 in
    let records = ref [] in
    let should_exit = ref false in
    let rec loop_sampling next_tick =
      match !should_exit with
      | true -> Lwt.return ()
      | false ->
      log "continuing ...";
      let%lwt () = Gdb.run gdb "continue" in (* TODO check running *)
      log "continued";
(*       let%lwt () = Lwt_unix.sleep (CCFloat.max 0.001 (next_tick -. Unix.gettimeofday ())) in *)
      let%lwt () = Lwt_unix.sleep 0.001 in
(*       let%lwt () = Lwt_unix.yield () in *)
      log "sampling gdb";
      let time = Unix.gettimeofday () in
      let%lwt frames = sample gdb in
      let frames = Gdb.collapse_recursive_frames frames in
      records := (frames, time, 0) :: !records;
      log "sampled gdb";
      Hashtbl.replace h frames @@ ExtLib.Hashtbl.find_default h frames 0 + 1;
      log "%d entries" (Hashtbl.length h);
      log "frames: %s" @@ print_frames frames;
      List.iter (fun frame -> log "frame: %s" @@ print_frame frame) frames;
      log "next iteration";
      loop_sampling (next_tick +. 0.010)
    in
    let rec loop_draw () =
      match !should_exit with
      | true -> Lwt.return ()
      | false ->
      let%lwt () = display term h in
      let%lwt () = Lwt_unix.sleep 0.050 in
      loop_draw ()
    in
    let%lwt () = Lwt.join [wait_for_user_quit should_exit term; loop_sampling (Unix.gettimeofday () +. 0.010); loop_draw ()] in
    let end_time = Unix.gettimeofday () in
    save_profile (List.rev !records) end_time cpuprofile_file
  end [%finally LTerm.leave_raw_mode term mode]
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

let read_file file =
  let h = Hashtbl.create 10 in
  let parse_line s =
    try
      match String.strip s with
      | "" -> ()
      | s ->
        match Gdb.parse_io s with
        | Output (Result (_, Done [x])) ->
          begin try
            let frames = Gdb.Proto.stack x in
            Hashtbl.replace h frames @@ ExtLib.Hashtbl.find_default h frames 0 + 1
          with _ -> () (* no stack frames here *)
          end
        | _ -> ()
    with exn -> eprintfn "%s" (Printexc.to_string exn)
  in
  let%lwt term = init_term () in
  let%lwt () = Lwt_io.lines_of_file file |> Lwt_stream.iter parse_line in
(*   List.iter print_endline @@ List.rev @@ analyze h; *)
  let%lwt () = display term h in
  Lwt.return ()

let () =
  let () = Printexc.record_backtrace true in
  let pid = ref (-1) in
  let callgrind_file = ref "" in
  let cpuprofile_file = ref "" in
  let spec = [
    "-p", Arg.Set_int pid,
      ": process id (pid)";
    "--cpuprofile", Arg.Set_string cpuprofile_file,
      ": Write out cpuprofile file to the given path (can be opened with Chromium)";
    "--callgrind", Arg.Set_string callgrind_file,
      ": Write out callgrind file to the given path (can be opened with kcachegrind)";
  ]
  in
  let usage = "Usage: rmp -p <pid> [--cpuprofile path] [--callgrind path]" in
  Arg.parse spec (fun _ -> ()) usage;
  if !pid = -1 then begin
    Arg.usage spec usage
  end
  else
    Lwt_main.run @@ pmp !pid !cpuprofile_file !callgrind_file
