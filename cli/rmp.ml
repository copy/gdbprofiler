open Printf
module String = ExtLib.String
module List = ExtLib.List

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
  |> List.sort ~cmp:(fun (_,a) (_,b) -> compare b a)
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

module Cpuprofile = struct
  type fn = {
    functionName: string;
    scriptId: string;
    url: string;
    lineNumber: int;
    columnNumber: int;
    hitCount: int;
    callUID: int;
    children: fn list;
    deoptReason: string;
    id: int;
    positionTicks: int list;
  } [@@deriving yojson]

  let create_fn name ~node_id ~call_id =
    {
      functionName = name;
      id = node_id;
      scriptId = "0";
      url = "";
      lineNumber = 0;
      columnNumber = 0;
      hitCount = 0;
      callUID = call_id;
      children = [];
      deoptReason = "";
      positionTicks = [];
    }

  type t = {
    head: fn;
    startTime: float;
    endTime: float;
    samples: int list;
    timestamps: Int64.t list;
  } [@@deriving yojson]

  let lookup_id (table, last_id) name =
    match CCHashtbl.get table name with
    | None ->
      let id = !last_id in
      Hashtbl.add table name id;
      last_id := id + 1;
      id
    | Some id ->
      id

  let create_id_table () =
    Hashtbl.create 1024, ref 0

  let of_frames records =
    if records = [] then
      None
    else
    let func_table = create_id_table () in
    let script_table = create_id_table () in
    let last_node_id = ref 1 in (* 0 is for root *)
    let root = create_fn "(root)" ~call_id:(lookup_id func_table "(root)") ~node_id:0 in
    let rec add_to_node node = function
      | [] ->
        { node with hitCount = node.hitCount + 1 }, node.id
      | (frame : Gdbmi_proto.frame) :: remaining_frames ->
        let func_name = Gdb.demangle frame.func in
        let rec update = function
          | [] ->
            let call_id = lookup_id func_table func_name in
            let node_id = !last_node_id in
            last_node_id := !last_node_id + 1;
            let func = create_fn func_name ~node_id ~call_id in
            let file =
              match frame.fullname, frame.file, frame.from with
              (* fullname is too long and chromium doesn't follow it anyway *)
(*               | (Some f, _, _) | (_, Some f, _) | (_, _, Some f) -> f *)
              | (_, Some f, _) | (_, _, Some f) -> f
              | _ -> ""
            in
            let script_id = lookup_id script_table file in
            let node = {
              func with
              lineNumber = CCOpt.get_or ~default:0 frame.line;
              url = file;
              scriptId = string_of_int script_id;
            }
            in
            let node, sample_id = add_to_node node remaining_frames in
            [node], sample_id
          | child :: rest ->
            if child.functionName = func_name then
              let node, sample_id = add_to_node child remaining_frames in
              let sample_id = if remaining_frames = [] then child.id else sample_id in
              node :: rest, sample_id
            else
              let rest, sample_id = update rest in
              child :: rest, sample_id
        in
        let children, sample_id = update node.children in
        { node with children }, sample_id
    in
    let root, samples = List.fold_left begin fun (node, samples) (frames, _) ->
        let frames = List.rev frames in
        let node, sample_id = add_to_node node frames in
        node, sample_id :: samples
      end (root, []) records
    in
    let _, startTime = List.last records in
    let _, endTime = List.first records in
    assert (startTime <= endTime);
    let to_microsecond t = Int64.of_float (t *. 1000. *. 1000.) in
    Some {
      head = root;
      startTime;
      endTime;
      samples;
      timestamps = List.map (fun (_, t) -> to_microsecond t) records;
    }
end

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
  Lwt_list.iter_s line @@ List.take (rows - 1) @@ analyze h

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

let pmp pid cpuprofile_file =
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
    let rec loop_sampling () =
      match !should_exit with
      | true -> Lwt.return ()
      | false ->
      log "continuing ...";
      let%lwt () = Gdb.run gdb "continue" in (* TODO check running *)
      log "continued";
      let%lwt () = Lwt_unix.sleep 0.001 in
(*       let%lwt () = Lwt_unix.yield () in *)
      log "sampling gdb";
      let time = Unix.gettimeofday () in
      let%lwt frames = sample gdb in
      records := (frames, time) :: !records;
      log "sampled gdb";
      Hashtbl.replace h frames @@ ExtLib.Hashtbl.find_default h frames 0 + 1;
      log "%d entries" (Hashtbl.length h);
      log "frames: %s" @@ print_frames frames;
      List.iter (fun frame -> log "frame: %s" @@ print_frame frame) frames;
      log "next iteration";
      loop_sampling ()
    in
    let rec loop_draw () =
      match !should_exit with
      | true -> Lwt.return ()
      | false ->
      let%lwt () = display term h in
      let%lwt () = Lwt_unix.sleep 0.050 in
      loop_draw ()
    in
    let rec loop_user () =
      match%lwt LTerm.read_event term with
      | LTerm_event.Key key when is_exit_key key -> should_exit := true; Lwt.return ()
      | _ -> loop_user ()
    in
    let save () =
      let start_create_cpuprofile = Unix.gettimeofday () in
      let profile = Cpuprofile.of_frames !records in
      let took = Unix.gettimeofday () -. start_create_cpuprofile in
      log "creating cpu profile took %f" @@ took;
      Lwt_io.with_file ~mode:Lwt_io.Output cpuprofile_file begin fun channel ->
          match profile with
          | Some profile ->
            Printf.printf "%s written\n" @@ cpuprofile_file;
            Lwt_io.write channel @@ Yojson.Safe.to_string (Cpuprofile.to_yojson profile)
          | None ->
            failwith "Failed to create cpuprofile file"
        end
    in
    let%lwt () = Lwt.join [loop_user (); loop_sampling (); loop_draw ()] in
    save ()
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
  match List.tl @@ Array.to_list Sys.argv with
  | ["top"; pid; file] -> Lwt_main.run @@ pmp (int_of_string pid) file
(*   | ["dump";file] -> Lwt_main.run @@ dump_file file *)
(*   | ["read";file] -> Lwt_main.run @@ read_file file *)
  | _ -> Printf.eprintf  "Usage: rmp.native top <pid> <out.cpuprofile>\n"
