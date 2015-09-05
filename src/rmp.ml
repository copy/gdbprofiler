open Printf
open ExtLib

let printfn fmt = ksprintf print_endline fmt
let eprintfn fmt = ksprintf prerr_endline fmt

(** @return most frequent first *)
let analyze h =
  h |> Hashtbl.enum
  |> List.of_enum
  |> List.sort ~cmp:(fun (_,a) (_,b) -> compare b a)
  |> List.map (fun (frames,n) -> sprintf "%4d %s" n (String.concat " " @@ List.map Gdb.show_frame_function frames))

let sample gdb =
  (Gdb.inferior gdb)#kill Sys.sigint;
  let%lwt _lines = Gdb.execute gdb "" in (* read notifications TODO check stopped *)
(*     List.iter (fun r -> print_endline @@ show_output_record r) lines; *)
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

let pmp pid =
  let%lwt gdb = Gdb.launch () in
  begin
    let%lwt term = init_term () in
    let%lwt mode = LTerm.enter_raw_mode term in
  begin
    let%lwt () = Gdb.run gdb "attach %d" pid in
    let h = Hashtbl.create 10 in
    let should_exit = ref false in
    let rec loop_sampling () =
      match !should_exit with
      | true -> Lwt.return ()
      | false ->
      let%lwt () = Gdb.run gdb "continue" in (* TODO check running *)
      let%lwt () = Lwt_unix.sleep 0.05 in
      let%lwt frames = sample gdb in
      Hashtbl.replace h frames @@ Hashtbl.find_default h frames 0 + 1;
      let%lwt () = display term h in
      loop_sampling ()
    in
    let rec loop_user () =
      match%lwt LTerm.read_event term with
      | LTerm_event.Key key when is_exit_key key -> should_exit := true; Lwt.return ()
      | _ -> loop_user ()
    in
    Lwt.join [loop_user (); loop_sampling ()]
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
        | Output r -> printfn "OUT: %s" @@ Gdbmi_types.show_output_record r
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
            let frames = Gdbmi_proto.stack x in
            Hashtbl.replace h frames @@ Hashtbl.find_default h frames 0 + 1
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
  match List.tl @@ Array.to_list Sys.argv with
  | ["top";pid] -> Lwt_main.run @@ pmp (int_of_string pid)
  | ["dump";file] -> Lwt_main.run @@ dump_file file
  | ["read";file] -> Lwt_main.run @@ read_file file
  | _ -> assert false
