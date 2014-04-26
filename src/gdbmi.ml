
open Printf
open ExtLib
open Gdbmi_types

external (|>) : 'a -> ('a -> 'b) -> 'b = "%revapply"
external (@@) : ('a -> 'b) -> 'a -> 'b = "%apply"

let eprintfn fmt = Printf.ksprintf prerr_endline fmt
let printfn fmt = Printf.ksprintf print_endline fmt

let dump = ref false

let is_alnum = function
| 'a'..'z' -> true
| 'A'..'Z' -> true
| '0'..'9' -> true
| _ -> false

let handle_parser_error line exn =
  match exn with
  | Parser_utils.Error (exn,(_line,_cnum,tok,tail)) ->
   let error = match exn with
   | Failure s -> s
   | exn -> Printexc.to_string exn 
   in
   eprintfn "==> %s" line;
   eprintfn "  error: %s" error;
   eprintfn "  at: %s%s" tok (String.slice ~last:32 tail);
  | _ -> raise exn

let send_command gdb s =
  if !dump then print_endline s;
  Lwt_io.write_line gdb#stdin s

let read_input gdb =
  let rec loop acc =
    lwt s = try_lwt Lwt_io.read_line gdb#stdout with End_of_file -> Lwt.return "(gdb)" in (* timeout? *)
    if !dump then print_endline s;
    match String.strip s with
    | "" -> loop acc
    | "(gdb)" -> Lwt.return @@ List.rev acc
    | s ->
      let r = try Some (Parser.parse_output s) with exn -> handle_parser_error s exn; None in
      loop (match r with None -> acc | Some x -> x :: acc)
  in
  loop []

let launch () =
  let gdb = Lwt_process.open_process ("",[|"gdb"; "--interpreter=mi"|]) in
  lwt _greeting = read_input gdb in
  Lwt.return gdb

let execute gdb s = send_command gdb s >> read_input gdb

let index = ref 0

let mi gdb s =
  incr index;
  let token = sprintf "%d" !index in
  lwt () = send_command gdb @@ sprintf "%s-%s" token s in
  let rec loop () =
    lwt r = read_input gdb in (* skip until token matches *)
    match List.filter_map (function Result (Some x,r) when x = token -> Some r | _ -> None) r with
    | [] -> loop ()
    | x::_ -> Lwt.return x
  in
  loop ()

type frame = {
  level : int;
  addr : string;
  func : string;
  from : string option; (* binary *)
  file : string option; (* source *)
  fullname : string option;
  line : int option;
}

let list = function List x -> x | _ -> assert false
let string = function String x -> x | _ -> assert false
let int = function String x -> int_of_string x | _ -> assert false
let assoc map k l = map @@ List.assoc k l
let assoc_opt map k l = try Some (assoc map k l) with Not_found -> None

let extract_stack_frames x =
  x |> List.assoc "stack"
  |> list
  |> List.filter_map (function ("frame", Tuple l) -> Some l | _ -> None)
  |> List.map begin fun t ->
    { level = assoc int "level" t;
      addr = assoc string "addr" t;
      func = assoc string "func" t;
      from = assoc_opt string "from" t;
      file = assoc_opt string "file" t;
      fullname = assoc_opt string "fullname" t;
      line = assoc_opt int "line" t;
    }
  end

let stack_list_frames gdb =
  match_lwt mi gdb "stack-list-frames" with
  | Done x ->
(*     print_endline @@ string_of_value @@ Tuple x; *)
    Lwt.return @@ extract_stack_frames x
  | x -> eprintfn "stack-list-frames error result: %s" (string_of_result x); Lwt.return []

let run gdb s =
  lwt r = execute gdb s in
  match List.exists (function Result (_,Done _) -> true | _ -> false) r with
  | true -> Lwt.return ()
  | false -> assert false 

(*
  if ( targ ~ /[<\\(]/ && targ !~ /^operator[<\\(]/ ) {
     # Shorten C++ templates, e.g. in t/samples/stacktrace-004.txt
     while ( targ ~ />( *\$|::)/ ) {
        if ( 0 == gsub(/<[^<>]*>/, "", targ) ) {
           break;
        }
     }
  }
*)

let replace_all str sub by =
  let rec loop str =
    match String.replace ~str ~sub ~by with
    | true, str -> loop str
    | false, s -> s
  in
  loop str

let is_number s = try ignore (int_of_string s); true with _ -> false
let truncate_at s sub = try fst @@ String.split s sub with _ -> s

let demangle s =
  if String.starts_with s "caml" && not (String.starts_with s "caml_") then
  begin
    let s = String.slice ~first:4 s in
    let s = replace_all s "__" "." in
    match List.rev @@ String.nsplit s "_" with
    | last'::prev::_ when is_number last' ->
      let last = - (String.length last' + 1) in
      if String.ends_with prev ".fun" then
        String.slice s ~last:(last - 4) ^ "#" ^ last'
      else
        String.slice s ~last
    | _ -> s
  end
  else 
    (* get rid of symbol versioning such as 'pthread_cond_wait@@GLIBC_2.3.2' *)
    truncate_at s "@@"

let show_frame_function r =
  match r.func with
  | "??" -> (* use name of library *)
    begin match r.from with
    | None | Some "" -> r.func
    | Some so -> (truncate_at (Filename.basename so) ".") ^ "::??"
    end
  | s -> demangle s

let analyze h =
  h |> Hashtbl.enum
  |> List.of_enum
  |> List.sort ~cmp:(fun (_,a) (_,b) -> compare a b)
  |> List.iter (fun (frames,n) -> printfn "%4d %s" n (String.concat " " @@ List.map show_frame_function frames))

let pmp pid =
  lwt gdb = launch () in
  let run fmt = ksprintf (run gdb) fmt in
  lwt () = run "attach %d" pid in
  let h = Hashtbl.create 10 in
  let t = Unix.gettimeofday () in
  lwt () = while_lwt Unix.gettimeofday () -. t < 10. do
    lwt () = run "continue" in (* TODO check running *)
    lwt () = Lwt_unix.sleep 0.05 in
    gdb#kill Sys.sigint;
    lwt _lines = execute gdb "" in (* read notifications TODO check stopped *)
(*     List.iter (fun r -> print_endline @@ string_of_output_record r) lines; *)
    lwt frames = stack_list_frames gdb in
    Hashtbl.replace h frames @@ Hashtbl.find_default h frames 0 + 1;
    Lwt.return ()
  done in
  analyze h;
  Lwt.return ()

let dump_file file =
  let parse_line s =
    try
      match String.strip s with
      | "" -> ()
      | s ->
        match Parser.parse_io s with
        | Prompt -> printfn "---"
        | Input _ -> printfn "IN: %s" s
        | Output r -> printfn "OUT: %s" @@ string_of_output_record r
    with
      exn -> handle_parser_error s exn
  in
  Lwt_io.lines_of_file file |> Lwt_stream.iter parse_line

let read_file file =
  let h = Hashtbl.create 10 in
  let parse_line s =
    try
      match String.strip s with
      | "" -> ()
      | s ->
        match Parser.parse_io s with
        | Output (Result (_, Done x)) ->
          begin try
            let frames = extract_stack_frames x in
            Hashtbl.replace h frames @@ Hashtbl.find_default h frames 0 + 1
          with _ -> () (* no stack frames here *)
          end
        | _ -> ()
    with exn -> handle_parser_error s exn
  in
  lwt () = Lwt_io.lines_of_file file |> Lwt_stream.iter parse_line in
  analyze h;
  Lwt.return ()

let () =
  match List.tl @@ Array.to_list Sys.argv with
  | ["pmp";pid] -> Lwt_main.run @@ pmp (int_of_string pid)
  | ["dump";file] -> Lwt_main.run @@ dump_file file
  | ["read";file] -> Lwt_main.run @@ read_file file
  | _ -> assert false
