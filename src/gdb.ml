open Printf

module Types = Gdbmi_types
module Proto = Gdbmi_proto

exception Parse_error of string * string * string

exception Not_permitted
exception Exited of Unix.process_status

let log_verbose = false

let section = Lwt_log.Section.make "gdb"
let () = if log_verbose then Lwt_log.Section.set_level section Lwt_log.Debug
let logger = Lwt_main.run @@ Lwt_log.file ~template:"$(date)+$(milliseconds) | $(name): $(message)"
    ~mode:`Truncate ~file_name:"gdb.log" ()
let log fmt = Lwt_log.ign_debug_f  ~logger ~section (fmt ^^ "\n")

let eprintfn fmt = ksprintf prerr_endline fmt
let lwt_fail fmt = ksprintf (fun s -> Lwt.fail (Failure s)) fmt

let make_parser f s =
  let lexbuf = Lexing.from_string s in
  try
    f Gdbmi_lexer.ruleMain lexbuf
  with
  | exn ->
    let descr = match exn with Failure s -> s | exn -> Printexc.to_string exn in
    let tok = Lexing.lexeme lexbuf in
    let tail = Gdbmi_lexer.ruleTail "" lexbuf in
    raise (Parse_error (s,descr,tok ^ tail))

let () =
  Printexc.register_printer @@ function
  | Parse_error (s,descr,tail) -> Some (sprintf "==> %s\n  error: %s\n  at: %s" s descr tail)
  | _ -> None

let parse_output = make_parser Gdbmi_parser.output
let parse_io = make_parser Gdbmi_parser.input_output

type gdb = {
  proc : Lwt_process.process;
  mutable index : int;
  dump : (string * string * out_channel) option;
}

let record gdb s = match gdb.dump with
| Some (_,_,ch) -> fprintf ch "%s\n%!" s
| None -> ()

let send_command gdb s =
  record gdb s;
  log "send: %s" s;
  Lwt_io.write_line gdb.proc#stdin s

let read_input gdb =
  let rec loop acc =
    let%lwt s = try%lwt Lwt_io.read_line gdb.proc#stdout with End_of_file ->
      log "EOF";
      Lwt.fail End_of_file
    in (* timeout? *)
    log "receive: %s" s;
    record gdb s;
    match CCString.trim s with
    | "" -> loop acc
    | {|&"ptrace: Operation not permitted.\n"|} ->
      Lwt.fail Not_permitted
    | "(gdb)" -> Lwt.return @@ List.rev acc
    | s ->
      let r =
        try
          Some (parse_output s)
        with exn -> eprintfn "Parse error: %s" (Printexc.to_string exn); None
      in
      loop (match r with None -> acc | Some x -> x :: acc)
  in
  match gdb.proc#state with
  | Running ->
    loop []
  | Exited status ->
    log "Exited with %s" "";
    Lwt.fail (Exited status)

let inferior gdb = gdb.proc
let execute gdb s = let%lwt () = send_command gdb s in read_input gdb

let launch ?dump ~debugger () =
  let proc = Lwt_process.open_process ("", debugger) in
  let dump =
    match dump with
    | None -> None
    | Some path ->
      let (temp,ch) = Filename.(open_temp_file ~temp_dir:(dirname path) (basename path) ".temp") in
      Some (temp, path, ch)
  in
  let gdb = { proc; index = 0; dump; } in
  let%lwt _greeting = read_input gdb in
(*   let%lwt _ = execute gdb "shell date" in (* FIXME shell *) *)
  Lwt.return gdb

let quit gdb =
  let finish_dump () =
    match gdb.dump with
    | Some (temp, final, ch) ->
  (*     U.fsync !!(gdb.dump); *)
      close_out ch;
      Sys.rename temp final
    | None -> ()
  in
  log "quit";
  let%lwt (_:'a list) =
    try%lwt
      Lwt_unix.with_timeout 10.0 (fun () -> execute gdb "quit")
    with End_of_file -> Lwt.return []
  in
  finish_dump ();
  Lwt.return gdb.proc#terminate

let mi gdb s args =
  gdb.index <- gdb.index + 1;
  let token = sprintf "%d" gdb.index in
  let%lwt () = send_command gdb @@ String.concat " " (sprintf "%s-%s" token s :: args) in
  let rec loop () =
    let%lwt r = read_input gdb in (* skip until token matches *)
    match
      CCList.filter_map (function Types.Result (Some x,r) when x = token -> Some r | _ -> None) r
    with
    | [] -> loop ()
    | x::_ -> Lwt.return x
  in
  loop ()

let make_command gdb cmd unpack args =
  match%lwt mi gdb cmd args with
  | Done l -> Lwt.wrap1 unpack l
  | x -> Lwt.fail @@ Failure (sprintf "%s error result: %s" cmd (Types.show_result x))

module Unparse = struct

let zero k gdb = k (gdb,[])
let one f k (gdb,acc) x = k (gdb,(f x :: acc))
let int k acc x = one string_of_int k acc x
let string k acc x = one (sprintf "%S") k acc x
(* let list f k acc l = k (List.concat (List.map (f (fun x -> x) []) l @ acc)) *)
let list f k acc l =
  let rec loop k l acc =
    match l with
    | [] -> k acc
    | x::xs -> f (loop k xs) acc x
  in
  loop k l acc

let unpack1 f = function [x] -> f x | _ -> invalid_arg "return"
let unpack0 = function [] -> () | _ -> invalid_arg "unit"
let make unpack cmd = (fun (gdb,args) -> make_command gdb cmd unpack @@ List.rev args)

let unit name = make unpack0 name
let ret typ name = make (unpack1 typ) name
let (<=) func f = zero @@ f func
let ( * ) g f x = g @@ f x
let none x = x

end

module Cmd = struct

open! Unparse

let stack_list_frames = ret Proto.stack "stack-list-frames" <= none
let break_after = unit "break-after" <= int * int
let break_list = ret Proto.breakpoint_table "break-list" <= none
let break_disable = unit "break-disable" <= list int
let break_enable = unit "break-enable" <= list int
let break_delete = unit "break-delete" <= list int
let break_commands = unit "break-commands" <= int * list string
let break_insert = ret ignore "break-insert" <= string
let data_evaluate_expression = ret Proto.value "data-evaluate-expression" <= string

end

let rec collapse_recursive_frames : Proto.frame list -> Proto.frame list = function
  | [] -> []
  | [x] -> [x]
  | x :: (y :: _ as rest) ->
    if x.func = y.func
    then collapse_recursive_frames rest
    else x :: collapse_recursive_frames rest

let run gdb cmd =
  let%lwt r = execute gdb cmd in
  match CCList.filter_map (function Types.Result (_,r) -> Some r | _ -> None) r with
  | [] -> lwt_fail "no result from %S" cmd
  | _::_::_ -> lwt_fail "multiple results from %S" cmd
  | [Done _] -> Lwt.return ()
  | [OpError (err,_)] -> lwt_fail "error from %S : %s" cmd err
  | [_] -> lwt_fail "unexpected error from %S" cmd

let run gdb fmt = ksprintf (run gdb) fmt

let replace_all str sub by = CCString.replace ~which:`All ~sub ~by str

let get_hex = function
  | 'a'..'f' as c -> Some (Char.code c - Char.code 'a' + 10)
  | '0'..'9' as c -> Some (Char.code c - Char.code '0')
  | _ -> None

let get_hex2 s =
  if String.length s >= 2 then
    match get_hex s.[0], get_hex s.[1] with
    | Some n1, Some n0 -> Some (Char.chr (n1 * 16 + n0))
    | _ -> None
  else
    None

let unescape s =
  match CCString.split s ~by:"$" with
  | [] -> assert false
  | head :: tail ->
    let rest = List.map begin fun part ->
        match get_hex2 part with
        | Some n ->
          CCString.of_char n ^ CCString.drop 2 part
        | None ->
          "$" ^ part
      end tail
    in
    String.concat "" (head :: rest)

let () = assert (unescape "abc" = "abc")
let () = assert (unescape "abc$20XXX" = "abc XXX")

let is_number s = try ignore (int_of_string s); true with _ -> false
let truncate_at s sub = match CCString.Split.left s ~by:sub with None -> s | Some (l, _) -> l
let drop_prefix s pre = match CCString.chop_prefix ~pre s with Some pre -> pre | None -> s

let demangle s =
  if CCString.prefix s ~pre:"caml" && not (CCString.prefix s ~pre:"caml_") then
  begin
    let s = CCString.drop 4 s in
    let s = replace_all s "__" "." in
    let s = unescape s in
    match CCString.Split.right s ~by:"_" with
    | Some (left, suffix) ->
      if is_number suffix then
        left
      else
        s
    | None -> s
  end
  else
    (* get rid of symbol versioning such as 'pthread_cond_wait@@GLIBC_2.3.2' *)
    truncate_at s "@@"

let show_frame_function r =
  let open Proto in
  match (r:frame).func with
  | "??" -> (* use name of library *)
    begin match r.from with
    | None | Some "" -> r.func
    | Some so -> (drop_prefix (truncate_at (Filename.basename so) ".") "lib") ^ "::??"
    end
  | s -> demangle s
