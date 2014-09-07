
open Printf
open ExtLib
open Gdbmi_types

external (|>) : 'a -> ('a -> 'b) -> 'b = "%revapply"
external (@@) : ('a -> 'b) -> 'a -> 'b = "%apply"
let (!!) = Lazy.force

let eprintfn fmt = ksprintf prerr_endline fmt
let printfn fmt = ksprintf print_endline fmt
let lwt_fail fmt = ksprintf (fun s -> Lwt.fail (Failure s)) fmt

module Parser = struct
let make f s = Parser_utils.parse_buf_exn (f Gdbmi_lexer.ruleMain) (Lexing.from_string s)
let parse_output = make Gdbmi_parser.output
let parse_io = make Gdbmi_parser.input_output
end

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

type gdb = { proc : Lwt_process.process; mutable index : int; dump : out_channel Lazy.t; }

let dump_file_final = "pmp.txt"
let dump_file_temp = "." ^ dump_file_final

let record gdb s = fprintf !!(gdb.dump) "%s\n%!" s

let send_command gdb s =
  record gdb s;
  Lwt_io.write_line gdb.proc#stdin s

let read_input gdb =
  let rec loop acc =
    lwt s = try_lwt Lwt_io.read_line gdb.proc#stdout with End_of_file -> Lwt.return "(gdb)" in (* timeout? *)
    record gdb s;
    match String.strip s with
    | "" -> loop acc
    | "(gdb)" -> Lwt.return @@ List.rev acc
    | s ->
      let r = try Some (Parser.parse_output s) with exn -> handle_parser_error s exn; None in
      loop (match r with None -> acc | Some x -> x :: acc)
  in
  loop []

let execute gdb s = send_command gdb s >> read_input gdb

let launch () =
  let proc = Lwt_process.open_process ("",[|"gdb"; "--interpreter=mi"|]) in
  let gdb = { proc; index = 0; dump = lazy (open_out dump_file_temp); } in
  lwt _greeting = read_input gdb in
(*   lwt _ = execute gdb "shell date" in (* FIXME shell *) *)
  Lwt.return gdb

let quit gdb = (* FIXME wait -> timeout -> kill *)
  let finish_dump () =
    if Lazy.is_val gdb.dump then
    begin
  (*     U.fsync !!(gdb.dump); *)
      close_out !!(gdb.dump);
      Sys.rename dump_file_temp dump_file_final;
    end
  in
  execute gdb "quit" >> (finish_dump (); Lwt.return gdb.proc#terminate)

let mi gdb s args =
  gdb.index <- gdb.index + 1;
  let token = sprintf "%d" gdb.index in
  lwt () = send_command gdb @@ String.concat " " (sprintf "%s-%s" token s :: args) in
  let rec loop () =
    lwt r = read_input gdb in (* skip until token matches *)
    match List.filter_map (function Result (Some x,r) when x = token -> Some r | _ -> None) r with
    | [] -> loop ()
    | x::_ -> Lwt.return x
  in
  loop ()

let make_command gdb cmd unpack args =
  match_lwt mi gdb cmd args with
  | Done l -> Lwt.wrap1 unpack l
  | x -> Lwt.fail @@ Failure (sprintf "%s error result: %s" cmd (string_of_result x))

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

module P = Gdbmi_proto

let stack_list_frames = ret P.stack "stack-list-frames" <= none
let break_after = unit "break-after" <= int * int
let break_list = ret P.breakpoint_table "break-list" <= none
let break_disable = unit "break-disable" <= list int
let break_enable = unit "break-enable" <= list int
let break_delete = unit "break-delete" <= list int
let break_commands = unit "break-commands" <= int * list string

end

let run gdb cmd =
  lwt r = execute gdb cmd in
  match List.filter_map (function Result (_,r) -> Some r | _ -> None) r with
  | [] -> lwt_fail "no result from %S" cmd
  | _::_::_ -> lwt_fail "multiple results from %S" cmd
  | [Done _] -> Lwt.return ()
  | [OpError (err,_)] -> lwt_fail "error from %S : %s" cmd err
  | [_] -> lwt_fail "unexpected error from %S" cmd

let run gdb fmt = ksprintf (run gdb) fmt

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
let drop_prefix s pre = if String.starts_with s pre then String.slice ~first:(String.length pre) s else s

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
  let open Gdbmi_proto in
  match (r:frame).func with
  | "??" -> (* use name of library *)
    begin match r.from with
    | None | Some "" -> r.func
    | Some so -> (drop_prefix (truncate_at (Filename.basename so) ".") "lib") ^ "::??"
    end
  | s -> demangle s
