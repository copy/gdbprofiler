
exception Parse_error of string * string * string

type gdb
(*
val send_command : gdb -> string -> unit Lwt.t
val read_input : gdb -> Gdbmi_types.output_record list Lwt.t
*)
val launch : ?dump:string -> unit -> gdb Lwt.t
val quit : gdb -> unit Lwt.t

val inferior : gdb -> Lwt_process.process
val execute : gdb -> string -> Gdbmi_types.output_record list Lwt.t
val mi : gdb -> string -> string list -> Gdbmi_types.result Lwt.t

val parse_io : string -> Gdbmi_types.input_output
val parse_output : string -> Gdbmi_types.output_record

module Cmd :
sig
  module P = Gdbmi_proto
  val stack_list_frames : gdb -> P.frame list Lwt.t
  val break_after : gdb -> int -> int -> unit Lwt.t
  val break_list : gdb -> P.breakpoint_table Lwt.t
  val break_disable : gdb -> int list -> unit Lwt.t
  val break_enable : gdb -> int list -> unit Lwt.t
  val break_delete : gdb -> int list -> unit Lwt.t
  val break_commands : gdb -> int -> string list -> unit Lwt.t
end

val run : gdb -> ('a, unit, string, unit Lwt.t) format4 -> 'a
val demangle : string -> string
val show_frame_function : Gdbmi_proto.frame -> string
