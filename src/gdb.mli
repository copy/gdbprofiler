
module Types = Gdbmi_types
module Proto = Gdbmi_proto

exception Parse_error of string * string * string
exception Not_permitted

(** type representing GDB/MI session *)
type gdb
val send_command : gdb -> string -> unit Lwt.t
val read_input : gdb -> Types.output_record list Lwt.t

(** launch GDB *)
val launch : ?dump:string -> debugger:string array -> unit -> gdb Lwt.t

(** terminate GDB session and process *)
val quit : gdb -> unit Lwt.t

val collapse_recursive_frames : Proto.frame list -> Proto.frame list

(** @retrun GDB process *)
val inferior : gdb -> Lwt_process.process

(** execute GDB/MI command - low-level interface *)
val execute : gdb -> string -> Types.output_record list Lwt.t

(** exevcute GDB/MI, fail on error, ignore result *)
val run : gdb -> ('a, unit, string, unit Lwt.t) format4 -> 'a

(** execute GDB/MI command and get the result *)
val mi : gdb -> string -> string list -> Types.result Lwt.t

val parse_io : string -> Types.input_output
val parse_output : string -> Types.output_record

module Cmd :
sig
  val stack_list_frames : gdb -> Proto.frame list Lwt.t
  val break_after : gdb -> int -> int -> unit Lwt.t
  val break_list : gdb -> Proto.breakpoint_table Lwt.t
  val break_disable : gdb -> int list -> unit Lwt.t
  val break_enable : gdb -> int list -> unit Lwt.t
  val break_delete : gdb -> int list -> unit Lwt.t
  val break_commands : gdb -> int -> string list -> unit Lwt.t
  val break_insert : gdb -> string -> unit Lwt.t
  val data_evaluate_expression : gdb -> string -> string Lwt.t
end

(** @return demangled human readable function name *)
val demangle : string -> string

(** pretty-print frame information *)
val show_frame_function : Proto.frame -> string
