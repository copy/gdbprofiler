open Gdbmi_utils

type frame = {
  level : int;
  addr : string;
  func : string;
  from : string option; (* binary *)
  file : string option; (* source *)
  fullname : string option;
  line : int option;
} [@@inject]

type stack = frame list [@@inject]

type bkpt = {
  number : int;
  typ : string;
  disp : string;
  enabled : string;
  addr : int;
  func : string;
  file : string option;
  fullname : string option;
  line : int option;
  cond : int option;
  thread_groups : string values;
  times : int;
  ignore : int option;
} [@@inject]

type col_desc = {
  width : int;
  alignment : int;
  col_name : string;
  colhdr : string;
} [@@inject unnamed]

type breakpoint_table = {
  nr_rows : int;
  nr_cols : int;
  hdr : col_desc values;
  body : bkpt list;
} [@@inject] [@@inject_name "BreakpointTable"]
