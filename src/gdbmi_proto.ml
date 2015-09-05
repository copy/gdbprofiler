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
#if OCAML_VERSION < (4,02,2)
  typ [@name "type"] : string;
#else
  typ : string [@name "type"];
#endif
  disp : string;
  enabled : string;
  addr : int;
  func : string;
  file : string option;
  fullname : string option;
  line : int option;
  cond : int option;
#if OCAML_VERSION < (4,02,2)
  thread_groups [@name "thread-groups"] : string values;
#else
  thread_groups : string values [@name "thread-groups"];
#endif
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
} [@@inject] [@@name "BreakpointTable"]
