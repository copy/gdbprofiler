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
