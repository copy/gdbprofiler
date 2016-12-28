open Gdbmi_utils

type frame =
  {
    level: int;
    addr: string;
    func: string;
    from: string option;
    file: string option;
    fullname: string option;
    line: int option
  }
let frame x =
  let x = named "frame" x  in
  let x = tuple x  in
  {
    level = (int (List.assoc "level" x));
    addr = (string (List.assoc "addr" x));
    func = (string (List.assoc "func" x));
    from = (assoc_map string "from" x);
    file = (assoc_map string "file" x);
    fullname = (assoc_map string "fullname" x);
    line = (assoc_map int "line" x)
  }

type stack = frame list
let stack x = let x = named "stack" x  in (list frame) x

type bkpt =
  {
    number: int;
    typ: string;
    disp: string;
    enabled: string;
    addr: int;
    func: string;
    file: string option;
    fullname: string option;
    line: int option;
    cond: int option;
    thread_groups: string values;
    times: int;
    ignore: int option
  }
let bkpt x =
  let x = named "bkpt" x  in
  let x = tuple x  in
  {
    number = (int (List.assoc "number" x));
    typ = (string (List.assoc "type" x));
    disp = (string (List.assoc "disp" x));
    enabled = (string (List.assoc "enabled" x));
    addr = (int (List.assoc "addr" x));
    func = (string (List.assoc "func" x));
    file = (assoc_map string "file" x);
    fullname = (assoc_map string "fullname" x);
    line = (assoc_map int "line" x);
    cond = (assoc_map int "cond" x);
    thread_groups = ((values string) (List.assoc "thread-groups" x));
    times = (int (List.assoc "times" x));
    ignore = (assoc_map int "ignore" x)
  }

type col_desc =
  {
    width: int;
    alignment: int;
    col_name: string;
    colhdr: string
  }
let col_desc x =
  let x = tuple x  in
  {
    width = (int (List.assoc "width" x));
    alignment = (int (List.assoc "alignment" x));
    col_name = (string (List.assoc "col_name" x));
    colhdr = (string (List.assoc "colhdr" x))
  }

type breakpoint_table =
  {
    nr_rows: int;
    nr_cols: int;
    hdr: col_desc values;
    body: bkpt list
  }
let breakpoint_table x =
  let x = named "BreakpointTable" x  in
  let x = tuple x  in
  {
    nr_rows = (int (List.assoc "nr_rows" x));
    nr_cols = (int (List.assoc "nr_cols" x));
    hdr = ((values col_desc) (List.assoc "hdr" x));
    body = ((list bkpt) (List.assoc "body" x))
  }

type value = string
let value x = let x = named "value" x  in string x
