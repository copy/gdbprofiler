type fn = {
  functionName: string;
  scriptId: string;
  url: string;
  lineNumber: int;
  columnNumber: int;
  hitCount: int;
  callUID: int;
  children: fn list;
  deoptReason: string;
  id: int;
  positionTicks: int list;
  selfSize: int; (* only for memory .heapprofile *)
} [@@deriving yojson]

let create_fn name ~node_id ~call_id =
  {
    functionName = name;
    id = node_id;
    scriptId = "0";
    url = "";
    lineNumber = 0;
    columnNumber = 0;
    hitCount = 0;
    callUID = call_id;
    children = [];
    deoptReason = "";
    positionTicks = [];
    selfSize = 0;
  }

type t = {
  head: fn;
  startTime: float;
  endTime: float;
  samples: int list;
  timestamps: Int64.t list;
} [@@deriving yojson]

let lookup_id (table, last_id) name =
  match CCHashtbl.get table name with
  | None ->
    let id = !last_id in
    Hashtbl.add table name id;
    last_id := id + 1;
    id
  | Some id ->
    id

let create_id_table () =
  Hashtbl.create 1024, ref 0

(* if `pred x` for one element of xs then replace it with `f (Some x)` else append `f None` *)
let rec update_element_or_add pred f xs =
  match xs with
  | [] -> [f None]
  | x :: xs' ->
    if pred x then f (Some x) :: xs
    else x :: update_element_or_add pred f xs'

let of_frames records =
  if records = [] then
    None
  else
  let func_table = create_id_table () in
  let script_table = create_id_table () in
  let last_node_id = ref 1 in (* 0 is for root *)
  let root = create_fn "(root)" ~call_id:(lookup_id func_table "(root)") ~node_id:0 in
  let rec add_to_node node allocation frames =
    match frames with
    | [] ->
      { node with
        hitCount = node.hitCount + 1;
        selfSize = node.selfSize + allocation;
      }, node.id
    | (frame : Gdbmi_proto.frame) :: remaining_frames ->
      let func_name = Gdb.demangle frame.func in
      let rec update = function
        | [] ->
          let call_id = lookup_id func_table func_name in
          let node_id = !last_node_id in
          last_node_id := !last_node_id + 1;
          let func = create_fn func_name ~node_id ~call_id in
          let file =
            match frame.fullname, frame.file, frame.from with
            (* fullname is too long and chromium doesn't follow it anyway *)
(*               | (Some f, _, _) | (_, Some f, _) | (_, _, Some f) -> f *)
            | (_, Some f, _) | (_, _, Some f) -> f
            | _ -> ""
          in
          let script_id = lookup_id script_table file in
          let node = {
            func with
            lineNumber = CCOpt.get_or ~default:0 frame.line;
            url = file;
            scriptId = string_of_int script_id;
          }
          in
          let node, sample_id = add_to_node node allocation remaining_frames in
          [node], sample_id
        | child :: rest ->
          if child.functionName = func_name then
            let node, sample_id = add_to_node child allocation remaining_frames in
            let sample_id = if remaining_frames = [] then child.id else sample_id in
            node :: rest, sample_id
          else
            let rest, sample_id = update rest in
            child :: rest, sample_id
      in
      let children, sample_id = update node.children in
      { node with children }, sample_id
  in
  let root, samples = List.fold_left begin fun (node, samples) (frames, _, allocation) ->
      let frames = List.rev frames in
      let node, sample_id = add_to_node node allocation frames in
      node, sample_id :: samples
    end (root, []) records
  in
  let _, startTime, _ = match List.rev records with r::_ -> r | _ -> assert false in
  let _, endTime, _ = match records with r::_ -> r | _ -> assert false in
  assert (startTime <= endTime);
  let to_microsecond t = Int64.of_float (t *. 1000. *. 1000.) in
  Some {
    head = root;
    startTime;
    endTime;
    samples;
    timestamps = List.map (fun (_, t, _) -> to_microsecond t) records;
  }
