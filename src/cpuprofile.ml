type time = float

type position_tick = {
  line: int;
  ticks: int;
}

let rec (position_tick_to_yojson : position_tick -> Yojson.Safe.json) =
  ((
      fun x  ->
        let fields = []  in
        let fields = ("ticks", ((fun x  -> `Int x) x.ticks)) :: fields  in
        let fields = ("line", ((fun x  -> `Int x) x.line)) :: fields  in
        `Assoc fields))

type fn = {
  function_name: string [@key "functionName"];
  script_id: string [@key "scriptId"];
  url: string;
  line_number: int [@key "lineNumber"];
  column_number: int [@key "columnNumber"];
  hit_count: int [@key "hitCount"];
  call_uid: int [@key "callUID"];
  deopt_reason: string [@key "deoptReason"];
  id: int;
  position_ticks: position_tick list [@key "positionTicks"];
  children: fn list;
}

let rec (fn_to_yojson : fn -> Yojson.Safe.json) =
  ((
      fun x  ->
        let fields = []  in
        let fields =
          ("children",
            ((fun x  -> `List (List.map (fun x  -> fn_to_yojson x) x))
               x.children))
          :: fields  in
        let fields =
          ("positionTicks",
            ((fun x  ->
                `List (List.map (fun x  -> position_tick_to_yojson x) x))
               x.position_ticks))
          :: fields  in
        let fields = ("id", ((fun x  -> `Int x) x.id)) :: fields  in
        let fields = ("deoptReason", ((fun x  -> `String x) x.deopt_reason))
          :: fields  in
        let fields = ("callUID", ((fun x  -> `Int x) x.call_uid)) :: fields
           in
        let fields = ("hitCount", ((fun x  -> `Int x) x.hit_count)) :: fields
           in
        let fields = ("columnNumber", ((fun x  -> `Int x) x.column_number))
          :: fields  in
        let fields = ("lineNumber", ((fun x  -> `Int x) x.line_number)) ::
          fields  in
        let fields = ("url", ((fun x  -> `String x) x.url)) :: fields  in
        let fields = ("scriptId", ((fun x  -> `String x) x.script_id)) ::
          fields  in
        let fields =
          ("functionName", ((fun x  -> `String x) x.function_name)) :: fields
           in
        `Assoc fields))

type node = {
  function_name: string;
  script_id: string;
  url: string;
  line_number: int;
  column_number: int;
  hit_count: int;
  total_hit_count: int;
  call_uid: int;
  id: int;
  position_ticks: position_tick list;
  time: float;
  total_time: float;
  address: int;
  full_name: string;
  children: node list;
}

let rec node_to_fn node =
  let { function_name; script_id; url; line_number; column_number;
        hit_count; call_uid; id; position_ticks; children; _ } : node = node in
  ({
    function_name;
    script_id;
    url;
    line_number;
    column_number;
    hit_count;
    call_uid;
    deopt_reason = "";
    id;
    position_ticks;
    children = List.map node_to_fn children;
  } : fn)

let create_node name ~node_id ~call_id =
  {
    function_name = name;
    id = node_id;
    script_id = "0";
    url = "";
    line_number = 0;
    column_number = 0;
    hit_count = 0;
    total_hit_count = 0;
    call_uid = call_id;
    position_ticks = [];
    time = 0.0;
    total_time = 0.0;
    address = 0;
    full_name = "";
    children = [];
  }

type t = {
  head: fn;
  start_time: float [@key "startTime"];
  end_time: float [@key "endTime"];
  samples: int list;
  timestamps: Int64.t list;
}

let rec (to_yojson : t -> Yojson.Safe.json) =
  ((
      fun x  ->
        let fields = []  in
        let fields =
          ("timestamps",
            ((fun x  ->
                `List (List.map (fun x  -> `Intlit (Int64.to_string x)) x))
               x.timestamps))
          :: fields  in
        let fields =
          ("samples",
            ((fun x  -> `List (List.map (fun x  -> `Int x) x)) x.samples))
          :: fields  in
        let fields = ("endTime", ((fun x  -> `Float x) x.end_time)) :: fields
           in
        let fields = ("startTime", ((fun x  -> `Float x) x.start_time)) ::
          fields  in
        let fields = ("head", ((fun x  -> fn_to_yojson x) x.head)) :: fields
           in
        `Assoc fields))

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

let of_frames records end_time =
  if records = [] then failwith "Empty records";
  let func_table = create_id_table () in
  let script_table = create_id_table () in
  let last_node_id = ref 1 in (* 0 is for root *)
  let root = create_node "(root)" ~call_id:(lookup_id func_table "(root)") ~node_id:0 in
  let rec bump_position_ticks ticks line =
    match ticks with
    | [] -> [{ line; ticks = 1 }]
    | tick :: rest ->
      if tick.line = line then
        { tick with ticks = tick.ticks + 1 } :: rest
      else
        tick :: bump_position_ticks rest line
  in
  let rec add_frames_to_node node time frames =
    match frames with
    | [] ->
      { node with
        hit_count = node.hit_count + 1;
        total_hit_count = node.total_hit_count + 1;
        time;
      }, node.id
    | (frame : Gdbmi_proto.frame) :: remaining_frames ->
      let call_id = lookup_id func_table frame.func in
      let rec update = function
        | [] ->
          let node_id = !last_node_id in
          last_node_id := !last_node_id + 1;
          let demangled_name = Gdb.demangle frame.func in
          let func = create_node demangled_name ~node_id ~call_id in
          let file =
            match frame.fullname, frame.file, frame.from with
            (* fullname is too long and chromium doesn't follow it anyway *)
(*               | (Some f, _, _) | (_, Some f, _) | (_, _, Some f) -> f *)
            | (_, Some f, _) | (_, _, Some f) -> f
            | _ -> ""
          in
          let file = "http://localhost/ocaml/" ^ file in
          let script_id = lookup_id script_table file in
          let line = CCOpt.get_or ~default:1 frame.line in
          let address = int_of_string frame.addr in
          let position_ticks =
            if remaining_frames = []
            then bump_position_ticks func.position_ticks line
            else func.position_ticks
          in
          let node = {
            func with
            line_number = line;
            url = file;
            script_id = string_of_int script_id;
            address;
            full_name = CCOpt.get_or ~default:file frame.fullname;
            position_ticks;
          }
          in
          let node, sample_id = add_frames_to_node node time remaining_frames in
          [node], sample_id
        | child :: rest ->
          if child.call_uid = call_id then
            let node, sample_id = add_frames_to_node child time remaining_frames in
            let sample_id = if remaining_frames = [] then child.id else sample_id in
            let line = CCOpt.get_or ~default:1 frame.line in
            let node =
              if remaining_frames = []
              then { node with position_ticks = bump_position_ticks node.position_ticks line }
              else node
            in
            node :: rest, sample_id
          else
            let rest, sample_id = update rest in
            child :: rest, sample_id
      in
      let children, sample_id = update node.children in
      { node with
        children;
        total_hit_count = node.total_hit_count + 1;
        total_time = node.total_time +. time
      }, sample_id
  in
  let rec calculate_time_deltas = function
    | [] -> []
    | (frames, time) :: [] ->
      [(frames, end_time -. time)]
    | (frames, time) :: ((_, next_time) as next) :: rest ->
      (frames, next_time -. time) :: calculate_time_deltas (next :: rest)
  in
  let root, samples = List.fold_left begin fun (node, samples) (frames, time) ->
      assert (time >= 0.);
      let frames = List.rev frames in
      let node, sample_id = add_frames_to_node node time frames in
      node, sample_id :: samples
    end (root, []) (calculate_time_deltas records)
  in
  let _, start_time = match CCList.head_opt records with Some r -> r | None -> assert false in
  assert (start_time <= end_time);
  let to_microsecond t = Int64.of_float (t *. 1000. *. 1000.) in
  ({
    head = node_to_fn root;
    start_time;
    end_time;
    samples;
    timestamps = List.map (fun (_, t) -> to_microsecond t) records;
  }, root)
