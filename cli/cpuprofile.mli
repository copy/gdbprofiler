type position_tick = { line : int; ticks : int; }
type node = {
  function_name : string;
  script_id : string;
  url : string;
  line_number : int;
  column_number : int;
  hit_count : int;
  total_hit_count : int;
  call_uid : int;
  id : int;
  position_ticks : position_tick list;
  time : float;
  total_time : float;
  self_size : int;
  address : int;
  full_name : string;
  children : node list;
}

type fn
type t = {
  head : fn;
  start_time : float;
  end_time : float;
  samples : int list;
  timestamps : Int64.t list;
}

val of_frames : (Gdbmi_proto.frame list * float * int) CCList.t -> float -> t * node
val to_yojson : t -> Yojson.Safe.json
