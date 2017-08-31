(* http://valgrind.org/docs/manual/cl-format.html *)

let with_addresses = false

let rec iter_tree f (t : Cpuprofile.node) =
  f t;
  List.iter (fun t' -> iter_tree f t') t.children

let of_node node =
  let out = Buffer.create 20000 in
  let add fmt = Printf.ksprintf (fun s -> Buffer.add_string out s) (fmt ^^ "\n") in
  add "creator: rmp (https://github.com/copy/rmp)";
  add (if with_addresses then "positions: instr line" else "positions: line");
  add "events: time hits";
  add "";
  iter_tree begin fun node ->
    add "fl=%s" node.full_name;
    add "fn=%s" node.function_name;
    let hits = node.hit_count in
    let parent_line = node.line_number in
    let parent_address = node.address in
    let source_line = parent_line in (* TODO: For now *)
    let source_address = parent_address in (* TODO: For now *)
    let time = (Int64.of_float (node.time *. 1000.)) in
    if with_addresses then
      add "%d %d %Ld %d" parent_address parent_line time hits
    else
      add "%d %Ld %d" parent_line time hits;
    List.iter begin fun (child : Cpuprofile.node) ->
      let child_line = child.line_number in
      let call_hits = child.total_hit_count in
      let call_time = Int64.of_float (child.total_time *. 1000.) in
      let child_address = child.address in
      add "cfi=%s" child.full_name;
      add "cfn=%s" child.function_name;
      if with_addresses then begin
        add "calls=%d %d %d" call_hits child_address child_line;
        add "%d %d %Ld %d" source_address source_line call_time call_hits;
      end else begin
        add "calls=%d %d" call_hits child_line;
        add "%d %Ld %d" source_line call_time call_hits;
      end;
    end node.children;
    add "";
  end node;
  Buffer.contents out
