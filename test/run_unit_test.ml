module Gdbmi_proto = Gdb_lib.Gdbmi_proto
module Cpuprofile = Gdb_lib.Cpuprofile

let make_frame level addr func =
  Gdbmi_proto.{
    level;
    addr;
    func;
    from = None;
    file = None;
    fullname = None;
    line = None;
  }

let () = (* Cpuprofile.of_frames *)
  let start_time = 0. in
  let end_time = 1000. in
  let traces = [
    [], start_time
  ] in
  let _profile, tree = Cpuprofile.of_frames traces end_time in
  assert (tree.children = []);
  assert (tree.hit_count = 1);
  assert (tree.total_hit_count = 1);
  prerr_endline "ok 1"

let () = (* Cpuprofile.of_frames *)
  let start_time = 0. in
  let end_time = 1000. in
  let frame = make_frame 0 "0x1234" "func" in
  let traces = [
    [frame], start_time
  ] in
  let _profile, tree = Cpuprofile.of_frames traces end_time in
  assert (tree.hit_count = 0);
  assert (tree.total_hit_count = 1);
  begin match tree.children with
    | [child] ->
      assert (child.children = []);
      assert (child.hit_count = 1);
      assert (child.total_hit_count = 1);
      assert (child.function_name = frame.func);
    | _
      -> failwith "Bad tree"
  end;
  prerr_endline "ok 2"


let () = (* Cpuprofile.of_frames *)
  let start_time = 0. in
  let end_time = 1000. in
  let frame1 = make_frame 0 "0x1234" "func1" in
  let frame2 = make_frame 1 "0x1235" "func2" in
  let traces = [
    [frame2; frame1], start_time;
    [frame1], (start_time +. 100.);
  ] in
  let _profile, tree = Cpuprofile.of_frames traces end_time in
  assert (tree.hit_count = 0);
  assert (tree.total_hit_count = 2);
  begin match tree.children with
    | [child] ->
      assert (child.hit_count = 1);
      assert (child.total_hit_count = 2);
      assert (child.function_name = frame1.func);
      begin match child.children with
        | [subchild] ->
          assert (subchild.hit_count = 1);
          assert (subchild.total_hit_count = 1);
          assert (subchild.function_name = frame2.func);
        | _ -> failwith "Bad child tree"
      end;
    | _
      -> failwith "Bad tree"
  end;
  prerr_endline "ok 3"
