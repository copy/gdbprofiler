let rec wait_for_termination ~timeout pid =
  let new_pid, status = Unix.waitpid [Unix.WNOHANG] pid in
  if new_pid <> 0 then
    (assert (new_pid = pid); `Stopped status)
  else if timeout <= 0 then
    `Not_stopped
  else
    (Unix.sleep 1; wait_for_termination pid ~timeout:(timeout - 1))

let run gdbprofiler_path =
  let null = Unix.openfile "/dev/null" [] 0o777 in
  let sleep_pid = Unix.create_process "sleep" [| "sleep"; "99999"; |] null Unix.stdout Unix.stdout in
  let cpuprofile = "/tmp/rmp_integration_test.cpuprofile" in
  let callgrind =  "/tmp/rmp_integration_test.callgrind" in
  let rmp_args = [|
    "./gdbprofiler";
    "-p"; string_of_int sleep_pid;
    "--cpuprofile"; cpuprofile;
    "--callgrind"; callgrind;
  |]
  in
  let rmp_stdin, rmp_stdin_write = Unix.pipe () in
  let delete_temporary_files () =
    CCIO.File.remove_noerr cpuprofile;
    CCIO.File.remove_noerr callgrind;
  in
  CCFun.finally ~h:delete_temporary_files ~f:begin fun () ->
    print_endline "Now starting integration test. stdout of rmp is shown below";
    let rmp_pid = Unix.create_process gdbprofiler_path rmp_args rmp_stdin Unix.stdout Unix.stdout in
    print_endline "Sleeping";
    Unix.sleep 5;
    print_endline "Sending enter";
    let written = Unix.write_substring rmp_stdin_write "\n" 0 1 in (* send enter to stop *)
    assert (written = 1);
    print_endline "Killing child";
    Unix.kill sleep_pid 2;
    print_endline "Waiting for termination of rmp";
    match wait_for_termination ~timeout:30 rmp_pid with
    | `Not_stopped ->
      assert false
    | `Stopped status ->
      assert (CCIO.File.exists cpuprofile);
      let _ : Yojson.Safe.json = Yojson.Safe.from_file cpuprofile in
      assert (CCIO.File.exists callgrind);
      assert (status = Unix.WEXITED 0)
  end

let () =
  match Sys.argv with
  | [| _; gdbprofiler_path |] ->
    run gdbprofiler_path
  | _ ->
    prerr_endline "Usage: run_integration_test path_to_gdbprofiler";
    exit 1
