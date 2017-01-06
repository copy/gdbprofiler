let () =
  let null = Unix.openfile "/dev/null" [] 0o777 in
  let sleep_pid = Unix.create_process "sleep" [| "sleep"; "99999"; |] null Unix.stdout Unix.stdout in
  let cpuprofile = "/tmp/rmp_integration_test.cpuprofile" in
  let callgrind =  "/tmp/rmp_integration_test.callgrind" in
  let rmp_args = [|
    "./rmp.native";
    "-p"; string_of_int sleep_pid;
    "--cpuprofile"; cpuprofile;
    "--callgrind"; callgrind;
  |]
  in
  let rmp_stdin, rmp_stdin_write = Unix.pipe () in
  let _rmp_pid = Unix.create_process "./rmp.native" rmp_args rmp_stdin Unix.stdout Unix.stdout in
  Unix.sleep 5;
  let written = Unix.write rmp_stdin_write "\n" 0 1 in (* send enter to stop *)
  assert (written = 1);
  Unix.kill sleep_pid 2;
  Unix.sleep 1;
  assert (CCIO.File.exists cpuprofile);
  assert (CCIO.File.exists callgrind);
  ()
