open Ocamlbuild_plugin

let () =
  flag ["ocaml";"pp";"pp_cppo"] & S[A"cppo"; A"-V"; A (Printf.sprintf "OCAML:%s" Sys.ocaml_version)];
  ()
