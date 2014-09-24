open Ocamlbuild_plugin;;

flag ["ocaml";"compile";"dsource"] (S [A"-dsource"]);;
dep ["file:src/gdbmi_proto.ml"] ["ppx/ppx_inject.byte"];;
