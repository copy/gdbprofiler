open Ocamlbuild_plugin;;

mark_tag_used "use_menhir";;
flag ["ocaml";"compile";"ppx"] (S [A"-ppx";A"./ppx/ppx_inject.byte"]);;
flag ["ocaml";"compile";"dsource"] (S [A"-dsource"]);;
dep ["ppx"] ["ppx/ppx_inject.byte"];;
