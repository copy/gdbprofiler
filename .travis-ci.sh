wget https://raw.githubusercontent.com/ocaml/ocaml-travisci-skeleton/master/.travis-ocaml.sh
sh .travis-ocaml.sh

eval `opam config env`

#opam pin add --yes -n $(pwd)
#opam install --yes gdb

opam install menhir extlib lwt ppx_deriving ppx_deriving_yojson ppx_tools oasis cppo containers
make
