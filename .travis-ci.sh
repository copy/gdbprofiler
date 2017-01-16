wget https://raw.githubusercontent.com/ocaml/ocaml-travisci-skeleton/master/.travis-ocaml.sh
sh .travis-ocaml.sh

eval `opam config env`

opam pin add --yes -n $(pwd)
opam install --yes gdb

make
./run_integration_test.native
