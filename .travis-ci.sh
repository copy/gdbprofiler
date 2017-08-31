wget https://raw.githubusercontent.com/ocaml/ocaml-travisci-skeleton/master/.travis-ocaml.sh
sh .travis-ocaml.sh

eval `opam config env`

opam pin add $(pwd) --yes --no-action
opam install gdbprofiler --yes --deps-only

make unit-test
