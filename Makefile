.PHONY: build clean test

build:
	dune build @install

test:
	dune runtest --force
	dune build --force @run_integration_test

unit-test:
	dune runtest --force

install: build
	dune install

uninstall:
	dune uninstall

clean:
	rm -rf _build *.install
