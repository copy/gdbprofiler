.PHONY: build clean test

build:
	jbuilder build @install

test:
	jbuilder runtest

unit-test:
	jbuilder build test/run_unit_test.exe
	./_build/default/test/run_unit_test.exe

install: build
	jbuilder install

uninstall:
	jbuilder uninstall

clean:
	rm -rf _build *.install
