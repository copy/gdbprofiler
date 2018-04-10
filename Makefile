.PHONY: build clean test

build:
	jbuilder build @install

test:
	jbuilder runtest --force
	jbuilder build --force @run_integration_test

unit-test:
	jbuilder --force runtest

install: build
	jbuilder install

uninstall:
	jbuilder uninstall

clean:
	rm -rf _build *.install
