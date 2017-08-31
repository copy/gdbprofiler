.PHONY: build clean test

build:
	jbuilder build @install

test:
	jbuilder runtest

install: build
	jbuilder install

uninstall:
	jbuilder uninstall

clean:
	rm -rf _build *.install
