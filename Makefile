all:
	dune build @install

install:
	dune install

test:
	dune runtest

clean:
	dune clean

uninstall:
	dune uninstall

.PHONY: all install test clean uninstall

