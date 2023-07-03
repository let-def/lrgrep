all:
	dune build

test:
	dune runtest

clean:
	dune clean

.PHONY: all clean test
