all:
	dune build

test:
	dune runtest
	dune build @test

promote:
	dune promote

clean:
	dune clean

.PHONY: all clean test
