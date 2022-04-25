export OCAMLPARAM=

all:
	dune build src/main.exe ocaml/interpreter.exe ocaml/frontend.bc

clean:
	dune clean

.PHONY: all clean
