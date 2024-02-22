test:
	dune exec ./main.exe < test.txt

all:
	dune build

coverage:
	dune exec ../../src/main.exe -- -coverage parse_errors.mlyl -g ../../_build/default/examples/calc/parser.cmly
