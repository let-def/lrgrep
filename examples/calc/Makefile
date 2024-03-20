test:
	dune exec ./main.exe < test.txt

all:
	dune build

coverage:
	dune build parser.cmly
	dune exec ../../src/main.exe -- -coverage parse_errors.mlyl -g ../../_build/default/examples/calc/parser.cmly

enumerate:
	dune build parser.cmly
	dune exec ../../src/main.exe -- -enumerate lr0 parse_errors.mlyl -g ../../_build/default/examples/calc/parser.cmly
