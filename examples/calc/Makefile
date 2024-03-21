test:
	dune exec ./main.exe < test.txt

all:
	dune build

coverage:
	dune build parser.cmly
	dune exec ../../src/main.exe -- compile -coverage parse_errors.mlyl -g ../../_build/default/examples/calc/parser.cmly

enumerate:
	dune build parser.cmly
	dune exec ../../src/main.exe -- enumerate -g ../../_build/default/examples/calc/parser.cmly

interpret:
	dune build parser.cmly
	dune exec ../../src/main.exe -- interpret -g ../../_build/default/examples/calc/parser.cmly -
