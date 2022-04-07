GRAMMAR=/home/def/Work/merlin/_build/default/src/ocaml/preprocess/parser_raw.cmly

all: main
	rlwrap dune exec ./main.exe -- -n -d test_lexer.mlyl -g "$(GRAMMAR)" -o test_lexer.ml

main:
	dune build main.exe

clean:
	dune clean

.PHONY: all main clean
