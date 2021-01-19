GRAMMAR=/home/def/Work/Git/malosinus/_build/default/ocaml/parser_raw.cmly

all: main
	rlwrap dune exec ./main.exe -- -n -d test_lexer.mlyl -g "$(GRAMMAR)"

main:
	dune build main.exe

clean:
	dune clean

.PHONY: all main clean
