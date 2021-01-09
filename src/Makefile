all: main
	dune exec ./main.exe -- -n -d test_lexer.mlyl

main:
	dune build main.exe

clean:
	dune clean

.PHONY: all main clean
