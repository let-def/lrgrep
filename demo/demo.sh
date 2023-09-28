#!/bin/sh

if which term-invert-colors; then
	invert()
	{
		term-invert-colors
	}
else
	invert()
	{
		cat -
	}
fi

if which source-highlight; then
	hl()
	{
		source-highlight -f esc256 -i "$1" -o /dev/fd/1
	}
else
	hl()
	{
		cat "$1"
	}
fi

dune build ../ocaml/frontend.bc
eval `./setup_shell.sh`
for i in "$@" test_ko_*.ml curry_pattern*.ml; do
  printf "\033c"
  printf "$ cat $i\n"
  read
  hl "$i" | invert
  read

  printf "$ ocamlc -c $i"
  read
  ocamlc -c "$i"
  read
done
