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

if [ -r "${BASH_SOURCE[0]:-${(%):-%x}}" ]; then
  LRGREPOCAMLPARAM="pp=$(realpath $(dirname ${BASH_SOURCE[0]:-${(%):-%x}})/../_build/default/ocaml/frontend.bc),_"
else
  LRGREPOCAMLPARAM="pp=$(realpath $(dirname $0/../_build/default/ocaml/frontend.bc)),_"
fi

function lrgrep_ocamlc()
{
  OCAMLPARAM="$LRGREPOCAMLPARAM" unbuffer ocamlc -c "$i" | ghead -n -4
}

for i in test_ko_03.ml test_ko_01.ml test_ko_05.ml test_ko_06.ml test_ko_08.ml curry_pattern_4.ml; do
  printf "\033c"
  printf "$ cat $i\n"
  hl "$i" | invert
  read

  printf "$ lrgrep-ocamlc -c $i"
  read
  lrgrep_ocamlc -c "$i"
  read

  printf "$ ocamlc -c $i"
  read
  ocamlc -c "$i"
  read
done
