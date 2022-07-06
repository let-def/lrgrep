#!/bin/sh
eval `./setup_shell.sh`
for i in test_ko_01.ml test_ko_02.ml test_ko_03.ml test_ko_04.ml test_ko_05.ml test_ko_06.ml test_ko_07.ml; do
  printf "\033c"
  echo '$' cat "$i"
  source-highlight -f esc256 -i "$i" -o /dev/fd/1 | term-invert-colors
  read

  echo -n '$' ocamlc -c "$i"
  read
  ocamlc -c "$i"
  read
done
