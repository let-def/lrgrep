#!/bin/sh

EXTENSIONS='*.ml*'

set -e
cd `git rev-parse --show-toplevel`
printf '\\newcommand{\\ixrevision}{%s}\n' `git rev-parse HEAD`
git grep -no '\\ix[a-z]*{[^{}]*}\+' -- $EXTENSIONS |
  while IFS=':' read file line def; do
    printf "%s{%s}{%s}\n" "\\$def" "$file" "$line"
  done
