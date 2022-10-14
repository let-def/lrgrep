#!/bin/sh

# Directly export in case setup_shell.sh
if [ -r "${BASH_SOURCE[0]:-${(%):-%x}}" ]; then
  export OCAMLPARAM="pp=$(realpath $(dirname ${BASH_SOURCE[0]:-${(%):-%x}})/../_build/default/ocaml/frontend.bc,_)"
else
  export OCAMLPARAM="pp=$(realpath $(dirname $0/../_build/default/ocaml/frontend.bc,_))"
fi
echo export "OCAMLPARAM='$OCAMLPARAM'"
