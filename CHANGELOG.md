# Beta 0.9, Wed May 20 07:20:49 PM JST 2026

First publicly announced release.

New features:
- enumeration and coverage are now precise (and reasonably fast)
- reports can be provided in a low-level JSON format, to be post-processed by jq;
  the format is not stable yet
- "Getting started" guide
- `lrgrep.top` provides a functor to customize lexical syntax
- application to OCaml with exhaustive coverage (builtin and https://github.com/let-def/lrgrep-ocaml)
- prototype on OxCaml

Still pending:
- prototype on latest OxCaml
- sample applications to (Mini-)Elm and Catala have not been merged yet
- guards for detecting "unreachable actions" are not yet supported

# Alpha 0.3, Mon Jan 26 10:05:28 PM CET 2026

First release published on Opam.
Main functionalities are working: compiler, interpreter, coverage, enumeration and .messages importer.

Main features missing for stable release:
- enumeration and coverage reports miss some ϵ-reductions
- reports are not provided in a processing-friendly format (most probably JSON)
- the frontend is grammar agnostic and use symbol names; I would like to provide a public functor to easily make LRgrep frontends specialized for a concrete lexical syntax
- documentation and tutorial
- applications to O(x)Caml are not ready, application to (Mini-)Elm has not been merged yet
