# Alpha 0.3, Mon Jan 26 10:05:28 PM CET 2026

First release published on Opam.
Main functionalities are working: compiler, interpreter, coverage, enumeration and .messages importer.

Main features missing for stable release:
- enumeration and coverage reports miss some ϵ-reductions
- reports are not provided in a processing-friendly format (most probably JSON)
- the frontend is grammar agnostic and use symbol names; I would like to provide a public functor to easily make LRgrep frontends specialized for a concrete lexical syntax
- documentation and tutorial
- applications to O(x)Caml are not ready, application to (Mini-)Elm has not been merged yet
