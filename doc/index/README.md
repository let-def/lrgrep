# A simple code indexing system

`mkindex.sh` uses grep is used to collect comments of the form `\ix...{...}`. They are grouped in `ix-data.tex` and `{file}{line}` is appended, so it is possible to refer to the source where the reference was defined.

`ix-commands.tex` defines a few commands building upon this. A TeX file should contain `\input{ix-commands}\input{ix-data}` in its preamble.

WARNING: Experimental, I will tweak things as use patterns emerge.

## ixlabel and ixref

`\ixlabel{tag}{file}{line}` registers tag <tag> to be located at <file> and <line>.
`\ixref{tag}` is used in LaTeX code to refer a tag.

For instance, if `foo.ml` contains `(* \ixlabel{foo-module} *)` in line 10, then `\ixref{foo-module}` expands to `file foo.ml, line 10`.

## ixfile and ixline

To refer to the precise file and line, one can use the lower-level commands `\ixfile[foo-module]` that expands to `foo.ml` and `\ixline[foo-module]` that expands to `10`. Note the `[`, the tag is an optional argument (explained next).

## ixdef and ixloc

The lower level primitive `\ixdef{tag}{desc}{file}{line}` can be used to customize the message associated to tag.
For instance, a comment `(* \ixdef{foo-type}{the type of foo} *)` causes `\ixref{foo-type}` to expand to `the type of foo`.

In the definition, one can use `\ixfile` and `\ixline` to refer to the current file and line. Finally, `\ixloc` is `fille \ixfile, line \ixline`.
The comment `(* \ixdef{foo-type}{the type of foo (\ixloc)} *)` causes `\ixref{foo-type}` to expand to `the type of foo (file foo.ml, line 42)`.

## ixrevision

`\ixrevision` expands to the git revision of the current index.

This can be used to make a permanent link. A comment `(* \ixdef{tag}{\href{https://github.com/let-def/lrgrep/blob/\ixrevision/\ixfile#L\ixline}{\ixfile:\ixline}} *)` generates a permanent link to the file in current revision on github.
