.PHONY: test promote

# [make test] runs each test and checks that its output matches the
# expected output stored in the file *.exp.

test:
	@ dune build @test

# [make promote] re-generates the expected-output files *.exp.

promote:
	@ dune promote

# [make dune] re-generates the file [dune] so that each file named *.in
# is considered as an input file for a test.

dune: $(wildcard *.in)
	@ rm -f $@
	@ for f in *.in ; do \
	  sed -e s/NAME/$${f%%.in}/g dune.template >> $@ ; \
	  echo >> $@ ; \
	done
