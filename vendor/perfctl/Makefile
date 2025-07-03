all:
	dune build

clean:
	dune clean

test:
	taskset -c 0 dune exec bin/main.exe -- record -- dune exec test/test_perfctl.exe
	@echo "# not_in_profile should not appear in perf report"
	perf report

.PHONY: all clean test
