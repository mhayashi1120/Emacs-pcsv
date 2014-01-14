check: compile
	emacs -q -batch -l pcsv.el -l pcsv-test.el \
		-eval "(ert-run-tests-batch-and-exit '(tag pcsv))"
	emacs -q -batch -l pcsv.elc -l pcsv-test.el \
		-eval "(ert-run-tests-batch-and-exit '(tag pcsv))"


compile:
	emacs -q -batch -eval "(byte-compile-file \"pcsv.el\")"; \
