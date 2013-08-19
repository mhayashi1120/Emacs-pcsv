check:
	emacs -q -batch -eval "(byte-compile-file \"pcsv.el\")"; \
	emacs -q -batch -l pcsv.el -l pcsv-test.el \
		-eval "(ert-run-tests-batch-and-exit '(tag pcsv))"
