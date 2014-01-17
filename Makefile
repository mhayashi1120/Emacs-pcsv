EMACS = emacs

check: compile
	$(EMACS) -q -batch -l pcsv.el -l pcsv-test.el \
		-f ert-run-tests-batch-and-exit
	$(EMACS) -q -batch -l pcsv.elc -l pcsv-test.el \
		-f ert-run-tests-batch-and-exit

compile:
	$(EMACS) -q -batch -f batch-byte-compile pcsv.el
