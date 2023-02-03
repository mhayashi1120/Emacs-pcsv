-include env.mk

EMACS ?= emacs
BATCH = $(EMACS) -Q -batch

EL := pcsv.el
ELC := $(EL:%.el=%.elc)

LOAD_EL := $(EL:%=-l %)
LOAD_ELC := $(ELC:%=-l %)

GENERATED = *.elc

check: compile
	$(BATCH) $(LOAD_EL) -l pcsv-test.el \
		-f ert-run-tests-batch-and-exit
	$(BATCH) $(LOAD_ELC) -l pcsv-test.el \
		-f ert-run-tests-batch-and-exit

compile:
	$(BATCH) -f batch-byte-compile $(EL)

clean:
	rm -f $(GENERATED)