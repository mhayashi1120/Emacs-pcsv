-include env.mk

# This come from `package-lint/run-tests.sh`
define package-installer
  "(progn \
   (require 'package) \
   (push '(\"melpa\" . \"https://melpa.org/packages/\") package-archives) \
   (package-initialize) \
   (package-refresh-contents) \
   (dolist (pkg '($(1))) \
    (unless (package-installed-p pkg) \
      (package-install pkg))))"
endef


EMACS ?= emacs
BATCH := $(EMACS) -Q -batch 
ifdef ELPA-DIR
	BATCH += -eval "(setq package-user-dir (expand-file-name \"$(ELPA-DIR)\"))"
endif

# NOTE: This come from `pacakge-lint/run-tests.sh`
LINT_BATCH := $(BATCH) -eval $(call package-installer, package-lint $(NEEDED-PACKAGES))
INSTALL_BATCH := $(BATCH) -eval $(call package-installer, $(NEEDED-PACKAGES))

EL := pcsv.el
ELC := $(EL:%.el=%.elc)

LOAD_EL := $(EL:%=-l %)
LOAD_ELC := $(ELC:%=-l %)

GENERATED := *.elc

MAINTAINER-GENERATED := elpa

###
### General rule
###

.PHONY: all check compile clean

all: check

check: compile
	$(BATCH) $(LOAD_EL) -l pcsv-test.el -f ert-run-tests-batch-and-exit
	$(BATCH) $(LOAD_ELC) -l pcsv-test.el -f ert-run-tests-batch-and-exit

compile:
	$(BATCH) -f batch-byte-compile $(EL)

clean:
	rm -rf $(GENERATED)

###
### Maintainer rule
###

.PHONY: lint package maintainer-clean

lint:
	$(LINT_BATCH) -f package-lint-batch-and-exit $(EL)

package: lint check compile


maintainer-clean:
	rm -rf $(MAINTAINER-GENERATED)

###
### CI/CD rule
###

.PHONY: ci prepare-cicd

ci: prepare-cicd package

prepare-cicd:
	$(INSTALL_BATCH)
