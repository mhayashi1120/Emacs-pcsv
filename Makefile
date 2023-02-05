###
### Basic
###

-include env.mk

EMACS ?= emacs
BATCH := $(EMACS) -Q -batch 
ifdef ELPA-DIR
	BATCH += -eval "(setq package-user-dir (expand-file-name \"$(ELPA-DIR)\"))"
endif

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

###
### Command
###

ifdef EMACS_LINT_IGNORE
	LINT_BATCH := true
else
	LINT_BATCH := $(BATCH) -eval $(call package-installer, package-lint $(NEEDED-PACKAGES))
endif

INSTALL_BATCH := $(BATCH) -eval $(call package-installer, $(NEEDED-PACKAGES))
COMPILE_BATCH := $(BATCH)
ifndef EMACS_LINT_IGNORE
	COMPILE_BATCH += -eval "(setq byte-compile-error-on-warn t)"
endif

###
### Files
###

EL := pcsv.el
ELC := $(EL:%.el=%.elc)
BUILD_GENERATED := *.elc
MAINTAINER_GENERATED := elpa *~

LOAD_EL := $(EL:%=-l %)
LOAD_ELC := $(ELC:%=-l %)

###
### General rule
###

.PHONY: all check compile clean

all: check

check: compile
	$(BATCH) $(LOAD_EL) -l pcsv-test.el -f ert-run-tests-batch-and-exit
	$(BATCH) $(LOAD_ELC) -l pcsv-test.el -f ert-run-tests-batch-and-exit

compile:
	$(COMPILE_BATCH) -f batch-byte-compile $(EL)

clean:
	rm -rf $(BUILD_GENERATED)

###
### Maintainer rule
###

.PHONY: lint package maintainer-clean

lint:
	$(LINT_BATCH) -f package-lint-batch-and-exit $(EL)

package: lint check compile


maintainer-clean: clean
	rm -rf $(MAINTAINER_GENERATED)

###
### CI/CD rule
###

.PHONY: ci prepare-cicd

ci: prepare-cicd package

prepare-cicd:
	$(INSTALL_BATCH)
