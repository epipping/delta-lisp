INPUT  ?= ${PWD}/input
SCRIPT ?= ${PWD}/test.sh

FLAGS  ?= # --processes=4 --verbose

# Expected to accept --load. Tested with sbcl, ccl
LISP      ?= sbcl --non-interactive

# For perl
DELTA_PERL       ?= delta
DELTA_PERL_FLAGS ?= -quiet -cp_minimal=output-minimal-perl

LISP_FILES = delta.lisp processes.lisp utilities.lisp

.PHONY: all
all:
	@echo Please read the README.

.PHONY: run-delta-perl
run-delta-perl:
	@time $(DELTA_PERL) $(DELTA_PERL_FLAGS) -test=${SCRIPT} ${INPUT}

.PHONY: install-dependencies-via-quicklisp
install-dependencies-via-quicklisp: delta.asd delta-standalone.asd delta-tests.asd
	@$(LISP) --load mk/install.lisp

.PHONY: run-delta-lisp
run-delta-lisp: delta-standalone
	@time ./$< $(SCRIPT) $(INPUT) $(FLAGS)

delta-standalone: $(LISP_FILES) delta-standalone.lisp delta-standalone.asd
	@$(LISP) --load mk/build.lisp

.PHONY: test
test:
	@$(LISP) --load mk/test.lisp
