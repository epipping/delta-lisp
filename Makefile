INPUT  ?= ${PWD}/t/silly/input
SCRIPT ?= ${PWD}/t/silly/test.sh

FLAGS  ?= # --processes=4 --quiet

CL_LAUNCH         ?= cl  # for roswell: ros
CL_LAUNCH_OPTIONS ?= -Q  # for roswell: -Q -l

# For perl
DELTA_PERL       ?= delta
DELTA_PERL_FLAGS ?= -quiet -cp_minimal=output-minimal-perl

LISP_FILES = delta.lisp utilities.lisp

.PHONY: all
all:
	@echo Please read the README.

.PHONY: run-delta-perl
run-delta-perl:
	@time $(DELTA_PERL) $(DELTA_PERL_FLAGS) -test=${SCRIPT} ${INPUT}

.PHONY: install-dependencies-via-quicklisp
install-dependencies-via-quicklisp: delta.asd delta-standalone.asd delta-tests.asd
	@$(CL_LAUNCH) $(CL_LAUNCH_OPTIONS) mk/install.lisp

.PHONY: run-delta-lisp
run-delta-lisp: delta-standalone
	@time ./$< $(SCRIPT) $(INPUT) $(FLAGS)

delta-standalone: $(LISP_FILES) delta-standalone.lisp delta-standalone.asd
	@$(CL_LAUNCH) $(CL_LAUNCH_OPTIONS) mk/build.lisp

.PHONY: test
test:
	@$(CL_LAUNCH) $(CL_LAUNCH_OPTIONS) mk/test.lisp
