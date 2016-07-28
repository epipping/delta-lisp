INPUT  ?= ${PWD}/input
SCRIPT ?= ${PWD}/test.sh

FLAGS  ?= # --processes=4 --verbose

# For lisp
SBCL      ?= sbcl

# For perl
DELTA_PERL       ?= delta
DELTA_PERL_FLAGS ?= -quiet -cp_minimal=output-minimal-perl

LISP_FILES = delta.lisp processes.lisp

.PHONY: all
all:
	@echo Please read the README.

.PHONY: run-delta-perl
run-delta-perl:
	@time $(DELTA_PERL) $(DELTA_PERL_FLAGS) -test=${SCRIPT} ${INPUT}

.PHONY: install-dependencies-via-quicklisp
install-dependencies-via-quicklisp: delta.asd
	@$(SBCL) --load mk/install.lisp --quit

.PHONY: run-delta-lisp
run-delta-lisp: delta-standalone
	@time ./$< $(SCRIPT) $(INPUT) $(FLAGS)

delta-standalone: $(LISP_FILES) main.lisp
	@$(SBCL) --non-interactive --load mk/build.lisp --quit

.PHONY: test
test:
	@$(SBCL) --non-interactive --load mk/test.lisp --quit

.PHONY: clean
clean:
	@rm -rf \
	 output output-minimal output-minimal-perl \
	 delta-standalone *.fasl tmp*
