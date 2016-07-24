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
	sbcl \
	 --eval '(push (uiop:ensure-absolute-pathname *default-pathname-defaults*) asdf:*central-registry*)' \
	 --eval '(asdf:disable-output-translations)' \
	 --eval '(ql:quickload "delta")' \
	 --eval '(ql:quickload "delta-standalone")' \
	 --eval '(ql:quickload "delta-tests")' \
	 --quit

.PHONY: run-delta-lisp
run-delta-lisp: delta-standalone
	@time ./$< $(SCRIPT) $(INPUT) $(FLAGS)

delta-standalone: $(LISP_FILES) main.lisp
	@$(SBCL) --non-interactive \
	 --eval '(push (uiop:ensure-absolute-pathname *default-pathname-defaults*) asdf:*central-registry*)' \
	 --eval '(asdf:disable-output-translations)' \
	 --eval "(asdf:operate 'asdf:program-op :delta-standalone)" >/dev/null

.PHONY: test
test:
	@$(SBCL) --non-interactive \
	 --eval '(push (uiop:ensure-absolute-pathname *default-pathname-defaults*) asdf:*central-registry*)' \
	 --eval '(asdf:disable-output-translations)' \
	 --eval "(asdf:operate 'asdf:load-op :delta-tests)" \
	 --eval "(5am:run 'delta-tests::delta-utility-tests)" \
     --quit

.PHONY: clean
clean:
	@rm -rf \
	 output output-minimal output-minimal-perl \
	 delta-standalone *.fasl tmp*
