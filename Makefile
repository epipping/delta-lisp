INPUT  ?= ${PWD}/input
SCRIPT ?= ${PWD}/test.sh

# For lisp
PROCESSES ?= 1
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

.PHONY: run-delta-lisp
run-delta-lisp:
	@$(SBCL) --non-interactive \
	 --eval '(push (uiop:ensure-absolute-pathname *default-pathname-defaults*) asdf:*central-registry*)' \
	 --eval '(ql:quickload "delta")' \
	 --eval "(time (delta:delta-file \"${INPUT}\" \"${SCRIPT}\" :processes ${PROCESSES}))" \
	 --quit

.PHONY: run-delta-lisp-standalone
run-delta-lisp-standalone: delta-lisp-standalone
	@time ./delta-lisp-standalone $(INPUT) $(SCRIPT) $(PROCESSES)

buildapp:
	@$(SBCL) \
	 --disable-debugger \
	 --eval '(ql:quickload "buildapp")' \
	 --eval '(buildapp:build-buildapp)' --quit >/dev/null

quicklisp-manifest.txt: delta.asd
	@$(SBCL) --no-userinit --no-sysinit --non-interactive \
	 --load ~/quicklisp/setup.lisp \
	 --eval '(push (uiop:ensure-absolute-pathname *default-pathname-defaults*) asdf:*central-registry*)' \
	 --eval '(ql:quickload "delta")' \
	 --eval '(ql:write-asdf-manifest-file "quicklisp-manifest.txt")'

delta-lisp-standalone: buildapp quicklisp-manifest.txt $(LISP_FILES) main.lisp
	@./buildapp \
	 --manifest-file quicklisp-manifest.txt \
	 --asdf-path . \
	 --load-system delta \
	 --eval '(sb-ext:disable-debugger)' \
	 --eval '(defun main (argv) (delta:delta-file (second argv) (third argv) :processes (parse-integer (fourth argv))))' \
	 --entry main \
	 --output $@

.PHONY: clean
clean:
	@rm -rf output output-minimal quicklisp-manifest.txt delta-lisp-standalone buildapp
