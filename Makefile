INPUT  ?= ${PWD}/input
SCRIPT ?= ${PWD}/test.sh

.PHONY: all
all:
	@echo Please read the README.

.PHONY: run-delta-perl
run-delta-perl:
	@time delta -quiet -test=${SCRIPT} ${INPUT}

.PHONY: run-delta-lisp
run-delta-lisp:
	@sbcl --non-interactive \
	 --eval '(push (uiop:ensure-absolute-pathname *default-pathname-defaults*) asdf:*central-registry*)' \
	 --eval '(ql:quickload "delta")' \
	 --eval "(time (delta:delta-file \"${INPUT}\" \"${SCRIPT}\"))" \
	 --quit

.PHONY: run-delta-lisp-standalone
run-delta-lisp-standalone: delta-lisp-standalone
	@./delta-lisp-standalone $(INPUT) $(SCRIPT)

buildapp:
	@sbcl \
	 --disable-debugger \
	 --eval '(ql:quickload "buildapp")' \
	 --eval '(buildapp:build-buildapp)' --quit >/dev/null

quicklisp-manifest.txt: delta.asd
	@sbcl --no-userinit --no-sysinit --non-interactive \
	 --load ~/quicklisp/setup.lisp \
	 --eval '(push (uiop:ensure-absolute-pathname *default-pathname-defaults*) asdf:*central-registry*)' \
	 --eval '(ql:quickload "delta")' \
	 --eval '(ql:write-asdf-manifest-file "quicklisp-manifest.txt")'

delta-lisp-standalone: buildapp quicklisp-manifest.txt delta.lisp
	@./buildapp \
	 --manifest-file quicklisp-manifest.txt \
	 --asdf-path . \
	 --load-system delta \
	 --eval '(sb-ext:disable-debugger)' \
	 --eval '(defun main (argv) (delta:delta-file (second argv) (third argv)))' \
	 --entry main \
	 --output $@

.PHONY: clean
clean:
	@rm -rf output output-minimal quicklisp-manifest.txt delta-lisp-standalone buildapp
