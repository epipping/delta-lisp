INPUT  ?= ${PWD}/input
SCRIPT ?= ${PWD}/test.sh

.PHONY: all
all:
	@echo Please read the README.

.PHONY: run-delta-perl
run-delta-perl:
	@delta -test=${SCRIPT} ${INPUT}

.PHONY: run-delta-ql
run-delta-ql:
	@sbcl \
	 --disable-debugger \
	 --eval '(push (uiop:ensure-absolute-pathname *default-pathname-defaults*) asdf:*central-registry*)' \
	 --eval '(ql:quickload "delta")' \
	 --eval "(time (delta:delta-file \"${INPUT}\" \"${SCRIPT}\"))" \
	 --eval '(quit)'

.PHONY: run-delta-asdf
run-delta-asdf:
	@sbcl \
	 --disable-debugger \
	 --eval '(require :asdf)' \
	 --eval '(push (uiop:ensure-absolute-pathname *default-pathname-defaults*) asdf:*central-registry*)' \
	 --eval '(asdf:load-system :delta)' \
	 --eval "(time (delta:delta-file \"${INPUT}\" \"${SCRIPT}\"))" \
	 --eval '(quit)'

.PHONY: clean
clean:
	@rm -rf output output-minimal
