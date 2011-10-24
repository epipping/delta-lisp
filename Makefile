INPUT  ?= ${PWD}/input
SCRIPT ?= ${PWD}/test.sh

.PHONY: all
all: delta-c++ delta-lisp

.PHONY: run-delta-perl
run-delta-perl:
	@delta -test=${SCRIPT} ${INPUT}

.PHONY: run-delta-ql
run-delta-ql:
	@sbcl \
	 --disable-debugger \
	 --eval '(ql:quickload "delta")' \
	 --eval "(time (delta:delta-file \"${INPUT}\" \"${SCRIPT}\"))" \
	 --eval '(quit)'

.PHONY: run-delta-asdf
run-delta-asdf:
	@sbcl \
	 --disable-debugger \
	 --no-userinit \
	 --eval '(require :asdf)' \
	 --eval "(asdf:oos 'asdf:load-op :delta)" \
	 --eval "(time (delta:delta-file \"${INPUT}\" \"${SCRIPT}\"))" \
	 --eval '(quit)'

delta-c++: delta.o
	$(CXX) $(LDFLAGS) $^ -o $@

.PHONY: run-delta-c++
run-delta-c++: delta-c++
	@time ./delta-c++ ${INPUT} ${SCRIPT}

.PHONY: clean
clean:
	@rm -rf tmp* delta-c++ *.o delta-lisp

.PHONY: distclean
distclean: clean
	@rm -f output output-minimal

delta-lisp: delta.lisp
	buildapp \
	 --load-system delta \
	 --eval '(defun main (argv) (delta:delta-file (third argv) (second argv)))' \
	 --entry main \
	 --output $@

.PHONY: run-delta-app
run-delta-app: delta-lisp
	@./delta-lisp ${SCRIPT} ${INPUT}
