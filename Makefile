.PHONY:
run-delta-perl:
	@delta -test=./test.sh ./input

.PHONY:
run-delta-ql:
	@sbcl \
	 --eval '(ql:quickload "delta")' \
	 --eval '(time (delta:delta-file "input" "./test.sh"))' \
	 --eval '(quit)'

.PHONY:
run-delta-asdf:
	@sbcl \
	 --no-userinit \
	 --eval '(require :asdf)' \
	 --eval "(asdf:oos 'asdf:load-op :delta)" \
	 --eval '(time (delta:delta-file "input" "./test.sh"))' \
	 --eval '(quit)'

delta-c++: delta.o
	$(CXX) $(LDFLAGS) $^ -o $@

.PHONY:
run-delta-c++: delta-c++
	@time ./delta-c++ input

.PHONY:
clean:
	@rm -rf tmp* delta-c++ *.o

.PHONY:
distclean: clean
	@rm -f output output-minimal
