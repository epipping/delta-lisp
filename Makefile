perl-delta:
	@delta -test=./test.sh ./input

delta-ql:
	@sbcl \
	 --eval '(ql:quickload "delta")' \
	 --eval '(time (delta:delta-file "input" "./test.sh"))' \
	 --eval '(quit)'

delta-asdf:
	@sbcl \
	 --no-userinit \
	 --eval '(require :asdf)' \
	 --eval "(asdf:oos 'asdf:load-op :delta)" \
	 --eval '(time (delta:delta-file "input" "./test.sh"))' \
	 --eval '(quit)'

clean:
	@rm -rf tmp*
