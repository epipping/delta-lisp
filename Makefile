check:
	@time delta -test=./test.sh ./input >/dev/null
	@sbcl --no-sysinit --no-userinit \
	 --eval '(load "delta-lisp.lisp")' \
	 --eval '(time (delta-file "input"))' \
	 --eval '(quit)' \
	| grep 'of real time'
