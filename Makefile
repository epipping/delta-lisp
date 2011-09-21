benchmark:
	@time delta -test=./test.sh ./input >/dev/null
	@sbcl \
	 --eval '(ql:quickload "delta")' \
	 --eval '(time (delta:delta-file "input"))' \
	 --eval '(quit)' | grep 'of real time'
	@rm -rf tmp0
