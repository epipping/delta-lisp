(load "mk/path.lisp")

(asdf:operate 'asdf:load-op :delta-tests)

(5am:run 'delta-tests::delta-utility-tests)
(5am:run 'delta-tests::delta-assumed-behaviour)
(write-line "")

(quit)
