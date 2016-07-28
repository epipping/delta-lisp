(load "mk/path.lisp")

(locally #+sbcl (declare (sb-ext:muffle-conditions
                          sb-kernel:redefinition-warning))
  (handler-bind (#+sbcl (sb-kernel:redefinition-warning #'muffle-warning))
    (asdf:operate 'asdf:load-op :delta-tests)))

(5am:run 'delta-tests::delta-utility-tests)

(quit)
