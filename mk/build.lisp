(load "mk/path.lisp")

(locally #+sbcl (declare (sb-ext:muffle-conditions
                         sb-kernel:redefinition-warning))
  (handler-bind (#+sbcl (sb-kernel:redefinition-warning #'muffle-warning))
    (asdf:operate 'asdf:program-op :delta-standalone)))

(quit)
