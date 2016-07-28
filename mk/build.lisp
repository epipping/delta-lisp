(push (uiop:ensure-absolute-pathname *default-pathname-defaults*)
      asdf:*central-registry*)

(asdf:disable-output-translations)

(locally (declare (sb-ext:muffle-conditions 
                   sb-kernel:redefinition-warning))
  (handler-bind ((sb-kernel:redefinition-warning #'muffle-warning))
    (asdf:operate 'asdf:program-op :delta-standalone)))
