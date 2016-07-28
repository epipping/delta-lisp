(asdf:defsystem #:delta-tests
  :serial t
  :depends-on (#:delta #:FiveAM)
  :components ((:file "delta-tests.package")
               (:file "delta-tests" :depends-on ("delta-tests.package"))))
