(asdf:defsystem #:delta-tests
  :serial t
  :depends-on (#:delta #:FiveAM)
  :components ((:file "tests")))
