(asdf:defsystem #:delta
  :serial t
  :depends-on (#:external-program #:osicat)
  :components ((:file "delta")))
