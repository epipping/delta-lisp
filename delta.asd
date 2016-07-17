(asdf:defsystem #:delta
  :serial t
  :depends-on (#:external-program #:uiop)
  :components ((:file "delta")))
