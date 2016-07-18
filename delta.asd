(asdf:defsystem #:delta
  :serial t
  :depends-on (#:uiop #:external-program #:iterate)
  :components ((:file "delta")))
