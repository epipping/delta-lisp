(asdf:defsystem #:delta
  :serial t
  :depends-on (#:uiop #:external-program #:iterate #:let-plus)
  :components ((:file "package")
               (:file "processes" :depends-on ("package"))
               (:file "delta" :depends-on ("package"))))
