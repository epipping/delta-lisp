(asdf:defsystem #:delta
  :serial t
  :depends-on (#:external-program #:iterate #:let-plus #:uiop)
  :components ((:file "package")
               (:file "processes" :depends-on ("package"))
               (:file "utilities" :depends-on ("package"))
               (:file "delta" :depends-on ("package"))))
