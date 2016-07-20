(asdf:defsystem #:delta
  :serial t
  :depends-on (#:uiop #:external-program #:iterate)
  :components ((:file "package")
               (:file "processes" :depends-on ("package"))
               (:file "delta" :depends-on ("package"))))
