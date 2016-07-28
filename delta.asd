(asdf:defsystem #:delta
  :serial t
  :depends-on (#:external-program #:iterate #:let-plus #:uiop)
  :components ((:file "delta.package")
               (:file "processes" :depends-on ("delta.package"))
               (:file "utilities" :depends-on ("delta.package"))
               (:file "delta" :depends-on ("delta.package"))))
