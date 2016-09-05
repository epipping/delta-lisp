(asdf:defsystem #:delta
  :serial t
  :depends-on (#:alexandria #:iterate #:let-plus #:uiop)
  :components ((:file "delta.package")
               (:file "utilities" :depends-on ("delta.package"))
               (:file "delta" :depends-on ("delta.package"))))
