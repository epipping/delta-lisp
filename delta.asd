(asdf:defsystem #:delta
  :serial t
  :depends-on (#:alexandria #:let-plus)
  :components ((:file "delta.package")
               (:file "utilities" :depends-on ("delta.package"))
               (:file "delta" :depends-on ("delta.package"))))
