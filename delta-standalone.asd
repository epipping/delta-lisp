(asdf:defsystem #:delta-standalone
  :serial t
  :depends-on (#:alexandria #:delta #:getopt)
  :components ((:file "delta-standalone.package")
               (:file "delta-standalone" :depends-on ("delta-standalone.package")))
  :entry-point "delta-standalone:main"
  :build-operation program-op
  :build-pathname "delta-standalone")
