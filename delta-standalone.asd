(asdf:defsystem #:delta-standalone
  :serial t
  :depends-on (#:delta :getopt)
  :components ((:file "main"))
  :entry-point "delta-standalone:main")
