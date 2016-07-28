(push (uiop:ensure-absolute-pathname *default-pathname-defaults*)
      asdf:*central-registry*)'

(asdf:disable-output-translations)

(ql:quickload "delta")
(ql:quickload "delta-standalone")
(ql:quickload "delta-tests")

