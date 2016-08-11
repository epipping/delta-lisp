(load "mk/path.lisp")

;; Required by ECL, see e.g.
;; https://mailman.common-lisp.net/pipermail/ecl-devel/2016-June/011120.html
(asdf:register-immutable-system :uiop)

(asdf:operate 'asdf:program-op :delta-standalone)

(quit)
