;; -*- mode:common-lisp; indent-tabs-mode: nil -*-

(in-package #:delta)

(alexandria:define-constant +kill-signal+ 9 :test #'=)
(alexandria:define-constant +term-signal+ 15 :test #'=)

;;; This functionality is currently missing from UIOP.

(defun terminate-process (process-info &key force)
  (let ((process (getf process-info :process))
        (sig (if force +kill-signal+ +term-signal+)))
    #+allegro (progn ; FIXME: untested
                #+os-unix (excl.osi:kill process sig)
                #+os-windows (uiop/run-program::%run-program
                              (format nil "taskkill /f /pid ~a" process)
                              :wait t)
                #-(or os-unix os-windows) (error "Cannot terminate a process.")
                (sys:reap-os-subprocess :pid process))
    #+clozure (ccl:signal-external-process process sig :error-if-exited nil)
    #+cmu (ext:process-kill process sig)
    #+sbcl (sb-ext:process-kill process sig)
    #+scl (ext:process-kill process sig) ; FIXME: untested
    #+mkcl (mk-ext:terminate-process process :force force)
    #-(or allegro clozure cmu mkcl sbcl) (error "Cannot terminate a process.")))

(defun process-running-p (process-info)
  (let ((process (getf process-info :process)))
    #+clozure (eq :running (ccl:external-process-status process))
    #+cmu (eq :running (ext:process-status process))
    #+ecl (eq :running (ext:external-process-status process))
    #+mkcl (eq :running (mk-ext:process-status process))
    #+sbcl (eq :running (sb-ext:process-status process))
    #-(or clozure cmu ecl mkcl sbcl)
    (error "Cannot determine if a process is running.")))
