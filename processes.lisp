;; -*- mode:common-lisp; indent-tabs-mode: nil -*-

(in-package #:delta)

(defconstant *kill-signal* 15)

(defclass status-and-return ()
    ((status :initarg :status)
     (return-value :initarg :return-value)))

(defun inspect-process (process)
  (multiple-value-bind (status return-value)
      (external-program:process-status process)
    (make-instance 'status-and-return
                   :status status
                   :return-value return-value)))

(defun wait-for-process (process)
  ;; Functionality missing from external-program (2016/07/20)
  ;; See also https://github.com/sellout/external-program/issues/30
  #+clozure (ccl::external-process-wait process)
  #+(or cmu scl) (ext:process-wait process)
  #+sbcl (sb-ext:process-wait process))
