;; -*- mode:common-lisp; indent-tabs-mode: nil -*-

(in-package #:delta)

(alexandria:define-constant +term-signal+ 15 :test #'=)

(defclass status-and-return ()
    ((status :initarg :status)
     (return-value :initarg :return-value)))

(defun terminate-process (process)
  #-ccl (external-program:signal-process process +term-signal+)
  ;; CCL treats signals sent to dead processes as errors by default.
  ;; Checking if a processes is alive and killing it conditionally
  ;; creates a race condition. This is addressed through :error-if-exited
  ;; See also http://trac.clozure.com/ccl/ticket/1015
  ;; But external-program does not support it yet (2016/07/20)
  ;; See also https://github.com/sellout/external-program/issues/32
  #+ccl (ccl:signal-external-process process +term-signal+
                                     :error-if-exited nil))

(defun inspect-process (process)
  (let+ (((&values status return-value)
          (external-program:process-status process)))
        (make-instance 'status-and-return
                       :status status
                       :return-value return-value)))

(defun wait-for-process (process)
  ;; Functionality missing from external-program (2016/07/20)
  ;; See also https://github.com/sellout/external-program/issues/30
  ;; ECL's ext:external-process-wait looks broken (2016/07/30)
  ;; See also https://gitlab.com/embeddable-common-lisp/ecl/issues/268
  #+clozure (ccl::external-process-wait process)
  #+(or cmu scl) (ext:process-wait process)
  #+sbcl (sb-ext:process-wait process))
