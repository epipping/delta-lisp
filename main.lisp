;; -*- mode:common-lisp; indent-tabs-mode: nil -*-

(defpackage #:delta-standalone
  (:export #:main)
  (:use #:cl)
  (:use #:let-plus))

(in-package #:delta-standalone)

(defun print-usage (name)
  (format nil "Usage: ~a [--processes=n] [--suffix=suffix] test input" name))

(defun main ()
  (let+ ((argv (uiop:command-line-arguments))
         ((&values files options unhandled-options)
                         (getopt:getopt argv '(("suffix" :optional)
                                               ("processes" :optional)))))
        (unless (= (length files) 2)
          (error (print-usage "delta")))
        (unless (null unhandled-options)
          (error (format nil "Unhandled option: ~a" (first unhandled-options))))
        (let ((test (first files))
              (test-input (second files))
              (processes (cdr (assoc "processes" options :test #'string=)))
              (suffix (cdr (assoc "suffix" options :test #'string=))))
          (apply 'delta:delta-file
                 `(,test ,test-input
                         ,@(when processes `(:processes ,(parse-integer processes)))
                         ,@(when suffix `(:suffix ,suffix)))))))
