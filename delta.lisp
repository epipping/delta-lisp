;; -*- mode:common-lisp; indent-tabs-mode: nil -*-

(defpackage #:delta
  (:export #:delta-file)
  (:use #:cl))

(in-package #:delta)

;; TODO: parallelise run-on-input invocations

;; FIXME: splits by newline at this point already

(defvar *file-contents* (make-array 1 :adjustable t :fill-pointer 0))

(defun read-file (filename)
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil 'end)
          until (eq line 'end)
          do (vector-push-extend line *file-contents*))))

(defun write-from-indices (indices stream)
  (loop for i in indices
        do (format stream "~a~%" (aref *file-contents* i))))

(defun indices->file (indices)
  (with-open-file (stream "output" :direction :output :if-exists :supersede)
    (write-from-indices indices stream)))

(defun run-on-indices (indices)
  (indices->file indices)
  (multiple-value-bind (status return-code) (external-program:run "./test.sh" '("output"))
    (declare (ignore status))
    (= 0 return-code)))

(defun compute-break (length part parts)
  (floor (* part (/ length parts))))

(defun test-complements (parts input)
  (let ((len (length input)))
    (loop for i from 0 below parts
          for begin = (compute-break len i parts) then end
          and end = (compute-break len (1+ i) parts)
          for complement = (append (subseq input 0 begin)
                                   (subseq input end))
          do (when (run-on-indices complement)
               (format t "Reduced to ~a lines~%" (length complement))
               (osicat-posix:rename "output" "output-minimal")
               (return complement)))))

(defun delta (input)
  (labels ((ddmin (list parts)
             (let ((complement (test-complements parts list)))
               (cond (complement (ddmin complement (max (1- parts) 2)))
                     ;; check if increasing granularity makes sense
                     ((< parts (length list)) (ddmin list (min (length list) (* 2 parts))))
                     ;; done: found a 1-minimal subset
                     (t list)))))
    (if (run-on-indices input)
        (format t "Starting with ~a lines~%" (length input))
        (error "Initial input does not satisfy the predicate"))
    (ddmin input 2)))

(defun delta-file (filename)
  (read-file filename)
  (indices->file (delta (loop for i from 0 below (length *file-contents*)
                              collect i))))
