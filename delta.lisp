;; -*- mode:common-lisp; indent-tabs-mode: nil -*-

(defpackage #:delta
  (:export #:delta-file)
  (:use #:cl))

(in-package #:delta)

;; TODO: parallelise run-on-input invocations

;; FIXME: splits by newline at this point already

(defun file->strings (filename)
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil 'end)
          until (eq line 'end)
          collect line)))

(defun write-strings (list stream)
  (format stream "~{~a~%~}" list))

(defun strings->file (input)
  (with-open-file (stream "output" :direction :output :if-exists :supersede)
    (write-strings input stream)))

(defun run-on-input (input)
  (strings->file input)
  (multiple-value-bind (status return-code) (external-program:run "./test.sh" '("output"))
    (declare (ignore status))
    (= 0 return-code)))

(defun compute-break (length part parts)
  (floor (* part (/ length parts))))

(defun test-complements (parts input)
  (let ((len (length input)))
    (loop for i from 0 below parts
          for breaks = (cons (compute-break len 0 parts)
                             (compute-break len 1 parts))
            then (cons (cdr breaks) (compute-break len (1+ i) parts))
          for complement = (append (subseq input 0 (car breaks))
                                   (subseq input (cdr breaks)))
          do (when (run-on-input complement)
               (format t "Reduced to ~a lines~%" (length complement))
               (osicat-posix:rename "output" "output-minimal")
               (return complement)))))

(defun delta (input)
  (if (run-on-input input)
      (format t "Starting with ~a lines~%" (length input))
      (error "Initial input does not satisfy the predicate"))
  (labels ((ddmin (list parts check-subsets)
             (let ((complement (test-complements parts list)))
               (cond (complement (ddmin complement (max (1- parts) 2) nil))
                     ;; check if increasing granularity makes sense
                     ((< parts (length list)) (ddmin list (min (length list) (* 2 parts)) t))
                     ;; done: found a 1-minimal subset
                     (t list)))))
    (ddmin input 2 t)))

(defun delta-file (filename)
  (strings->file (delta (file->strings filename))))
