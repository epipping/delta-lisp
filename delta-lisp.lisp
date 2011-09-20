;; -*- mode:common-lisp; indent-tabs-mode: nil -*-

(defpackage #:delta-lisp
  (:export #:delta-file)
  (:use #:cl #:external-program))

(in-package #:delta-lisp)

;; TODO: parallelise run-on-input invocations

;; FIXME: verify that initial version passes.

;; FIXME: splits by newline at this point already

(defvar *seen-args* nil)

;; Debugging
(defvar *unique-calls* 0)
(defvar *duplicate-calls* 0)

(defun file->annotated-strings (filename)
  (let (buf)
    (with-open-file (stream filename)
      (loop for line = (read-line stream nil 'end)
            for i = 0 then (1+ i)
            until (eq line 'end)
            do (push (cons line i) buf)))
    (reverse buf)))

(defun annotated-strings->string (list)
  (format nil "~{~a~%~}" (map 'list #'car list)))

(defun annotated-strings->file (input)
  (with-open-file (stream "output" :direction :output :if-exists :supersede)
    (format stream "~a" (annotated-strings->string input))))

(defun extract-indices (input)
  (map 'list #'cdr input))

(defun run-on-input (input)
  (let ((original-indices (extract-indices input)))
    (or (and (member original-indices *seen-args* :test #'equal)
             (incf *duplicate-calls*)
             nil)
        (progn
          (incf *unique-calls*)
          (push original-indices *seen-args*)
          (annotated-strings->file input)
          (multiple-value-bind (status return-code) (external-program:run "./test.sh" '("output"))
            (declare (ignore status))
            (= 0 return-code))))))

(defun compute-breaks (length parts)
  (do ((i 0 (1+ i))
       (breaks nil (cons (floor (* i (/ length parts))) breaks)))
      ((>= i parts) (reverse breaks))))

(defun test-complements (list-of-subsets input)
  (when list-of-subsets
    (loop for los = list-of-subsets then (cdr los)
          while los
          for begin = (first los)
          for end = (second los)
          for complement = (append (subseq input 0 begin)
                                   (and end (subseq input end)))
          do (when (run-on-input complement)
               (format t "Success with ~a lines~%" (length complement))
               (return complement)))))

(defun delta (input)
  (labels ((ddmin (list parts check-subsets)
             (let ((breaks (compute-breaks (length list) parts)))
               (or
                ;; check if the complement of a subset fails
                (let ((complement (test-complements breaks list)))
                  (and complement (ddmin complement (max (1- parts) 2) nil)))

                ;; check if increasing granularity makes sense
                (and (< parts (length list)) (ddmin list (min (length list) (* 2 parts)) t))

                ;; done: found a 1-minimal subset
                list))))
    (ddmin input 2 t)))

(defun delta-file (filename)
  (annotated-strings->file (delta (file->annotated-strings filename)))
  (format t "unique: ~a, duplicate: ~a~%" *unique-calls* *duplicate-calls*))
