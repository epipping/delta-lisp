;; -*- mode:common-lisp; indent-tabs-mode: nil -*-

(defpackage #:delta-lisp
  (:export #:delta-file)
  (:use #:cl))

(in-package #:delta-lisp)

;; TODO: parallelise run-on-input invocations

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

(defun compute-break (length part parts)
  (floor (* part (/ length parts))))

(defun test-complements (parts input)
  (let ((len (length input)))
    (loop for i = 0 then (1+ i) until (>= i parts)
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
             (or
                ;; check if the complement of a subset fails
              (let ((complement (test-complements parts list)))
                (and complement (ddmin complement (max (1- parts) 2) nil)))

              ;; check if increasing granularity makes sense
              (and (< parts (length list)) (ddmin list (min (length list) (* 2 parts)) t))

              ;; done: found a 1-minimal subset
              list)))
    (ddmin input 2 t)))

(defun delta-file (filename)
  (annotated-strings->file (delta (file->annotated-strings filename)))
  (format t "unique: ~a, duplicate: ~a~%" *unique-calls* *duplicate-calls*))
