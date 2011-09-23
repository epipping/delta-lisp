;; -*- mode:common-lisp; indent-tabs-mode: nil -*-

(defpackage #:delta
  (:export #:delta-file)
  (:use #:cl))

(in-package #:delta)

;; TODO: parallelise run-on-input invocations

;; FIXME: splits by newline at this point already

(defvar *file-contents* (make-array 0 :adjustable t :fill-pointer 0))
(defvar *number-of-lines* 0)

(defun read-file (filename)
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil 'end)
          until (eq line 'end)
          do (vector-push-extend line *file-contents*)))
  (setf *number-of-lines* (length *file-contents*)))

(defun write-from-indices (indices stream)
  (loop for bit across indices
        for index from 0
        do (when (= 1 bit) (format stream "~a~%" (aref *file-contents* index)))))

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

(defun test-complements (parts input len)
  (loop for i from 0 below parts
        for begin = (compute-break len i parts) then end
        and end = (compute-break len (1+ i) parts)
        for length-removed = (- end begin)
        for old-offset = 0 then new-offset
        for new-offset = (loop for index from old-offset
                               with index-without-zeroes = begin
                               until (= index-without-zeroes end)
                               do (when (= 1 (aref input index))
                                    (incf index-without-zeroes))
                               finally (return index))
        for complement = (copy-seq input)
        do (fill complement 0 :start old-offset :end new-offset)
        do (when (run-on-indices complement)
             (format t "Reduced to ~a lines~%" (- len length-removed))
             (osicat-posix:rename "output" "output-minimal")
             (return (values complement (- len length-removed))))))

(defun delta (input)
  (labels ((ddmin (list parts old-length)
             (multiple-value-bind (complement new-length) (test-complements parts list old-length)
               (cond (complement (ddmin complement (max (1- parts) 2) new-length))
                     ;; check if increasing granularity makes sense
                     ((< parts old-length) (ddmin list (min old-length (* 2 parts)) old-length))
                     ;; done: found a 1-minimal subset
                     (t list)))))
    (if (run-on-indices input)
        (format t "Starting with ~a lines~%" *number-of-lines*)
        (error "Initial input does not satisfy the predicate"))
    (ddmin input 2 *number-of-lines*)))

(defun delta-file (filename)
  (read-file filename)
  (indices->file (delta (make-array *number-of-lines* :element-type 'bit
                                                      :initial-element 1))))