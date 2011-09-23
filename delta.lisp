;; -*- mode:common-lisp; indent-tabs-mode: nil -*-

(defpackage #:delta
  (:export #:delta-file)
  (:use #:cl))

(in-package #:delta)

;; TODO: parallelise run-on-input invocations

(defvar *file-contents* (make-array 0 :adjustable t :fill-pointer 0))
(defvar *number-of-lines* 0)

(defun read-file (filename)
  "Split the file given by `filename` by newline and append the lines
as strings to the array `*file-contents*`."
  (with-open-file (stream filename)
    ;; FIXME: splits by newline at this point already
    (loop for line = (read-line stream nil 'end)
          until (eq line 'end)
          do (vector-push-extend line *file-contents*)))
  (setf *number-of-lines* (length *file-contents*)))

(defun write-from-indices (indices stream)
  "Write the subset of `*file-contents*` represented by the
byte-vector `indices` to the stream `stream`."
  (loop for bit across indices for index from 0 do (when (= 1
        bit) (format stream "~a~%" (aref *file-contents* index)))))

(defun indices->file (indices)
  "Write the subset of `*file-contents*` represented by the
byte-vector `indices` to the file \"output\"."
  (with-open-file (stream "output" :direction :output :if-exists :supersede)
    (write-from-indices indices stream)))

(defun run-on-indices (indices script-name)
  "Run the script given by `script-name` on the subset of
`*file-contents*` represented by the byte-vector `indices`."
  (indices->file indices)
  (multiple-value-bind (status return-code) (external-program:run script-name '("output"))
    (declare (ignore status))
    (= 0 return-code)))

(defun compute-break (length part parts)
  "Compute mark at which chunk #`part` begins when a list of length
`length` is divided into `parts`-many chunks of (roughly) equal size."
  (floor (* part (/ length parts))))

(defun test-complements (parts input len script-name)
  "Check if removing certain subsets of `input` yields a reduction.

The parameter `input` should be a subset of `*file-contents*`,
represented through a bit-vector. The subset will be divided into
`parts`-many chunks of (roughly) equal size; for eac chunk, its
complement with respect to the subset will be tested. The first one
that makes `script-name` pass will be returned in conjunction with its
size.

If no chunk passes, nil is returned."
  (loop for i from 0 below parts
        ;; Relative to the subset, where chunk #i begins/ends
        for begin = (compute-break len i parts) then end
        and end = (compute-break len (1+ i) parts)
        for length-removed = (- end begin)
        ;; Relative to the whole bit-vector, where chunk #i begins/ends
        for old-offset = 0 then new-offset
        for new-offset = (loop for index from old-offset
                               with index-without-zeroes = begin
                               until (= index-without-zeroes end)
                               do (when (= 1 (aref input index))
                                    (incf index-without-zeroes))
                               finally (return index))
        ;; Create a copy of the bit-vector `input` and remove a chunk
        for complement = (copy-seq input)
        do (fill complement 0 :start old-offset :end new-offset)
        do (when (run-on-indices complement script-name)
             (format t "Reduced to ~a lines~%" (- len length-removed))
             (osicat-posix:rename "output" "output-minimal")
             (return (values complement (- len length-removed))))))

(defun delta (input script-name)
  "Minimise a subset of `*file-contents*` represented by the
bit-vector `input` under the constraint that `script-name` returns 0
when a file consisting of that subset is passed as its sole argument."
  (labels ((ddmin (list parts old-length)
             (multiple-value-bind (complement new-length) (test-complements parts list old-length script-name)
               (cond (complement (ddmin complement (max (1- parts) 2) new-length))
                     ;; check if increasing granularity makes sense
                     ((< parts old-length) (ddmin list (min old-length (* 2 parts)) old-length))
                     ;; done: found a 1-minimal subset
                     (t list)))))
    (if (run-on-indices input script-name)
        (format t "Starting with ~a lines~%" *number-of-lines*)
        (error "Initial input does not satisfy the predicate"))
    (ddmin input 2 *number-of-lines*)))

(defun delta-file (filename script-name)
  "Minimise the file given by `filename` under the constraint that
`script-name` should continue to return 0 when passed the name of the
resulting file as its sole argument.

If `filename` can be reduced, a file will be created by the name
\"output-minimal\". The solution will not in general be a global
minimum. It will satisfy the condition of 1-minimility, i.e. that no
different solution can be found by removing a single line."
  (read-file filename)
  (indices->file (delta (make-array *number-of-lines* :element-type 'bit
                                                      :initial-element 1)
                        script-name)))