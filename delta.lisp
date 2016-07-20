;; -*- mode:common-lisp; indent-tabs-mode: nil -*-

(defpackage #:delta
  (:export #:delta-file)
  (:use #:cl))

(in-package #:delta)

;; TODO: parallelise subset-passed invocations

(defvar *script-name*)
(defvar *output-name* "output")
(defvar *minimal-output-name* "output-minimal")
(defvar *file-contents*)
(defvar *number-of-lines*)

(defun read-file (filename)
  "Split the file given by `filename` by newline and append the lines
as strings to the array `*file-contents*`."
  (setf *file-contents* (make-array 0 :adjustable t :fill-pointer 0))
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil 'end)
       until (eq line 'end)
       do (vector-push-extend line *file-contents*)))
  (setf *number-of-lines* (length *file-contents*)))

(defun write-from-indices (indices stream)
  "Write the subset of `*file-contents*` represented by the index list
`indices` to the stream `stream`."
  (loop for index in indices
     do (format stream "~a~%" (aref *file-contents* index))))

(defun indices->file (indices)
  "Write the subset of `*file-contents*` represented by the index list
  `indices` to the file \"output\"."
  (with-open-file (stream *output-name*
                          :direction :output
                          :if-exists :supersede)
    (write-from-indices indices stream)))

(defun subset-passed (indices)
  "Run the script `*script-name*` on the subset of `*file-contents*`
represented by the index list `indices`."
  (indices->file indices)
  (null (handler-case
            (uiop:run-program (list *script-name* *output-name*))
          (uiop/run-program:subprocess-error () 'script-failed))))

(defun compute-break (length part numparts)
  "Compute mark at which chunk #`part` begins when a list of length
`length` is divided into `numparts`-many chunks of (roughly) equal
size."
  (floor (* part (/ length numparts))))

(defun exclude-range (first last list)
  "Remove range of indices from `first` to `last` from list
`list`. The range is taken is left-inclusive and right-exclusive."
  (cond
    ((plusp first)
     (cons (car list) (exclude-range (1- first) (1- last) (cdr list))))
    ((plusp last)
     (exclude-range first (1- last) (cdr list)))
    (t list)))

(defun test-removal (indices numparts)
  "Check if removing certain subsets of `indices` yields a reduction.

The parameter `indices` should be a subset of `*file-contents*`,
represented through a list of indices. The subset will be divided into
`numparts`-many chunks of (roughly) equal size; for each chunk, its
complement with respect to the subset will be tested. The first one
that makes `*script-name*` pass will be returned.

If no chunk passes, nil is returned."
  (loop for i from 0 below numparts
     ;; Relative to the subset, where chunk #i begins/ends
     for begin = (compute-break (length indices) i numparts) then end
     and end = (compute-break (length indices) (1+ i) numparts)
     ;; Remove a chunk
     for complement = (exclude-range begin end indices)
     do (when (subset-passed complement)
          (format t "Reduced to ~a lines.~%" (length complement))
          (uiop:rename-file-overwriting-target *output-name* *minimal-output-name*)
          (return complement))))

(defun ddmin (indices old-numparts)
  (let ((passing-subset (test-removal indices old-numparts)))
    (cond
      (passing-subset
       (ddmin passing-subset (max (1- old-numparts) 2)))
      ;; check if increasing granularity makes sense
      ((< old-numparts (length indices))
       (let ((new-numparts (min (length indices) (* 2 old-numparts))))
         (format t "Increasing granularity. Number of segments now: ~a.~%"
                 new-numparts)
         (ddmin indices new-numparts)))
      ;; done: found a 1-minimal subset
      (t indices))))

(defun delta (indices)
  "Minimise a subset of `*file-contents*` represented by the index
list `indices` under the constraint that `*script-name*` returns 0
when a file consisting of that subset is passed as its sole argument."
  (format t "Starting with ~a lines~%" *number-of-lines*)
  (unless (subset-passed indices)
    (error "Initial input does not satisfy the predicate"))
  (ddmin indices 2))

(defun delta-file (filename script-name)
  "Minimise the file given by `filename` under the constraint that
`script-name` should continue to return 0 when passed the name of the
resulting file as its sole argument.

If `filename` can be reduced, a file will be created by the name
`*minimal-output-name*`. The solution will not in general be a global
minimum. It will satisfy the condition of 1-minimility, i.e. that no
different solution can be found by removing a single line."
  (read-file filename)
  (setf *script-name* script-name)
  (let ((minimal-subset (delta
                         (loop for index from 0 below *number-of-lines*
                            collect index))))
    (indices->file minimal-subset)))
