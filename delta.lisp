;; -*- mode:common-lisp; indent-tabs-mode: nil -*-

(defpackage #:delta
  (:export #:delta-file)
  (:use #:cl)
  (:use #:iterate))

(in-package #:delta)

(defvar *max-processes*)
(defvar *script-name*)
(defvar *minimal-output-name* "output-minimal")
(defvar *file-contents*)
(defvar *number-of-lines* 0)
(defvar *extension*)

(defconstant *kill-signal* 15)
(defconstant *sleep-between-checks* 1e-4)

(defun read-file (filename)
  "Split the file given by `filename` by newline and append the lines
as strings to the array `*file-contents*`."
  (setf *file-contents* (make-array 0 :adjustable t :fill-pointer 0))
  (iter (for line in-file filename :using #'read-line)
        (after-each (vector-push-extend line *file-contents*)
                    (incf *number-of-lines*))))

(defun write-from-indices (indices stream)
  "Write the subset of `*file-contents*` represented by the index list
`indices` to the stream `stream`."
  (iter (for index in indices)
        (after-each (format stream "~a~%" (aref *file-contents* index)))))

(defun indices->file (indices filename)
  "Write the subset of `*file-contents*` represented by the index list
  `indices` to the file \"output\"."
  (with-open-file (stream filename
                          :direction :output
                          :if-exists :supersede)
    (write-from-indices indices stream)))

(defun compute-break (length part numparts)
  "Compute mark at which chunk #`part` begins when a list of length
`length` is divided into `numparts`-many chunks of (roughly) equal
size."
  (floor (* part length) numparts))

(defun exclude-range (first last list)
  "Remove range of indices from `first` to `last` from list
`list`. The range is taken is left-inclusive and right-exclusive."
  (cond
    ((plusp first)
     (cons (car list) (exclude-range (1- first) (1- last) (cdr list))))
    ((plusp last)
     (exclude-range first (1- last) (cdr list)))
    (t list)))

(defclass reduction ()
    ((part :initarg :part :initform -1)
     (complement :initarg :complement :initform nil)))

(defclass process-with-result ()
    ((process :initarg :process)
     (result :initarg :result)))

(defun test-removal (indices numparts initial-part final-part)
  "Check if removing certain subsets of `indices` yields a reduction.

The parameter `indices` should be a subset of `*file-contents*`,
represented through a list of indices. The subset will be divided into
`numparts`-many chunks of (roughly) equal size; for each chunk, its
complement with respect to the subset will be tested. The first one
that makes `*script-name*` pass will be returned.

If no chunk passes, nil is returned."
  (iter reducing
        (with pwr-list)
        (with part = initial-part)
        (after-each
         ;; Fill process list
         (iter (until (or (>= part final-part)
                          (>= (length pwr-list) *max-processes*)))
               (after-each (push (test-removal-helper indices numparts part)
                                 pwr-list)
                           (incf part)))
         ;; Check if a process has terminated
         (iter (for pwr in pwr-list)
               (for process = (slot-value pwr 'process))
               (for status-and-return = (inspect-process process))
               (for status = (slot-value status-and-return 'status))
               (for return-value = (slot-value status-and-return
                                               'return-value))
               (after-each
                (when (eq status :exited)
                  (cond
                    ;; Successful exit: Kill everyone and return
                    ((= return-value 0)
                     (iter (for p in pwr-list)
                           (after-each (external-program:signal-process
                                        (slot-value p 'process)
                                        *kill-signal*)))
                     (let* ((reduction (slot-value pwr 'result))
                            (complement (slot-value reduction 'complement)))
                       (format t "Lines: ~a. Segments: ~a.~%"
                               (length complement) (1- numparts))
                       (indices->file complement *minimal-output-name*)
                       (return-from reducing reduction)))
                    ;; Otherwise: Treat the reduction as a failure
                    (t (setf pwr-list (remove pwr pwr-list)))))))
         ;; Return a dummy to signal overall reduction failure.
         (when (and (>= part final-part)
                    (zerop (length pwr-list)))
           (return-from reducing (make-instance 'reduction)))
         ;; Sleep
         (sleep *sleep-between-checks*))))

(defclass status-and-return ()
    ((status :initarg :status)
     (return-value :initarg :return-value)))

(defun inspect-process (process)
  (multiple-value-bind (status return-value)
      (external-program:process-status process)
    (make-instance 'status-and-return
                   :status status
                   :return-value return-value)))

(defun run-on-subset (indices)
    (uiop/stream:with-temporary-file (:pathname p
                                      :prefix "delta"
                                      :direction :output
                                      :keep t
                                      :element-type 'character
                                      :type *extension*)
      (indices->file indices p)
      (external-program:start *script-name*
                              (list (namestring p)))))

(defun test-removal-helper (indices numparts part)
  (let* ((begin (compute-break (length indices) part numparts))
         (end (compute-break (length indices) (1+ part) numparts))
         (complement (exclude-range begin end indices)))
    (make-instance 'process-with-result
                   :process (run-on-subset complement)
                   :result (make-instance 'reduction
                                          :part part
                                          :complement complement))))

(defun ddmin (indices numparts &key (initial-part 0) (final-part numparts))
  (let* ((reduction (test-removal indices numparts initial-part final-part))
         (passing-subset (slot-value reduction 'complement))
         (removed-part (slot-value reduction 'part))
         (subset-length (length passing-subset))
         (numindices (length indices)))
    (cond
      ;; Keep granularity. Try subsequent complements
      ((> subset-length 1)
       (ddmin passing-subset (max (1- numparts) 2)
              :initial-part removed-part :final-part (1- numparts)))
      ;; Keep granularity. Try preceding complements
      ((plusp initial-part)
       (ddmin indices numparts :final-part initial-part))
      ;; Increase granularity.
      ((and (zerop subset-length) (< numparts numindices))
       (let ((new-numparts (min numindices (* 2 numparts))))
         (format t "Lines: ~a. Segments: ~a (granularity increased).~%"
                 numindices new-numparts)
         (ddmin indices new-numparts)))
      ;; Done: Cannot partition single-line input
      ((= subset-length 1) passing-subset)
      ;; Done: Unable to remove any subset of size 1.
      (t indices))))

(defun delta (indices)
  "Minimise a subset of `*file-contents*` represented by the index
list `indices` under the constraint that `*script-name*` returns 0
when a file consisting of that subset is passed as its sole argument."
  (format t "Lines: ~a. Segments: ~a.~%"
          (length indices) 1)
  (iter
   (with process = (run-on-subset indices))
   (for status-and-return = (inspect-process process))
   (for status = (slot-value status-and-return 'status))
   (for return-value = (slot-value status-and-return 'return-value))
   (after-each (cond
                 ((and (eq status :exited) (eq return-value 0))
                  (leave))
                 ((eq status :exited)
                  (error "Initial input does not satisfy the predicate"))
                 (t (sleep *sleep-between-checks*)))))
  (format t "Lines: ~a. Segments: ~a (granularity increased).~%"
          (length indices) 2)
  (ddmin indices 2))

(defun delta-file (filename script-name
                   &key
                     (extension (multiple-value-bind (name type)
                                    (uiop/pathname:split-name-type
                                     (file-namestring filename))
                                  type))
                     (processes 1))
  "Minimise the file given by `filename` under the constraint that
`script-name` should continue to return 0 when passed the name of the
resulting file as its sole argument.

If `filename` can be reduced, a file will be created by the name
`*minimal-output-name*`. The solution will not in general be a global
minimum. It will satisfy the condition of 1-minimility, i.e. that no
different solution can be found by removing a single line."
  (read-file filename)
  (setf *max-processes* processes)
  (setf *script-name* script-name)
  (setf *extension* extension)
  (delta (iter (for index from 0 below *number-of-lines*)
               (collect index))))
