;; -*- mode:common-lisp; indent-tabs-mode: nil -*-

(in-package #:delta)

(defvar *max-processes*)
(defvar *script-name*)
(defvar *minimal-output-name* "output-minimal")
(defvar *file-contents*)
(defvar *number-of-lines* 0)
(defvar *suffix*)
(defvar *quiet*)
(defvar *show-stdout*)
(defvar *show-stderr*)

(alexandria:define-constant +sleep-between-checks+ 1e-4 :test #'=)

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

(defclass reduction ()
    ((part :initarg :part :initform -1)
     (complement :initarg :complement :initform nil)))

(defclass process-with-result ()
    ((process :initarg :process)
     (result :initarg :result)))

(defun report-status (lines segments &key (granularity-increased nil))
  (unless *quiet*
    (format t (if granularity-increased
                  "Lines: ~a. Segments: ~a (granularity increased).~%"
                  "Lines: ~a. Segments: ~a.~%")
            lines segments)))

(defun test-removal (indices numparts initial-part)
  "Check if removing certain subsets of `indices` yields a reduction.

The parameter `indices` should be a subset of `*file-contents*`,
represented through a list of indices. The subset will be divided into
`numparts`-many chunks of (roughly) equal size; for each chunk, its
complement with respect to the subset will be tested. The first one
that makes `*script-name*` pass will be returned.

If no chunk passes, nil is returned."
  (iter reducing
        (with pwr-list)
        (with part = 0)
        (after-each
         ;; Fill process list
         (iter (until (or (>= part numparts)
                          (>= (length pwr-list) *max-processes*)))
               (after-each (push (test-removal-helper indices numparts
                                                      :relative-part part
                                                      :shift-by initial-part)
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
                     (iter (for p in (delete process pwr-list))
                           (after-each (terminate-process (slot-value p 'process))))
                     (let+ (((&slots-r/o (reduction result)) pwr)
                            ((&slots-r/o complement) reduction))
                           (report-status (length complement) (1- numparts))
                           (indices->file complement *minimal-output-name*)
                           (return-from reducing reduction)))
                    ;; Otherwise: Treat the reduction as a failure
                    (t (setf pwr-list (remove pwr pwr-list)))))))
         ;; Return a dummy to signal overall reduction failure.
         (when (and (>= part numparts)
                    (zerop (length pwr-list)))
           (return-from reducing (make-instance 'reduction)))
         ;; Sleep
         (sleep +sleep-between-checks+))))

(defun run-on-subset (indices)
  (uiop:with-temporary-file (:pathname p
                             :prefix "delta"
                             :direction :output
                             :keep t
                             :element-type 'character
                             :type *suffix*)
    (indices->file indices p)
    (external-program:start *script-name* (list (namestring p))
                            :output *show-stdout*
                            :error *show-stderr*)))

(defun test-removal-helper (indices numparts &key relative-part shift-by)
  (let* ((part (shift-and-wrap relative-part shift-by numparts))
         (begin (compute-break (length indices) part numparts))
         (end (compute-break (length indices) (1+ part) numparts))
         (complement (exclude-range begin end indices)))
    (make-instance 'process-with-result
                   :process (run-on-subset complement)
                   :result (make-instance 'reduction
                                          :part part
                                          :complement complement))))

(defun ddmin (indices numparts &key (initial-part 0))
  (let+ (((&slots-r/o (passing-subset complement)
                      (removed-part part))
          (test-removal indices numparts initial-part))
         (subset-length (length passing-subset))
         (numindices (length indices)))
    (cond
      ;; Keep granularity. Try subsequent complements (wraps around)
      ((> subset-length 1)
       (ddmin passing-subset (max (1- numparts) 2)
              :initial-part removed-part))
      ;; Increase granularity.
      ((and (zerop subset-length) (< numparts numindices))
       (let ((new-numparts (min numindices (* 2 numparts))))
         (report-status numindices new-numparts :granularity-increased T)
         (ddmin indices new-numparts)))
      ;; Done: Cannot partition single-line input
      ((= subset-length 1) passing-subset)
      ;; Done: Unable to remove any subset of size 1.
      (t indices))))

(defun delta (indices)
  "Minimise a subset of `*file-contents*` represented by the index
list `indices` under the constraint that `*script-name*` returns 0
when a file consisting of that subset is passed as its sole argument."
  (report-status (length indices) 1)
  (let ((process (run-on-subset indices)))
    (wait-for-process process)
    (let+ ((status-and-return (inspect-process process))
           ((&slots-r/o return-value) status-and-return))
      (unless (eq return-value 0)
        (error "Initial input does not satisfy the predicate"))))
  (report-status (length indices) 2 :granularity-increased T)
  (ddmin indices 2))

(defun delta-file (script-name filename
                   &key
                     (suffix (nth-value 1 (uiop:split-name-type
                                           (file-namestring filename))))
                     (processes 1)
                     quiet
                     show-stdout
                     show-stderr)
  "Minimise the file given by `filename` under the constraint that
`script-name` should continue to return 0 when passed the name of the
resulting file as its sole argument.

If `filename` can be reduced, a file will be created by the name
`*minimal-output-name*`. The solution will not in general be a global
minimum. It will satisfy the condition of 1-minimility, i.e. that no
different solution can be found by removing a single line."
  (read-file filename)
  (setf *quiet* quiet)
  (setf *show-stdout* show-stdout)
  (setf *show-stderr* show-stderr)
  (setf *max-processes* processes)
  (setf *script-name* script-name)
  (setf *suffix* suffix)
  (delta (iter (for index from 0 below *number-of-lines*)
               (collect index))))
