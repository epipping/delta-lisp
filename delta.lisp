;; -*- mode:common-lisp; indent-tabs-mode: nil -*-

(in-package #:delta)

(defvar *max-processes*)
(defvar *script-name*)
(defvar *minimal-output-name* "output-minimal")
(defvar *file-contents*)
(defvar *suffix*)
(defvar *quiet*)
(defvar *show-stdout*)
(defvar *show-stderr*)

(alexandria:define-constant +sleep-between-checks+ 1e-4 :test #'=)

(defun read-file (filename)
  "Split the file given by `filename` by newline and append the lines
as strings to the array `*file-contents*`."
  (setf *file-contents* (coerce (uiop:read-file-lines filename) 'vector)))

(defun write-from-indices (indices stream)
  "Write the subset of `*file-contents*` represented by the index list
`indices` to the stream `stream`."
  (loop
     :for index :in indices
     :for line = (aref *file-contents* index)
     :do (format stream "~a~%" line)))

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

(defun terminate-process-cleanly (process)
  (uiop:terminate-process process)
  (uiop:wait-process process))

(defun test-removal (indices numparts initial-part)
  "Check if removing certain subsets of `indices` yields a reduction.

The parameter `indices` should be a subset of `*file-contents*`,
represented through a list of indices. The subset will be divided into
`numparts`-many chunks of (roughly) equal size; for each chunk, its
complement with respect to the subset will be tested. The first one
that makes `*script-name*` pass will be returned.

If no chunk passes, nil is returned."
  (loop
     :with pwr-list
     :with part = 0
     :do (loop ;; Fill process list
            :until (or (>= part numparts)
                       (>= (length pwr-list) *max-processes*))
            :do (push (test-removal-helper indices numparts
                                           :relative-part part
                                           :shift-by initial-part)
                      pwr-list)
            :do (incf part))
     :do (loop ;; Check if a process has terminated
            :for pwr :in pwr-list
            :for process = (slot-value pwr 'process)
            :unless (uiop:process-alive-p process)
            :do (cond
                  ;; Successful exit: Kill everyone and return
                  ((= 0 (uiop:wait-process process))
                   (loop
                      :for other-pwr :in (delete process pwr-list)
                      :for other-process = (slot-value other-pwr 'process)
                      :do (terminate-process-cleanly other-process))
                   (let+ (((&slots-r/o (reduction result)) pwr)
                          ((&slots-r/o complement) reduction))
                         (report-status (length complement) (1- numparts))
                         (indices->file complement *minimal-output-name*)
                         (return-from test-removal reduction)))
                  ;; Otherwise: Treat the reduction as a failure
                  (t (setf pwr-list (remove pwr pwr-list))))
            :and :do (uiop:close-streams process))
     ;; Return a dummy to signal overall reduction failure.
     :when (and (>= part numparts) (null pwr-list))
     :do (return-from test-removal (make-instance 'reduction))
     :do (sleep +sleep-between-checks+)))

(defun run-on-subset (indices)
  (uiop:with-temporary-file (:pathname p
                             :prefix "delta"
                             :direction :output
                             :keep t
                             :element-type 'character
                             :type *suffix*)
    (indices->file indices p)
    (uiop:launch-program (list *script-name* (namestring p))
                         :output (when *show-stdout* :interactive)
                         :error-output (when *show-stderr* :interactive))))

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
  (let* ((process (run-on-subset indices))
         (exit-code (uiop:wait-process process)))
    (unless (= 0 exit-code)
      (error "Initial input does not satisfy the predicate")))
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
  (delta (loop
            :for line :across *file-contents*
            :for index :from 0
            :collect index)))
