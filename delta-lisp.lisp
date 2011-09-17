;; -*- mode:common-lisp; indent-tabs-mode: nil -*-

;; FIXME: Do caching
;; FIXME: verify that initial version passes.

;; FIXME: splits by newline at this point already
(defun read-input (filename)
  (let (buf)
    (with-open-file (stream filename)
      (loop for line = (read-line stream nil 'end)
            until (eq line 'end)
            do (push line buf)))
    (reverse buf)))

(defun add-newlines (list-of-strings)
  (format nil "~{~a~%~}" list-of-strings))

(defun run-on-input (input)
  ;; Create temporary file
  (with-open-file (stream "output" :direction :output :if-exists :supersede)
    (format stream (add-newlines input)))
  ;; Run test program on the temporary file
  (= 0 (sb-ext:process-exit-code (sb-ext:run-program "./test.sh" '("output")))))

(defun compute-breaks (length parts)
  (do ((i 0 (1+ i))
       (breaks nil (cons (floor (* i (/ length parts))) breaks)))
      ((>= i parts) (reverse breaks))))

;; FIXME: these two functions share a lot of code
(defun test-subsets (list-of-subsets input)
  (loop for los = list-of-subsets then (cdr los)
        while los
        for begin = (first los)
        for end = (second los)
        for subset = (or (and end (subseq input begin end))
                         (subseq input begin))
        do (when (run-on-input subset)
             (return subset))))

(defun test-complements (list-of-subsets input)
  (when (cddr list-of-subsets) ; Minor optimization as long as there is no caching
    (loop for los = list-of-subsets then (cdr los)
          while los
          for begin = (first los)
          for end = (second los)
          for complement = (append (subseq input 0 begin)
                                   (and end (subseq input end)))
          do (when (run-on-input complement)
               (return complement)))))

(defun delta (input)
  (labels ((ddmin (list parts)
             (let ((breaks (compute-breaks (length list) parts)))
               (or
                ;; check if a subset fails
                (let ((subset (test-subsets breaks list)))
                  (and subset (progn (format t "Subset: ~{~a~^, ~}~%" subset) ; Debugging
                                     (ddmin subset 2))))

                ;; check if the complement of a subset fails
                (let ((complement (test-complements breaks list)))
                  (and complement (progn (format t "Complement with ~a chunks: ~{~a~^, ~}~%" (max (1- parts) 2) complement) ; Debugging
                                         (ddmin complement (max (1- parts) 2)))))

                ;; check if increasing granularity makes sense
                (and (< parts (length list)) (ddmin list (min (length list) (* 2 parts))))

                ;; done: found a 1-minimal subset
                list))))
    (ddmin input 2)))

(defun delta-file (filename)
  (delta (read-input filename)))