;; -*- mode:common-lisp; indent-tabs-mode: nil -*-

;; TODO: parallise run-on-input invocations

;; FIXME: Do caching
;; FIXME: verify that initial version passes.

;; FIXME: splits by newline at this point already
(defun read-input (filename)
  (let (buf)
    (with-open-file (stream filename)
      (loop for line = (read-line stream nil 'end)
            for i = 0 then (1+ i)
            until (eq line 'end)
            do (push (cons line i) buf)))
    (reverse buf)))

;; (defvar *seen-args* nil)
;; (defvar *unique-calls* 0)
;; (defvar *duplicate-calls* 0)

(defun annotated-strings->string (list)
  (format nil "~{~a~%~}" (map 'list #'car list)))

(defun write-input (input)
  (with-open-file (stream "output" :direction :output :if-exists :supersede)
    (format stream (annotated-strings->string input))))

(defun run-on-input (input)
  (write-input input)
;  (format t "Calling program with args: ~a~%" input)
;  (if (member input *seen-args* :test #'equal)
;      (progn
;        (incf *duplicate-calls*)
;        (print "DUPL"))
;      (progn (incf *unique-calls*)
;             (push input *seen-args*)))
  (= 0 (sb-ext:process-exit-code (sb-ext:run-program "./test.sh" '("output")))))

(defun compute-breaks (length parts)
  (do ((i 0 (1+ i))
       (breaks nil (cons (floor (* i (/ length parts))) breaks)))
      ((>= i parts) (reverse breaks))))

;; FIXME: these two functions share a lot of code
(defun test-subsets (list-of-subsets input)
  (loop for los = list-of-subsets then (cdr los)
        while los
        for subset = (apply #'subseq input (list (first los) (second los)))
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
  (labels ((ddmin (list parts check-subsets)
             (let ((breaks (compute-breaks (length list) parts)))
               (or
                ;; check if a subset fails
                (and check-subsets
                     (let ((subset (test-subsets breaks list)))
                       (and subset (progn (format t "Subset: ~{~a~^, ~}~%" (map 'list #'car subset)) ; Debugging
                                          (ddmin subset 2 t)))))

                ;; check if the complement of a subset fails
                (let ((complement (test-complements breaks list)))
                  (and complement (progn (format t "Complement with ~a chunks: ~{~a~^, ~}~%"
                                                 (max (1- parts) 2) (map 'list #'car complement)) ; Debugging
                                         (ddmin complement (max (1- parts) 2) nil))))

                ;; check if increasing granularity makes sense
                (and (< parts (length list)) (ddmin list (min (length list) (* 2 parts)) t))

                ;; done: found a 1-minimal subset
                list))))
    (ddmin input 2 t)))

(defun delta-file (filename)
  (write-input (delta (read-input filename)))
;  (format t "unique: ~a, duplicate: ~a~%" *unique-calls* *duplicate-calls*)
)
