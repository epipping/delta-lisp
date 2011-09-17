;; -*- mode:common-lisp; indent-tabs-mode: nil -*-

;; FIXME: Do caching
;; FIXME: verify that initial version passes.

(defparameter sample-list '(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16
                            17 18 19 20 21 22 23 24 25 26 27 28 29 30
                            31 32 33 34 35 36 37 38 39 40 41 42 43 44
                            45 46 47 48 49 50 51 52 53 54 55 56 57 58
                            59 60 61 62 63 64 65 66 67 68 69 70 71 72
                            73 74 75 76 77 78 79 80 81 82 83 84 85 86
                            87 88 89 90 91 92 93 94 95 96 97 98 99 100
                            101 102 103 104 105 106 107 108 109 110
                            111 112 113 114 115 116 117 118 119 120
                            121 122 123 124 125 126 127 128 129 130
                            131 132 133 134 135 136 137 138 139 140
                            141 142 143 144 145 146 147 148 149 150
                            151 152 153 154 155 156 157))

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
  (and list-of-subsets
       (let ((begin (first list-of-subsets))
             (end (second list-of-subsets)))
         (let ((subset (or (and end (subseq input begin end))
                           (subseq input begin))))
           (or (and (run-on-input subset) subset)
               (test-subsets (cdr list-of-subsets) input))))))

(defun test-complements (list-of-subsets input)
  (and list-of-subsets
       (let ((begin (first list-of-subsets))
             (end (second list-of-subsets)))
         (let ((subset (append (subseq input 0 begin)
                               (and end (subseq input end)))))
           (or (and (run-on-input subset) subset)
               (test-complements (cdr list-of-subsets) input))))))

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