;; -*- mode:common-lisp; indent-tabs-mode: nil -*-

(in-package #:delta-standalone)

(alexandria:define-constant +delta-options+
  '("processes=<integer>" "suffix=<string>" "quiet"
    "show-stdout" "show-stderr")
  :test #'equal)

(defun print-usage (name)
  (format nil
          "Usage: ~a [options] test input~% Options: ~{--~a~^, ~}"
          name +delta-options+))

(defun extract-option (assoc-cell)
  (or (cdr assoc-cell)
      (not (null assoc-cell))))

(defun generate-option-arguments (option-string options)
  (mapcan
   #'(lambda (str)
       (let* ((position (position #\= str))
              (name (subseq str 0 position))
              (is-integer (and position
                               (string= (subseq str (1+ position))
                                        "<integer>")))
              (option (extract-option (assoc name options :test #'string=))))
         (when option (list (alexandria:make-keyword (string-upcase name))
                            (if is-integer (parse-integer option) option)))))
   option-string))

(defun main ()
  (let+ ((argv (uiop:command-line-arguments))
         ((&values files options unhandled-options)
          (getopt:getopt argv
                         (mapcar #'(lambda (str)
                                     (list (subseq str 0 (position #\= str))
                                           :optional))
                                 +delta-options+))))
        (unless (= (length files) 2)
          (error (print-usage (uiop:argv0))))
        (unless (null unhandled-options)
          (error (format nil "Unhandled option: ~a" (first unhandled-options))))
        (let ((test (first files))
              (test-input (second files)))
          (apply
           'delta:delta-file
           (append (list test test-input)
                   (generate-option-arguments +delta-options+
                                              options))))))
