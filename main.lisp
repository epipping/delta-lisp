(require :getopt)

(defun print-usage (name)
  (format nil "Usage: ~a [--processes=n] [--suffix=suffix] test input" name))

(defun main (argv)
  (multiple-value-bind (files options unhandled-options)
      (getopt:getopt (cdr argv)
                     '(("suffix" :optional)
                       ("processes" :optional)))
    (unless (= (length files) 2)
      (error (print-usage (car argv))))
    (unless (null unhandled-options)
      (error (format nil "Unhandled option: ~a" (first unhandled-options))))
    (let ((test (first files))
          (test-input (second files))
          (processes (cdr (assoc "processes" options :test #'string=)))
          (suffix (cdr (assoc "suffix" options :test #'string=))))
      (apply 'delta:delta-file
             `(,test ,test-input
                     ,@(when processes `(:processes ,(parse-integer processes)))
                     ,@(when suffix `(:suffix ,suffix)))))))
