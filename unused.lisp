;; Code that might be useful at some point

(defun slurp-input (filename)
  (with-open-file (s filename)
    (let* ((len (file-length s))
           (data (make-string len)))
      (progn (read-sequence data s)
             data))))

;; (osicat:with-temporary-file (s)
;;   (format s "foo")
;;   (print s))
