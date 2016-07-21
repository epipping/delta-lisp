(defun main (argv)
  (delta:delta-file (second argv) (third argv) :processes (parse-integer (fourth argv))))
