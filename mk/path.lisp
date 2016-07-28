(push (uiop:ensure-absolute-pathname
       ;; https://github.com/Inaimathi/cl-cwd does something far more complicated.
       ;; Probably for good reasons but I don't see them.
       (truename "."))
      asdf:*central-registry*)
(asdf:disable-output-translations)



