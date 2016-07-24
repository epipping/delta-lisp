;; -*- mode:common-lisp; indent-tabs-mode: nil -*-

(in-package #:delta)

(defun compute-break (length part numparts)
  "Compute mark at which chunk #`part` begins when a list of length
`length` is divided into `numparts`-many chunks of (roughly) equal
size."
  (floor (* part length) numparts))

(defun exclude-range (first last list)
  "Remove range of indices from `first` to `last` from list
`list`. The range is taken is left-inclusive and right-exclusive."
  (cond
    ((plusp first)
     (cons (car list) (exclude-range (1- first) (1- last) (cdr list))))
    ((plusp last)
     (exclude-range first (1- last) (cdr list)))
    (t list)))

(defun shift-and-wrap (part shift numparts)
  (mod (+ part shift) numparts))

