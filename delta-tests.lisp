;; -*- mode:common-lisp; indent-tabs-mode: nil -*-

(in-package #:delta-tests)

(def-suite delta-utility-tests
    :description "Test utilities contained within delta.")

(in-suite delta-utility-tests)

(test compute-break
      "Test the compute-break function."
      (is (= 0 (delta::compute-break 10 0 2)))
      (is (= 5 (delta::compute-break 10 1 2)))
      (is (= 10 (delta::compute-break 10 2 2))))

(test exclude-range
      "Test the exclude-range function."
      (is (equal '(b c d e) (delta::exclude-range 0 1 '(a b c d e))))
      (is (equal '(d e) (delta::exclude-range 0 3 '(a b c d e))))
      (is (equal '(a b d e) (delta::exclude-range 2 3 '(a b c d e)))))

(test shift-and-wrap
      "Test shift-and-wrap function."
      (is (= 3 (delta::shift-and-wrap 0 3 10)))
      (is (= 3 (delta::shift-and-wrap 3 0 10)))
      (is (= 4 (delta::shift-and-wrap 1 3 10)))
      (is (= 2 (delta::shift-and-wrap 6 6 10))))
