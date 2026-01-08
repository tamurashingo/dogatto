; -*- mode: lisp -*-
(in-package #:cl-user)
(defpackage #:dogatto-test-system
  (:use #:asdf #:cl))
(in-package #:dogatto-test-system)

(defsystem dogatto-test
  :class :package-inferred-system
  :pathname "test"
  :depends-on ("clails"
               "rove"
               "dogatto"
               "dogatto-test/test-loader")
  :perform (test-op (o c)
             (uiop:symbol-call :rove :run c)))
