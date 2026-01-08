; -*- mode: lisp -*-
(in-package #:cl-user)
(defpackage #:dogatto-system
  (:use #:asdf #:cl))
(in-package #:dogatto-system)

(defsystem dogatto
  :class :package-inferred-system
  :description ""
  :version "0.0.1"
  :author "tamura.shingo@gmail.com"
  :license "MIT"
  :pathname "app"
  :depends-on ("clails"
               "swank"
               "dogatto/application-loader")
  :in-order-to ((test-op (test-op "dogatto-test"))))

