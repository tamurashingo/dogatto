; -*- mode: lisp -*-
(in-package #:cl-user)
(defpackage #:dogatto-test/controllers/pages-controller
  (:use #:cl
        #:rove
        #:clails/test))
(in-package #:dogatto-test/controllers/pages-controller)

(deftest-suite :controller test-pages-controller
  (testing "Test pages controller"
    (ok (= 1 1) "This test should be replaced with actual test")))
