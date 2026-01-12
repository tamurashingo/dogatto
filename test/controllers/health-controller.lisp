; -*- mode: lisp -*-
(in-package #:cl-user)
(defpackage #:dogatto-test/controllers/health-controller
  (:use #:cl
        #:rove
        #:clails/test))
(in-package #:dogatto-test/controllers/health-controller)

(deftest-suite :controller test-health-controller
  (testing "Test health controller"
    (ok (= 1 0) "This test should be replaced with actual test")))
