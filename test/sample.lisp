(in-package #:cl-user)
(defpackage #:dogatto-test/sample
  (:use #:cl
        #:rove
        #:clails/test))
(in-package #:dogatto-test/sample)

(deftest-suite :sample sample-basic-test
  (testing "Sample basic test"
    (ok t "Always passes")))

(deftest-suite (:sample :number) sample-number-test
  (testing "Sample number test"
    (ok (= 1 1) "1 equals 1")
    (ok (> 2 1) "2 is greater than 1")))

(deftest-suite (:sample :string) sample-string-test
  (testing "Sample string test"
    (ok (stringp "hello world") "String is a string")
    (ok (string= "hello" "hello") "Strings are equal")))
