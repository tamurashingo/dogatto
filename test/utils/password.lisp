; -*- mode: lisp -*-
(in-package #:cl-user)
(defpackage #:dogatto-test/utils/password
  (:use #:cl
        #:rove
        #:clails/test
        #:dogatto/utils/password))
(in-package #:dogatto-test/utils/password)

(deftest-suite :utils test-hash-password
  (testing "hash-password returns a non-empty string"
    (let ((hash (hash-password "password123")))
      (ok (stringp hash))
      (ok (> (length hash) 0))))
  
  (testing "hash-password generates different hashes for the same password"
    (let ((hash1 (hash-password "password123"))
          (hash2 (hash-password "password123")))
      ;; Due to random salt, hashes should be different
      (ok (not (string= hash1 hash2))))))

(deftest-suite :utils test-verify-password
  (testing "verify-password succeeds with correct password"
    (let ((hash (hash-password "password123")))
      (ok (verify-password "password123" hash))))
  
  (testing "verify-password fails with incorrect password"
    (let ((hash (hash-password "password123")))
      (ok (not (verify-password "wrongpassword" hash)))))
  
  (testing "verify-password handles invalid hash gracefully"
    (ok (not (verify-password "password123" "invalid-hash")))))

(deftest-suite (:utils :validation) test-validate-password
  (testing "validate-password accepts valid password"
    (multiple-value-bind (valid errors)
        (validate-password "Password123")
      (ok valid)
      (ok (null errors))))
  
  (testing "validate-password rejects short password"
    (multiple-value-bind (valid errors)
        (validate-password "Pass1")
      (ok (not valid))
      (ok (member "Password must be at least 8 characters long" errors :test #'string=))))
  
  (testing "validate-password rejects password without letters"
    (multiple-value-bind (valid errors)
        (validate-password "12345678")
      (ok (not valid))
      (ok (member "Password must contain at least one letter" errors :test #'string=))))
  
  (testing "validate-password rejects password without numbers"
    (multiple-value-bind (valid errors)
        (validate-password "Password")
      (ok (not valid))
      (ok (member "Password must contain at least one number" errors :test #'string=))))
  
  (testing "validate-password returns multiple errors"
    (multiple-value-bind (valid errors)
        (validate-password "abc")
      (ok (not valid))
      (ok (>= (length errors) 2)))))

(deftest-suite (:utils :integration) test-integration
  (testing "full password workflow"
    (let ((password "SecurePass123"))
      ;; Validate
      (multiple-value-bind (valid errors)
          (validate-password password)
        (ok valid)
        (ok (null errors)))
      
      ;; Hash
      (let ((hash (hash-password password)))
        (ok (stringp hash))
        
        ;; Verify correct password
        (ok (verify-password password hash))
        
        ;; Verify incorrect password
        (ok (not (verify-password "WrongPass123" hash)))))))
