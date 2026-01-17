; -*- mode: lisp -*-
(in-package #:cl-user)
(defpackage #:dogatto-test/models/user
  (:use #:cl
        #:rove
        #:clails/test
        #:dogatto/models/user)
  (:import-from #:clails/model
                #:ref
                #:ref-error
                #:has-error-p
                #:make-record
                #:save
                #:destroy))
(in-package #:dogatto-test/models/user)

(deftest-suite :model test-find-user-by-email
  (testing "find-user-by-email returns user when email exists"
    ;; Create test user
    (let* ((test-ulid "01234567890123456789012345")
           (test-email "test-find@example.com")
           (user (create-user :username "testuser"
                             :email test-email
                             :password-hash "hashedpassword"
                             :ulid test-ulid)))
      (ok (not (null user)) "User should be created")
      
      ;; Find by email
      (let ((found-user (find-user-by-email test-email)))
        (ok (not (null found-user)) "User should be found")
        (ok (string= (ref found-user :email) test-email) "Email should match"))
      
      ;; Cleanup
      (destroy user)))
  
  (testing "find-user-by-email returns nil when email does not exist"
    (let ((found-user (find-user-by-email "nonexistent@example.com")))
      (ok (null found-user) "Should return nil for non-existent email"))))

(deftest-suite :model test-find-user-by-id
  (testing "find-user-by-id returns user when ID exists"
    ;; Create test user
    (let* ((test-ulid "01234567890123456789012346")
           (user (create-user :username "testuser2"
                             :email "test-id@example.com"
                             :password-hash "hashedpassword"
                             :ulid test-ulid)))
      (ok (not (null user)) "User should be created")
      
      (let* ((user-id (ref user :id))
             (found-user (find-user-by-id user-id)))
        (ok (not (null found-user)) "User should be found")
        (ok (= (ref found-user :id) user-id) "ID should match"))
      
      ;; Cleanup
      (destroy user)))
  
  (testing "find-user-by-id returns nil when ID does not exist"
    (let ((found-user (find-user-by-id 999999)))
      (ok (null found-user) "Should return nil for non-existent ID"))))

(deftest-suite :model test-create-user
  (testing "create-user creates a new user successfully"
    (let* ((test-ulid "01234567890123456789012347")
           (user (create-user :username "newuser"
                             :email "newuser@example.com"
                             :password-hash "hashedpassword"
                             :ulid test-ulid)))
      (ok (not (null user)) "User should be created")
      (ok (ref user :id) "User should have an ID")
      (ok (string= (ref user :username) "newuser") "Username should match")
      (ok (string= (ref user :email) "newuser@example.com") "Email should match")
      (ok (string= (ref user :registration-status) "provisional") "Default status should be provisional")
      
      ;; Cleanup
      (destroy user)))
  
  (testing "create-user with all optional parameters"
    (let* ((test-ulid "01234567890123456789012348")
           (expires-at (+ (get-universal-time) 3600))
           (user (create-user :username "fulluser"
                             :email "fulluser@example.com"
                             :password-hash "hashedpassword"
                             :ulid test-ulid
                             :registration-status "active"
                             :github-id "github123"
                             :registration-token "token123"
                             :registration-token-expires-at expires-at)))
      (ok (not (null user)) "User should be created")
      (ok (string= (ref user :registration-status) "active") "Status should be active")
      (ok (string= (ref user :github-id) "github123") "GitHub ID should match")
      (ok (string= (ref user :registration-token) "token123") "Token should match")
      
      ;; Cleanup
      (destroy user)))
  
  (testing "create-user fails with missing required fields"
    (let ((user (create-user :username ""
                            :email "test@example.com"
                            :password-hash "hash"
                            :ulid "01234567890123456789012349")))
      (ok (null user) "Should fail with empty username"))))

(deftest-suite :model test-user-exists-p
  (testing "user-exists-p returns T when user exists"
    (let* ((test-ulid "01234567890123456789012350")
           (test-email "exists@example.com")
           (user (create-user :username "existsuser"
                             :email test-email
                             :password-hash "hashedpassword"
                             :ulid test-ulid)))
      (ok (user-exists-p test-email) "Should return T for existing email")
      
      ;; Cleanup
      (destroy user)))
  
  (testing "user-exists-p returns NIL when user does not exist"
    (ok (not (user-exists-p "doesnotexist@example.com"))
        "Should return NIL for non-existent email")))

(deftest-suite (:model :validation) test-user-validation
  (testing "validation catches missing username"
    (let ((user (make-record '<user>
                            :email "test@example.com"
                            :ulid "01234567890123456789012351"
                            :username "")))
      (ok (null (save user)) "Save should fail")
      (ok (has-error-p user) "Should have errors")
      (ok (ref-error user :username)
          "Should have username error")))
  
  (testing "validation catches missing email"
    (let ((user (make-record '<user>
                            :username "testuser"
                            :ulid "01234567890123456789012352"
                            :email "")))
      (ok (null (save user)) "Save should fail")
      (ok (has-error-p user) "Should have errors")
      (ok (ref-error user :email)
          "Should have email error")))
  
  (testing "validation catches missing ulid"
    (let ((user (make-record '<user>
                            :username "testuser"
                            :email "test@example.com"
                            :ulid "")))
      (ok (null (save user)) "Save should fail")
      (ok (has-error-p user) "Should have errors")
      (ok (ref-error user :ulid)
          "Should have ulid error"))))
