; -*- mode: lisp -*-
(in-package #:cl-user)
(defpackage #:dogatto-test/services/auth-service
  (:use #:cl
        #:rove
        #:clails/test
        #:dogatto/services/auth-service)
  (:import-from #:clails/model
                #:ref
                #:destroy)
  (:import-from #:dogatto/models/user
                #:find-user-by-email
                #:<user>)
  (:import-from #:dogatto/utils/session
                #:delete-session
                #:session-valid-p))

(in-package #:dogatto-test/services/auth-service)

(deftest-suite :service test-register-user-success
  (testing "register-user creates user with valid input"
    (let* ((result (register-user "Test User" 
                                  "test-register-service@example.com" 
                                  "SecurePass123!"))
           (success (getf result :success))
           (user (getf result :user)))
      
      (ok success "Registration should succeed")
      (ok user "User should be returned")
      (ok (string= (ref user :username) "Test User") "Username should match")
      (ok (string= (ref user :email) "test-register-service@example.com") "Email should match")
      
      ;; Cleanup
      (when user (destroy user)))))

(deftest-suite :service test-register-user-validation-errors
  (testing "register-user fails with missing username"
    (let* ((result (register-user nil "test@example.com" "password"))
           (success (getf result :success))
           (errors (getf result :errors)))
      
      (ng success "Registration should fail")
      (ok errors "Errors should be returned")
      (ok (member "Name is required" errors :test #'string=) 
          "Should have name required error")))
  
  (testing "register-user fails with missing email"
    (let* ((result (register-user "Test User" nil "password"))
           (success (getf result :success))
           (errors (getf result :errors)))
      
      (ng success "Registration should fail")
      (ok (member "Email is required" errors :test #'string=) 
          "Should have email required error")))
  
  (testing "register-user fails with missing password"
    (let* ((result (register-user "Test User" "test@example.com" nil))
           (success (getf result :success))
           (errors (getf result :errors)))
      
      (ng success "Registration should fail")
      (ok (member "Password is required" errors :test #'string=) 
          "Should have password required error"))))

(deftest-suite :service test-register-user-duplicate-email
  (testing "register-user fails with duplicate email"
    ;; Create first user
    (let* ((result1 (register-user "User One" 
                                   "duplicate-service@example.com" 
                                   "SecurePass123!"))
           (user1 (getf result1 :user)))
      
      (ok (getf result1 :success) "First registration should succeed")
      
      ;; Try to create second user with same email
      (let* ((result2 (register-user "User Two" 
                                     "duplicate-service@example.com" 
                                     "SecurePass456!"))
             (success (getf result2 :success))
             (errors (getf result2 :errors)))
        
        (ng success "Second registration should fail")
        (ok (member "Email already registered" errors :test #'string=) 
            "Should have duplicate email error"))
      
      ;; Cleanup
      (when user1 (destroy user1)))))

(deftest-suite :service test-login-user-success
  (testing "login-user succeeds with valid credentials"
    ;; Register user first
    (let* ((reg-result (register-user "Login Test" 
                                      "login-service@example.com" 
                                      "SecurePass123!"))
           (reg-user (getf reg-result :user)))
      
      (ok (getf reg-result :success) "Registration should succeed")
      
      ;; Login
      (let* ((login-result (login-user "login-service@example.com" "SecurePass123!"))
             (success (getf login-result :success))
             (user (getf login-result :user))
             (session-id (getf login-result :session-id)))
        
        (ok success "Login should succeed")
        (ok user "User should be returned")
        (ok session-id "Session ID should be returned")
        (ok (session-valid-p session-id) "Session should be valid")
        
        ;; Cleanup
        (when session-id (delete-session session-id))
        (when reg-user (destroy reg-user))))))

(deftest-suite :service test-login-user-validation-errors
  (testing "login-user fails with missing email"
    (let* ((result (login-user nil "password"))
           (success (getf result :success))
           (errors (getf result :errors)))
      
      (ng success "Login should fail")
      (ok (member "Email is required" errors :test #'string=) 
          "Should have email required error")))
  
  (testing "login-user fails with missing password"
    (let* ((result (login-user "test@example.com" nil))
           (success (getf result :success))
           (errors (getf result :errors)))
      
      (ng success "Login should fail")
      (ok (member "Password is required" errors :test #'string=) 
          "Should have password required error"))))

(deftest-suite :service test-login-user-invalid-credentials
  (testing "login-user fails with invalid email"
    (let* ((result (login-user "nonexistent@example.com" "password"))
           (success (getf result :success))
           (errors (getf result :errors)))
      
      (ng success "Login should fail")
      (ok (member "Invalid email or password" errors :test #'string=) 
          "Should have invalid credentials error")))
  
  (testing "login-user fails with wrong password"
    ;; Register user first
    (let* ((reg-result (register-user "Wrong Pass Test" 
                                      "wrongpass-service@example.com" 
                                      "CorrectPass123!"))
           (reg-user (getf reg-result :user)))
      
      ;; Try login with wrong password
      (let* ((result (login-user "wrongpass-service@example.com" "WrongPass456!"))
             (success (getf result :success))
             (errors (getf result :errors)))
        
        (ng success "Login should fail")
        (ok (member "Invalid email or password" errors :test #'string=) 
            "Should have invalid credentials error"))
      
      ;; Cleanup
      (when reg-user (destroy reg-user)))))

(deftest-suite :service test-logout-user
  (testing "logout-user succeeds with valid session"
    ;; Register and login
    (let* ((reg-result (register-user "Logout Test" 
                                      "logout-service@example.com" 
                                      "SecurePass123!"))
           (reg-user (getf reg-result :user))
           (login-result (login-user "logout-service@example.com" "SecurePass123!"))
           (session-id (getf login-result :session-id)))
      
      ;; Logout
      (let* ((result (logout-user session-id))
             (success (getf result :success)))
        
        (ok success "Logout should succeed")
        (ng (session-valid-p session-id) "Session should be invalid after logout"))
      
      ;; Cleanup
      (when reg-user (destroy reg-user))))
  
  (testing "logout-user succeeds without session (idempotent)"
    (let* ((result (logout-user nil))
           (success (getf result :success)))
      
      (ok success "Logout without session should still succeed"))))

(deftest-suite :service test-get-current-user
  (testing "get-current-user returns user with valid session"
    ;; Register and login
    (let* ((reg-result (register-user "Current User Test" 
                                      "current-service@example.com" 
                                      "SecurePass123!"))
           (reg-user (getf reg-result :user))
           (login-result (login-user "current-service@example.com" "SecurePass123!"))
           (session-id (getf login-result :session-id)))
      
      ;; Get current user
      (let* ((result (get-current-user session-id))
             (success (getf result :success))
             (user (getf result :user)))
        
        (ok success "Get current user should succeed")
        (ok user "User should be returned")
        (ok (string= (ref user :email) "current-service@example.com") 
            "Email should match"))
      
      ;; Cleanup
      (when session-id (delete-session session-id))
      (when reg-user (destroy reg-user))))
  
  (testing "get-current-user fails with missing session"
    (let* ((result (get-current-user nil))
           (success (getf result :success))
           (errors (getf result :errors)))
      
      (ng success "Should fail without session")
      (ok (member "Session ID is required" errors :test #'string=) 
          "Should have session required error")))
  
  (testing "get-current-user fails with invalid session"
    (let* ((result (get-current-user "invalid-session-id"))
           (success (getf result :success))
           (errors (getf result :errors)))
      
      (ng success "Should fail with invalid session")
      (ok (member "Invalid or expired session" errors :test #'string=) 
          "Should have invalid session error"))))
