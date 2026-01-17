; -*- mode: lisp -*-
(in-package #:cl-user)
(defpackage #:dogatto-test/controllers/auth-controller
  (:use #:cl
        #:rove
        #:clails/test)
  (:import-from #:dogatto/controllers/auth-controller
                #:<auth-register-controller>
                #:<auth-login-controller>
                #:<auth-logout-controller>
                #:<auth-me-controller>
                #:do-post
                #:do-get)
  (:import-from #:dogatto/models/user
                #:find-user-by-email
                #:<user>)
  (:import-from #:clails/model
                #:ref
                #:destroy)
  (:import-from #:dogatto/utils/session
                #:delete-session
                #:get-session)
  (:import-from #:clails/controller/base-controller
                #:response))
(in-package #:dogatto-test/controllers/auth-controller)

(deftest-suite :controller test-auth-register-success
  (testing "Register a new user successfully"
    ;; Clean up any existing user with the same email
    (let ((existing-user (find-user-by-email "test@example.com")))
      (when existing-user
        (destroy existing-user)))
    
    (let ((controller (make-instance '<auth-register-controller>)))
      (setf (gethash "name" (slot-value controller 'clails/controller/base-controller::params))
            "testuser")
      (setf (gethash "email" (slot-value controller 'clails/controller/base-controller::params))
            "test@example.com")
      (setf (gethash "password" (slot-value controller 'clails/controller/base-controller::params))
            "SecureP@ssw0rd123")
      
      (do-post controller)
      
      (let ((resp (response controller)))
        (ok (= (slot-value controller 'clails/controller/base-controller::code) 201)
            "Should return 201 Created")
        (ok (assoc "status" resp :test #'string=) "Response should have status")
        (ok (string= (cdr (assoc "status" resp :test #'string=)) "success")
            "Status should be success")
        (ok (assoc "data" resp :test #'string=) "Response should have data")
        (let* ((data (cdr (assoc "data" resp :test #'string=)))
               (user (cdr (assoc "user" data :test #'string=))))
          (ok (assoc "name" user :test #'string=) "Data should have username")
          (ok (string= (cdr (assoc "name" user :test #'string=)) "testuser")
              "Username should match")))
      
      (let ((user (find-user-by-email "test@example.com")))
        (when user
          (destroy user))))))

(deftest-suite :controller test-auth-register-missing-username
  (testing "Register fails when username is missing"
    (let ((controller (make-instance '<auth-register-controller>)))
      (setf (gethash "email" (slot-value controller 'clails/controller/base-controller::params))
            "test@example.com")
      (setf (gethash "password" (slot-value controller 'clails/controller/base-controller::params))
            "SecureP@ssw0rd123")
      
      (do-post controller)
      
      (let ((resp (response controller)))
        (ok (= (slot-value controller 'clails/controller/base-controller::code) 400)
            "Should return 400 Bad Request")
        (ok (string= (cdr (assoc "status" resp :test #'string=)) "error")
            "Status should be error")
        (ok (string= (cdr (assoc "message" resp :test #'string=)) "Name is required")
            "Should show username required message")))))

(deftest-suite :controller test-auth-register-missing-email
  (testing "Register fails when email is missing"
    (let ((controller (make-instance '<auth-register-controller>)))
      (setf (gethash "name" (slot-value controller 'clails/controller/base-controller::params))
            "testuser")
      (setf (gethash "password" (slot-value controller 'clails/controller/base-controller::params))
            "SecureP@ssw0rd123")
      
      (do-post controller)
      
      (let ((resp (response controller)))
        (ok (= (slot-value controller 'clails/controller/base-controller::code) 400)
            "Should return 400 Bad Request")
        (ok (string= (cdr (assoc "status" resp :test #'string=)) "error")
            "Status should be error")
        (ok (string= (cdr (assoc "message" resp :test #'string=)) "Email is required")
            "Should show email required message")))))

(deftest-suite :controller test-auth-register-missing-password
  (testing "Register fails when password is missing"
    (let ((controller (make-instance '<auth-register-controller>)))
      (setf (gethash "name" (slot-value controller 'clails/controller/base-controller::params))
            "testuser")
      (setf (gethash "email" (slot-value controller 'clails/controller/base-controller::params))
            "test@example.com")
      
      (do-post controller)
      
      (let ((resp (response controller)))
        (ok (= (slot-value controller 'clails/controller/base-controller::code) 400)
            "Should return 400 Bad Request")
        (ok (string= (cdr (assoc "status" resp :test #'string=)) "error")
            "Status should be error")
        (ok (string= (cdr (assoc "message" resp :test #'string=)) "Password is required")
            "Should show password required message")))))

(deftest-suite :controller test-auth-register-weak-password
  (testing "Register fails with weak password"
    (let ((controller (make-instance '<auth-register-controller>)))
      (setf (gethash "name" (slot-value controller 'clails/controller/base-controller::params))
            "testuser")
      (setf (gethash "email" (slot-value controller 'clails/controller/base-controller::params))
            "test2@example.com")
      (setf (gethash "password" (slot-value controller 'clails/controller/base-controller::params))
            "weak")
      
      (do-post controller)
      
      (let ((resp (response controller)))
        (ok (= (slot-value controller 'clails/controller/base-controller::code) 400)
            "Should return 400 Bad Request")
        (ok (string= (cdr (assoc "status" resp :test #'string=)) "error")
            "Status should be error")
        (ok (string= (cdr (assoc "message" resp :test #'string=)) "Password validation failed")
            "Should show password validation failed message")))))

(deftest-suite :controller test-auth-login-success
  (testing "Login succeeds with correct credentials"
    (let ((register-ctrl (make-instance '<auth-register-controller>)))
      (setf (gethash "name" (slot-value register-ctrl 'clails/controller/base-controller::params))
            "loginuser")
      (setf (gethash "email" (slot-value register-ctrl 'clails/controller/base-controller::params))
            "login@example.com")
      (setf (gethash "password" (slot-value register-ctrl 'clails/controller/base-controller::params))
            "SecureP@ssw0rd123")
      (do-post register-ctrl))
    
    (let ((controller (make-instance '<auth-login-controller>)))
      (setf (gethash "email" (slot-value controller 'clails/controller/base-controller::params))
            "login@example.com")
      (setf (gethash "password" (slot-value controller 'clails/controller/base-controller::params))
            "SecureP@ssw0rd123")
      
      (do-post controller)
      
      (let ((resp (response controller)))
        (ok (= (slot-value controller 'clails/controller/base-controller::code) 200)
            "Should return 200 OK")
        (ok (string= (cdr (assoc "status" resp :test #'string=)) "success")
            "Status should be success")
        (ok (assoc "data" resp :test #'string=) "Response should have data")
        (let ((header (slot-value controller 'clails/controller/base-controller::header)))
          (ok (getf header :set-cookie) "Should set session cookie")))
      
      (let ((user (find-user-by-email "login@example.com")))
        (when user
          (destroy user))))))

(deftest-suite :controller test-auth-login-wrong-password
  (testing "Login fails with wrong password"
    (let ((register-ctrl (make-instance '<auth-register-controller>)))
      (setf (gethash "name" (slot-value register-ctrl 'clails/controller/base-controller::params))
            "wrongpwuser")
      (setf (gethash "email" (slot-value register-ctrl 'clails/controller/base-controller::params))
            "wrongpw@example.com")
      (setf (gethash "password" (slot-value register-ctrl 'clails/controller/base-controller::params))
            "SecureP@ssw0rd123")
      (do-post register-ctrl))
    
    (let ((controller (make-instance '<auth-login-controller>)))
      (setf (gethash "email" (slot-value controller 'clails/controller/base-controller::params))
            "wrongpw@example.com")
      (setf (gethash "password" (slot-value controller 'clails/controller/base-controller::params))
            "WrongPassword123")
      
      (do-post controller)
      
      (let ((resp (response controller)))
        (ok (= (slot-value controller 'clails/controller/base-controller::code) 401)
            "Should return 401 Unauthorized")
        (ok (string= (cdr (assoc "status" resp :test #'string=)) "error")
            "Status should be error")
        (ok (string= (cdr (assoc "message" resp :test #'string=)) "Invalid email or password")
            "Should show invalid credentials message"))
      
      (let ((user (find-user-by-email "wrongpw@example.com")))
        (when user
          (destroy user))))))

(deftest-suite :controller test-auth-login-missing-email
  (testing "Login fails when email is missing"
    (let ((controller (make-instance '<auth-login-controller>)))
      (setf (gethash "password" (slot-value controller 'clails/controller/base-controller::params))
            "SecureP@ssw0rd123")
      
      (do-post controller)
      
      (let ((resp (response controller)))
        (ok (= (slot-value controller 'clails/controller/base-controller::code) 400)
            "Should return 400 Bad Request")
        (ok (string= (cdr (assoc "status" resp :test #'string=)) "error")
            "Status should be error")
        (ok (string= (cdr (assoc "message" resp :test #'string=)) "Email is required")
            "Should show email required message")))))

(deftest-suite :controller test-auth-logout-success
  (testing "Logout succeeds and clears session"
    (let ((controller (make-instance '<auth-logout-controller>)))
      (setf (slot-value controller 'clails/controller/base-controller::env)
            (list :headers (make-hash-table :test 'equal)))
      
      (do-post controller)
      
      (let ((resp (response controller)))
        (ok (= (slot-value controller 'clails/controller/base-controller::code) 200)
            "Should return 200 OK")
        (ok (string= (cdr (assoc "status" resp :test #'string=)) "success")
            "Status should be success")
        (let ((header (slot-value controller 'clails/controller/base-controller::header)))
          (ok (getf header :set-cookie) "Should clear session cookie")
          (ok (search "Max-Age=0" (getf header :set-cookie))
              "Cookie should have Max-Age=0"))))))

(deftest-suite :controller test-auth-me-not-authenticated
  (testing "GET /me fails when not authenticated"
    (let ((controller (make-instance '<auth-me-controller>)))
      (setf (slot-value controller 'clails/controller/base-controller::env)
            (list :headers (make-hash-table :test 'equal)))
      
      (do-get controller)
      
      (let ((resp (response controller)))
        (ok (= (slot-value controller 'clails/controller/base-controller::code) 401)
            "Should return 401 Unauthorized")
        (ok (string= (cdr (assoc "status" resp :test #'string=)) "error")
            "Status should be error")
        (ok (string= (cdr (assoc "message" resp :test #'string=)) "Not authenticated")
            "Should show not authenticated message")))))
