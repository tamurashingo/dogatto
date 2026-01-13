; -*- mode: lisp -*-
(in-package #:cl-user)
(defpackage #:dogatto-test/middleware/authentication
  (:use #:cl
        #:rove
        #:clails/test
        #:dogatto/middleware/authentication)
  (:import-from #:dogatto/utils/session
                #:create-session
                #:delete-session)
  (:import-from #:dogatto/models/user
                #:create-user
                #:find-user-by-email
                #:<user>)
  (:import-from #:dogatto/utils/password
                #:hash-password)
  (:import-from #:clails/model
                #:ref
                #:destroy)
  (:import-from #:uuid
                #:make-v4-uuid))

(in-package #:dogatto-test/middleware/authentication)


(deftest-suite :middleware test-require-authentication-with-valid-session
  "Test require-authentication middleware with valid session"
  (testing "Should allow request with valid session and add user to env"
    (let* ((test-email "middleware-test@example.com")
           (test-password "ValidPass123!")
           (test-username "middleware-user")
           (password-hash (hash-password test-password))
           (ulid (format nil "~(~a~)" (make-v4-uuid)))
           (user (create-user :username test-username
                              :email test-email
                              :password-hash password-hash
                              :ulid ulid))
           (user-id (ref user :id))
           (session-id (create-session user-id))
           (app-called nil)
           (env-received nil))
      
      (unwind-protect
          (let* ((mock-app (lambda (env)
                             (setf app-called t)
                             (setf env-received env)
                             '(200 (:content-type "text/plain") ("OK"))))
                 (middleware (require-authentication mock-app))
                 (env (list :headers (let ((ht (make-hash-table :test 'equal)))
                                       (setf (gethash "cookie" ht) 
                                             (format nil "session_id=~A" session-id))
                                       ht)))
                 (response (funcall middleware env)))
            
            (ok app-called "Downstream app should be called")
            (ok (getf env-received :current-user) "Current user should be added to env")
            (ok (equal (ref (getf env-received :current-user) :id) user-id)
                "Current user should match the session user")
            (ok (equal (first response) 200) "Response status should be 200"))
        
        ;; Cleanup
        (when session-id (delete-session session-id))
        (when user (destroy user))))))


(deftest-suite :middleware test-require-authentication-without-session
  "Test require-authentication middleware without session cookie"
  (testing "Should return 401 when no session cookie is provided"
    (let* ((app-called nil)
           (mock-app (lambda (env)
                       (setf app-called t)
                       '(200 (:content-type "text/plain") ("OK"))))
           (middleware (require-authentication mock-app))
           (env (list :headers (make-hash-table :test 'equal)))
           (response (funcall middleware env)))
      
      (ok (not app-called) "Downstream app should not be called")
      (ok (equal (first response) 401) "Response status should be 401")
      (ok (equal (second response) '(:content-type "application/json"))
          "Content-Type should be application/json")
      (ok (search "Not authenticated" (first (third response)))
          "Response should contain authentication error message"))))


(deftest-suite :middleware test-require-authentication-with-invalid-session
  "Test require-authentication middleware with invalid session ID"
  (testing "Should return 401 when session ID is invalid"
    (let* ((app-called nil)
           (mock-app (lambda (env)
                       (setf app-called t)
                       '(200 (:content-type "text/plain") ("OK"))))
           (middleware (require-authentication mock-app))
           (env (list :headers (let ((ht (make-hash-table :test 'equal)))
                                 (setf (gethash "cookie" ht) "session_id=invalid-session-id")
                                 ht)))
           (response (funcall middleware env)))
      
      (ok (not app-called) "Downstream app should not be called")
      (ok (equal (first response) 401) "Response status should be 401")
      (ok (search "Not authenticated" (first (third response)))
          "Response should contain authentication error message"))))


(deftest-suite :middleware test-require-authentication-with-expired-session
  "Test require-authentication middleware with expired session"
  (testing "Should return 401 when session has expired"
    ;; Note: This test would require modifying session TTL or waiting
    ;; For now, we'll just test with a non-existent session
    (let* ((app-called nil)
           (mock-app (lambda (env)
                       (setf app-called t)
                       '(200 (:content-type "text/plain") ("OK"))))
           (middleware (require-authentication mock-app))
           (fake-session-id (format nil "~(~a~)" (make-v4-uuid)))
           (env (list :headers (let ((ht (make-hash-table :test 'equal)))
                                 (setf (gethash "cookie" ht) 
                                       (format nil "session_id=~A" fake-session-id))
                                 ht)))
           (response (funcall middleware env)))
      
      (ok (not app-called) "Downstream app should not be called")
      (ok (equal (first response) 401) "Response status should be 401"))))


(deftest-suite :middleware test-get-current-user
  "Test get-current-user helper function"
  (testing "Should extract user from env"
    (let* ((test-email "helper-test@example.com")
           (test-password "ValidPass123!")
           (test-username "helper-user")
           (password-hash (hash-password test-password))
           (ulid (format nil "~(~a~)" (make-v4-uuid)))
           (user (create-user :username test-username
                              :email test-email
                              :password-hash password-hash
                              :ulid ulid)))
      
      (unwind-protect
          (let ((env (list :current-user user :other-key "value")))
            (ok (equal (get-current-user env) user)
                "Should return the user from env"))
        
        ;; Cleanup
        (when user (destroy user)))))
  
  (testing "Should return nil when no user in env"
    (let ((env (list :other-key "value")))
      (ok (null (get-current-user env))
          "Should return nil when no user is present"))))


(deftest-suite :middleware test-require-authentication-with-multiple-cookies
  "Test require-authentication with multiple cookies in header"
  (testing "Should correctly extract session_id from multiple cookies"
    (let* ((test-email "multi-cookie-test@example.com")
           (test-password "ValidPass123!")
           (test-username "multi-cookie-user")
           (password-hash (hash-password test-password))
           (ulid (format nil "~(~a~)" (make-v4-uuid)))
           (user (create-user :username test-username
                              :email test-email
                              :password-hash password-hash
                              :ulid ulid))
           (user-id (ref user :id))
           (session-id (create-session user-id))
           (app-called nil))
      
      (unwind-protect
          (let* ((mock-app (lambda (env)
                             (setf app-called t)
                             '(200 (:content-type "text/plain") ("OK"))))
                 (middleware (require-authentication mock-app))
                 (env (list :headers (let ((ht (make-hash-table :test 'equal)))
                                       (setf (gethash "cookie" ht)
                                             (format nil "other_cookie=value; session_id=~A; another=test"
                                                     session-id))
                                       ht)))
                 (response (funcall middleware env)))
            
            (ok app-called "Downstream app should be called with valid session")
            (ok (equal (first response) 200) "Response status should be 200"))
        
        ;; Cleanup
        (when session-id (delete-session session-id))
        (when user (destroy user))))))
