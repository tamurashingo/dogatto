; -*- mode: lisp -*-
(in-package #:cl-user)
(defpackage #:dogatto/services/auth-service
  (:use #:cl)
  (:import-from #:clails/model
                #:ref
                #:save)
  (:import-from #:dogatto/models/user
                #:find-user-by-email
                #:find-user-by-id
                #:create-user
                #:user-exists-p
                #:<user>)
  (:import-from #:dogatto/utils/password
                #:hash-password
                #:verify-password
                #:validate-password)
  (:import-from #:dogatto/utils/session
                #:create-session
                #:get-session
                #:delete-session
                #:session-valid-p)
  (:import-from #:dogatto/utils/ulid
                #:generate-ulid)
  (:export #:register-user
           #:login-user
           #:logout-user
           #:get-current-user))

(in-package #:dogatto/services/auth-service)

(defun register-user (username email password)
  "Register a new user account.
   
   Validates input parameters, checks for duplicate email,
   validates password strength, hashes password, and creates user record.
   
   @param username [string] User's display name
   @param email [string] User's email address
   @param password [string] User's password (plain text, will be hashed)
   @return [plist] Success with :user or error with :errors
   "
  (let ((errors '()))
    
    ;; Validate username
    (when (or (null username) (string= (string-trim '(#\Space #\Tab) username) ""))
      (push "Name is required" errors))
    
    ;; Validate email
    (when (or (null email) (string= (string-trim '(#\Space #\Tab) email) ""))
      (push "Email is required" errors))
    
    ;; Validate password
    (when (or (null password) (string= password ""))
      (push "Password is required" errors))
    
    ;; Return early if basic validation fails
    (when errors
      (return-from register-user
        (list :success nil :errors (nreverse errors))))
    
    ;; Check if user already exists
    (when (user-exists-p email)
      (return-from register-user
        (list :success nil :errors '("Email already registered"))))
    
    ;; Validate password strength
    (multiple-value-bind (valid password-errors)
        (validate-password password)
      (unless valid
        (return-from register-user
          (list :success nil :errors password-errors))))
    
    ;; Hash password and create user
    (handler-case
        (let* ((password-hash (hash-password password))
               (ulid (generate-ulid))
               (user (create-user :username username
                                 :email email
                                 :password-hash password-hash
                                 :ulid ulid)))
          (if user
              (list :success t :user user)
              (list :success nil :errors '("Failed to create user"))))
      (error (e)
        (list :success nil :errors (list (format nil "Registration failed: ~A" e)))))))

(defun login-user (email password)
  "Authenticate user and create session.
   
   Validates input, finds user by email, verifies password,
   and creates session on successful authentication.
   
   @param email [string] User's email address
   @param password [string] User's password (plain text)
   @return [plist] Success with :user, :session-id or error with :errors
   "
  (let ((errors '()))
    
    ;; Validate email
    (when (or (null email) (string= (string-trim '(#\Space #\Tab) email) ""))
      (push "Email is required" errors))
    
    ;; Validate password
    (when (or (null password) (string= password ""))
      (push "Password is required" errors))
    
    ;; Return early if validation fails
    (when errors
      (return-from login-user
        (list :success nil :errors (nreverse errors))))
    
    ;; Find user and verify password
    (let ((user (find-user-by-email email)))
      (if (and user (verify-password password (ref user :password-hash)))
          ;; Authentication success - create session
          (handler-case
              (let ((session-id (create-session (ref user :id))))
                (list :success t
                      :user user
                      :session-id session-id))
            (error (e)
              (list :success nil :errors (list (format nil "Failed to create session: ~A" e)))))
          ;; Authentication failed
          (list :success nil :errors '("Invalid email or password"))))))

(defun logout-user (session-id)
  "Logout user by deleting session.
   
   Validates session-id and deletes the session from Redis.
   
   @param session-id [string] Session ID from cookie
   @param session-id [nil] If no session ID provided
   @return [plist] Success or error with :errors
   "
  ;; Allow logout even without session (idempotent)
  (when (and session-id (not (string= (string-trim '(#\Space #\Tab) session-id) "")))
    (handler-case
        (delete-session session-id)
      (error (e)
        (return-from logout-user
          (list :success nil :errors (list (format nil "Failed to delete session: ~A" e)))))))
  
  ;; Always return success for logout
  (list :success t))

(defun get-current-user (session-id)
  "Get current authenticated user information.
   
   Validates session-id, retrieves session data, and fetches user record.
   
   @param session-id [string] Session ID from cookie
   @param session-id [nil] If no session ID provided
   @return [plist] Success with :user or error with :errors
   "
  ;; Validate session-id
  (when (or (null session-id) (string= (string-trim '(#\Space #\Tab) session-id) ""))
    (return-from get-current-user
      (list :success nil :errors '("Session ID is required"))))
  
  ;; Check session validity
  (unless (session-valid-p session-id)
    (return-from get-current-user
      (list :success nil :errors '("Invalid or expired session"))))
  
  ;; Get session data and user
  (handler-case
      (let* ((session-data (get-session session-id))
             (user-id (getf session-data :user-id)))
        (if user-id
            (let ((user (find-user-by-id user-id)))
              (if user
                  (list :success t :user user)
                  (list :success nil :errors '("User not found"))))
            (list :success nil :errors '("Invalid session data"))))
    (error (e)
      (list :success nil :errors (list (format nil "Failed to retrieve user: ~A" e))))))
