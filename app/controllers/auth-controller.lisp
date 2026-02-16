; -*- mode: lisp -*-
(in-package #:cl-user)
(defpackage #:dogatto/controllers/auth-controller
  (:use #:cl
        #:clails/controller/base-controller)
  (:import-from #:dogatto/services/auth-service
                #:register-user
                #:login-user
                #:logout-user
                #:get-current-user)
  (:import-from #:dogatto/helpers/auth-helper
                #:get-cookie-value)
  (:import-from #:dogatto/helpers/json-converters
                #:user-to-json)
  (:import-from #:clails/model
                #:ref)
  (:export #:<auth-controller>
           #:<auth-register-controller>
           #:<auth-login-controller>
           #:<auth-logout-controller>
           #:<auth-me-controller>))

(in-package #:dogatto/controllers/auth-controller)

(defclass <auth-register-controller> (<rest-controller>)
  ()
  (:documentation "Controller for user registration endpoint"))

(defclass <auth-login-controller> (<rest-controller>)
  ()
  (:documentation "Controller for user login endpoint"))

(defclass <auth-logout-controller> (<rest-controller>)
  ()
  (:documentation "Controller for user logout endpoint"))

(defclass <auth-me-controller> (<rest-controller>)
  ()
  (:documentation "Controller for getting current user information"))

;; POST /api/v1/auth/register
(defmethod do-post ((controller <auth-register-controller>))
  "Register a new user.

   Expected parameters:
   - name: User's display name
   - email: User's email address
   - password: User's password (plain text, will be hashed)

   Returns 201 with user data on success, 400 on validation errors.
   "
  (let* ((name (param controller "name"))
         (email (param controller "email"))
         (password (param controller "password"))
         (result (register-user name email password)))
    
    (if (getf result :success)
        ;; Success
        (progn
          (setf (slot-value controller 'clails/controller/base-controller:code) 201)
          (set-response controller
                       `(("status" . "success")
                         ("data" . (("user" . ,(user-to-json (getf result :user))))))))
        ;; Error
        (let ((errors (getf result :errors)))
          (setf (slot-value controller 'clails/controller/base-controller:code) 400)
          ;; Check if it's password validation errors (multiple errors from password validation)
          (if (and (> (length errors) 1)
                   (not (member (car errors) '("Name is required" 
                                                "Email is required" 
                                                "Password is required"
                                                "Email already registered")
                                :test #'string=)))
              ;; Password validation errors - use generic message for backward compatibility
              (set-response controller
                           `(("status" . "error")
                             ("message" . "Password validation failed")
                             ("errors" . ,errors)))
              ;; Other errors - use first error as message
              (set-response controller
                           `(("status" . "error")
                             ("message" . ,(car errors))
                             ,@(when (cdr errors)
                                 `(("errors" . ,errors))))))))))

;; POST /api/v1/auth/login
(defmethod do-post ((controller <auth-login-controller>))
  "Authenticate user and create session.

   Expected parameters:
   - email: User's email address
   - password: User's password (plain text)

   Returns 200 with user data and sets session cookie on success,
   401 on authentication failure, 400 on validation error.
   "
  (let* ((email (param controller "email"))
         (password (param controller "password"))
         (result (login-user email password)))
    
    (if (getf result :success)
        ;; Success - set cookie and return user
        (let ((session-id (getf result :session-id))
              (user (getf result :user)))
          (setf (slot-value controller 'clails/controller/base-controller:header)
                `(:content-type "application/json"
                  :set-cookie ,(format nil "session_id=~A; Path=/; HttpOnly; SameSite=Strict; Max-Age=~D"
                                       session-id
                                       (* 7 24 60 60)))) ; 7 days
          (set-response controller
                       `(("status" . "success")
                         ("data" . (("user" . ,(user-to-json user)))))))
        ;; Error - determine status code based on error type
        (let ((error-msg (car (getf result :errors))))
          ;; Use 400 for validation errors, 401 for authentication failures
          (if (member error-msg '("Email is required" "Password is required") :test #'string=)
              (setf (slot-value controller 'clails/controller/base-controller:code) 400)
              (setf (slot-value controller 'clails/controller/base-controller:code) 401))
          (set-response controller
                       `(("status" . "error")
                         ("message" . ,error-msg)))))))

;; POST /api/v1/auth/logout
(defmethod do-post ((controller <auth-logout-controller>))
  "Logout user by deleting session.

   Reads session ID from cookie, deletes the session from Redis,
   and clears the cookie.

   Returns 200 on success.
   "
  (let* ((env-data (env controller))
         (headers (getf env-data :headers))
         (session-id (get-cookie-value headers "session_id"))
         (result (logout-user session-id)))
    
    ;; Clear cookie
    (setf (slot-value controller 'clails/controller/base-controller:header)
          `(:content-type "application/json"
            :set-cookie "session_id=; Path=/; HttpOnly; SameSite=Strict; Max-Age=0"))
    
    (if (getf result :success)
        (set-response controller
                     `(("status" . "success")
                       ("message" . "Logged out successfully")))
        (progn
          (setf (slot-value controller 'clails/controller/base-controller:code) 500)
          (set-response controller
                       `(("status" . "error")
                         ("message" . ,(car (getf result :errors)))))))))

;; GET /api/v1/auth/me
(defmethod do-get ((controller <auth-me-controller>))
  "Get current authenticated user information.

   Reads session ID from cookie, retrieves user from database,
   and returns user information.

   Returns 200 with user data on success, 401 if not authenticated.
   "
  (let* ((env-data (env controller))
         (headers (getf env-data :headers))
         (session-id (get-cookie-value headers "session_id"))
         (result (get-current-user session-id)))
    
    (if (getf result :success)
        (set-response controller
                     `(("status" . "success")
                       ("data" . (("user" . ,(user-to-json (getf result :user)))))))
        (progn
          (setf (slot-value controller 'clails/controller/base-controller:code) 401)
          ;; Return "Not authenticated" for backward compatibility with tests
          (set-response controller
                       `(("status" . "error")
                         ("message" . "Not authenticated")))))))
