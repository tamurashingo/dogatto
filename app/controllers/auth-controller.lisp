; -*- mode: lisp -*-
(in-package #:cl-user)
(defpackage #:dogatto/controllers/auth-controller
  (:use #:cl)
  (:import-from #:clails/controller/base-controller
                #:<rest-controller>
                #:param
                #:env
                #:code
                #:header
                #:set-response)
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
  (:import-from #:clails/model
                #:ref)
  (:import-from #:uuid
                #:make-v4-uuid)
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

(defun user-to-json (user)
  "Convert user model to JSON-safe alist without password hash.

   @param user [<user>] User model instance
   @return [alist] User data as alist
   "
  `((:id . ,(ref user :id))
    (:username . ,(ref user :username))
    (:email . ,(ref user :email))
    (:ulid . ,(ref user :ulid))
    (:registration-status . ,(ref user :registration-status))
    (:created-at . ,(ref user :created-at))
    (:updated-at . ,(ref user :updated-at))))

(defun get-cookie-value (headers cookie-name)
  "Extract cookie value from request headers.

   @param headers [hash-table] Request headers
   @param cookie-name [string] Name of the cookie to extract
   @return [string] Cookie value if found
   @return [nil] If cookie not found
   "
  (let ((cookie-header (gethash "cookie" headers)))
    (when cookie-header
      (let* ((cookies (cl-ppcre:split ";\\s*" cookie-header))
             (target-cookie (find-if (lambda (c)
                                       (cl-ppcre:scan (format nil "^~A=" cookie-name) c))
                                     cookies)))
        (when target-cookie
          (cadr (cl-ppcre:split "=" target-cookie :limit 2)))))))

;; POST /api/v1/auth/register
(defmethod do-post ((controller <auth-register-controller>))
  "Register a new user.

   Expected parameters:
   - username: User's display name
   - email: User's email address
   - password: User's password (plain text, will be hashed)

   Returns 201 with user data on success, 400 on validation errors.
   "
  (let ((username (param controller "username"))
        (email (param controller "email"))
        (password (param controller "password")))
    
    ;; Input validation
    (cond
      ((or (null username) (string= username ""))
       (setf (slot-value controller 'clails/controller/base-controller:code) 400)
       (set-response controller
                     `((:status . "error")
                       (:message . "Username is required"))))
      
      ((or (null email) (string= email ""))
       (setf (slot-value controller 'clails/controller/base-controller:code) 400)
       (set-response controller
                     `((:status . "error")
                       (:message . "Email is required"))))
      
      ((or (null password) (string= password ""))
       (setf (slot-value controller 'clails/controller/base-controller:code) 400)
       (set-response controller
                     `((:status . "error")
                       (:message . "Password is required"))))
      
      ;; Check email duplication
      ((user-exists-p email)
       (setf (slot-value controller 'clails/controller/base-controller:code) 400)
       (set-response controller
                     `((:status . "error")
                       (:message . "Email already registered"))))
      
      ;; Validate password strength
      (t (multiple-value-bind (valid errors)
             (validate-password password)
           (if valid
               ;; Create user
               (let* ((password-hash (hash-password password))
                      (ulid (format nil "~(~a~)" (make-v4-uuid)))
                      (user (create-user :username username
                                         :email email
                                         :password-hash password-hash
                                         :ulid ulid)))
                 (if user
                     (progn
                       (setf (slot-value controller 'clails/controller/base-controller:code) 201)
                       (set-response controller
                                     `((:status . "success")
                                       (:data . ,(user-to-json user)))))
                     (progn
                       (setf (slot-value controller 'clails/controller/base-controller:code) 400)
                       (set-response controller
                                     `((:status . "error")
                                       (:message . "Failed to create user"))))))
               ;; Password validation failed
               (progn
                 (setf (slot-value controller 'clails/controller/base-controller:code) 400)
                 (set-response controller
                               `((:status . "error")
                                 (:message . "Password validation failed")
                                 (:errors . ,errors))))))))))

;; POST /api/v1/auth/login
(defmethod do-post ((controller <auth-login-controller>))
  "Authenticate user and create session.

   Expected parameters:
   - email: User's email address
   - password: User's password (plain text)

   Returns 200 with user data and sets session cookie on success,
   401 on authentication failure.
   "
  (let ((email (param controller "email"))
        (password (param controller "password")))
    
    ;; Input validation
    (cond
      ((or (null email) (string= email ""))
       (setf (slot-value controller 'clails/controller/base-controller:code) 400)
       (set-response controller
                     `((:status . "error")
                       (:message . "Email is required"))))
      
      ((or (null password) (string= password ""))
       (setf (slot-value controller 'clails/controller/base-controller:code) 400)
       (set-response controller
                     `((:status . "error")
                       (:message . "Password is required"))))
      
      (t
       (let ((user (find-user-by-email email)))
         (if (and user
                  (verify-password password (ref user :password-hash)))
             ;; Authentication success
             (let ((session-id (create-session (ref user :id))))
               ;; Set cookie
               (setf (slot-value controller 'clails/controller/base-controller:header)
                     `(:content-type "application/json"
                       :set-cookie ,(format nil "session_id=~A; Path=/; HttpOnly; SameSite=Strict; Max-Age=~D"
                                            session-id
                                            (* 7 24 60 60)))) ; 7 days
               (set-response controller
                             `((:status . "success")
                               (:data . ,(user-to-json user)))))
             ;; Authentication failed
             (progn
               (setf (slot-value controller 'clails/controller/base-controller:code) 401)
               (set-response controller
                             `((:status . "error")
                               (:message . "Invalid email or password"))))))))))

;; POST /api/v1/auth/logout
(defmethod do-post ((controller <auth-logout-controller>))
  "Logout user by deleting session.

   Reads session ID from cookie, deletes the session from Redis,
   and clears the cookie.

   Returns 200 on success.
   "
  (let* ((env-data (env controller))
         (headers (getf env-data :headers))
         (session-id (get-cookie-value headers "session_id")))
    
    (when session-id
      (delete-session session-id))
    
    ;; Clear cookie
    (setf (slot-value controller 'clails/controller/base-controller:header)
          `(:content-type "application/json"
            :set-cookie "session_id=; Path=/; HttpOnly; SameSite=Strict; Max-Age=0"))
    
    (set-response controller
                  `((:status . "success")
                    (:message . "Logged out successfully")))))

;; GET /api/v1/auth/me
(defmethod do-get ((controller <auth-me-controller>))
  "Get current authenticated user information.

   Reads session ID from cookie, retrieves user from database,
   and returns user information.

   Returns 200 with user data on success, 401 if not authenticated.
   "
  (let* ((env-data (env controller))
         (headers (getf env-data :headers))
         (session-id (get-cookie-value headers "session_id")))
    
    (if (and session-id (session-valid-p session-id))
        (let* ((session-data (get-session session-id))
               (user-id (getf session-data :user-id))
               (user (find-user-by-id user-id)))
          (if user
              (set-response controller
                            `((:status . "success")
                              (:data . ,(user-to-json user))))
              (progn
                (setf (slot-value controller 'clails/controller/base-controller:code) 401)
                (set-response controller
                              `((:status . "error")
                                (:message . "User not found"))))))
        (progn
          (setf (slot-value controller 'clails/controller/base-controller:code) 401)
          (set-response controller
                        `((:status . "error")
                          (:message . "Not authenticated")))))))

