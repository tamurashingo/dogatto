; -*- mode: lisp -*-
(in-package #:cl-user)
(defpackage #:dogatto/middleware/authentication
  (:use #:cl)
  (:import-from #:dogatto/utils/session
                #:get-session
                #:session-valid-p)
  (:import-from #:dogatto/models/user
                #:find-user-by-id
                #:<user>)
  (:import-from #:clails/model
                #:ref)
  (:export #:require-authentication
           #:get-current-user))

(in-package #:dogatto/middleware/authentication)


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


(defun require-authentication (app)
  "Authentication middleware that requires valid session.

   This middleware:
   - Extracts session ID from cookie
   - Validates the session
   - Loads user information from database
   - Adds user information to env as :current-user
   - Returns 401 if session is invalid or user not found

   The downstream application can access the current user via:
   (getf env :current-user)

   @param app [function] The downstream application
   @return [function] Middleware function
   "
  (lambda (env)
    (let* ((headers (getf env :headers))
           (session-id (get-cookie-value headers "session_id")))
      
      (if (and session-id (session-valid-p session-id))
          ;; Session is valid, load user
          (let* ((session-data (get-session session-id))
                 (user-id (getf session-data :user-id))
                 (user (when user-id (find-user-by-id user-id))))
            
            (if user
                ;; User found, add to env and continue
                (progn
                  (setf (getf env :current-user) user)
                  (funcall app env))
                ;; User not found
                '(401
                  (:content-type "application/json")
                  ("{\"status\":\"error\",\"message\":\"User not found\"}"))))
          
          ;; Session invalid or missing
          '(401
            (:content-type "application/json")
            ("{\"status\":\"error\",\"message\":\"Not authenticated\"}"))))))


(defun get-current-user (env)
  "Helper function to get current authenticated user from env.

   @param env [plist] The request environment
   @return [<user>] The current user instance
   @return [nil] If no user is authenticated
   "
  (getf env :current-user))
