; -*- mode: lisp -*-
(in-package #:cl-user)
(defpackage #:dogatto/helpers/auth-helper
  (:use #:cl)
  (:import-from #:cl-ppcre
                #:split
                #:scan)
  (:import-from #:dogatto/utils/session
                #:get-session
                #:session-valid-p)
  (:import-from #:dogatto/models/user
                #:find-user-by-id)
  (:export #:get-authenticated-user
           #:get-cookie-value))

(in-package #:dogatto/helpers/auth-helper)

(defun get-cookie-value (headers cookie-name)
  "Extract cookie value from request headers.

   Parses the Cookie header and extracts the value for the specified cookie name.

   @param headers [hash-table] Request headers hash table
   @param cookie-name [string] Name of the cookie to extract
   @return [string] Cookie value if found
   @return [nil] If cookie header not present or cookie not found
   "
  (let ((cookie-header (gethash "cookie" headers)))
    (when cookie-header
      (let* ((cookies (cl-ppcre:split ";\\s*" cookie-header))
             (target-cookie (find-if (lambda (c)
                                       (cl-ppcre:scan (format nil "^~A=" cookie-name) c))
                                     cookies)))
        (when target-cookie
          (cadr (cl-ppcre:split "=" target-cookie :limit 2)))))))

(defun get-authenticated-user (env)
  "Get authenticated user from session.

   Extracts session ID from cookies, validates the session, and returns
   the authenticated user if valid.

   @param env [plist] Request environment containing :headers
   @return [<user>] User instance if authenticated
   @return [nil] If session_id cookie not found, session invalid, or user not found
   "
  (let* ((headers (getf env :headers))
         (session-id (get-cookie-value headers "session_id")))
    (when (and session-id (session-valid-p session-id))
      (let* ((session-data (get-session session-id))
             (user-id (getf session-data :user-id)))
        (when user-id
          (find-user-by-id user-id))))))
