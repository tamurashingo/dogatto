; -*- mode: lisp -*-
(in-package #:cl-user)
(defpackage #:dogatto/middleware/csrf
  (:use #:cl)
  (:import-from #:dogatto/utils/error-response
                #:forbidden-error)
  (:export #:csrf-middleware
           #:generate-csrf-token
           #:validate-csrf-token))
(in-package #:dogatto/middleware/csrf)

(defparameter *csrf-token-length* 
  (parse-integer (clails/util:env-or-default "CSRF_TOKEN_LENGTH" "32")))

(defun generate-random-string (length)
  "Generate random string for CSRF token"
  (let ((chars "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"))
    (coerce (loop repeat length
                  collect (char chars (random (length chars))))
            'string)))

(defun generate-csrf-token ()
  "Generate a new CSRF token"
  (generate-random-string *csrf-token-length*))

(defun validate-csrf-token (token session-token)
  "Validate CSRF token against session token"
  (and token session-token (string= token session-token)))

(defun csrf-middleware (app)
  "CSRF protection middleware"
  (lambda (env)
    (let* ((method (getf env :request-method))
           (session (getf env :clack.session))
           (safe-methods '(:get :head :options)))
      
      ;; Safe methods don't need CSRF validation
      (when (member method safe-methods)
        (return-from csrf-middleware (funcall app env)))
      
      ;; Validate CSRF token for unsafe methods
      (let ((csrf-token (gethash "csrf-token" (getf env :headers)))
            (session-token (gethash :csrf-token session)))
        
        (unless (validate-csrf-token csrf-token session-token)
          (return-from csrf-middleware 
            (list 403 
                  '(:content-type "application/json")
                  (list (json:encode-json-to-string 
                         (forbidden-error "Invalid CSRF token"))))))
        
        (funcall app env)))))
