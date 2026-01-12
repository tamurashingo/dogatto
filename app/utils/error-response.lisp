; -*- mode: lisp -*-
(in-package #:cl-user)
(defpackage #:dogatto/utils/error-response
  (:use #:cl)
  (:export #:make-error-response
           #:validation-error
           #:not-found-error
           #:unauthorized-error
           #:forbidden-error
           #:conflict-error
           #:internal-server-error))
(in-package #:dogatto/utils/error-response)

(defun make-error-response (status code message &key details)
  "Create standardized API error response
   
   Parameters:
   - status: HTTP status code (integer)
   - code: Error code string (e.g., 'VALIDATION_ERROR')
   - message: Human-readable error message
   - details: Optional additional error details (list or plist)"
  (let ((response (list :|status| status
                       :|error| (list :|code| code
                                     :|message| message))))
    (when details
      (setf (getf (getf response :|error|) :|details|) details))
    response))

(defun validation-error (message &key details)
  "Create validation error response (400)"
  (make-error-response 400 "VALIDATION_ERROR" message :details details))

(defun not-found-error (message &key details)
  "Create not found error response (404)"
  (make-error-response 404 "NOT_FOUND" message :details details))

(defun unauthorized-error (message &key details)
  "Create unauthorized error response (401)"
  (make-error-response 401 "UNAUTHORIZED" message :details details))

(defun forbidden-error (message &key details)
  "Create forbidden error response (403)"
  (make-error-response 403 "FORBIDDEN" message :details details))

(defun conflict-error (message &key details)
  "Create conflict error response (409)"
  (make-error-response 409 "CONFLICT" message :details details))

(defun internal-server-error (message &key details)
  "Create internal server error response (500)"
  (make-error-response 500 "INTERNAL_SERVER_ERROR" message :details details))
