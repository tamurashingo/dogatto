; -*- mode: lisp -*-
(in-package #:cl-user)
(defpackage #:dogatto/middleware/cors
  (:use #:cl)
  (:export #:cors-middleware
           #:*allowed-origins*
           #:*allowed-methods*
           #:*allowed-headers*))
(in-package #:dogatto/middleware/cors)

(defparameter *allowed-origins* 
  (clails/util:env-or-default "CORS_ALLOWED_ORIGINS" "*"))

(defparameter *allowed-methods* "GET, POST, PUT, DELETE, OPTIONS")

(defparameter *allowed-headers* "Content-Type, Authorization, X-CSRF-Token")

(defun cors-middleware (app)
  "CORS middleware for handling cross-origin requests"
  (lambda (env)
    (let* ((method (getf env :request-method))
           (origin (gethash "origin" (getf env :headers)))
           (cors-headers (list :access-control-allow-origin *allowed-origins*
                              :access-control-allow-methods *allowed-methods*
                              :access-control-allow-headers *allowed-headers*
                              :access-control-allow-credentials "true"
                              :access-control-max-age "3600")))
      
      ;; Handle preflight OPTIONS request
      (when (eq method :options)
        (return-from cors-middleware 
          (list 204 cors-headers '())))
      
      ;; Process request and add CORS headers to response
      (let ((response (funcall app env)))
        (list (first response)
              (append cors-headers (second response))
              (third response))))))
