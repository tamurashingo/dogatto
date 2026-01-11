; -*- mode: lisp -*-
(in-package #:cl-user)
(defpackage #:dogatto/routes/health
  (:use #:cl)
  (:export #:health-check))
(in-package #:dogatto/routes/health)

(defun health-check (env)
  "Health check endpoint for container monitoring"
  (declare (ignore env))
  (list 200 
        '(:content-type "application/json")
        (list (json:encode-json-to-string 
               (list :|status| "healthy"
                     :|timestamp| (get-universal-time))))))
