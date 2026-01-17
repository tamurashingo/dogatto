; -*- mode: lisp -*-
(in-package #:cl-user)
(defpackage #:dogatto/config/middleware
  (:use #:cl)
  (:import-from #:clails/middleware/core
                #:add-middleware-before)
  (:import-from #:dogatto/middleware/cors
                #:cors-middleware)
  (:import-from #:dogatto/middleware/session
                #:session-middleware)
  (:import-from #:dogatto/middleware/authentication
                #:require-authentication))

(in-package #:dogatto/config/middleware)

;; Register CORS middleware first
;; This handles cross-origin requests from frontend dev server
(add-middleware-before #'cors-middleware)

;; Register session middleware
;; This provides session management for all routes
(add-middleware-before #'session-middleware)

;; Note: Authentication middleware (require-authentication) is available
;; but not applied globally. Protected routes should implement authentication
;; checks in their controllers using (require-authentication app) wrapper
;; or by checking session validity explicitly.
