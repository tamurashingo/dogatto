; -*- mode: lisp -*-
(in-package #:cl-user)
(defpackage #:dogatto/config/environment
  (:use #:cl)
  (:import-from #:clails/environment
                #:*project-environment*
                #:*routing-tables*))

(in-package #:dogatto/config/environment)

;; project name
(setf clails/environment:*project-name* "dogatto")

;; project environment
(setf clails/environment:*project-environment* :develop)

;; routing tables
(setf clails/environment:*routing-tables*
  '((:path "/health"
     :controller "dogatto/controllers/health-controller:<health-controller>")
    ;; Authentication endpoints (must be before wildcard)
    (:path "/api/v1/auth/register"
     :controller "dogatto/controllers/auth-controller:<auth-register-controller>")
    (:path "/api/v1/auth/login"
     :controller "dogatto/controllers/auth-controller:<auth-login-controller>")
    (:path "/api/v1/auth/logout"
     :controller "dogatto/controllers/auth-controller:<auth-logout-controller>")
    (:path "/api/v1/auth/me"
     :controller "dogatto/controllers/auth-controller:<auth-me-controller>")
    ;; TODO endpoints
    (:path "/api/v1/todos"
     :controller "dogatto/controllers/todos-controller:<todos-list-controller>")
    (:path "/api/v1/todos/:id"
     :controller "dogatto/controllers/todos-controller:<todo-item-controller>")
    ;; SPA wildcard route (must be last)
    ;; All non-API routes return the same HTML for client-side routing
    (:path "/*"
     :controller "dogatto/controllers/pages-controller:<pages-controller>"
     :scanner "^/.*$")))

;; startup hooks
(push "dogatto/config/logger:initialize-logger" clails/environment:*startup-hooks*)
(push "clails/model/base-model:initialize-table-information" clails/environment:*startup-hooks*)

;; shutdown hooks
(push "dogatto/config/logger:finalize-logger" clails/environment:*shutdown-hooks*)
