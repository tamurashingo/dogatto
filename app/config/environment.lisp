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
;; API routes
(setf clails/environment:*routing-tables*
  '((:path "/health"
     :controller "dogatto/controllers/health-controller:<health-controller>")
    (:path "/"
     :controller "dogatto/controllers/pages-controller:<pages-controller>")))

;; startup hooks
(push "dogatto/config/logger:initialize-logger" clails/environment:*startup-hooks*)
(push "clails/model/base-model:initialize-table-information" clails/environment:*startup-hooks*)

;; shutdown hooks
(push "dogatto/config/logger:finalize-logger" clails/environment:*shutdown-hooks*)
