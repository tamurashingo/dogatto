; -*- mode: lisp -*-
(in-package #:cl-user)
(defpackage #:dogatto/config/redis
  (:use #:cl)
  (:import-from #:clails/util
                #:env-or-default)
  (:export #:*redis-host*
           #:*redis-port*
           #:get-redis-connection))
(in-package #:dogatto/config/redis)

(defparameter *redis-host* (env-or-default "REDIS_HOST" "localhost"))
(defparameter *redis-port* (parse-integer (env-or-default "REDIS_PORT" "6379")))

(defun get-redis-connection ()
  "Returns Redis connection parameters"
  (list :host *redis-host* :port *redis-port*))
