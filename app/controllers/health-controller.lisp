; -*- mode: lisp -*-
(in-package #:cl-user)
(defpackage #:dogatto/controllers/health-controller
  (:use #:cl
        #:clails/controller/base-controller)
  (:export #:<health-controller>))

(in-package #:dogatto/controllers/health-controller)

(defclass <health-controller> (<rest-controller>)
  ())

(defmethod do-get ((controller <health-controller>))
  (set-response controller
                `((:status . "healthy")
                  (:timestamp . ,(get-universal-time)))))

;(defmethod do-post ((controller <health-controller>))
;  (set-view controller "health/new.html"))

;(defmethod do-put ((controller <health-controller>))
;  (set-view controller "health/edit.html"))

;(defmethod do-delete ((controller <health-controller>))
;  (set-view controller "health/delete.html"))

