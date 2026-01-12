; -*- mode: lisp -*-
(in-package #:cl-user)
(defpackage #:dogatto/controllers/pages-controller
  (:use #:cl
        #:clails/controller/base-controller)
  (:export #:<pages-controller>))

(in-package #:dogatto/controllers/pages-controller)

(defclass <pages-controller> (<web-controller>)
  ())


(defmethod do-get ((controller <pages-controller>))
  (set-view controller "pages/show/show.html"))

;(defmethod do-post ((controller <pages-controller>))
;  (set-view controller "pages/new.html"))

;(defmethod do-put ((controller <pages-controller>))
;  (set-view controller "pages/edit.html"))

;(defmethod do-delete ((controller <pages-controller>))
;  (set-view controller "pages/delete.html"))

