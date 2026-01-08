; -*- mode: lisp -*-
(in-package #:cl-user)
(defpackage #:dogatto/views/package
  (:use #:cl)
  (:import-from #:clails/view/view-helper
                #:*view-context*
                #:view)
  (:import-from #:dogatto/controllers/application-controller
                #:lisp-type
                #:lisp-version))

(in-package #:dogatto/views/package)