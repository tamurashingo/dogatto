; -*- mode: lisp -*-
(in-package #:cl-user)
(defpackage #:dogatto-db
  (:use #:cl
        #:clails/model)
  (:import-from #:clails/model/migration
                #:defmigration
                #:create-table
                #:add-column
                #:add-index
                #:drop-table
                #:drop-table
                #:drop-column
                #:drop-index))
(in-package #:dogatto-db)
