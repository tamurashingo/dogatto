; -*- mode: lisp -*-
(in-package #:dogatto-db)

(defmigration "20260205070023_add-description-to-labels"
  (:up #'(lambda (connection)
           ;; Add description column to labels
           (add-column connection
                       :table "labels"
                       :columns '(("description" :type :text
                                                 :not-null nil))))
   
   :down #'(lambda (connection)
             ;; Drop description column
             (drop-column connection
                          :table "labels"
                          :column "description"))))
