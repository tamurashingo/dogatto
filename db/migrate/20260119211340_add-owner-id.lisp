; -*- mode: lisp -*-
(in-package #:dogatto-db)

(defmigration "20260119211340_add-owner-id"
  (:up #'(lambda (connection)
           (add-column connection
                       :table "todo-tags"
                       :columns '(("owner-id" :type :integer
                                              :not-null t)))
           (add-index connection
                      :table "todo-tags" 
                      :index "idx-todo-tags-tag-owner-id"
                      :columns '("owner-id")))
   :down #'(lambda (connection)
             (drop-column connection
                          :table "todo-tags"
                          :column "owner-id"))))
