; -*- mode: lisp -*-
(in-package #:dogatto-db)

(defmigration "20260111201334_create-todo-tags-table"
  (:up #'(lambda (connection)
           (create-table connection
                         :table "todo_tags"
                         :columns '(("todo-id" :type :bigint
                                               :not-null t)
                                    ("tag-id" :type :bigint
                                              :not-null t))))
   :down #'(lambda (connection)
             (drop-table connection :table "todo_tags"))))
