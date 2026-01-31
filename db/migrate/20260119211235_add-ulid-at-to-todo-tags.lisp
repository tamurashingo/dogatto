; -*- mode: lisp -*-
(in-package #:dogatto-db)

(defmigration "20260119211235_add-ulid-at-to-todo-tags"
  (:up #'(lambda (connection)
           (add-column connection
                       :table "todo-tags"
                       :columns '(("tag-ulid" :type :string
                                              :not-null t)))
           (add-index connection
                      :table "todo-tags" 
                      :index "idx-todo-tags-tag-ulid"
                      :columns '("tag-ulid")))
   :down #'(lambda (connection)
             (drop-column connection
                          :table "todo_tags"
                          :column "tag-ulid"))))
