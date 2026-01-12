; -*- mode: lisp -*-
(in-package #:dogatto-db)

(defmigration "20260111201329_create-todo-comments-table"
  (:up #'(lambda (connection)
           (create-table connection
                         :table "todo_comments"
                         :columns '(("ulid" :type :string
                                            :size 26
                                            :not-null t
                                            :unique t)
                                    ("todo-id" :type :integer
                                               :not-null t)
                                    ("user-id" :type :integer
                                               :not-null t)
                                    ("comment" :type :text
                                               :not-null t))))
   :down #'(lambda (connection)
             (drop-table connection :table "todo_comments"))))
