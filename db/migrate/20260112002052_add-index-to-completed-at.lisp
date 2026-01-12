; -*- mode: lisp -*-
(in-package #:dogatto-db)

(defmigration "20260112002052_add-index-to-completed-at"
  (:up #'(lambda (connection)
           (add-index connection
                      :table "todos"
                      :index "idx-todos-completed-at"
                      :columns '("completed-at")))
   :down #'(lambda (connection)
             (drop-index connection
                         :table "todos"
                         :index "idx-todos-completed-at"))))
