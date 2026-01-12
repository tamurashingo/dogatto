; -*- mode: lisp -*-
(in-package #:dogatto-db)

(defmigration "20260112002024_add-completed-at-to-todos"
  (:up #'(lambda (connection)
           (add-column connection
                       :table "todos"
                       :columns '(("completed-at" :type :datetime
                                                   :not-null nil))))
   :down #'(lambda (connection)
             (drop-column connection
                          :table "todos"
                          :column "completed-at"))))
