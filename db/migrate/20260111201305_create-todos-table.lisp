; -*- mode: lisp -*-
(in-package #:dogatto-db)

(defmigration "20260111201305_create-todos-table"
  (:up #'(lambda (connection)
           (create-table connection
                         :table "todos"
                         :columns '(("ulid" :type :string
                                            :size 26
                                            :not-null t
                                            :unique t)
                                    ("owner-id" :type :bigint
                                                :not-null t)
                                    ("title" :type :string
                                             :size 255
                                             :not-null t)
                                    ("content" :type :text
                                               :not-null nil)
                                    ("due-date" :type :datetime
                                                :not-null nil)
                                    ("status" :type :string
                                              :size 20
                                              :not-null t
                                              :default-value "active"))))
   :down #'(lambda (connection)
             (drop-table connection :table "todos"))))
