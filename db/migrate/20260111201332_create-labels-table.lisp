; -*- mode: lisp -*-
(in-package #:dogatto-db)

(defmigration "20260111201332_create-labels-table"
  (:up #'(lambda (connection)
           (create-table connection
                         :table "labels"
                         :columns '(("ulid" :type :string
                                            :size 26
                                            :not-null t
                                            :unique t)
                                    ("owner-id" :type :integer
                                                :not-null t)
                                    ("name" :type :string
                                            :size 100
                                            :not-null t)
                                    ("merged-to-ulid" :type :string
                                                      :size 26
                                                      :not-null nil))))
   :down #'(lambda (connection)
             (drop-table connection :table "labels"))))
