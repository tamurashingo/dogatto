; -*- mode: lisp -*-
(in-package #:dogatto-db)

(defmigration "20260111201331_create-tags-table"
  (:up #'(lambda (connection)
           (create-table connection
                         :table "tags"
                         :columns '(("ulid" :type :string
                                            :size 26
                                            :not-null t
                                            :unique t)
                                    ("owner-id" :type :bigint
                                                :not-null t)
                                    ("name" :type :string
                                            :size 100
                                            :not-null t)
                                    ("color" :type :string
                                             :size 7
                                             :not-null nil)
                                    ("merged-to-ulid" :type :string
                                                      :size 26
                                                      :not-null nil))))
   :down #'(lambda (connection)
             (drop-table connection :table "tags"))))
