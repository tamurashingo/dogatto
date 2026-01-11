; -*- mode: lisp -*-
(in-package #:dogatto-db)

(defmigration "20260111201336_create-label-tags-table"
  (:up #'(lambda (connection)
           (create-table connection
                         :table "label_tags"
                         :columns '(("label-id" :type :bigint
                                                :not-null t)
                                    ("tag-id" :type :bigint
                                              :not-null t))))
   :down #'(lambda (connection)
             (drop-table connection :table "label_tags"))))
