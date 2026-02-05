; -*- mode: lisp -*-
(in-package #:dogatto-db)

(defmigration "20260205055053_alter-label-tags-for-phase5"
  (:up #'(lambda (connection)
           ;; Add UNIQUE index for (label_id, tag_id)
           (add-index connection
                      :table "label-tags"
                      :index "idx-label-tags-unique"
                      :unique t
                      :columns '("label-id" "tag-id")))
   
   :down #'(lambda (connection)
             ;; Drop UNIQUE index
             (drop-index connection
                         :table "label-tags"
                         :index "idx-label-tags-unique"))))
