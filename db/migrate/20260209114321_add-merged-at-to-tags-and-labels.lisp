; -*- mode: lisp -*-
(in-package #:dogatto-db)

(defmigration "20260209114321_add-merged-at-to-tags-and-labels"
  (:up #'(lambda (connection)
           ;; Add merged-at column to tags table
           (add-column connection
                       :table "tags"
                       :columns '(("merged-at" :type :integer
                                               :not-null nil)))
           
           ;; Add index for merged-at in tags
           (add-index connection
                      :table "tags"
                      :index "idx-tags-merged-at"
                      :columns '("merged-at"))
           
           ;; Add index for merged-to-ulid in tags
           (add-index connection
                      :table "tags"
                      :index "idx-tags-merged-to-ulid"
                      :columns '("merged-to-ulid"))
           
           ;; Add merged-at column to labels table
           (add-column connection
                       :table "labels"
                       :columns '(("merged-at" :type :integer
                                               :not-null nil)))
           
           ;; Add index for merged-at in labels
           (add-index connection
                      :table "labels"
                      :index "idx-labels-merged-at"
                      :columns '("merged-at"))
           
           ;; Add index for merged-to-ulid in labels
           (add-index connection
                      :table "labels"
                      :index "idx-labels-merged-to-ulid"
                      :columns '("merged-to-ulid")))
   :down #'(lambda (connection)
             ;; Drop indexes and columns for tags
             (drop-index connection
                         :table "tags"
                         :index "idx-tags-merged-to-ulid")
             (drop-index connection
                         :table "tags"
                         :index "idx-tags-merged-at")
             (drop-column connection
                          :table "tags"
                          :column "merged-at")
             
             ;; Drop indexes and columns for labels
             (drop-index connection
                         :table "labels"
                         :index "idx-labels-merged-to-ulid")
             (drop-index connection
                         :table "labels"
                         :index "idx-labels-merged-at")
             (drop-column connection
                          :table "labels"
                          :column "merged-at"))))
