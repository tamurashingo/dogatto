; -*- mode: lisp -*-
(in-package #:dogatto-db)

(defmigration "20260205071350_add-label-ulid-and-owner-id-to-label-tags"
  (:up #'(lambda (connection)
           ;; Add label-ulid column to label_tags
           (add-column connection
                       :table "label_tags"
                       :columns '(("label-ulid" :type :string
                                                :size 26
                                                :not-null t)))
           
           ;; Add owner-id column to label_tags
           (add-column connection
                       :table "label_tags"
                       :columns '(("owner-id" :type :integer
                                              :not-null t)))
           
           ;; Add index for label-ulid
           (add-index connection
                      :table "label_tags"
                      :index "idx-label-tags-label-ulid"
                      :columns '("label-ulid")))
   
   :down #'(lambda (connection)
             ;; Drop index
             (drop-index connection
                         :table "label_tags"
                         :index "idx-label-tags-label-ulid")
             
             ;; Drop columns
             (drop-column connection
                          :table "label_tags"
                          :column "owner-id")
             
             (drop-column connection
                          :table "label_tags"
                          :column "label-ulid"))))
