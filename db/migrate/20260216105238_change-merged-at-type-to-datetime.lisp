; -*- mode: lisp -*-
(in-package #:dogatto-db)

(defmigration "20260216105238_change-merged-at-type-to-datetime"
  (:up #'(lambda (connection)
           ;; Drop merged-at column from tags table
           (drop-column connection
                        :table "tags"
                        :column "merged-at")
           
           ;; Add merged-at column with datetime type to tags table
           (add-column connection
                       :table "tags"
                       :columns '(("merged-at" :type :datetime
                                               :not-null nil)))
           
           ;; Add index for merged-at in tags
           (add-index connection
                      :table "tags"
                      :index "idx-tags-merged-at"
                      :columns '("merged-at"))
           
           ;; Drop merged-at column from labels table
           (drop-column connection
                        :table "labels"
                        :column "merged-at")
           
           ;; Add merged-at column with datetime type to labels table
           (add-column connection
                       :table "labels"
                       :columns '(("merged-at" :type :datetime
                                               :not-null nil)))
           
           ;; Add index for merged-at in labels
           (add-index connection
                      :table "labels"
                      :index "idx-labels-merged-at"
                      :columns '("merged-at")))
   :down #'(lambda (connection)
             ;; Drop merged-at column from tags table
             (drop-column connection
                          :table "tags"
                          :column "merged-at")
             
             ;; Add merged-at column with integer type to tags table
             (add-column connection
                         :table "tags"
                         :columns '(("merged-at" :type :integer
                                                 :not-null nil)))
             
             ;; Add index for merged-at in tags
             (add-index connection
                        :table "tags"
                        :index "idx-tags-merged-at"
                        :columns '("merged-at"))
             
             ;; Drop merged-at column from labels table
             (drop-column connection
                          :table "labels"
                          :column "merged-at")
             
             ;; Add merged-at column with integer type to labels table
             (add-column connection
                         :table "labels"
                         :columns '(("merged-at" :type :integer
                                                 :not-null nil)))
             
             ;; Add index for merged-at in labels
             (add-index connection
                        :table "labels"
                        :index "idx-labels-merged-at"
                        :columns '("merged-at")))))
