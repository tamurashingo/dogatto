; -*- mode: lisp -*-
(in-package #:dogatto-db)

(defmigration "20260209114321_add-merged-at-to-tags-and-labels"
  (:up #'(lambda (connection)
           ;; Add merged-at column to tags table
           (execute-sql connection
                        "ALTER TABLE tags ADD COLUMN `merged-at` BIGINT NULL
                         COMMENT 'Unix timestamp when tag was merged'")
           
           ;; Add index for merged-at in tags
           (execute-sql connection
                        "CREATE INDEX idx_tags_merged_at ON tags(`merged-at`)")
           
           ;; Add index for merged-to-ulid in tags
           (execute-sql connection
                        "CREATE INDEX idx_tags_merged_to_ulid ON tags(`merged-to-ulid`)")
           
           ;; Add merged-at column to labels table
           (execute-sql connection
                        "ALTER TABLE labels ADD COLUMN `merged-at` BIGINT NULL
                         COMMENT 'Unix timestamp when label was merged'")
           
           ;; Add index for merged-at in labels
           (execute-sql connection
                        "CREATE INDEX idx_labels_merged_at ON labels(`merged-at`)")
           
           ;; Add index for merged-to-ulid in labels
           (execute-sql connection
                        "CREATE INDEX idx_labels_merged_to_ulid ON labels(`merged-to-ulid`)")))
   :down #'(lambda (connection)
             ;; Drop indexes and columns for tags
             (execute-sql connection "DROP INDEX idx_tags_merged_to_ulid ON tags")
             (execute-sql connection "DROP INDEX idx_tags_merged_at ON tags")
             (execute-sql connection "ALTER TABLE tags DROP COLUMN `merged-at`")
             
             ;; Drop indexes and columns for labels
             (execute-sql connection "DROP INDEX idx_labels_merged_to_ulid ON labels")
             (execute-sql connection "DROP INDEX idx_labels_merged_at ON labels")
             (execute-sql connection "ALTER TABLE labels DROP COLUMN `merged-at`"))))
