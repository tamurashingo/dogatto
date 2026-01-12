; -*- mode: lisp -*-
(in-package #:dogatto-db)

(defmigration "20260111232543_add-ulid-indexes"
  (:up #'(lambda (connection)
           ;; Add ULID indexes for all main tables
           (add-index connection
                      :table "users"
                      :index "idx-users-ulid"
                      :columns '("ulid"))
           (add-index connection
                      :table "todos"
                      :index "idx-todos-ulid"
                      :columns '("ulid"))
           (add-index connection
                      :table "todo-comments"
                      :index "idx-todo-comments-ulid"
                      :columns '("ulid"))
           (add-index connection
                      :table "tags"
                      :index "idx-tags-ulid"
                      :columns '("ulid"))
           (add-index connection
                      :table "labels"
                      :index "idx-labels-ulid"
                      :columns '("ulid")))
   :down #'(lambda (connection)
             ;; Drop ULID indexes
             (drop-index connection
                         :table "users"
                         :index "idx-users-ulid")
             (drop-index connection
                         :table "todos"
                         :index "idx-todos-ulid")
             (drop-index connection
                         :table "todo-comments"
                         :index "idx-todo-comments-ulid")
             (drop-index connection
                         :table "tags"
                         :index "idx-tags-ulid")
             (drop-index connection
                         :table "labels"
                         :index "idx-labels-ulid"))))
