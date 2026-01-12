; -*- mode: lisp -*-
(in-package #:dogatto-db)

(defmigration "20260112000600_add-foreign-key-and-search-indexes"
  (:up #'(lambda (connection)
           ;; users table indexes
           (add-index connection
                      :table "users"
                      :index "idx-users-email"
                      :columns '("email"))
           (add-index connection
                      :table "users"
                      :index "idx-users-username"
                      :columns '("username"))
           (add-index connection
                      :table "users"
                      :index "idx-users-github-id"
                      :columns '("github-id"))
           
           ;; todos table indexes
           (add-index connection
                      :table "todos"
                      :index "idx-todos-owner-id"
                      :columns '("owner-id"))
           (add-index connection
                      :table "todos"
                      :index "idx-todos-status"
                      :columns '("status"))
           (add-index connection
                      :table "todos"
                      :index "idx-todos-due-date"
                      :columns '("due-date"))
           (add-index connection
                      :table "todos"
                      :index "idx-todos-created-at"
                      :columns '("created-at"))
           (add-index connection
                      :table "todos"
                      :index "idx-todos-owner-status"
                      :columns '("owner-id" "status"))
           (add-index connection
                      :table "todos"
                      :index "idx-todos-owner-due-date"
                      :columns '("owner-id" "due-date"))
           
           ;; todo-comments table indexes
           (add-index connection
                      :table "todo-comments"
                      :index "idx-todo-comments-todo-id"
                      :columns '("todo-id"))
           (add-index connection
                      :table "todo-comments"
                      :index "idx-todo-comments-user-id"
                      :columns '("user-id"))
           (add-index connection
                      :table "todo-comments"
                      :index "idx-todo-comments-created-at"
                      :columns '("created-at"))
           (add-index connection
                      :table "todo-comments"
                      :index "idx-todo-comments-todo-created"
                      :columns '("todo-id" "created-at"))
           
           ;; tags table indexes
           (add-index connection
                      :table "tags"
                      :index "idx-tags-owner-id"
                      :columns '("owner-id"))
           (add-index connection
                      :table "tags"
                      :index "idx-tags-name"
                      :columns '("name"))
           (add-index connection
                      :table "tags"
                      :index "idx-tags-merged-to-ulid"
                      :columns '("merged-to-ulid"))
           (add-index connection
                      :table "tags"
                      :index "idx-tags-owner-name"
                      :columns '("owner-id" "name"))
           
           ;; todo-tags table indexes
           (add-index connection
                      :table "todo-tags"
                      :index "idx-todo-tags-todo-id"
                      :columns '("todo-id"))
           (add-index connection
                      :table "todo-tags"
                      :index "idx-todo-tags-tag-id"
                      :columns '("tag-id"))
           
           ;; labels table indexes
           (add-index connection
                      :table "labels"
                      :index "idx-labels-owner-id"
                      :columns '("owner-id"))
           (add-index connection
                      :table "labels"
                      :index "idx-labels-name"
                      :columns '("name"))
           (add-index connection
                      :table "labels"
                      :index "idx-labels-merged-to-ulid"
                      :columns '("merged-to-ulid"))
           (add-index connection
                      :table "labels"
                      :index "idx-labels-owner-name"
                      :columns '("owner-id" "name"))
           
           ;; label-tags table indexes
           (add-index connection
                      :table "label-tags"
                      :index "idx-label-tags-label-id"
                      :columns '("label-id"))
           (add-index connection
                      :table "label-tags"
                      :index "idx-label-tags-tag-id"
                      :columns '("tag-id")))
   :down #'(lambda (connection)
             ;; users table indexes
             (drop-index connection :table "users" :index "idx-users-email")
             (drop-index connection :table "users" :index "idx-users-username")
             (drop-index connection :table "users" :index "idx-users-github-id")
             
             ;; todos table indexes
             (drop-index connection :table "todos" :index "idx-todos-owner-id")
             (drop-index connection :table "todos" :index "idx-todos-status")
             (drop-index connection :table "todos" :index "idx-todos-due-date")
             (drop-index connection :table "todos" :index "idx-todos-created-at")
             (drop-index connection :table "todos" :index "idx-todos-owner-status")
             (drop-index connection :table "todos" :index "idx-todos-owner-due-date")
             
             ;; todo-comments table indexes
             (drop-index connection :table "todo-comments" :index "idx-todo-comments-todo-id")
             (drop-index connection :table "todo-comments" :index "idx-todo-comments-user-id")
             (drop-index connection :table "todo-comments" :index "idx-todo-comments-created-at")
             (drop-index connection :table "todo-comments" :index "idx-todo-comments-todo-created")
             
             ;; tags table indexes
             (drop-index connection :table "tags" :index "idx-tags-owner-id")
             (drop-index connection :table "tags" :index "idx-tags-name")
             (drop-index connection :table "tags" :index "idx-tags-merged-to-ulid")
             (drop-index connection :table "tags" :index "idx-tags-owner-name")
             
             ;; todo-tags table indexes
             (drop-index connection :table "todo-tags" :index "idx-todo-tags-todo-id")
             (drop-index connection :table "todo-tags" :index "idx-todo-tags-tag-id")
             
             ;; labels table indexes
             (drop-index connection :table "labels" :index "idx-labels-owner-id")
             (drop-index connection :table "labels" :index "idx-labels-name")
             (drop-index connection :table "labels" :index "idx-labels-merged-to-ulid")
             (drop-index connection :table "labels" :index "idx-labels-owner-name")
             
             ;; label-tags table indexes
             (drop-index connection :table "label-tags" :index "idx-label-tags-label-id")
             (drop-index connection :table "label-tags" :index "idx-label-tags-tag-id"))))
