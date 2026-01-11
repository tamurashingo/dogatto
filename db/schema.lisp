; -*- mode: lisp -*-
(in-package #:dogatto-db)

(lambda (connection)
  (create-table connection :table "label_tags"
                           :columns '(
                                      ("label-id" :TYPE :INTEGER :NOT-NULL T)
                                      ("tag-id" :TYPE :INTEGER :NOT-NULL T)
                                     )

  (create-table connection :table "todo_tags"
                           :columns '(
                                      ("todo-id" :TYPE :INTEGER :NOT-NULL T)
                                      ("tag-id" :TYPE :INTEGER :NOT-NULL T)
                                     )

  (create-table connection :table "labels"
                           :columns '(
                                      ("ulid" :TYPE :STRING :SIZE 26 :NOT-NULL T :UNIQUE T)
                                      ("owner-id" :TYPE :INTEGER :NOT-NULL T)
                                      ("name" :TYPE :STRING :SIZE 100 :NOT-NULL T)
                                      ("merged-to-ulid" :TYPE :STRING :SIZE 26 :NOT-NULL NIL)
                                     )

  (create-table connection :table "tags"
                           :columns '(
                                      ("ulid" :TYPE :STRING :SIZE 26 :NOT-NULL T :UNIQUE T)
                                      ("owner-id" :TYPE :INTEGER :NOT-NULL T)
                                      ("name" :TYPE :STRING :SIZE 100 :NOT-NULL T)
                                      ("color" :TYPE :STRING :SIZE 7 :NOT-NULL NIL)
                                      ("merged-to-ulid" :TYPE :STRING :SIZE 26 :NOT-NULL NIL)
                                     )

  (create-table connection :table "todo_comments"
                           :columns '(
                                      ("ulid" :TYPE :STRING :SIZE 26 :NOT-NULL T :UNIQUE T)
                                      ("todo-id" :TYPE :INTEGER :NOT-NULL T)
                                      ("user-id" :TYPE :INTEGER :NOT-NULL T)
                                      ("comment" :TYPE :TEXT :NOT-NULL T)
                                     )

  (create-table connection :table "todos"
                           :columns '(
                                      ("ulid" :TYPE :STRING :SIZE 26 :NOT-NULL T :UNIQUE T)
                                      ("owner-id" :TYPE :INTEGER :NOT-NULL T)
                                      ("title" :TYPE :STRING :SIZE 255 :NOT-NULL T)
                                      ("content" :TYPE :TEXT :NOT-NULL NIL)
                                      ("due-date" :TYPE :DATETIME :NOT-NULL NIL)
                                      ("status" :TYPE :STRING :SIZE 20 :NOT-NULL T :DEFAULT-VALUE "active")
                                     )

  (create-table connection :table "users"
                           :columns '(
                                      ("ulid" :TYPE :STRING :SIZE 26 :NOT-NULL T :UNIQUE T)
                                      ("username" :TYPE :STRING :SIZE 255 :NOT-NULL T :UNIQUE T)
                                      ("email" :TYPE :STRING :SIZE 255 :NOT-NULL T :UNIQUE T)
                                      ("password-hash" :TYPE :STRING :SIZE 255 :NOT-NULL NIL)
                                      ("github-id" :TYPE :STRING :SIZE 255 :NOT-NULL NIL :UNIQUE T)
                                      ("registration-status" :TYPE :STRING :SIZE 20 :NOT-NULL T :DEFAULT-VALUE
 "provisional")
                                      ("registration-token" :TYPE :STRING :SIZE 255 :NOT-NULL NIL)
                                      ("registration-token-expires-at" :TYPE :DATETIME :NOT-NULL NIL)
                                     )
  )
; ok
