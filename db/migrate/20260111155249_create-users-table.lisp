; -*- mode: lisp -*-
(in-package #:dogatto-db)

(defmigration "20260111155249_create-users-table"
  (:up #'(lambda (connection)
           (create-table connection
                         :table "users"
                         :columns '(("ulid" :type :string
                                            :size 26
                                            :not-null t
                                            :unique t)
                                    ("username" :type :string
                                                :size 255
                                                :not-null t
                                                :unique t)
                                    ("email" :type :string
                                             :size 255
                                             :not-null t
                                             :unique t)
                                    ("password-hash" :type :string
                                                     :size 255
                                                     :not-null nil)
                                    ("github-id" :type :string
                                                 :size 255
                                                 :not-null nil
                                                 :unique t)
                                    ("registration-status" :type :string
                                                           :size 20
                                                           :not-null t
                                                           :default-value "provisional")
                                    ("registration-token" :type :string
                                                          :size 255
                                                          :not-null nil)
                                    ("registration-token-expires-at" :type :datetime
                                                                     :not-null nil))))
   :down #'(lambda (connection)
             (drop-table connection :table "users"))))
