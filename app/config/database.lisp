; -*- mode: lisp -*-
(in-package #:cl-user)
(defpackage #:dogatto/config/database
  (:use #:cl)
  (:import-from #:clails/environment
                #:*project-environment*)
  (:import-from #:clails/util
                #:env-or-default
                #:env)
  (:import-from #:clails/model/impl/mysql)
  (:export #:initialize-database-config))
(in-package #:dogatto/config/database)

(defun initialize-database-config ()
  (setf clails/environment:*database-config*
        `(:database :mysql
          :develop (:database-name ,(env-or-default "CLAILS_DB_NAME_DEVELOPMENT" "dogatto_development")
                    :host ,(env-or-default "CLAILS_DB_HOST_DEVELOPMENT" "localhost")
                    :port ,(env-or-default "CLAILS_DB_PORT_DEVELOPMENT" "3306")
                    :username ,(env-or-default "CLAILS_DB_USERNAME_DEVELOPMENT" "dogatto")
                    :password ,(env-or-default "CLAILS_DB_PASSWORD_DEVELOPMENT" "password")
                    ;; Connection pool parameters (optional)
                    ;; :initial-size 10
                    ;; :max-size 10
                    ;; :checkout-timeout 30
                    ;; :idle-timeout 600
                    ;; :max-lifetime 1800
                    ;; :keepalive-interval 600
                    ;; :validation-query "SELECT 1"
                    ;; :reaper-interval 60
                    )
          :test (:database-name ,(env-or-default "CLAILS_DB_NAME_TEST" "dogatto_test")
                 :host ,(env-or-default "CLAILS_DB_HOST_TEST" "localhost")
                 :port ,(env-or-default "CLAILS_DB_PORT_TEST" "3306")
                 :username ,(env-or-default "CLAILS_DB_USERNAME_TEST" "dogatto")
                 :password ,(env-or-default "CLAILS_DB_PASSWORD_TEST" "password")
                 ;; Connection pool parameters (optional)
                 ;; :initial-size 10
                 ;; :max-size 10
                 ;; :checkout-timeout 30
                 ;; :idle-timeout 600
                 ;; :max-lifetime 1800
                 ;; :keepalive-interval 600
                 ;; :validation-query "SELECT 1"
                 ;; :reaper-interval 60
                 )
          :production (:database-name ,(env "CLAILS_DB_NAME_PRODUCTION")
                       :host ,(env "CLAILS_DB_HOST_PRODUCTION")
                       :port ,(env "CLAILS_DB_PORT_PRODUCTION")
                       :username ,(env "CLAILS_DB_USERNAME_PRODUCTION")
                       :password ,(env "CLAILS_DB_PASSWORD_PRODUCTION")
                       ;; Connection pool parameters (optional)
                       ;; :initial-size 10
                       ;; :max-size 10
                       ;; :checkout-timeout 30
                       ;; :idle-timeout 600
                       ;; :max-lifetime 1800
                       ;; :keepalive-interval 600
                       ;; :validation-query "SELECT 1"
                       ;; :reaper-interval 60
                       ))))

(setf clails/environment:*database-type*
      (make-instance 'clails/environment::<database-type-mysql>))
