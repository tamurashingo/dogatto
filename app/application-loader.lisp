; -*- mode: lisp -*-
(in-package #:cl-user)
(defpackage #:dogatto/application-loader
  (:use #:cl)
  (:import-from #:dogatto/config/environment)
  (:import-from #:dogatto/config/logger)
  (:import-from #:dogatto/config/database)
  (:import-from #:dogatto/config/redis)
  (:import-from #:dogatto/config/middleware)
  (:import-from #:dogatto/controllers/application-controller)
  (:import-from #:dogatto/views/package)
  (:import-from #:dogatto/helpers/asset-helper)
  (:import-from #:dogatto/utils/password)
  (:import-from #:dogatto/utils/session)
  (:import-from #:dogatto/utils/time-conversion)
  (:import-from #:dogatto/middleware/cors)
  (:import-from #:dogatto/middleware/authentication)
  (:import-from :dogatto/controllers/health-controller)
  (:import-from :dogatto/controllers/pages-controller)
  (:import-from :dogatto/views/pages/show/package)
  (:import-from :dogatto/models/user)
  (:import-from :dogatto/models/todo)
  (:import-from :dogatto/models/tag)
  (:import-from :dogatto/models/todo-tag)
  (:import-from :dogatto/models/label)
  (:import-from :dogatto/models/label-tag)
  (:import-from :dogatto/controllers/auth-controller
                #:<auth-register-controller>
                #:<auth-login-controller>
                #:<auth-logout-controller>
                #:<auth-me-controller>)
  (:import-from :dogatto/controllers/todos-controller
                #:<todos-list-controller>
                #:<todo-item-controller>
                #:<todo-complete-controller>)
  (:import-from :dogatto/controllers/tags-controller
                #:<tags-list-controller>
                #:<tag-item-controller>)
  (:import-from :dogatto/controllers/labels-controller
                #:<labels-list-controller>
                #:<label-item-controller>
                #:<label-estimate-controller>)
  (:import-from :dogatto/controllers/todo-tags-controller
                #:<todo-tags-controller>))
(in-package #:dogatto/application-loader)
