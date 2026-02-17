; -*- mode: lisp -*-
(in-package #:cl-user)
(defpackage #:dogatto/controllers/todos-controller
  (:use #:cl
        #:clails/controller/base-controller)
  (:import-from #:dogatto/services/todo-service
                #:list-todos
                #:get-todo
                #:create-new-todo
                #:update-existing-todo
                #:delete-existing-todo
                #:toggle-todo-complete)
  (:import-from #:dogatto/helpers/auth-helper
                #:get-authenticated-user)
  (:import-from #:dogatto/helpers/json-converters
                #:todo-to-json)
  (:import-from #:dogatto/utils/time-conversion
                #:unix-time-to-universal-time)
  (:import-from #:clails/model
                #:ref)
  (:export #:<todos-list-controller>
           #:<todo-item-controller>
           #:<todo-complete-controller>))

(in-package #:dogatto/controllers/todos-controller)

(defclass <todos-list-controller> (<rest-controller>)
  ()
  (:documentation "Controller for todos collection (GET /todos, POST /todos)"))

(defclass <todo-item-controller> (<rest-controller>)
  ()
  (:documentation "Controller for single todo item (GET /todos/:id, PUT /todos/:id, DELETE /todos/:id)"))

(defclass <todo-complete-controller> (<rest-controller>)
  ()
  (:documentation "Controller for completing a todo (POST /todos/:id/complete)"))

;; GET /api/v1/todos
(defmethod do-get ((controller <todos-list-controller>))
  "Get all todos for the authenticated user with optional filtering."
  (let ((user (get-authenticated-user (env controller))))
    (unless user
      (setf (slot-value controller 'clails/controller/base-controller:code) 401)
      (return-from do-get
        (set-response controller
                     `(("status" . "error")
                       ("message" . "Authentication required")))))
    
    (let* ((user-id (ref user :id))
           (tags-param (param controller "tags"))
           (label-param (param controller "label"))
           (status-param (param controller "status"))
           (untagged-param (param controller "untagged"))
           (tag-ulids (when tags-param
                        (if (listp tags-param)
                            tags-param
                            (cl-ppcre:split "," tags-param))))
           (untagged (or (string= untagged-param "true")
                         (string= untagged-param "1")))
           (result (list-todos user-id
                              :tag-ulids tag-ulids
                              :label-ulid label-param
                              :status status-param
                              :untagged untagged)))
      
      (if (getf result :success)
          (let* ((todos (getf result :todos))
                 (todos-json (mapcar #'(lambda (todo) (todo-to-json todo :owner-id user-id)) todos)))
            (setf (slot-value controller 'clails/controller/base-controller:code) 200)
            (set-response controller
                         `(("status" . "success")
                           ("data" . (("todos" . ,todos-json))))))
          (progn
            (setf (slot-value controller 'clails/controller/base-controller:code) 500)
            (set-response controller
                         `(("status" . "error")
                           ("message" . ,(car (getf result :errors))))))))))

;; POST /api/v1/todos
(defmethod do-post ((controller <todos-list-controller>))
  "Create a new todo for the authenticated user."
  (let ((user (get-authenticated-user (env controller))))
    (unless user
      (setf (slot-value controller 'clails/controller/base-controller:code) 401)
      (return-from do-post
        (set-response controller
                     `(("status" . "error")
                       ("message" . "Authentication required")))))
    
    (let* ((title (param controller "title"))
           (content (param controller "content"))
           (due-date-raw (param controller "dueDate"))
           (due-date (cond
                       ((null due-date-raw) nil)
                       ((numberp due-date-raw) (unix-time-to-universal-time due-date-raw))
                       ((stringp due-date-raw) 
                        (let ((unix-time (parse-integer due-date-raw :junk-allowed t)))
                          (when unix-time (unix-time-to-universal-time unix-time))))
                       (t nil)))
           (result (create-new-todo (ref user :id) title
                                   :content content
                                   :due-date due-date)))
      
      (if (getf result :success)
          (progn
            (setf (slot-value controller 'clails/controller/base-controller:code) 201)
            (set-response controller
                         `(("status" . "success")
                           ("data" . (("todo" . ,(todo-to-json (getf result :todo) :owner-id (ref user :id))))))))
          (progn
            (setf (slot-value controller 'clails/controller/base-controller:code) 400)
            (set-response controller
                         `(("status" . "error")
                           ("message" . ,(car (getf result :errors))))))))))

;; GET /api/v1/todos/:id
(defmethod do-get ((controller <todo-item-controller>))
  "Get a specific todo by ULID."
  (let ((user (get-authenticated-user (env controller))))
    (unless user
      (setf (slot-value controller 'clails/controller/base-controller:code) 401)
      (return-from do-get
        (set-response controller
                     `(("status" . "error")
                       ("message" . "Authentication required")))))
    
    (let* ((todo-ulid (param controller "id"))
           (result (get-todo todo-ulid (ref user :id))))
      
      (if (getf result :success)
          (progn
            (setf (slot-value controller 'clails/controller/base-controller:code) 200)
            (set-response controller
                         `(("status" . "success")
                           ("data" . (("todo" . ,(todo-to-json (getf result :todo) :owner-id (ref user :id))))))))
          (progn
            (setf (slot-value controller 'clails/controller/base-controller:code) 404)
            (set-response controller
                         `(("status" . "error")
                           ("message" . ,(car (getf result :errors))))))))))

;; PUT /api/v1/todos/:id
(defmethod do-put ((controller <todo-item-controller>))
  "Update an existing todo."
  (let ((user (get-authenticated-user (env controller))))
    (unless user
      (setf (slot-value controller 'clails/controller/base-controller:code) 401)
      (return-from do-put
        (set-response controller
                     `(("status" . "error")
                       ("message" . "Authentication required")))))
    
    (let* ((todo-ulid (param controller "id"))
           (title (param controller "title"))
           (content (param controller "content"))
           (due-date-raw (param controller "dueDate"))
           (due-date (cond
                       ((null due-date-raw) nil)
                       ((numberp due-date-raw) (unix-time-to-universal-time due-date-raw))
                       ((stringp due-date-raw)
                        (let ((unix-time (parse-integer due-date-raw :junk-allowed t)))
                          (when unix-time (unix-time-to-universal-time unix-time))))
                       (t nil)))
           (result (update-existing-todo todo-ulid (ref user :id)
                                        :title title
                                        :content content
                                        :due-date due-date)))
      
      (if (getf result :success)
          (progn
            (setf (slot-value controller 'clails/controller/base-controller:code) 200)
            (set-response controller
                         `(("status" . "success")
                           ("data" . (("todo" . ,(todo-to-json (getf result :todo) :owner-id (ref user :id))))))))
          (progn
            (setf (slot-value controller 'clails/controller/base-controller:code) 404)
            (set-response controller
                         `(("status" . "error")
                           ("message" . ,(car (getf result :errors))))))))))

;; DELETE /api/v1/todos/:id
(defmethod do-delete ((controller <todo-item-controller>))
  "Delete a todo."
  (let ((user (get-authenticated-user (env controller))))
    (unless user
      (setf (slot-value controller 'clails/controller/base-controller:code) 401)
      (return-from do-delete
        (set-response controller
                     `(("status" . "error")
                       ("message" . "Authentication required")))))
    
    (let* ((todo-ulid (param controller "id"))
           (result (delete-existing-todo todo-ulid (ref user :id))))
      
      (if (getf result :success)
          (progn
            (setf (slot-value controller 'clails/controller/base-controller:code) 200)
            (set-response controller
                         `(("status" . "success")
                           ("message" . "TODO deleted successfully"))))
          (progn
            (setf (slot-value controller 'clails/controller/base-controller:code) 404)
            (set-response controller
                         `(("status" . "error")
                           ("message" . ,(car (getf result :errors))))))))))

;; PUT /api/v1/todos/:id/complete
(defmethod do-put ((controller <todo-complete-controller>))
  "Mark a todo as completed or toggle completion status."
  (let ((user (get-authenticated-user (env controller))))
    (unless user
      (setf (slot-value controller 'clails/controller/base-controller:code) 401)
      (return-from do-put
        (set-response controller
                     `(("status" . "error")
                       ("message" . "Authentication required")))))
    
    (let* ((todo-ulid (param controller "id"))
           (result (toggle-todo-complete todo-ulid (ref user :id))))
      
      (if (getf result :success)
          (progn
            (setf (slot-value controller 'clails/controller/base-controller:code) 200)
            (set-response controller
                         `(("status" . "success")
                           ("data" . (("todo" . ,(todo-to-json (getf result :todo) :owner-id (ref user :id))))))))
          (progn
            (setf (slot-value controller 'clails/controller/base-controller:code) 404)
            (set-response controller
                         `(("status" . "error")
                           ("message" . ,(car (getf result :errors))))))))))
