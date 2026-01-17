; -*- mode: lisp -*-
(in-package #:cl-user)
(defpackage #:dogatto/controllers/todos-controller
  (:use #:cl
        #:clails/controller/base-controller)
  (:import-from #:dogatto/models/todo
                #:create-todo
                #:find-todo-by-id
                #:find-todos-by-user
                #:update-todo
                #:delete-todo
                #:<todo>)
  (:import-from #:dogatto/middleware/authentication
                #:get-current-user)
  (:import-from #:clails/model
                #:ref)
  (:import-from #:jonathan
                #:to-json)
  (:export #:<todos-list-controller>
           #:<todo-item-controller>))

(in-package #:dogatto/controllers/todos-controller)

(defclass <todos-list-controller> (<rest-controller>)
  ()
  (:documentation "Controller for todos collection (GET /todos, POST /todos)"))

(defclass <todo-item-controller> (<rest-controller>)
  ()
  (:documentation "Controller for single todo item (GET /todos/:id, PUT /todos/:id, DELETE /todos/:id)"))

(defun todo-to-json (todo)
  "Convert todo model to JSON-safe alist.

   @param todo [<todo>] TODO model instance
   @return [list] Alist representation of TODO
   "
  (list (cons "id" (ref todo :id))
        (cons "ulid" (ref todo :ulid))
        (cons "ownerId" (ref todo :owner-id))
        (cons "title" (ref todo :title))
        (cons "content" (ref todo :content))
        (cons "dueDate" (ref todo :due-date))
        (cons "status" (ref todo :status))
        (cons "completedAt" (ref todo :completed-at))
        (cons "createdAt" (ref todo :created-at))
        (cons "updatedAt" (ref todo :updated-at))))

(defmethod do-get ((controller <todos-list-controller>))
  "Get all todos for the authenticated user.

   Returns JSON array of todos.

   @param controller [<todos-list-controller>] Controller instance
   @return [list] HTTP response via set-response
   "
  (let ((user (get-current-user (env controller))))
    (unless user
      (setf (slot-value controller 'clails/controller/base-controller:code) 401)
      (return-from do-get
        (set-response controller
                     `(("status" . "error")
                       ("message" . "Authentication required")))))
    
    (let* ((user-id (ref user :id))
           (todos (find-todos-by-user user-id))
           (todos-json (mapcar #'todo-to-json todos)))
      (setf (slot-value controller 'clails/controller/base-controller:code) 200)
      (set-response controller
                   `(("status" . "success")
                     ("data" . (("todos" . ,todos-json))))))))

(defmethod do-post ((controller <todos-list-controller>))
  "Create a new todo for the authenticated user.

   Expects JSON body with: title (required), content (optional), dueDate (optional)

   @param controller [<todos-list-controller>] Controller instance
   @return [list] HTTP response via set-response
   "
  (let ((user (get-current-user (env controller))))
    (unless user
      (setf (slot-value controller 'clails/controller/base-controller:code) 401)
      (return-from do-post
        (set-response controller
                     `(("status" . "error")
                       ("message" . "Authentication required")))))
    
    (let* ((title (param controller "title"))
           (content (param controller "content"))
           (due-date-str (param controller "dueDate"))
           (due-date (when due-date-str
                      (parse-integer due-date-str :junk-allowed t))))
      
      (unless title
        (setf (slot-value controller 'clails/controller/base-controller:code) 400)
        (return-from do-post
          (set-response controller
                       `(("status" . "error")
                         ("message" . "Title is required")))))
      
      (handler-case
          (let ((todo (create-todo (ref user :id) title
                                  :content content
                                  :due-date due-date)))
            (setf (slot-value controller 'clails/controller/base-controller:code) 201)
            (set-response controller
                         `(("status" . "success")
                           ("data" . (("todo" . ,(todo-to-json todo)))))))
        (simple-error (e)
          (setf (slot-value controller 'clails/controller/base-controller:code) 400)
          (set-response controller
                       `(("status" . "error")
                         ("message" . ,(format nil "~A" e)))))))))

(defmethod do-get ((controller <todo-item-controller>))
  "Get a specific todo by ID.

   Only returns the todo if it belongs to the authenticated user.

   @param controller [<todo-item-controller>] Controller instance
   @return [list] HTTP response via set-response
   "
  (let ((user (get-current-user (env controller))))
    (unless user
      (setf (slot-value controller 'clails/controller/base-controller:code) 401)
      (return-from do-get
        (set-response controller
                     `(("status" . "error")
                       ("message" . "Authentication required")))))
    
    (let* ((todo-id (parse-integer (param controller "id") :junk-allowed t))
           (todo (when todo-id (find-todo-by-id todo-id))))
      
      (unless todo
        (setf (slot-value controller 'clails/controller/base-controller:code) 404)
        (return-from do-get
          (set-response controller
                       `(("status" . "error")
                         ("message" . "TODO not found")))))
      
      ;; Check if todo belongs to current user
      (unless (= (ref todo :owner-id) (ref user :id))
        (setf (slot-value controller 'clails/controller/base-controller:code) 403)
        (return-from do-get
          (set-response controller
                       `(("status" . "error")
                         ("message" . "Access denied")))))
      
      (setf (slot-value controller 'clails/controller/base-controller:code) 200)
      (set-response controller
                   `(("status" . "success")
                     ("data" . (("todo" . ,(todo-to-json todo)))))))))

(defmethod do-put ((controller <todo-item-controller>))
  "Update an existing todo.

   Only updates if todo belongs to authenticated user.
   Expects JSON body with: title (optional), content (optional), dueDate (optional)

   @param controller [<todo-item-controller>] Controller instance
   @return [list] HTTP response via set-response
   "
  (let ((user (get-current-user (env controller))))
    (unless user
      (setf (slot-value controller 'clails/controller/base-controller:code) 401)
      (return-from do-put
        (set-response controller
                     `(("status" . "error")
                       ("message" . "Authentication required")))))
    
    (let* ((todo-id (parse-integer (param controller "id") :junk-allowed t))
           (todo (when todo-id (find-todo-by-id todo-id))))
      
      (unless todo
        (setf (slot-value controller 'clails/controller/base-controller:code) 404)
        (return-from do-put
          (set-response controller
                       `(("status" . "error")
                         ("message" . "TODO not found")))))
      
      ;; Check if todo belongs to current user
      (unless (= (ref todo :owner-id) (ref user :id))
        (setf (slot-value controller 'clails/controller/base-controller:code) 403)
        (return-from do-put
          (set-response controller
                       `(("status" . "error")
                         ("message" . "Access denied")))))
      
      (let* ((title (param controller "title"))
             (content (param controller "content"))
             (due-date-str (param controller "dueDate"))
             (due-date (when due-date-str
                        (parse-integer due-date-str :junk-allowed t))))
        
        (handler-case
            (progn
              (update-todo todo
                          :title title
                          :content content
                          :due-date due-date)
              (setf (slot-value controller 'clails/controller/base-controller:code) 200)
              (set-response controller
                           `(("status" . "success")
                             ("data" . (("todo" . ,(todo-to-json todo)))))))
          (simple-error (e)
            (setf (slot-value controller 'clails/controller/base-controller:code) 400)
            (set-response controller
                         `(("status" . "error")
                           ("message" . ,(format nil "~A" e))))))))))

(defmethod do-delete ((controller <todo-item-controller>))
  "Delete a todo.

   Only deletes if todo belongs to authenticated user.

   @param controller [<todo-item-controller>] Controller instance
   @return [list] HTTP response via set-response
   "
  (let ((user (get-current-user (env controller))))
    (unless user
      (setf (slot-value controller 'clails/controller/base-controller:code) 401)
      (return-from do-delete
        (set-response controller
                     `(("status" . "error")
                       ("message" . "Authentication required")))))
    
    (let* ((todo-id (parse-integer (param controller "id") :junk-allowed t))
           (todo (when todo-id (find-todo-by-id todo-id))))
      
      (unless todo
        (setf (slot-value controller 'clails/controller/base-controller:code) 404)
        (return-from do-delete
          (set-response controller
                       `(("status" . "error")
                         ("message" . "TODO not found")))))
      
      ;; Check if todo belongs to current user
      (unless (= (ref todo :owner-id) (ref user :id))
        (setf (slot-value controller 'clails/controller/base-controller:code) 403)
        (return-from do-delete
          (set-response controller
                       `(("status" . "error")
                         ("message" . "Access denied")))))
      
      (delete-todo todo)
      (setf (slot-value controller 'clails/controller/base-controller:code) 200)
      (set-response controller
                   `(("status" . "success")
                     ("message" . "TODO deleted successfully"))))))
