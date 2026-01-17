; -*- mode: lisp -*-
(in-package #:cl-user)
(defpackage #:dogatto/models/todo
  (:use #:cl)
  (:import-from #:clails/model
                #:<base-model>
                #:defmodel
                #:make-record
                #:ref
                #:save
                #:destroy
                #:execute-query
                #:query)
  (:import-from #:clails/model/base-model
                #:validate)
  (:import-from #:dogatto/utils/ulid
                #:generate-ulid)
  (:export #:<todo>
           #:create-todo
           #:find-todo-by-id
           #:find-todos-by-user
           #:update-todo
           #:delete-todo
           #:toggle-todo-status))
(in-package #:dogatto/models/todo)

(defmodel <todo> (<base-model>)
  (:table "todos"
   :relations ((:belongs-to "dogatto/models/user::<user>"
                 :column :owner
                 :key :owner-id))))

(defmethod validate ((todo <todo>))
  "Validate TODO data.

   Checks:
   - title is required and non-empty
   - title length is between 1 and 255 characters
   - owner-id is present

   @param todo [<todo>] TODO instance to validate
   @return [boolean] T if valid
   @condition simple-error When validation fails
   "
  (unless (ref todo :owner-id)
    (error "Owner ID is required"))
  
  (unless (ref todo :title)
    (error "Title is required"))
  
  (when (string= (ref todo :title) "")
    (error "Title cannot be empty"))
  
  (when (> (length (ref todo :title)) 255)
    (error "Title must be 255 characters or less"))
  
  t)

(defun create-todo (owner-id title &key content due-date)
  "Create and save a new TODO to the database.

   Validates the TODO before saving (automatically via save method).

   @param owner-id [integer] User ID who owns this TODO
   @param title [string] TODO title (required, 1-255 characters)
   @param content [string] TODO content/description (optional)
   @param due-date [integer] Due date as Universal Time (optional)
   @return [<todo>] Created TODO instance
   @condition simple-error When validation fails
   "
  (let ((todo (make-record '<todo>
                           :ulid (generate-ulid)
                           :owner-id owner-id
                           :title title
                           :content content
                           :due-date due-date
                           :status "active")))
    (save todo)
    todo))

(defun find-todo-by-id (id)
  "Find a TODO by its database ID.

   @param id [integer] TODO database ID
   @return [<todo>] TODO instance or NIL if not found
   "
  (let ((results (execute-query
                   (query <todo>
                          :as :todo
                          :where (:= (:todo :id) :id))
                   (list :id id))))
    (if results
        (car results)
        nil)))

(defun find-todos-by-user (user-id)
  "Find all TODOs belonging to a user.

   Returns TODOs ordered by created_at descending (newest first).

   @param user-id [integer] User ID
   @return [list] List of <todo> instances
   "
  (execute-query
    (query <todo>
           :as :todo
           :where (:= (:todo :owner-id) :user-id)
           :order-by ((:todo :created-at :desc)))
    (list :user-id user-id)))

(defun update-todo (todo &key title content due-date)
  "Update an existing TODO.

   Only updates provided fields (non-nil values).
   Validates the TODO before saving (automatically via save method).

   @param todo [<todo>] TODO instance to update
   @param title [string] New title (optional)
   @param content [string] New content (optional)
   @param due-date [integer] New due date as Universal Time (optional)
   @return [<todo>] Updated TODO instance
   @condition simple-error When validation fails
   "
  (when title
    (setf (ref todo :title) title))
  (when content
    (setf (ref todo :content) content))
  (when due-date
    (setf (ref todo :due-date) due-date))
  (save todo)
  todo)

(defun delete-todo (todo)
  "Delete a TODO from the database.

   @param todo [<todo>] TODO instance to delete
   @return [boolean] T on success
   "
  (destroy todo))

(defun toggle-todo-status (todo)
  "Toggle TODO status between active and completed.

   When completing, sets completed-at to current timestamp.
   When activating, clears completed-at.

   @param todo [<todo>] TODO instance to toggle
   @return [<todo>] Updated TODO instance
   "
  (if (string= (ref todo :status) "active")
      (progn
        (setf (ref todo :status) "completed")
        (setf (ref todo :completed-at) (get-universal-time)))
      (progn
        (setf (ref todo :status) "active")
        (setf (ref todo :completed-at) nil)))
  (save todo)
  todo)
