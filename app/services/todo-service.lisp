; -*- mode: lisp -*-
(in-package #:cl-user)
(defpackage #:dogatto/services/todo-service
  (:use #:cl)
  (:import-from #:clails/model
                #:ref
                #:save)
  (:import-from #:dogatto/models/todo
                #:create-todo
                #:find-todo-by-ulid
                #:find-todos-by-user
                #:update-todo
                #:delete-todo
                #:toggle-todo-status
                #:<todo>)
  (:import-from #:dogatto/models/todo-tag
                #:find-tags-for-todo
                #:find-todos-by-tag-ulids
                #:find-todos-by-label-tags
                #:find-todos-untagged)
  (:import-from #:dogatto/models/label-tag
                #:find-tags-for-label)
  (:export #:list-todos
           #:get-todo
           #:create-new-todo
           #:update-existing-todo
           #:delete-existing-todo
           #:toggle-todo-complete))

(in-package #:dogatto/services/todo-service)

(defun list-todos (owner-id &key tag-ulids label-ulid status untagged)
  "List TODOs for the specified owner with optional filtering.
   
   Supports multiple filtering options:
   - tag-ulids: Filter by tag ULIDs (OR condition)
   - label-ulid: Filter by label's tags (AND condition)
   - status: Filter by status (active/completed)
   - untagged: If true, return only untagged TODOs
   
   @param owner-id [integer] Owner ID
   @param tag-ulids [list] Optional list of tag ULIDs for OR filtering
   @param label-ulid [string] Optional label ULID for AND filtering
   @param status [string] Optional status filter
   @param untagged [boolean] If true, return only untagged TODOs
   @return [plist] Success with :todos or error with :errors
   "
  (handler-case
      (let ((todos (cond
                     ;; Filter by label's tags (AND condition)
                     (label-ulid
                      (let* ((label-tags (find-tags-for-label label-ulid owner-id))
                             (label-tag-ulids (mapcar #'(lambda (tag) (ref tag :ulid)) label-tags)))
                        (if label-tag-ulids
                            (find-todos-by-label-tags owner-id label-tag-ulids :status status)
                            nil)))
                     ;; Filter by untagged
                     (untagged
                      (find-todos-untagged owner-id :status status))
                     ;; Filter by tag ULIDs (OR condition)
                     (tag-ulids
                      (find-todos-by-tag-ulids owner-id tag-ulids :status status))
                     ;; No filter - get all
                     (t
                      (find-todos-by-user owner-id)))))
        (list :success t :todos todos))
    (error (e)
      (list :success nil :errors (list (format nil "Failed to list TODOs: ~A" e))))))

(defun get-todo (todo-ulid owner-id)
  "Get a single TODO by ULID for the specified owner.
   
   @param todo-ulid [string] TODO ULID
   @param owner-id [integer] Owner ID
   @return [plist] Success with :todo or error with :errors
   "
  ;; Validate inputs
  (when (or (null todo-ulid) (string= (string-trim '(#\Space #\Tab) todo-ulid) ""))
    (return-from get-todo
      (list :success nil :errors '("TODO ULID is required"))))
  
  (handler-case
      (let ((todo (find-todo-by-ulid todo-ulid owner-id)))
        (if todo
            (list :success t :todo todo)
            (list :success nil :errors '("TODO not found"))))
    (error (e)
      (list :success nil :errors (list (format nil "Failed to get TODO: ~A" e))))))

(defun create-new-todo (owner-id title &key content due-date)
  "Create a new TODO.
   
   @param owner-id [integer] Owner ID
   @param title [string] TODO title
   @param content [string] TODO content (optional)
   @param due-date [integer] Due date as universal time (optional)
   @return [plist] Success with :todo or error with :errors
   "
  (let ((errors '()))
    
    ;; Validate title
    (when (or (null title) (string= (string-trim '(#\Space #\Tab) title) ""))
      (push "Title is required" errors))
    
    ;; Return errors if any
    (when errors
      (return-from create-new-todo
        (list :success nil :errors (nreverse errors))))
    
    ;; Create TODO
    (handler-case
        (let ((todo (create-todo owner-id title
                                :content content
                                :due-date due-date)))
          (if todo
              (list :success t :todo todo)
              (list :success nil :errors '("Failed to create TODO"))))
      (error (e)
        (list :success nil :errors (list (format nil "Failed to create TODO: ~A" e)))))))

(defun update-existing-todo (todo-ulid owner-id &key title content due-date)
  "Update an existing TODO.
   
   @param todo-ulid [string] TODO ULID
   @param owner-id [integer] Owner ID
   @param title [string] New title (optional)
   @param content [string] New content (optional)
   @param due-date [integer] New due date as universal time (optional)
   @return [plist] Success with :todo or error with :errors
   "
  ;; Validate todo-ulid
  (when (or (null todo-ulid) (string= (string-trim '(#\Space #\Tab) todo-ulid) ""))
    (return-from update-existing-todo
      (list :success nil :errors '("TODO ULID is required"))))
  
  (handler-case
      (let ((todo (find-todo-by-ulid todo-ulid owner-id)))
        (unless todo
          (return-from update-existing-todo
            (list :success nil :errors '("TODO not found"))))
        
        ;; Update TODO
        (update-todo todo
                    :title title
                    :content content
                    :due-date due-date)
        (list :success t :todo todo))
    (error (e)
      (list :success nil :errors (list (format nil "Failed to update TODO: ~A" e))))))

(defun delete-existing-todo (todo-ulid owner-id)
  "Delete a TODO.
   
   @param todo-ulid [string] TODO ULID
   @param owner-id [integer] Owner ID
   @return [plist] Success or error with :errors
   "
  ;; Validate todo-ulid
  (when (or (null todo-ulid) (string= (string-trim '(#\Space #\Tab) todo-ulid) ""))
    (return-from delete-existing-todo
      (list :success nil :errors '("TODO ULID is required"))))
  
  (handler-case
      (let ((todo (find-todo-by-ulid todo-ulid owner-id)))
        (unless todo
          (return-from delete-existing-todo
            (list :success nil :errors '("TODO not found"))))
        
        ;; Delete TODO
        (delete-todo todo)
        (list :success t))
    (error (e)
      (list :success nil :errors (list (format nil "Failed to delete TODO: ~A" e))))))

(defun toggle-todo-complete (todo-ulid owner-id)
  "Toggle TODO completion status between pending and completed.
   
   @param todo-ulid [string] TODO ULID
   @param owner-id [integer] Owner ID
   @return [plist] Success with :todo or error with :errors
   "
  ;; Validate todo-ulid
  (when (or (null todo-ulid) (string= (string-trim '(#\Space #\Tab) todo-ulid) ""))
    (return-from toggle-todo-complete
      (list :success nil :errors '("TODO ULID is required"))))
  
  (handler-case
      (let ((todo (find-todo-by-ulid todo-ulid owner-id)))
        (unless todo
          (return-from toggle-todo-complete
            (list :success nil :errors '("TODO not found"))))
        
        ;; Toggle status
        (toggle-todo-status todo)
        (list :success t :todo todo))
    (error (e)
      (list :success nil :errors (list (format nil "Failed to toggle TODO status: ~A" e))))))
