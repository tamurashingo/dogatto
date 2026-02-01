; -*- mode: lisp -*-
(in-package #:cl-user)
(defpackage #:dogatto/models/todo-tag
  (:use #:cl
        #:clails/model)
  (:import-from #:cl-batis
                #:defsql)
  (:import-from #:cl-syntax
                #:use-syntax)
  (:import-from #:dogatto/models/tag
                #:<tag>
                #:find-tag-by-ulid)
  (:import-from #:dogatto/models/todo
                #:<todo>
                #:find-todo-by-ulid)
  (:export #:assign-tags-to-todo
           #:remove-tag-from-todo
           #:find-tags-for-todo
           #:find-todos-for-tag
           #:find-todos-by-tag-ulids
           #:find-todos-untagged
           #:get-tag-statistics))

(in-package #:dogatto/models/todo-tag)

(defmodel <todo-tag> (<base-model>)
  (:table "todo-tags"
   :relations ((:belongs-to "dogatto/models/todo:<todo>"
                :column :todo
                :key :todo-id)
               (:belongs-to "dogatto/models/tag:<tag>"
                :column :tag
                :key :tag-id))))

(cl-syntax:use-syntax :annot)

;; Native queries
@cl-batis:update
("DELETE FROM todo_tags WHERE todo_id = :todo_id")
(defsql delete-tags-for-todo (todo_id))

@cl-batis:update
("INSERT INTO todo_tags (todo_id, tag_id, created_at) VALUES (:todo_id, :tag_id, :created_at)")
(defsql insert-todo-tag (todo_id tag_id created_at))

@cl-batis:update
("DELETE FROM todo_tags WHERE todo_id = :todo_id AND tag_id = :tag_id")
(defsql delete-todo-tag (todo_id tag_id))



(defparameter *find-todos-for-tag-query*
  (query <todo>
         :as :todo
         :joins ((:inner-join :todo-tags))
         :where (:= (:todo-tags :tag-id) :tag-id)
         :order-by ((:todo :created-at :desc))))


@cl-batis:select
("SELECT 
    COUNT(*) as total,
    SUM(CASE WHEN status = 'active' THEN 1 ELSE 0 END) as active,
    SUM(CASE WHEN status = 'completed' THEN 1 ELSE 0 END) as completed
  FROM todos t 
  INNER JOIN todo_tags tt ON t.id = tt.todo_id 
  WHERE tt.tag_id = :tag_id
  AND   t.owner_id = :owner_id")
(defsql select-tag-statistics (tag_id owner_id))

;; Functions
(defun assign-tags-to-todo (todo-ulid tag-ulids)
  "Assign tags to a TODO.

   Replaces all existing tags with the new set of tags.
   Maximum 10 tags allowed per TODO.

   @param todo-ulid [string] TODO ULID
   @param tag-ulids [list] List of tag ULIDs to assign
   @return [boolean] T if successful
   @condition validation-error If more than 10 tags specified
   @condition not-found-error If TODO or any tag not found
   "
  (when (> (length tag-ulids) 10)
    (error "Maximum 10 tags allowed per TODO"))
  
  (let ((todo (find-todo-by-ulid todo-ulid)))
    (unless todo
      (error "TODO not found"))
    
    ;; Verify all tags exist
    (let ((tags (mapcar #'(lambda (tag-ulid)
                            (or (find-tag-by-ulid tag-ulid)
                                (error "Tag not found: ~A" tag-ulid)))
                        tag-ulids)))
      
      ;; Remove existing associations
      (clails/model:execute-query delete-tags-for-todo
                                  (list :todo_id (ref todo :id)))
      
      ;; Add new associations
      (dolist (tag tags)
        (clails/model:execute-query insert-todo-tag
                                    (list :todo_id (ref todo :id)
                                          :tag_id (ref tag :id)
                                          :created_at (get-universal-time))))
      
      t)))

(defun remove-tag-from-todo (todo-ulid tag-ulid)
  "Remove a tag from a TODO.

   @param todo-ulid [string] TODO ULID
   @param tag-ulid [string] Tag ULID to remove
   @return [boolean] T if successful
   @condition not-found-error If TODO or tag not found
   "
  (let ((todo (find-todo-by-ulid todo-ulid))
        (tag (find-tag-by-ulid tag-ulid)))
    (unless todo
      (error "TODO not found"))
    (unless tag
      (error "Tag not found"))
    
    (clails/model:execute-query delete-todo-tag
                                (list :todo_id (ref todo :id)
                                      :tag_id (ref tag :id)))
    t))

(defparameter *find-tags-for-todo-query*
  (query <tag>
         :as :tag
         :joins ((:inner-join :todo-tags))
         :where (:and (:= (:todo-tags :todo-id) :todo-id)
                      (:= (:tag :owner-id) :owner-id))
         :order-by ((:tag :name :asc))))

(defun find-tags-for-todo (todo-ulid owner-id)
  "Find all tags assigned to a TODO.

   Returns tags ordered by name.

   @param todo-ulid [string] TODO ULID
   @param owner-id [string] TODO owner-id
   @return [list] List of tag instances
   "
  (let ((todo (find-todo-by-ulid todo-ulid owner-id)))
    (unless todo
      (return-from find-tags-for-todo nil))
    
    (execute-query *find-tags-for-todo-query*
                   (list :todo-id (ref todo :id)
                         :owner-id owner-id))))`

(defun find-todos-for-tag (tag-ulid)
  "Find all TODOs with a specific tag.

   Returns TODOs ordered by created_at descending.

   @param tag-ulid [string] Tag ULID
   @return [list] List of TODO instances
   "
  (let ((tag (find-tag-by-ulid tag-ulid)))
    (unless tag
      (return-from find-todos-for-tag nil))
    
    (execute-query *find-todos-for-tag-query*
                   (list :tag-id (ref tag :id)))))

(defun get-tag-statistics (tag-ulid owner-id)
  "Get statistics for a tag.

   Returns counts of total, active, and completed TODOs with this tag.

   @param tag-ulid [string] Tag ULID
   @param owner-id [string] Tag owner-id
   @return [plist] Statistics (:total :active :completed)
   @return [nil] If tag not found
   "
  (let ((tag (find-tag-by-ulid tag-ulid owner-id)))
    (unless tag
      (return-from get-tag-statistics nil))
    
    (let* ((result (clails/model:execute-query select-tag-statistics
                                               (list :tag_id (ref tag :id)
                                                     :owner_id owner-id)))
           (row (first result)))
      (list :total (getf row :total)
            :active (getf row :active)
            :completed (getf row :completed)))))

(defparameter *find-todos-by-tag-ulids-query*
  (query <todo-tag>
         :as :todo-tags
         :joins ((:inner-join :todo)
                 (:inner-join :tag))
         :where (:and (:= (:todo :owner-id) :owner-id)
                      (:in (:tag :ulid) :tag-ulids))
         :order-by ((:todo :created-at :desc))))

(defparameter *find-todos-by-tag-ulids-and-status-query*
  (query <todo-tag>
         :as :todo-tags
         :joins ((:inner-join :todo)
                 (:inner-join :tag))
         :where (:and (:= (:todo :owner-id) :owner-id)
                      (:in (:tag :ulid) :tag-ulids)
                      (:= (:todo :status) :status))
         :order-by ((:todo :created-at :desc))))

(defparameter *find-todos-untagged-query*
  (query <todo>
         :as :todo
         :joins ((:left-join :todo-tags))
         :where (:and (:= (:todo :owner-id) :owner-id)
                      (:is-null (:todo-tags :id)))
         :order-by ((:todo :created-at :desc))))

(defparameter *find-todos-untagged-and-status-query*
  (query <todo>
         :as :todo
         :joins ((:left-join :todo-tags))
         :where (:and (:= (:todo :owner-id) :owner-id)
                      (:is-null (:todo-tags :id))
                      (:= (:todo :status) :status))
         :order-by ((:todo :created-at :desc))))

(defun find-todos-by-tag-ulids (owner-id tag-ulids &key status)
  "Find TODOs filtered by tag ULIDs.

   Returns TODOs that have at least one of the specified tags (OR condition).

   @param owner-id [integer] Owner ID
   @param tag-ulids [list] List of tag ULIDs to filter by
   @param status [string] Status filter (\"active\" or \"completed\") (optional)
   @return [list] List of <todo> instances
   "
  (let ((todo-tags (if status
                       (execute-query *find-todos-by-tag-ulids-and-status-query*
                                      (list :owner-id owner-id :tag-ulids tag-ulids :status status))
                       (execute-query *find-todos-by-tag-ulids-query*
                                      (list :owner-id owner-id :tag-ulids tag-ulids)))))
    ;; Extract unique TODOs from todo-tags
    (remove-duplicates
     (mapcar #'(lambda (todo-tag)
                 (ref todo-tag :todo))
             todo-tags)
     :key #'(lambda (todo) (ref todo :id)))))

(defun find-todos-untagged (owner-id &key status)
  "Find TODOs without any tags.

   @param owner-id [integer] Owner ID
   @param status [string] Status filter (\"active\" or \"completed\") (optional)
   @return [list] List of <todo> instances
   "
  (if status
      (execute-query *find-todos-untagged-and-status-query*
                     (list :owner-id owner-id :status status))
      (execute-query *find-todos-untagged-query*
                     (list :owner-id owner-id))))
