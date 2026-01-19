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
           #:get-tag-statistics))

(in-package #:dogatto/models/todo-tag)

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

@cl-batis:select
("SELECT t.* FROM tags t 
  INNER JOIN todo_tags tt ON t.id = tt.tag_id 
  WHERE tt.todo_id = :todo_id 
  ORDER BY t.name ASC")
(defsql select-tags-for-todo (todo_id))

@cl-batis:select
("SELECT t.* FROM todos t 
  INNER JOIN todo_tags tt ON t.id = tt.todo_id 
  WHERE tt.tag_id = :tag_id 
  ORDER BY t.created_at DESC")
(defsql select-todos-for-tag (tag_id))

@cl-batis:select
("SELECT 
    COUNT(*) as total,
    SUM(CASE WHEN status = 'active' THEN 1 ELSE 0 END) as active,
    SUM(CASE WHEN status = 'completed' THEN 1 ELSE 0 END) as completed
  FROM todos t 
  INNER JOIN todo_tags tt ON t.id = tt.todo_id 
  WHERE tt.tag_id = :tag_id")
(defsql select-tag-statistics (tag_id))

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

(defun find-tags-for-todo (todo-ulid)
  "Find all tags assigned to a TODO.

   Returns tags ordered by name.

   @param todo-ulid [string] TODO ULID
   @return [list] List of tag instances
   "
  (let ((todo (find-todo-by-ulid todo-ulid)))
    (unless todo
      (return-from find-tags-for-todo nil))
    
    (let ((results (clails/model:execute-query select-tags-for-todo
                                               (list :todo_id (ref todo :id)))))
      (mapcar #'(lambda (row)
                  (hydrate '<tag> row))
              results))))

(defun find-todos-for-tag (tag-ulid)
  "Find all TODOs with a specific tag.

   Returns TODOs ordered by created_at descending.

   @param tag-ulid [string] Tag ULID
   @return [list] List of TODO instances
   "
  (let ((tag (find-tag-by-ulid tag-ulid)))
    (unless tag
      (return-from find-todos-for-tag nil))
    
    (let ((results (clails/model:execute-query select-todos-for-tag
                                               (list :tag_id (ref tag :id)))))
      (mapcar #'(lambda (row)
                  (hydrate '<todo> row))
              results))))

(defun get-tag-statistics (tag-ulid)
  "Get statistics for a tag.

   Returns counts of total, active, and completed TODOs with this tag.

   @param tag-ulid [string] Tag ULID
   @return [plist] Statistics (:total :active :completed)
   @return [nil] If tag not found
   "
  (let ((tag (find-tag-by-ulid tag-ulid)))
    (unless tag
      (return-from get-tag-statistics nil))
    
    (let* ((result (clails/model:execute-query select-tag-statistics
                                               (list :tag_id (ref tag :id))))
           (row (first result)))
      (list :total (getf row :total)
            :active (getf row :active)
            :completed (getf row :completed)))))
