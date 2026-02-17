; -*- mode: lisp -*-
(in-package #:cl-user)
(defpackage #:dogatto/services/tag-service
  (:use #:cl)
  (:import-from #:clails/model
                #:ref
                #:save)
  (:import-from #:dogatto/models/tag
                #:create-tag
                #:find-tags-by-user
                #:find-tag-by-ulid
                #:update-tag
                #:delete-tag
                #:<tag>)
  (:import-from #:dogatto/models/todo-tag
                #:get-tag-statistics)
  (:export #:list-tags
           #:get-tag-with-stats
           #:create-new-tag
           #:update-existing-tag
           #:delete-existing-tag))

(in-package #:dogatto/services/tag-service)

(defun list-tags (owner-id &key exclude-merged)
  "List all tags for the specified owner.
   
   @param owner-id [integer] Owner ID
   @param exclude-merged [boolean] If true, exclude merged tags (currently always excluded by model)
   @return [plist] Success with :tags or error with :errors
   "
  (declare (ignore exclude-merged)) ; Model already filters merged tags
  (handler-case
      (let ((tags (find-tags-by-user owner-id)))
        (list :success t :tags tags))
    (error (e)
      (list :success nil :errors (list (format nil "Failed to list tags: ~A" e))))))

(defun get-tag-with-stats (tag-ulid owner-id)
  "Get a single tag with usage statistics.
   
   @param tag-ulid [string] Tag ULID
   @param owner-id [integer] Owner ID
   @return [plist] Success with :tag, :statistics or error with :errors
   "
  ;; Validate tag-ulid
  (when (or (null tag-ulid) (string= (string-trim '(#\Space #\Tab) tag-ulid) ""))
    (return-from get-tag-with-stats
      (list :success nil :errors '("Tag ULID is required"))))
  
  (handler-case
      (let ((tag (find-tag-by-ulid tag-ulid owner-id)))
        (unless tag
          (return-from get-tag-with-stats
            (list :success nil :errors '("Tag not found"))))
        
        ;; Get statistics
        (let ((stats (get-tag-statistics (ref tag :ulid) owner-id)))
          (list :success t :tag tag :statistics stats)))
    (error (e)
      (list :success nil :errors (list (format nil "Failed to get tag: ~A" e))))))

(defun create-new-tag (owner-id name &key color)
  "Create a new tag.
   
   @param owner-id [integer] Owner ID
   @param name [string] Tag name
   @param color [string] Tag color (optional, defaults to system default)
   @return [plist] Success with :tag or error with :errors
   "
  (let ((errors '()))
    
    ;; Validate name
    (when (or (null name) (string= (string-trim '(#\Space #\Tab) name) ""))
      (push "Name is required" errors))
    
    ;; Return errors if any
    (when errors
      (return-from create-new-tag
        (list :success nil :errors (nreverse errors))))
    
    ;; Create tag
    (handler-case
        (let ((tag (create-tag owner-id name :color color)))
          (if tag
              (list :success t :tag tag)
              (list :success nil :errors '("Failed to create tag"))))
      (error (e)
        (list :success nil :errors (list (format nil "Failed to create tag: ~A" e)))))))

(defun update-existing-tag (tag-ulid owner-id &key name color)
  "Update an existing tag.
   
   @param tag-ulid [string] Tag ULID
   @param owner-id [integer] Owner ID
   @param name [string] New tag name (optional)
   @param color [string] New tag color (optional)
   @return [plist] Success with :tag or error with :errors
   "
  ;; Validate tag-ulid
  (when (or (null tag-ulid) (string= (string-trim '(#\Space #\Tab) tag-ulid) ""))
    (return-from update-existing-tag
      (list :success nil :errors '("Tag ULID is required"))))
  
  (handler-case
      (let ((tag (find-tag-by-ulid tag-ulid owner-id)))
        (unless tag
          (return-from update-existing-tag
            (list :success nil :errors '("Tag not found"))))
        
        ;; Update tag
        (update-tag tag :name name :color color)
        (list :success t :tag tag))
    (error (e)
      (list :success nil :errors (list (format nil "Failed to update tag: ~A" e))))))

(defun delete-existing-tag (tag-ulid owner-id)
  "Delete a tag.
   
   @param tag-ulid [string] Tag ULID
   @param owner-id [integer] Owner ID
   @return [plist] Success or error with :errors
   "
  ;; Validate tag-ulid
  (when (or (null tag-ulid) (string= (string-trim '(#\Space #\Tab) tag-ulid) ""))
    (return-from delete-existing-tag
      (list :success nil :errors '("Tag ULID is required"))))
  
  (handler-case
      (let ((tag (find-tag-by-ulid tag-ulid owner-id)))
        (unless tag
          (return-from delete-existing-tag
            (list :success nil :errors '("Tag not found"))))
        
        ;; Delete tag
        (delete-tag tag)
        (list :success t))
    (error (e)
      (list :success nil :errors (list (format nil "Failed to delete tag: ~A" e))))))
