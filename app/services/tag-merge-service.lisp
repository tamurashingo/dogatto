; -*- mode: lisp -*-
(in-package #:cl-user)
(defpackage #:dogatto/services/tag-merge-service
  (:use #:cl)
  (:import-from #:clails/model
                #:ref
                #:save
                #:execute-query
                #:with-transaction)
  (:import-from #:clails/model/connection
                #:get-connection)
  (:import-from #:dbi
                #:do-sql)
  (:import-from #:dogatto/models/tag
                #:<tag>
                #:find-tag-by-ulid)
  (:import-from #:dogatto/models/todo-tag
                #:<todo-tag>)
  (:import-from #:dogatto/models/label-tag
                #:<label-tag>)
  (:export #:validate-merge-sources
           #:validate-merge-target
           #:merge-tags-to-existing
           #:merge-tags-to-new
           #:resolve-merged-tag))

(in-package #:dogatto/services/tag-merge-service)

(defun validate-merge-sources (source-ulids owner-id)
  "Validate merge source tags.

   Checks that all source tags exist, belong to the user, and are not already merged.

   @param source-ulids [list] List of source tag ULIDs
   @param owner-id [integer] Owner ID to verify ownership
   @return [list] List of two values: (valid-tags error-messages)
   "
  (let ((errors '())
        (tags '()))
    
    ;; Check at least one source
    (when (null source-ulids)
      (push "At least one source tag is required" errors))
    
    ;; Validate each source tag
    (dolist (ulid source-ulids)
      (let ((tag (find-tag-by-ulid ulid)))
        (cond
          ;; Tag not found
          ((null tag)
           (push (format nil "Tag ~A not found" ulid) errors))
          
          ;; Not owned by user
          ((/= (ref tag :owner-id) owner-id)
           (push (format nil "Tag ~A does not belong to you" ulid) errors))
          
          ;; Already merged
          ((ref tag :merged-to-ulid)
           (push (format nil "Tag ~A is already merged" ulid) errors))
          
          ;; Valid tag
          (t
           (push tag tags)))))
    
    (list (nreverse tags) (nreverse errors))))

(defun validate-merge-target (target-ulid owner-id &optional source-ulids)
  "Validate merge target tag.

   Checks that target tag exists, belongs to the user, is not merged, 
   and is not in the source list.

   @param target-ulid [string] Target tag ULID
   @param owner-id [integer] Owner ID to verify ownership
   @param source-ulids [list] Optional list of source ULIDs to check against
   @return [list] List of two values: (target-tag error-messages)
   "
  (let ((errors '())
        (target-tag nil))
    
    ;; Check target exists
    (when (null target-ulid)
      (push "Target tag is required" errors)
      (return-from validate-merge-target (list nil errors)))
    
    (setf target-tag (find-tag-by-ulid target-ulid))
    
    (cond
      ;; Tag not found
      ((null target-tag)
       (push (format nil "Target tag ~A not found" target-ulid) errors))
      
      ;; Not owned by user
      ((/= (ref target-tag :owner-id) owner-id)
       (push (format nil "Target tag ~A does not belong to you" target-ulid) errors))
      
      ;; Already merged
      ((ref target-tag :merged-to-ulid)
       (push (format nil "Target tag ~A is already merged" target-ulid) errors))
      
      ;; In source list
      ((and source-ulids (member target-ulid source-ulids :test #'string=))
       (push "Target tag cannot be in the source list" errors)))
    
    (list target-tag (nreverse errors))))

(defun merge-tags-to-existing (source-ulids target-ulid owner-id)
  "Merge multiple source tags into an existing target tag.

   Updates all TODOs and labels to reference the target tag, 
   then marks source tags as merged.

   @param source-ulids [list] List of source tag ULIDs to merge
   @param target-ulid [string] Target tag ULID
   @param owner-id [integer] Owner ID for authorization
   @return [plist] Merge result with :success, :merged-tags, :target-tag, or :errors
   "
  ;; Validate sources
  (destructuring-bind (source-tags source-errors)
      (validate-merge-sources source-ulids owner-id)
    (when source-errors
      (return-from merge-tags-to-existing
        (list :success nil :errors source-errors))))
  
  ;; Validate target
  (destructuring-bind (target-tag target-errors)
      (validate-merge-target target-ulid owner-id source-ulids)
    (when target-errors
      (return-from merge-tags-to-existing
        (list :success nil :errors target-errors))))
  
  ;; Execute merge in transaction
  (with-transaction
    (let ((merged-tags '())
          (target-tag (find-tag-by-ulid target-ulid))
          (target-id (ref target-tag :id))
          (merge-time (get-universal-time)))
      
      (dolist (source-ulid source-ulids)
        (let* ((source-tag (find-tag-by-ulid source-ulid))
               (source-id (ref source-tag :id))
               (conn (get-connection)))
          
          ;; Step 1: Copy todo_tags records (only for TODOs not already having target tag)
          (dbi:do-sql conn
                      "INSERT INTO todo_tags (todo_id, tag_id, created_at)
                       SELECT todo_id, ?, ?
                       FROM todo_tags
                       WHERE tag_id = ?
                         AND todo_id NOT IN (
                           SELECT todo_id FROM todo_tags WHERE tag_id = ?
                         )"
                      (list target-id merge-time source-id target-id))
          
          ;; Step 2: Delete todo_tags records with source tag
          (dbi:do-sql conn
                      "DELETE FROM todo_tags WHERE tag_id = ?"
                      (list source-id))
          
          ;; Step 3: Copy label_tags records (only for labels not already having target tag)
          (dbi:do-sql conn
                      "INSERT INTO label_tags (label_id, tag_id, label_ulid, owner_id, created_at)
                       SELECT label_id, ?, label_ulid, owner_id, ?
                       FROM label_tags
                       WHERE tag_id = ?
                         AND label_id NOT IN (
                           SELECT label_id FROM label_tags WHERE tag_id = ?
                         )"
                      (list target-id merge-time source-id target-id))
          
          ;; Step 4: Delete label_tags records with source tag
          (dbi:do-sql conn
                      "DELETE FROM label_tags WHERE tag_id = ?"
                      (list source-id))
          
          ;; Step 5: Mark source tag as merged
          (setf (ref source-tag :merged-to-ulid) target-ulid)
          (setf (ref source-tag :merged-at) merge-time)
          (save source-tag)
          
          (push source-tag merged-tags)))
      
      (list :success t
            :merged-tags (nreverse merged-tags)
            :target-tag target-tag))))

(defun merge-tags-to-new (source-ulids new-tag-name owner-id &key (color "#3B82F6"))
  "Merge multiple source tags into a new tag.

   Creates a new tag, updates all TODOs and labels to reference it,
   then marks source tags as merged.

   @param source-ulids [list] List of source tag ULIDs to merge
   @param new-tag-name [string] Name for the new tag
   @param owner-id [integer] Owner ID for authorization
   @param color [string] Color for the new tag (optional)
   @return [plist] Merge result with :success, :merged-tags, :new-tag, or :errors
   "
  ;; Validate sources
  (destructuring-bind (source-tags source-errors)
      (validate-merge-sources source-ulids owner-id)
    (when source-errors
      (return-from merge-tags-to-new
        (list :success nil :errors source-errors))))
  
  ;; Validate new tag name
  (let ((errors '()))
    (when (or (null new-tag-name) (string= (string-trim '(#\Space #\Tab) new-tag-name) ""))
      (push "New tag name is required" errors))
    (when (and new-tag-name (> (length new-tag-name) 50))
      (push "Tag name must be 50 characters or less" errors))
    (when errors
      (return-from merge-tags-to-new
        (list :success nil :errors (nreverse errors)))))
  
  ;; Execute merge in transaction
  (with-transaction
    (let* ((new-tag (dogatto/models/tag:create-tag owner-id new-tag-name :color color))
           (new-tag-id (ref new-tag :id))
           (new-tag-ulid (ref new-tag :ulid))
           (merged-tags '())
           (merge-time (get-universal-time)))
      
      (dolist (source-ulid source-ulids)
        (let* ((source-tag (find-tag-by-ulid source-ulid))
               (source-id (ref source-tag :id))
               (conn (get-connection)))
          
          ;; Step 1: Copy todo_tags records (only for TODOs not already having new tag)
          (dbi:do-sql conn
                      "INSERT INTO todo_tags (todo_id, tag_id, created_at)
                       SELECT todo_id, ?, ?
                       FROM todo_tags
                       WHERE tag_id = ?
                         AND todo_id NOT IN (
                           SELECT todo_id FROM todo_tags WHERE tag_id = ?
                         )"
                      (list new-tag-id merge-time source-id new-tag-id))
          
          ;; Step 2: Delete todo_tags records with source tag
          (dbi:do-sql conn
                      "DELETE FROM todo_tags WHERE tag_id = ?"
                      (list source-id))
          
          ;; Step 3: Copy label_tags records (only for labels not already having new tag)
          (dbi:do-sql conn
                      "INSERT INTO label_tags (label_id, tag_id, label_ulid, owner_id, created_at)
                       SELECT label_id, ?, label_ulid, owner_id, ?
                       FROM label_tags
                       WHERE tag_id = ?
                         AND label_id NOT IN (
                           SELECT label_id FROM label_tags WHERE tag_id = ?
                         )"
                      (list new-tag-id merge-time source-id new-tag-id))
          
          ;; Step 4: Delete label_tags records with source tag
          (dbi:do-sql conn
                      "DELETE FROM label_tags WHERE tag_id = ?"
                      (list source-id))
          
          ;; Step 5: Mark source tag as merged
          (setf (ref source-tag :merged-to-ulid) new-tag-ulid)
          (setf (ref source-tag :merged-at) merge-time)
          (save source-tag)
          
          (push source-tag merged-tags)))
      
      (list :success t
            :merged-tags (nreverse merged-tags)
            :new-tag new-tag))))

(defun resolve-merged-tag (tag &key (max-depth 10))
  "Resolve tag merge chain to find the final target tag.

   Follows the merge chain up to max-depth to prevent infinite loops.

   @param tag [<tag>] Tag to resolve
   @param max-depth [integer] Maximum depth to follow (default 10)
   @return [<tag>] Final target tag, or original tag if not merged
   "
  (let ((current-tag tag)
        (depth 0))
    
    (loop while (and (< depth max-depth)
                     (ref current-tag :merged-to-ulid))
          do (let ((next-tag (find-tag-by-ulid (ref current-tag :merged-to-ulid))))
               (when (null next-tag)
                 ;; Broken chain, return current
                 (return-from resolve-merged-tag current-tag))
               (setf current-tag next-tag)
               (incf depth)))
    
    ;; Check if we hit max depth
    (when (>= depth max-depth)
      (error "Merge chain exceeds maximum depth of ~D" max-depth))
    
    current-tag))
