; -*- mode: lisp -*-
(in-package #:cl-user)
(defpackage #:dogatto/models/tag
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
  (:import-from #:dogatto/utils/ulid
                #:generate-ulid)
  (:export #:<tag>
           #:create-tag
           #:find-tag-by-id
           #:find-tag-by-ulid
           #:find-tags-by-user
           #:update-tag
           #:delete-tag
           #:validate-tag))

(in-package #:dogatto/models/tag)

(defmodel <tag> (<base-model>)
  (:table "tags"
   :relations ((:has-many "dogatto/models/todo-tag::<todo-tag>"
                :as :todo-tags
                :foreign-key :tag-id)
               (:has-many "dogatto/models/label-tag::<label-tag>"
                :as :label-tags
                :foreign-key :tag-id))))

(defun validate-tag (tag)
  "Validate tag attributes.

   Validates tag name and color format.

   @param tag [<tag>] Tag instance to validate
   @return [list] List of error messages (empty if valid)
   "
  (let ((errors '())
        (name (ref tag :name))
        (color (ref tag :color)))
    ;; Name validation
    (when (or (null name) (string= (string-trim '(#\Space #\Tab) name) ""))
      (push "Tag name is required" errors))
    (when (and name (> (length name) 50))
      (push "Tag name must be 50 characters or less" errors))
    
    ;; Color validation (optional)
    (when (and color
               (not (null color))
               (not (string= color ""))
               (not (cl-ppcre:scan "^#[0-9A-Fa-f]{6}$" color)))
      (push "Color must be a valid hex color code (e.g., #3B82F6)" errors))
    
    (nreverse errors)))

(defun create-tag (owner-id name &key (color "#3B82F6") merged-to-ulid)
  "Create a new tag.

   Creates a tag with the specified attributes and saves it to the database.

   @param owner-id [integer] ID of the tag owner (user)
   @param name [string] Tag name (required, 1-50 characters)
   @param color [string] Tag color hex code (optional, default #3B82F6)
   @param merged-to-ulid [string] ULID of tag this was merged into (optional)
   @return [<tag>] Created tag instance
   @condition validation-error If validation fails
   "
  (let ((tag (make-record '<tag>
                          :ulid (generate-ulid)
                          :owner-id owner-id
                          :name (string-trim '(#\Space #\Tab) name)
                          :color (if (or (null color) (string= color ""))
                                     "#3B82F6"
                                     color)
                          :merged-to-ulid merged-to-ulid)))
    (let ((errors (validate-tag tag)))
      (when errors
        (error "Validation failed: ~{~A~^, ~}" errors)))
    (save tag)
    tag))

(defun find-tag-by-id (id owner-id)
  "Find a tag by its internal ID.

   @param id [integer] Tag ID
   @return [<tag>] Tag instance
   @return [nil] If tag not found
   "
  (first (execute-query
          (query <tag>
                 :as :tag
                 :where (:and (:= (:tag :id) :id)
                              (:= (:tag :owner-id) :owner-id)))
          (list :id id
                :owner-id owner-id))))

(defun find-tag-by-ulid (ulid owner-id)
  "Find a tag by its ULID.

   @param ulid [string] Tag ULID
   @return [<tag>] Tag instance
   @return [nil] If tag not found
   "
  (first (execute-query
          (query <tag>
                 :as :tag
                 :where (:and (:= (:tag :ulid) :ulid)
                              (:= (:tag :owner-id) :owner-id)))
          (list :ulid ulid
                :owner-id owner-id))))

(defun find-tags-by-user (owner-id)
  "Find all active (non-merged) tags belonging to a user.

   Returns only tags that have not been merged (merged_at is NULL).
   Results are ordered by name.

   @param owner-id [integer] User ID
   @return [list] List of tag instances
   "
  (execute-query
   (query <tag>
          :as :tag
          :where (:and (:= (:tag :owner-id) :owner-id)
                       (:null (:tag :merged-at)))
          :order-by ((:tag :name :asc)))
   (list :owner-id owner-id)))

(defun update-tag (tag &key name color merged-to-ulid)
  "Update tag attributes.

   Updates the specified attributes and saves changes to the database.

   @param tag [<tag>] Tag instance to update
   @param name [string] New tag name (optional)
   @param color [string] New tag color (optional)
   @param merged-to-ulid [string] New merged-to-ulid (optional)
   @return [<tag>] Updated tag instance
   @condition validation-error If validation fails
   "
  (when name
    (setf (ref tag :name) (string-trim '(#\Space #\Tab) name)))
  (when color
    (setf (ref tag :color) color))
  (when merged-to-ulid
    (setf (ref tag :merged-to-ulid) merged-to-ulid))
  
  (setf (ref tag :updated-at) (get-universal-time))
  
  (let ((errors (validate-tag tag)))
    (when errors
      (error "Validation failed: ~{~A~^, ~}" errors)))
  
  (save tag)
  tag)

(defun delete-tag (tag)
  "Delete a tag.

   Deletes the tag from the database. Associated todo_tags records
   will be deleted automatically by CASCADE constraint.

   @param tag [<tag>] Tag instance to delete
   @return [boolean] T if successful
   "
  (destroy tag)
  t)
