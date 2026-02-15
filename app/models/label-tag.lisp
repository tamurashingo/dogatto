; -*- mode: lisp -*-
(in-package #:cl-user)
(defpackage #:dogatto/models/label-tag
  (:use #:cl
        #:clails/model)
  (:import-from #:cl-batis
                #:defsql)
  (:import-from #:cl-syntax
                #:use-syntax)
  (:import-from #:dogatto/models/label
                #:<label>
                #:create-label
                #:update-label
                #:find-label-by-ulid)
  (:import-from #:dogatto/models/tag
                #:<tag>
                #:find-tag-by-ulid)
  (:export #:<label-tag>
           #:create-label-with-tags
           #:update-label-with-tags
           #:assign-tags-to-label
           #:remove-tag-from-label
           #:find-tags-for-label
           #:find-labels-by-tag-name
           #:insert-label-tags-from-source-to-target
           #:delete-label-tags-by-tag-id))

(in-package #:dogatto/models/label-tag)

(defmodel <label-tag> (<base-model>)
  (:table "label_tags"
   :relations ((:belongs-to "dogatto/models/label::<label>"
                :column :label
                :key :label-id)
               (:belongs-to "dogatto/models/tag::<tag>"
                :column :tag
                :key :tag-id))))

(defun create-label-with-tags (owner-id name description tag-ulids)
  "Create a new label with tag associations.

   Creates a label and assigns the specified tags to it in a single operation.
   All operations filter by owner-id at SQL level for security.

   @param owner-id [integer] ID of the label owner (user)
   @param name [string] Label name (required, 1-100 characters)
   @param description [string] Label description (optional, max 1000 characters)
   @param tag-ulids [list] List of tag ULIDs to associate with the label
   @return [<label>] Created label instance
   @return [nil] If validation fails
   @condition error If label name already exists for user
   @condition error If no tags specified
   @condition error If any tag not found or not owned by user
   "
  (when (or (null tag-ulids) (zerop (length tag-ulids)))
    (error "At least one tag is required"))
  
  ;; Create label (will validate and check uniqueness)
  (let ((label (create-label owner-id name description)))
    (unless label
      (return-from create-label-with-tags nil))
    
    ;; Assign tags
    (handler-case
        (progn
          (assign-tags-to-label (ref label :ulid) tag-ulids owner-id)
          label)
      (error (e)
        ;; If tag assignment fails, delete the created label
        (destroy label)
        (error e)))))

(defun update-label-with-tags (label-ulid owner-id &key name description tag-ulids)
  "Update a label with optional tag reassignment.

   Updates label attributes and/or reassigns tags if specified.
   All operations filter by owner-id at SQL level for security.

   @param label-ulid [string] Label ULID
   @param owner-id [integer] Owner user ID
   @param name [string] New label name (optional)
   @param description [string] New label description (optional)
   @param tag-ulids [list] List of tag ULIDs to assign (optional, replaces existing)
   @return [<label>] Updated label instance
   @return [nil] If label not found or validation fails
   @condition error If new name already exists for user
   @condition error If empty tag list specified
   @condition error If any tag not found or not owned by user
   "
  ;; Update label attributes if specified
  (let ((label (if (or name description)
                   (update-label label-ulid owner-id
                                :name name
                                :description description)
                   (find-label-by-ulid label-ulid owner-id))))
    (unless label
      (return-from update-label-with-tags nil))
    
    ;; Update tags if specified
    (when tag-ulids
      (assign-tags-to-label label-ulid tag-ulids owner-id))
    
    label))

(cl-syntax:use-syntax :annot)

;; Native queries
@cl-batis:update
("DELETE FROM label_tags WHERE label_id = :label_id AND owner_id = :owner_id")
(defsql delete-tags-for-label (label_id owner_id))

@cl-batis:update
("DELETE FROM label_tags WHERE label_id = :label_id AND tag_id = :tag_id AND owner_id = :owner_id")
(defsql delete-label-tag (label_id tag_id owner_id))

@cl-batis:update
("INSERT INTO label_tags (label_id, tag_id, label_ulid, owner_id, created_at)
  SELECT label_id, :target_id, label_ulid, owner_id, :merge_time
  FROM label_tags
  WHERE tag_id = :source_id
    AND label_id NOT IN (
      SELECT label_id FROM label_tags WHERE tag_id = :target_id
    )")
(defsql insert-label-tags-from-source-to-target (target_id merge_time source_id))

@cl-batis:update
("DELETE FROM label_tags WHERE tag_id = :tag_id")
(defsql delete-label-tags-by-tag-id (tag_id))

(defun assign-tags-to-label (label-ulid tag-ulids owner-id)
  "Assign tags to a label.

   Replaces all existing tags with the new set of tags.
   All operations filter by owner-id at SQL level for security.

   @param label-ulid [string] Label ULID
   @param tag-ulids [list] List of tag ULIDs to assign
   @param owner-id [integer] Owner user ID
   @return [boolean] T if successful
   @condition error If label or any tag not found or not owned by user
   @condition error If no tags specified
   "
  (when (or (null tag-ulids) (zerop (length tag-ulids)))
    (error "At least one tag is required"))
  
  (let ((label (find-label-by-ulid label-ulid owner-id)))
    (unless label
      (error "Label not found"))
    
    ;; Verify all tags exist and are owned by user (filtered by owner-id at SQL level)
    (let ((tags (mapcar #'(lambda (tag-ulid)
                            (or (find-tag-by-ulid tag-ulid owner-id)
                                (error "Tag not found: ~A" tag-ulid)))
                        tag-ulids)))
      
      ;; Remove existing associations (filtered by owner-id at SQL level)
      (clails/model:execute-query delete-tags-for-label
                                  (list :label_id (ref label :id)
                                        :owner_id owner-id))
      
      ;; Add new associations using make-record and save
      (dolist (tag tags)
        (let ((label-tag (make-record '<label-tag>
                                      :label-id (ref label :id)
                                      :tag-id (ref tag :id)
                                      :label-ulid label-ulid
                                      :owner-id owner-id)))
          (unless label-tag
            (error "Failed to create label-tag record"))
          (save label-tag)))
      t)))

(defun remove-tag-from-label (label-ulid tag-ulid owner-id)
  "Remove a tag from a label.

   Query filters by owner-id at SQL level for security.

   @param label-ulid [string] Label ULID
   @param tag-ulid [string] Tag ULID to remove
   @param owner-id [integer] Owner user ID
   @return [boolean] T if successful
   @condition error If label or tag not found or not owned by user
   @condition error If removing the last tag
   "
  (let ((label (find-label-by-ulid label-ulid owner-id))
        (tag (find-tag-by-ulid tag-ulid owner-id)))
    (unless label
      (error "Label not found"))
    (unless tag
      (error "Tag not found"))
    
    ;; Check if this is the last tag
    (let ((current-tags (find-tags-for-label label-ulid owner-id)))
      (when (<= (length current-tags) 1)
        (error "Cannot remove the last tag from a label")))
    
    ;; Delete association (filtered by owner-id at SQL level)
    (clails/model:execute-query delete-label-tag
                                (list :label_id (ref label :id)
                                      :tag_id (ref tag :id)
                                      :owner_id owner-id))
    t))

(defparameter *find-tags-for-label-query*
  (query <tag>
         :as :tag
         :joins ((:inner-join :label-tags))
         :where (:and (:= (:label-tags :label-id) :label-id)
                      (:= (:label-tags :owner-id) :owner-id)
                      (:= (:tag :owner-id) :owner-id))
         :order-by ((:tag :name :asc))))

(defun find-tags-for-label (label-ulid owner-id)
  "Find all tags assigned to a label.

   Returns tags ordered by name.
   Query filters by owner-id at SQL level for security.

   @param label-ulid [string] Label ULID
   @param owner-id [integer] Owner user ID
   @return [list] List of tag instances
   "
  (let ((label (find-label-by-ulid label-ulid owner-id)))
    (unless label
      (return-from find-tags-for-label nil))
    
    (execute-query *find-tags-for-label-query*
                   (list :label-id (ref label :id)
                         :owner-id owner-id))))

(defparameter *search-labels-by-tag-name-query*
  (query <label>
         :as :label
         :joins ((:inner-join :label-tags)
                 (:inner-join :tag :through :label-tags))
         :where (:and (:= (:label :owner-id) :owner-id)
                      (:= (:label-tags :owner-id) :owner-id)
                      (:= (:tag :owner-id) :owner-id)
                      (:like (:tag :name) :pattern))
         :order-by ((:label :name :asc))))

(defun find-labels-by-tag-name (owner-id search-term)
  "Find labels by associated tag name.

   Case-insensitive partial match search (MySQL default).
   Query filters by owner-id at SQL level for security.

   @param owner-id [integer] Owner user ID
   @param search-term [string] Search term (wildcards added automatically)
   @return [list] List of label instances
   "
  (let ((pattern (format nil "%~A%" (string-trim '(#\Space #\Tab) search-term))))
    (execute-query *search-labels-by-tag-name-query*
                   (list :owner-id owner-id
                         :pattern pattern))))
