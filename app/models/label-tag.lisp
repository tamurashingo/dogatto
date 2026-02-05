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
                #:find-label-by-ulid)
  (:import-from #:dogatto/models/tag
                #:<tag>
                #:find-tag-by-ulid)
  (:export #:assign-tags-to-label
           #:remove-tag-from-label
           #:find-tags-for-label
           #:<label-tag>))

(in-package #:dogatto/models/label-tag)

(defmodel <label-tag> (<base-model>)
  (:table "label_tags"
   :relations ((:belongs-to "dogatto/models/label::<label>"
                :column :label
                :key :label-id)
               (:belongs-to "dogatto/models/tag::<tag>"
                :column :tag
                :key :tag-id))))

(cl-syntax:use-syntax :annot)

;; Native queries
@cl-batis:update
("DELETE FROM label_tags WHERE label_id = :label_id AND owner_id = :owner_id")
(defsql delete-tags-for-label (label_id owner_id))

@cl-batis:update
("INSERT INTO label_tags (label_id, tag_id, label_ulid, owner_id, created_at, updated_at) 
  VALUES (:label_id, :tag_id, :label_ulid, :owner_id, :created_at, :updated_at)")
(defsql insert-label-tag (label_id tag_id label_ulid owner_id created_at updated_at))

@cl-batis:update
("DELETE FROM label_tags WHERE label_id = :label_id AND tag_id = :tag_id AND owner_id = :owner_id")
(defsql delete-label-tag (label_id tag_id owner_id))

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
      
      ;; Add new associations
      (let ((now (get-universal-time)))
        (dolist (tag tags)
          (clails/model:execute-query insert-label-tag
                                      (list :label_id (ref label :id)
                                            :tag_id (ref tag :id)
                                            :label_ulid label-ulid
                                            :owner_id owner-id
                                            :created_at now
                                            :updated_at now))))
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
