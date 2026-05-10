; -*- mode: lisp -*-
(in-package #:cl-user)
(defpackage #:dogatto/services/label-service
  (:use #:cl)
  (:import-from #:clails/model
                #:ref)
  (:import-from #:dogatto/models/label
                #:find-labels-by-owner
                #:find-label-by-ulid
                #:delete-label
                #:estimate-todo-count-by-tags
                #:search-labels-by-name
                #:get-label-stats
                #:<label>)
  (:import-from #:dogatto/models/label-tag
                #:create-label-with-tags
                #:update-label-with-tags
                #:find-tags-for-label
                #:find-labels-by-tag-name)
  (:export #:list-labels
           #:get-label
           #:create-new-label
           #:update-existing-label
           #:delete-existing-label
           #:estimate-todo-count))

(in-package #:dogatto/services/label-service)

(defun list-labels (owner-id &key page per-page sort order filter search-mode q)
  "List labels for the specified owner with optional filtering and search.

   Supports search modes:
   - label_name: Search labels by name (partial match)
   - tag_name: Search labels by associated tag name (partial match)

   @param owner-id [integer] Owner ID
   @param page [integer] Page number (default 1)
   @param per-page [integer] Items per page (default 20, max 100)
   @param sort [string] Sort field
   @param order [string] Sort order
   @param filter [string] Filter type
   @param search-mode [string] Search mode (\"label_name\" or \"tag_name\")
   @param q [string] Search query
   @return [plist] Success with :labels, :stats or error with :errors
   "
  (handler-case
      (let ((labels (cond
                      ((and q search-mode (string= search-mode "label_name"))
                       (search-labels-by-name owner-id q))
                      ((and q search-mode (string= search-mode "tag_name"))
                       (find-labels-by-tag-name owner-id q))
                      (t
                       (find-labels-by-owner owner-id
                                            :page page
                                            :per-page per-page
                                            :sort (when sort (intern (string-upcase sort) :keyword))
                                            :order (when order (intern (string-upcase order) :keyword))
                                            :filter (when filter (intern (string-upcase filter) :keyword))))))
            (labels-with-tags (mapcar #'(lambda (label)
                                          (let* ((tags (find-tags-for-label (ref label :ulid) owner-id))
                                                 (tag-count (length tags)))
                                            (list :label label
                                                  :tags tags
                                                  :tag-count tag-count
                                                  :todo-count 0)))
                                      labels))
            (stats (get-label-stats owner-id)))
        (list :success t :labels labels-with-tags :stats stats))
    (error (e)
      (list :success nil :errors (list (format nil "Failed to list labels: ~A" e))))))

(defun get-label (label-ulid owner-id)
  "Get a single label by ULID with associated tags.

   @param label-ulid [string] Label ULID
   @param owner-id [integer] Owner ID
   @return [plist] Success with :label, :tags, :tag-count, :todo-count or error with :errors
   "
  (when (or (null label-ulid) (string= (string-trim '(#\Space #\Tab) label-ulid) ""))
    (return-from get-label
      (list :success nil :errors '("Label ULID is required"))))

  (handler-case
      (let ((label (find-label-by-ulid label-ulid owner-id)))
        (unless label
          (return-from get-label
            (list :success nil :errors '("Label not found"))))

        (let* ((tags (find-tags-for-label label-ulid owner-id))
               (tag-count (length tags)))
          (list :success t
                :label label
                :tags tags
                :tag-count tag-count
                :todo-count 0)))
    (error (e)
      (list :success nil :errors (list (format nil "Failed to get label: ~A" e))))))

(defun create-new-label (owner-id name description tag-ulids)
  "Create a new label with tag associations.

   @param owner-id [integer] Owner ID
   @param name [string] Label name
   @param description [string] Label description (optional)
   @param tag-ulids [list] List of tag ULIDs to associate
   @return [plist] Success with :label, :tags, :tag-count, :todo-count or error with :errors
   "
  (let ((errors '()))

    (when (or (null name) (string= (string-trim '(#\Space #\Tab) name) ""))
      (push "Label name is required" errors))

    (unless (and tag-ulids (listp tag-ulids) (> (length tag-ulids) 0))
      (push "At least one tag is required" errors))

    (when errors
      (return-from create-new-label
        (list :success nil :errors (nreverse errors))))

    (handler-case
        (let ((label (create-label-with-tags owner-id name description tag-ulids)))
          (if label
              (let* ((tags (find-tags-for-label (ref label :ulid) owner-id))
                     (tag-count (length tags)))
                (list :success t
                      :label label
                      :tags tags
                      :tag-count tag-count
                      :todo-count 0))
              (list :success nil :errors '("Failed to create label"))))
      (error (e)
        (list :success nil :errors (list (format nil "~A" e)))))))

(defun update-existing-label (label-ulid owner-id &key name description tag-ulids)
  "Update an existing label with optional tag reassignment.

   @param label-ulid [string] Label ULID
   @param owner-id [integer] Owner ID
   @param name [string] New label name (optional)
   @param description [string] New label description (optional)
   @param tag-ulids [list] New tag ULIDs (optional, replaces existing)
   @return [plist] Success with :label, :tags, :tag-count, :todo-count or error with :errors
   "
  (when (or (null label-ulid) (string= (string-trim '(#\Space #\Tab) label-ulid) ""))
    (return-from update-existing-label
      (list :success nil :errors '("Label ULID is required"))))

  (handler-case
      (let ((label (update-label-with-tags label-ulid owner-id
                                           :name name
                                           :description description
                                           :tag-ulids tag-ulids)))
        (unless label
          (return-from update-existing-label
            (list :success nil :errors '("Label not found"))))

        (let* ((tags (find-tags-for-label label-ulid owner-id))
               (tag-count (length tags)))
          (list :success t
                :label label
                :tags tags
                :tag-count tag-count
                :todo-count 0)))
    (error (e)
      (list :success nil :errors (list (format nil "~A" e))))))

(defun delete-existing-label (label-ulid owner-id)
  "Delete a label.

   @param label-ulid [string] Label ULID
   @param owner-id [integer] Owner ID
   @return [plist] Success or error with :errors
   "
  (when (or (null label-ulid) (string= (string-trim '(#\Space #\Tab) label-ulid) ""))
    (return-from delete-existing-label
      (list :success nil :errors '("Label ULID is required"))))

  (handler-case
      (let ((result (delete-label label-ulid owner-id)))
        (if result
            (list :success t)
            (list :success nil :errors '("Label not found"))))
    (error (e)
      (list :success nil :errors (list (format nil "Failed to delete label: ~A" e))))))

(defun estimate-todo-count (owner-id tag-ulids)
  "Estimate TODO count matching all specified tags (AND condition).

   @param owner-id [integer] Owner ID
   @param tag-ulids [list] List of tag ULIDs
   @return [plist] Success with :count or error with :errors
   "
  (unless tag-ulids
    (return-from estimate-todo-count
      (list :success nil :errors '("tag_ulids parameter is required"))))

  (handler-case
      (let ((count (estimate-todo-count-by-tags owner-id tag-ulids)))
        (list :success t :count count))
    (error (e)
      (list :success nil :errors (list (format nil "Failed to estimate TODO count: ~A" e))))))
